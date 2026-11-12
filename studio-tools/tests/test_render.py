"""Validation and rendering, including failures and real Csound."""
import math
import os
import shutil
import unittest

from support import StudioTestCase, csd_text, real_csound_available, tree_snapshot

from studio import audio, environment, fsutil, render, staging, versioning

REAL_CSD = """<CsoundSynthesizer>
<CsOptions>
-odac -d
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 16
nchnls = 2
0dbfs = 1
instr 1
  iamp = 0.25 * p5 / 127
  aenv linseg 0, 0.02, iamp, p3 - 0.07, iamp, 0.05, 0
  asig poscil aenv, cpsmidinn(p4)
  aleft, aright pan2 asig, p7
  out aleft, aright
endin
</CsInstruments>
<CsScore>
i 1 0.0 0.5 60 100 0 0.25 0 0 0 1
i 1 0.5 0.5 67 100 0 0.75 0 0 0 1
e 1.2
</CsScore>
</CsoundSynthesizer>
"""


class AudioAnalysisTests(StudioTestCase):
    def test_levels_of_known_signals(self):
        for bits in (16, 24):
            path = self.tmp / f"sine{bits}.wav"
            audio.write_test_wav(path, seconds=0.25, amplitude=0.5, bits=bits)
            result = audio.analyze(path)
            self.assertEqual((result["sample_rate"], result["channels"], result["bits"]), (48000, 2, bits))
            self.assertEqual(result["frames"], 12000)
            self.assertAlmostEqual(result["duration_seconds"], 0.25)
            self.assertAlmostEqual(result["peak_dbfs"], -6.02, delta=0.05)
            self.assertAlmostEqual(result["rms_dbfs"], -9.03, delta=0.1)
            self.assertEqual(result["clipped_samples"], 0)

    def test_silence_and_clipping(self):
        silent = self.tmp / "silent.wav"
        audio.write_test_wav(silent, seconds=0.1, amplitude=0.0)
        result = audio.analyze(silent)
        self.assertIsNone(result["peak_dbfs"])
        self.assertEqual(result["peak"], 0.0)
        loud = self.tmp / "loud.wav"
        audio.write_test_wav(loud, seconds=0.1, amplitude=1.0, frequency=12000.0)
        self.assertGreater(audio.analyze(loud)["clipped_samples"], 0)

    def test_data_hash_ignores_nothing_but_the_samples(self):
        a, b, c = (self.tmp / n for n in ("a.wav", "b.wav", "c.wav"))
        audio.write_test_wav(a, seconds=0.1, frequency=440)
        audio.write_test_wav(b, seconds=0.1, frequency=440)
        audio.write_test_wav(c, seconds=0.1, frequency=441)
        self.assertEqual(audio.analyze(a)["data_sha256"], audio.analyze(b)["data_sha256"])
        self.assertNotEqual(audio.analyze(a)["data_sha256"], audio.analyze(c)["data_sha256"])

    def test_non_audio_is_rejected(self):
        path = self.tmp / "not.wav"
        path.write_bytes(b"this is not audio at all, not even close")
        with self.assertRaises(audio.AudioError):
            audio.analyze(path)


class RenderTests(StudioTestCase):
    def stage(self, marker, name="nu"):
        return self.prepare(name=name, marker=marker, do_render=False,
                            prompt="Revise." if self.comp(name).is_dir() else "Make.")

    def test_successful_render_record(self):
        directory = self.stage("FREQ=440")
        record = render.render(directory)
        self.assertEqual(record["status"], "ok")
        self.assertEqual(record["outputs"], ["outputs/nu.flac", "outputs/nu.mp3"])
        self.assertFalse((directory / "outputs" / "nu.wav").exists())
        self.assertTrue(all(check["passed"] for check in record["checks"]))
        self.assertEqual([c["step"] for c in record["commands"]],
                         ["syntax-check", "render", "encode-flac", "encode-mp3"])
        log = (directory / "outputs" / "nu-render.log").read_text()
        self.assertIn("--syntax-check-only", log)
        self.assertIn("end of Performance", log)
        self.assertEqual(staging.load_render(directory)["status"], "ok")

    def test_keep_wav(self):
        directory = self.stage("FREQ=440")
        record = render.render(directory, keep_wav=True)
        self.assertIn("outputs/nu.wav", record["outputs"])

    def test_without_ffmpeg_the_wav_is_the_master(self):
        os.environ["STUDIO_FFMPEG"] = "none"
        environment.reset_cache()
        directory = self.stage("FREQ=440")
        record = render.render(directory)
        self.assertEqual(record["outputs"], ["outputs/nu.wav"])
        self.assertTrue(any("ffmpeg" in w for w in record["warnings"]))
        versioning.accept(self.studio, directory)
        self.assertVerified()

    def failing(self, marker, expected, **options):
        directory = self.stage(marker)
        with self.assertRaises(render.RenderError) as caught:
            render.render(directory, **options)
        self.assertIn(expected, str(caught.exception))
        record = staging.load_render(directory)
        self.assertEqual(record["status"], "failed")
        self.assertTrue((directory / "outputs" / "nu-render.log").is_file())
        with self.assertRaises(staging.StagingError):
            versioning.accept(self.studio, directory)
        return directory

    def test_syntax_error(self):
        directory = self.failing("SYNTAX_ERROR", "syntax check failed")
        self.assertIn("syntax error", (directory / "outputs" / "nu-render.log").read_text())
        self.assertFalse(self.comp().exists())

    def test_render_crash(self):
        self.failing("RENDER_FAIL", "render failed")

    def test_runtime_errors_in_log(self):
        self.failing("PERF_ERROR", "run-time errors")

    def test_no_audio_written(self):
        self.failing("NO_OUTPUT", "no audio file")

    def test_silence_is_refused_unless_allowed(self):
        directory = self.failing("SILENT", "silent")
        record = render.render(directory, allow_silence=True)
        self.assertEqual(record["status"], "ok")
        self.assertTrue(any("silent" in w for w in record["warnings"]))

    def test_clipping_is_refused_unless_allowed(self):
        directory = self.failing("CLIP", "clips")
        record = render.render(directory, allow_clipping=True)
        self.assertEqual(record["status"], "ok")

    def test_incomplete_csd_is_refused(self):
        directory = staging.new_composition(self.studio, "nu")
        (directory / "nu.csd").write_text("<CsoundSynthesizer>\n<CsInstruments>\ninstr 1\nendin\n"
                                          "</CsInstruments>\n</CsoundSynthesizer>\n")
        with self.assertRaises(render.RenderError) as caught:
            render.render(directory)
        self.assertIn("CsScore", str(caught.exception))
        (directory / "nu.csd").unlink()
        with self.assertRaises(render.RenderError):
            render.render(directory)

    def test_missing_csound_is_reported(self):
        os.environ["STUDIO_CSOUND"] = str(self.tmp / "no-such-csound")
        directory = self.stage("FREQ=440")
        with self.assertRaises(render.RenderError) as caught:
            render.render(directory)
        self.assertIn("csound was not found", str(caught.exception))

    def test_failed_revision_leaves_current_version_intact(self):
        self.make_version()
        self.make_version(marker="FREQ=550", prompt="Two.")
        before = tree_snapshot(self.comp())
        for marker in ("SYNTAX_ERROR", "RENDER_FAIL", "PERF_ERROR", "SILENT", "CLIP", "NO_OUTPUT"):
            directory = self.stage(marker)
            with self.assertRaises(render.RenderError):
                render.render(directory)
            with self.assertRaises(staging.StagingError):
                versioning.accept(self.studio, directory)
            self.assertEqual(tree_snapshot(self.comp()), before, marker)
        self.assertEqual(versioning.list_versions(self.comp()), [1])
        self.assertVerified()

    def test_failed_render_can_be_corrected_and_accepted(self):
        self.make_version()
        directory = self.stage("SYNTAX_ERROR")
        with self.assertRaises(render.RenderError):
            render.render(directory)
        (directory / "nu.csd").write_text(csd_text("FREQ=660"))
        render.render(directory)
        result = versioning.accept(self.studio, directory)
        self.assertEqual(result["archived_as"], "version-0001")
        self.assertVerified()

    def test_unrendered_or_stale_staging_cannot_be_accepted(self):
        directory = self.stage("FREQ=440")
        with self.assertRaises(staging.StagingError):
            versioning.accept(self.studio, directory)
        render.render(directory)
        (directory / "nu.csd").write_text(csd_text("FREQ=441"))
        with self.assertRaises(staging.StagingError) as caught:
            versioning.accept(self.studio, directory)
        self.assertIn("changed after the last render", str(caught.exception))
        self.assertFalse(self.comp().exists())

    def test_failing_generator_is_reported_and_logged(self):
        directory = self.stage("FREQ=440")
        with self.assertRaises(staging.StagingError):
            staging.run_command(directory, ["python", "-c", "import sys; print('boom'); sys.exit(3)"])
        self.assertIn("boom", (directory / "outputs" / "nu-generate.log").read_text())
        self.assertEqual(staging.load_runs(directory)[0]["exit_code"], 3)

    def test_rerender_of_stored_versions(self):
        self.make_version()
        self.make_version(marker="FREQ=550", prompt="Two.")
        before = tree_snapshot(self.comp())
        for version in (None, 1):
            result = render.rerender(self.studio, "nu", version)
            self.assertEqual(result["audio"], "identical to the stored render")
        self.assertEqual(tree_snapshot(self.comp()), before)
        self.assertEqual(list(self.studio.staging_root.iterdir()), [])


@unittest.skipUnless(real_csound_available(), "csound is not installed")
class RealCsoundTests(StudioTestCase):
    """The same pipeline with the real Csound (and ffmpeg when present)."""

    use_fake_tools = False

    def test_render_real_audio(self):
        directory = staging.new_composition(self.studio, "nu")
        staging.add_prompt(directory, "Two notes.")
        (directory / "nu.csd").write_text(REAL_CSD)
        record = render.render(directory)
        analysis = record["analysis"]
        self.assertEqual(analysis["sample_rate"], 48000)
        self.assertEqual(analysis["channels"], 2)
        self.assertEqual(analysis["bits"], 24)
        self.assertAlmostEqual(analysis["duration_seconds"], 1.2, delta=0.01)
        self.assertGreater(analysis["peak_dbfs"], -30)
        self.assertLess(analysis["peak_dbfs"], 0)
        # The .csd asks for -odac; the studio must still have written a file.
        self.assertTrue(any("real-time output" in w for w in record["warnings"]))
        if shutil.which("ffmpeg"):
            self.assertEqual(record["outputs"], ["outputs/nu.flac", "outputs/nu.mp3"])
            self.assertEqual((directory / "outputs" / "nu.flac").read_bytes()[:4], b"fLaC")
            self.assertGreater((directory / "outputs" / "nu.mp3").stat().st_size, 1000)
        else:
            self.assertEqual(record["outputs"], ["outputs/nu.wav"])
        versioning.accept(self.studio, directory, ai_model="test")
        self.assertVerified()
        self.assertEqual(environment.csound_info()["major"], 7)
        result = render.rerender(self.studio, "nu")
        self.assertEqual(result["audio"], "identical to the stored render")

    def test_real_syntax_error(self):
        directory = staging.new_composition(self.studio, "nu")
        (directory / "nu.csd").write_text(REAL_CSD.replace("poscil aenv", "no_such_opcode aenv"))
        with self.assertRaises(render.RenderError) as caught:
            render.render(directory)
        self.assertIn("syntax check failed", str(caught.exception))


if __name__ == "__main__":
    unittest.main()
