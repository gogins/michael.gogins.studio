"""Validation and rendering, including failures, real Csound, and the
viewer's Csound engine. No soundfile may be left anywhere."""
import math
import os
import shutil
import tempfile
import unittest

from support import (
    BROWSER_ENGINE, StudioTestCase, browser_engine_available, csd_text, real_csound_available,
    soundfiles, tree_snapshot,
)

from studio import audio, environment, fsutil, manifest, render, staging, versioning

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

    def setUp(self):
        super().setUp()
        # A private temporary directory, to see what rendering leaves in it.
        self.scratch = self.tmp / "scratch"
        self.scratch.mkdir()
        self._saved_tempdir = tempfile.tempdir
        tempfile.tempdir = str(self.scratch)

    def tearDown(self):
        tempfile.tempdir = self._saved_tempdir
        super().tearDown()

    def test_successful_render_record(self):
        directory = self.stage("FREQ=440")
        record = render.render(directory)
        self.assertEqual(record["status"], "ok")
        self.assertNotIn("outputs", record)
        self.assertTrue(all(check["passed"] for check in record["checks"]))
        self.assertEqual([c["step"] for c in record["commands"]], ["syntax-check", "render"])
        self.assertEqual(record["warnings"], [])
        self.assertAlmostEqual(record["analysis"]["duration_seconds"], 2.0)
        self.assertEqual(len(record["wav_sha256"]), 64)
        log = (directory / "outputs" / "nu-render.log").read_text()
        self.assertIn("--syntax-check-only", log)
        self.assertIn("end of Performance", log)
        self.assertEqual(staging.load_render(directory)["status"], "ok")

    def test_no_soundfile_is_kept(self):
        directory = self.stage("FREQ=440")
        record = render.render(directory)
        # Nothing in the staging area, nothing in the archive, nothing left in
        # the temporary directory; the record names no machine-specific path.
        self.assertEqual(soundfiles(self.root), [])
        self.assertEqual(list(self.scratch.iterdir()), [])
        self.assertEqual(record["commands"][1]["argv"][-3:], ["-o", "<temporary>/nu.wav", "nu.csd"])
        log = (directory / "outputs" / "nu-render.log").read_text()
        self.assertNotIn(str(self.scratch), log)
        versioning.accept(self.studio, directory, ai_model="test")
        self.assertEqual(soundfiles(self.root), [])
        stored = manifest.load(self.comp(), "nu")
        self.assertEqual(sorted(stored["files"]), ["nu-prompt-001.md", "nu.csd", "outputs/nu-render.log"])
        self.assertNotIn("outputs", stored["render"])
        self.assertEqual(stored["render"]["wav_sha256"], record["wav_sha256"])
        self.assertAlmostEqual(stored["render"]["analysis"]["duration_seconds"], 2.0)
        self.assertVerified()

    def test_failed_renders_leave_no_soundfile_either(self):
        for marker in ("SYNTAX_ERROR", "RENDER_FAIL", "PERF_ERROR", "SILENT", "CLIP", "NO_OUTPUT"):
            directory = self.stage(marker)
            with self.assertRaises(render.RenderError):
                render.render(directory)
            self.assertEqual(soundfiles(self.root), [], marker)
            self.assertEqual(list(self.scratch.iterdir()), [], marker)
            staging.discard(self.studio, directory)

    def test_the_wav_is_copied_out_only_on_request(self):
        directory = self.stage("FREQ=440")
        kept = self.tmp / "elsewhere" / "nu take 1.wav"
        record = render.render(directory, wav_to=kept)
        self.assertEqual(audio.analyze(kept)["data_sha256"], record["wav_sha256"])
        self.assertEqual(soundfiles(self.root), [])
        self.assertEqual(sorted(path.name for path in kept.parent.iterdir()), ["nu take 1.wav"])

    def test_a_soundfile_in_the_staging_area_is_refused(self):
        for rel in ("outputs/nu.wav", "outputs/nu.mp3", "outputs/nu.flac", "nu.wav", "code/nu-sample.aiff"):
            directory = self.stage("FREQ=440")
            path = directory / rel
            path.parent.mkdir(exist_ok=True)
            audio.write_test_wav(path, seconds=0.05)
            with self.assertRaises(render.RenderError) as caught:
                render.render(directory)
            self.assertIn("soundfiles are not stored in the archive", str(caught.exception), rel)
            staging.discard(self.studio, directory)
        self.assertFalse(self.comp().exists())

    def test_a_soundfile_added_after_rendering_cannot_be_accepted(self):
        directory = self.stage("FREQ=440")
        render.render(directory)
        audio.write_test_wav(directory / "outputs" / "nu.wav", seconds=0.05)
        with self.assertRaises(staging.StagingError) as caught:
            versioning.accept(self.studio, directory)
        self.assertIn("soundfiles are not stored", str(caught.exception))
        self.assertFalse(self.comp().exists())

    def test_a_soundfile_put_into_a_version_fails_verification(self):
        self.make_version()
        audio.write_test_wav(self.comp() / "outputs" / "nu.flac", seconds=0.05)
        problems = versioning.verify(self.studio, "nu")
        self.assertTrue(any("soundfiles are not stored" in problem for problem in problems), problems)

    def test_an_output_named_in_csoptions_is_ignored_with_a_warning(self):
        for option in ("-o nu-take.wav", "-odac", "--output=dac:1"):
            directory = self.stage("FREQ=440")
            path = directory / "nu.csd"
            path.write_text(path.read_text().replace("<CsOptions>\n-d\n", f"<CsOptions>\n-d {option}\n"))
            record = render.render(directory)
            self.assertTrue(any("names an audio output" in w for w in record["warnings"]), option)
            self.assertEqual(soundfiles(self.root), [], option)
            staging.discard(self.studio, directory)

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
        self.failing("NO_OUTPUT", "rendered no audio")

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
            self.assertEqual(result["audio"], "identical to the render recorded in the manifest")
        self.assertEqual(tree_snapshot(self.comp()), before)
        self.assertEqual(list(self.studio.staging_root.iterdir()), [])
        self.assertEqual(soundfiles(self.root), [])
        self.assertEqual(list(self.scratch.iterdir()), [])

    def test_rerender_can_write_a_soundfile_elsewhere(self):
        self.make_version()
        self.make_version(marker="FREQ=550", prompt="Two.")
        before = tree_snapshot(self.comp())
        kept = self.tmp / "masters" / "nu-1.wav"
        result = render.rerender(self.studio, "nu", 1, wav_to=kept)
        self.assertEqual(result["wav"], str(kept))
        self.assertEqual(audio.analyze(kept)["data_sha256"],
                         manifest.load(self.comp() / "version-0001", "nu")["render"]["wav_sha256"])
        self.assertEqual(tree_snapshot(self.comp()), before)
        self.assertEqual(soundfiles(self.root), [])


BROKEN_FOR_BROWSER = csd_text("FREQ=440").replace(
    "  a1 poscil", "  aNone opcode_missing_in_the_browser 1\n  a1 poscil")


@unittest.skipUnless(browser_engine_available(), "node or the viewer's Csound engine is not available")
class BrowserEngineTests(StudioTestCase):
    """The check in the Csound WebAssembly engine the viewer plays with.

    Native Csound is the fake one here, so only the engine's verdict varies.
    """

    def setUp(self):
        super().setUp()
        os.environ["STUDIO_CSOUND_WASM"] = str(BROWSER_ENGINE)
        environment.reset_cache()

    def test_a_piece_that_performs_in_the_browser_engine(self):
        directory = self.prepare(do_render=False)
        record = render.render(directory)
        browser = record["browser"]
        self.assertTrue(browser["checked"])
        self.assertTrue(browser["playable"])
        self.assertEqual(browser["engine"]["package"], "@csound/browser")
        self.assertTrue(browser["engine"]["version"].startswith("7."))
        self.assertEqual(browser["engine"]["sha256"], fsutil.sha256_file(BROWSER_ENGINE))
        self.assertAlmostEqual(browser["duration_seconds"], 2.0, delta=0.01)
        self.assertEqual(browser["sample_rate"], 48000)
        self.assertGreater(browser["realtime_factor"], 1)
        self.assertEqual(record["warnings"], [])
        self.assertTrue(any("viewer's Csound engine" in c["check"] and c["passed"] for c in record["checks"]))
        self.assertIn("Browser engine (@csound/browser", (directory / "outputs" / "nu-render.log").read_text())
        versioning.accept(self.studio, directory, ai_model="test")
        stored = manifest.load(self.comp(), "nu")
        self.assertTrue(stored["render"]["browser"]["playable"])
        self.assertEqual(stored["environment"]["browser_csound"]["version"], browser["engine"]["version"])
        self.assertEqual(soundfiles(self.root), [])
        self.assertVerified()

    def test_a_piece_the_browser_engine_cannot_perform_is_refused(self):
        directory = self.prepare(do_render=False)
        (directory / "nu.csd").write_text(BROKEN_FOR_BROWSER)
        with self.assertRaises(render.RenderError) as caught:
            render.render(directory)
        message = str(caught.exception)
        self.assertIn("does not perform in the viewer's Csound engine", message)
        self.assertIn("opcode_missing_in_the_browser", message)
        self.assertEqual(staging.load_render(directory)["status"], "failed")
        with self.assertRaises(staging.StagingError):
            versioning.accept(self.studio, directory)
        self.assertFalse(self.comp().exists())

    def test_an_unplayable_piece_can_be_kept_deliberately(self):
        directory = self.prepare(do_render=False)
        (directory / "nu.csd").write_text(BROKEN_FOR_BROWSER)
        record = render.render(directory, allow_unplayable=True)
        self.assertEqual(record["status"], "ok")
        self.assertFalse(record["browser"]["playable"])
        self.assertTrue(any("--allow-unplayable" in w for w in record["warnings"]))
        versioning.accept(self.studio, directory, ai_model="test")
        self.assertFalse(manifest.load(self.comp(), "nu")["render"]["browser"]["playable"])

    def test_run_time_errors_in_the_browser_engine_are_failures(self):
        directory = self.prepare(do_render=False)
        (directory / "nu.csd").write_text(csd_text("FREQ=440").replace(
            "  a1 poscil", "  kNone tablei 5, 999\n  a1 poscil"))
        with self.assertRaises(render.RenderError) as caught:
            render.render(directory)
        self.assertIn("does not perform in the viewer's Csound engine", str(caught.exception))

    def test_an_output_file_named_in_csoptions_is_not_written(self):
        directory = self.prepare(do_render=False)
        path = directory / "nu.csd"
        path.write_text(path.read_text().replace("<CsOptions>\n-d\n", "<CsOptions>\n-d -o nu-take.wav\n"))
        record = render.render(directory)
        self.assertTrue(record["browser"]["playable"])
        self.assertEqual(soundfiles(self.root), [])

    def test_without_node_the_check_is_skipped_with_a_warning(self):
        os.environ["STUDIO_NODE"] = "none"
        environment.reset_cache()
        directory = self.prepare(do_render=False)
        record = render.render(directory)
        self.assertEqual(record["status"], "ok")
        self.assertFalse(record["browser"]["checked"])
        self.assertTrue(any("not checked in the viewer's Csound engine" in w for w in record["warnings"]))

    def test_without_the_engine_the_check_is_skipped_with_a_warning(self):
        os.environ["STUDIO_CSOUND_WASM"] = str(self.tmp / "no-such-engine.js")
        environment.reset_cache()
        directory = self.prepare(do_render=False)
        record = render.render(directory)
        self.assertFalse(record["browser"]["checked"])
        self.assertTrue(any("not checked in the viewer's Csound engine" in w for w in record["warnings"]))
        self.assertTrue(any("engine" in problem for problem in environment.problems()))

    def test_the_installed_engine_matches_its_version_record(self):
        info = environment.browser_engine_info()
        self.assertEqual((info["package"], info["available"]), ("@csound/browser", True))
        import json
        recorded = json.loads((BROWSER_ENGINE.parent / "VERSION.json").read_text())
        for name, entry in recorded["files"].items():
            self.assertEqual(fsutil.sha256_file(BROWSER_ENGINE.parent / name), entry["sha256"], name)


@unittest.skipUnless(real_csound_available(), "csound is not installed")
class RealCsoundTests(StudioTestCase):
    """The same pipeline with the real Csound, and the viewer's engine when present."""

    use_fake_tools = False

    def setUp(self):
        super().setUp()
        if browser_engine_available():
            os.environ["STUDIO_CSOUND_WASM"] = str(BROWSER_ENGINE)
            environment.reset_cache()

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
        # The .csd asks for -odac; the studio must still have rendered and measured it.
        self.assertTrue(any("names an audio output" in w for w in record["warnings"]))
        self.assertEqual(soundfiles(self.root), [])
        if browser_engine_available():
            # Native Csound and the viewer's engine agree about the piece.
            self.assertTrue(record["browser"]["playable"])
            self.assertAlmostEqual(record["browser"]["duration_seconds"], analysis["duration_seconds"], delta=0.01)
            self.assertEqual(record["browser"]["sample_rate"], analysis["sample_rate"])
        versioning.accept(self.studio, directory, ai_model="test")
        self.assertVerified()
        self.assertEqual(environment.csound_info()["major"], 7)
        kept = self.tmp / "kept.wav"
        result = render.rerender(self.studio, "nu", wav_to=kept)
        self.assertEqual(result["audio"], "identical to the render recorded in the manifest")
        self.assertEqual(kept.read_bytes()[:4], b"RIFF")
        self.assertEqual(soundfiles(self.root), [])

    def test_real_syntax_error(self):
        directory = staging.new_composition(self.studio, "nu")
        (directory / "nu.csd").write_text(REAL_CSD.replace("poscil aenv", "no_such_opcode aenv"))
        with self.assertRaises(render.RenderError) as caught:
            render.render(directory)
        self.assertIn("syntax check failed", str(caught.exception))


if __name__ == "__main__":
    unittest.main()
