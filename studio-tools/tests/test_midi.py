"""General MIDI files written from a Csound score."""
import unittest

from support import StudioTestCase

from studio import fsutil, midi, versioning
from studio.cli import main as cli_main


CSD = """\
<CsoundSynthesizer>
<CsInstruments>
sr = 48000
; gm 2 49
; gm 10 drums
instr 1
endin
</CsInstruments>
<CsScore>
t 0 60
i 1 1 2 60 100 0 0.5
i 2 0 1 64 80
i 10 0.5 0.2 36 100
i 99 0 5
e
</CsScore>
</CsoundSynthesizer>
"""


class SequenceTests(unittest.TestCase):
    def test_general_midi_notes_programs_and_timing(self):
        data, report = midi.sequence_from_csd(CSD)
        self.assertEqual(report["notes"], 3)
        self.assertEqual(report["duration_seconds"], 3.0)
        self.assertEqual(report["tempo_bpm"], 120)
        by_instr = {item["instrument"]: item for item in report["instruments"]}
        self.assertEqual(by_instr["1"]["program"], 1)
        self.assertEqual(by_instr["1"]["channel"], 2)
        self.assertEqual(by_instr["2"]["program"], 49)
        self.assertEqual(by_instr["2"]["channel"], 1)
        self.assertEqual(by_instr["10"]["program"], "drums")
        self.assertEqual(by_instr["10"]["channel"], 10)
        self.assertNotIn("99", by_instr)

        events = midi.events_from_smf(data)
        tempos = [event["tempo"] for event in events if "tempo" in event]
        self.assertEqual(tempos, [500000])
        ons = [event for event in events if event.get("kind") == "on"]
        self.assertEqual([(event["tick"], event["channel"], event["key"], event["velocity"]) for event in ons], [
            (0, 0, 64, 80),
            (480, 9, 36, 100),
            (960, 1, 60, 100),
        ])
        offs = [event for event in events if event.get("kind") == "off"]
        self.assertIn((2880, 1, 60), [(event["tick"], event["channel"], event["key"]) for event in offs])
        programs = [event for event in events if event.get("kind") == "program"]
        self.assertEqual({(event["channel"], event["program"]) for event in programs}, {(0, 48), (1, 0)})
        pans = [event for event in events if event.get("kind") == "cc"]
        self.assertEqual(pans, [{"tick": 960, "kind": "cc", "channel": 1, "controller": 10, "value": 64}])

    def test_a_tempo_other_than_60_is_warned_and_still_written_as_seconds(self):
        text = CSD.replace("t 0 60", "t 0 90")
        _data, report = midi.sequence_from_csd(text)
        self.assertTrue(any("seconds" in warning for warning in report["warnings"]))
        ons = [event for event in midi.events_from_smf(_data) if event.get("kind") == "on"]
        self.assertEqual(ons[0]["tick"], 0)

    def test_program_out_of_range_is_refused(self):
        with self.assertRaises(midi.MidiError):
            midi.sequence_from_csd("; gm 1 200\n<CsScore>\ni 1 0 1 60 80\n</CsScore>\n")

    def test_no_notes_is_refused(self):
        with self.assertRaises(midi.MidiError):
            midi.sequence_from_csd("<CsScore>\ni 99 0 10\n</CsScore>\n")

    def test_an_instrument_without_a_program_is_skipped(self):
        _data, report = midi.sequence_from_csd(
            "<CsScore>\ni 1 0 1 60 80\ni 200 0 1 60 80\n</CsScore>\n"
        )
        self.assertEqual(report["notes"], 1)
        self.assertTrue(any("200" in warning for warning in report["warnings"]))


class CommandTests(StudioTestCase):
    def test_staging_file_is_kept_when_the_version_is_accepted(self):
        directory = self.prepare("nu")
        report = midi.generate(self.studio, str(directory))
        self.assertEqual(report["file"], "outputs/nu.mid")
        self.assertFalse(report["manifest_updated"])
        versioning.accept(self.studio, directory, ai_model="test-model")
        self.assertTrue((self.comp("nu") / "outputs" / "nu.mid").is_file())
        self.assertVerified()

    def test_a_score_with_no_general_midi_notes_drops_a_stale_file(self):
        from support import csd_text
        directory = self.prepare("nu")
        destination = directory / "outputs" / "nu.mid"
        self.assertTrue(destination.is_file())
        text = csd_text().replace("i 1 0 2 60 80", "i 99 0 2")
        (directory / "nu.csd").write_text(text, encoding="utf-8")
        report = midi.write_alongside(directory, "nu")
        self.assertIsNone(report["file"])
        self.assertEqual(report["notes"], 0)
        self.assertFalse(destination.is_file())

    def test_current_version_updates_the_manifest(self):
        self.make_version()
        manifest_path = self.comp("nu") / "nu-manifest.json"
        destination = self.comp("nu") / "outputs" / "nu.mid"
        destination.unlink()
        manifest = fsutil.read_json(manifest_path)
        del manifest["files"]["outputs/nu.mid"]
        fsutil.atomic_write_json(manifest_path, manifest)
        before = fsutil.sha256_file(manifest_path)
        report = midi.generate(self.studio, "nu")
        self.assertTrue(report["manifest_updated"])
        self.assertNotEqual(before, fsutil.sha256_file(manifest_path))
        manifest = fsutil.read_json(manifest_path)
        self.assertIn("outputs/nu.mid", manifest["files"])
        self.assertVerified()
        self.assertEqual(cli_main(["--root", str(self.root), "midi", "nu"]), 0)
        self.assertVerified()

    def test_unknown_target_is_an_error(self):
        self.assertEqual(cli_main(["--root", str(self.root), "midi", "missing"]), 1)


if __name__ == "__main__":
    unittest.main()
