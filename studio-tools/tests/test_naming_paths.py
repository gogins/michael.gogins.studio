"""Naming conventions and protection against writes outside the studio."""
import os
import unittest

from support import StudioTestCase

from studio import naming, publish, staging
from studio.cli import main as cli_main
from studio.paths import AUTHORIZED_RELS, PathSafetyError, StudioError, check_relative, safe_join


class NamingTests(unittest.TestCase):
    def test_valid_composition_names(self):
        for name in ("nu", "Nu2", "piece_7", "A", "x" * 64, "2026_study"):
            self.assertEqual(naming.validate_composition_name(name), name)

    def test_invalid_composition_names(self):
        bad = ["", ".", "..", "../x", "a/b", "a\\b", "a b", "a-b", "a.b", ".hidden", "_x", "x" * 65,
               "nu\n", "nu\x00", "~nu", "/nu", "version-0001", "ñu", None, 7]
        for name in bad:
            with self.assertRaises(naming.NamingError, msg=repr(name)):
                naming.validate_composition_name(name)

    def test_file_names(self):
        self.assertEqual(naming.csd_name("nu"), "nu.csd")
        self.assertEqual(naming.manifest_name("nu"), "nu-manifest.json")
        self.assertEqual(naming.prompt_name("nu", 1), "nu-prompt-001.md")
        self.assertEqual(naming.prompt_name("nu", 1000), "nu-prompt-1000.md")
        self.assertEqual(naming.parse_prompt_number("nu", "nu-prompt-012.md"), 12)
        self.assertIsNone(naming.parse_prompt_number("nu", "nux-prompt-012.md"))
        self.assertIsNone(naming.parse_prompt_number("nu", "nu-prompt-12.md"))
        with self.assertRaises(naming.NamingError):
            naming.prompt_name("nu", 0)

    def test_version_directories(self):
        self.assertEqual(naming.version_dir_name(1), "version-0001")
        self.assertEqual(naming.version_dir_name(12345), "version-12345")
        self.assertEqual(naming.parse_version_dir("version-0042"), 42)
        self.assertEqual(naming.parse_version_dir("version-12345"), 12345)
        for bad in ("version-1", "version-0000", "version-00042", "Version-0001", "version-0001x",
                    "current", "version-", "version-00a1"):
            self.assertIsNone(naming.parse_version_dir(bad), bad)
        with self.assertRaises(naming.NamingError):
            naming.version_dir_name(0)

    def test_classification(self):
        kind = lambda rel: (naming.classify("nu", rel) or {}).get("kind")
        self.assertEqual(kind("nu.csd"), "csd")
        self.assertEqual(kind("nu-manifest.json"), "manifest")
        self.assertEqual(kind("nu-prompt-003.md"), "prompts")
        self.assertEqual(kind("code/nu-generate.py"), "code")
        self.assertEqual(kind("code/nu-generate.mjs"), "code")
        self.assertEqual(kind("code/nu-score.cpp"), "code")
        self.assertEqual(kind("outputs/nu.mid"), "derived")
        self.assertEqual(kind("outputs/nu.musicxml"), "derived")
        self.assertEqual(kind("outputs/nu.pdf"), "derived")
        self.assertEqual(kind("outputs/nu-render.log"), "logs")
        for rel in ("other.csd", "nu.orc", "notes.txt", "code/generate.py", "outputs/other.mid",
                    "outputs/nu", "current/nu.csd", "code/.hidden.py", "outputs/nu .mid", "nu-prompt-1.md"):
            self.assertIsNone(naming.classify("nu", rel), rel)

    def test_soundfiles_are_never_part_of_a_composition(self):
        good = ["nu.csd", "nu-manifest.json", "nu-prompt-001.md"]
        for rel in ("outputs/nu.wav", "outputs/nu.flac", "outputs/nu.mp3", "outputs/nu.WAV",
                    "outputs/nu.aiff", "outputs/nu.ogg", "outputs/nu.opus", "outputs/nu.m4a",
                    "outputs/nu-take2.wav", "nu.wav", "code/nu-sample.flac"):
            self.assertIsNone(naming.classify("nu", rel), rel)
            self.assertTrue(naming.is_soundfile(rel), rel)
            problems = naming.state_problems("nu", good + [rel])
            self.assertEqual(len(problems), 1, rel)
            self.assertIn("soundfiles are not stored in the archive", problems[0])
        for rel in ("nu.csd", "outputs/nu.mid", "outputs/nu.pdf", "outputs/nu-render.log", "code/nu-wave.py"):
            self.assertFalse(naming.is_soundfile(rel), rel)

    def test_state_problems(self):
        good = ["nu.csd", "nu-manifest.json", "nu-prompt-001.md", "nu-prompt-002.md",
                "code/nu-generate.py", "outputs/nu.mid", "outputs/nu-render.log"]
        self.assertEqual(naming.state_problems("nu", good), [])
        self.assertTrue(naming.state_problems("nu", good + ["stray.txt"]))
        self.assertTrue(naming.state_problems("nu", [f for f in good if f != "nu.csd"]))
        self.assertTrue(naming.state_problems("nu", [f for f in good if f != "nu-manifest.json"]))
        self.assertTrue(naming.state_problems("nu", [f for f in good if f != "nu-prompt-001.md"]))
        self.assertTrue(naming.state_problems("nu", good + ["code/nu-Generate.py"]))


class PathSafetyTests(StudioTestCase):
    def test_check_relative_rejects_escapes(self):
        for rel in ("", "/etc/passwd", "~/x", "../x", "a/../../x", "a//b", "./a", "a/./b", "a\\b",
                    "a\x00b", "a/..", ".."):
            with self.assertRaises(PathSafetyError, msg=repr(rel)):
                check_relative(rel)

    def test_safe_join(self):
        base = self.root / "base"
        base.mkdir()
        self.assertEqual(safe_join(base, "a/b.txt"), base.resolve() / "a" / "b.txt")
        with self.assertRaises(PathSafetyError):
            safe_join(base, "../outside.txt")
        with self.assertRaises(PathSafetyError):
            safe_join(base, ".hidden/x")
        self.assertTrue(safe_join(base, ".hidden/x", allow_hidden=True))

    def test_safe_join_refuses_symlinks(self):
        base = self.root / "base"
        base.mkdir()
        outside = self.tmp / "outside"
        outside.mkdir()
        os.symlink(outside, base / "link")
        with self.assertRaises(PathSafetyError):
            safe_join(base, "link/file.txt")
        self.assertFalse((outside / "file.txt").exists())

    def test_authorized_directories(self):
        self.assertEqual(
            set(AUTHORIZED_RELS),
            {"music/ai-compositions", "docs/compositions", "docs/studio", "studio-tools"},
        )
        for path in (self.studio.archive, self.studio.published, self.studio.viewer,
                     self.studio.tools, self.studio.staging_root, self.studio.locks_root):
            relative = path.relative_to(self.root).as_posix()
            self.assertTrue(any(relative == a or relative.startswith(a + "/") for a in AUTHORIZED_RELS))

    def test_malicious_names_create_nothing(self):
        before = sorted(p.name for p in self.tmp.iterdir())
        for name in ("../escape", "/tmp/escape", "a/b", "..", ".staging", "nu/../../x"):
            with self.assertRaises(StudioError, msg=name):
                staging.new_composition(self.studio, name)
            with self.assertRaises(StudioError, msg=name):
                staging.stage_revision(self.studio, name)
            with self.assertRaises(StudioError, msg=name):
                publish.publish(self.studio, [name])
            self.assertNotEqual(cli_main(["--root", str(self.root), "new", name]), 0)
        self.assertEqual(sorted(p.name for p in self.tmp.iterdir()), before)
        self.assertFalse(self.studio.published.exists())
        self.assertEqual(list(self.root.rglob("*escape*")), [])

    def test_staging_argument_must_be_a_staging_directory(self):
        self.make_version()
        for bad in (str(self.comp()), str(self.root), "../..", str(self.tmp), "nope"):
            with self.assertRaises(StudioError, msg=bad):
                staging.resolve(self.studio, bad)

    def test_everything_written_stays_in_authorized_directories(self):
        self.make_version(code={"nu-generate.py": "print('x')\n"})
        self.make_version(marker="FREQ=550", prompt="Higher.")
        publish.publish(self.studio, ["nu"])
        for path in self.root.rglob("*"):
            if path.is_file():
                relative = path.relative_to(self.root).as_posix()
                self.assertTrue(
                    any(relative.startswith(a + "/") for a in AUTHORIZED_RELS),
                    f"{relative} is outside the authorized directories",
                )


if __name__ == "__main__":
    unittest.main()
