"""Version allocation, archival, prompts, manifests and immutability."""
import json
import os
import threading
import unittest

from support import StudioTestCase, csd_text, tree_snapshot

from studio import fsutil, manifest as manifest_module, naming, render, staging, versioning
from studio.fsutil import IntegrityError
from studio.paths import StudioError


class VersioningTests(StudioTestCase):
    def test_new_composition_has_no_version_directories(self):
        result = self.make_version()
        self.assertEqual(result["ordinal"], 1)
        self.assertIsNone(result["archived_as"])
        self.assertEqual(versioning.list_versions(self.comp()), [])
        self.assertEqual(
            fsutil.list_files(self.comp()),
            ["nu-manifest.json", "nu-prompt-001.md", "nu.csd", "outputs/nu-render.log"],
        )
        self.assertFalse((self.comp() / "current").exists())
        self.assertVerified()

    def test_versions_are_allocated_sequentially(self):
        self.make_version()
        for expected in range(1, 6):
            result = self.make_version(marker=f"FREQ={440 + expected}", prompt=f"Revision {expected}.")
            self.assertEqual(result["archived_as"], naming.version_dir_name(expected))
            self.assertEqual(result["ordinal"], expected + 1)
            self.assertEqual(versioning.list_versions(self.comp()), list(range(1, expected + 1)))
        self.assertVerified()

    def test_archived_version_is_the_previous_current_state(self):
        self.make_version(code={"nu-generate.py": "print('one')\n"})
        before = tree_snapshot(self.comp())
        before_hash = self.current_hash()
        self.make_version(marker="FREQ=550", prompt="Higher.")
        archived = self.comp() / "version-0001"
        self.assertEqual(tree_snapshot(archived), before)
        self.assertEqual(fsutil.state_hash(archived), before_hash)
        # Code, outputs, prompts and manifest all travel with the version.
        for rel in ("nu.csd", "nu-manifest.json", "nu-prompt-001.md", "code/nu-generate.py",
                    "outputs/nu-render.log"):
            self.assertTrue((archived / rel).is_file(), rel)
        self.assertIn("FREQ=440", (archived / "nu.csd").read_text())
        self.assertIn("FREQ=550", (self.comp() / "nu.csd").read_text())

    def test_archived_versions_never_change_afterwards(self):
        self.make_version()
        self.make_version(marker="FREQ=550", prompt="Two.")
        first = tree_snapshot(self.comp() / "version-0001")
        self.make_version(marker="FREQ=660", prompt="Three.")
        second = tree_snapshot(self.comp() / "version-0002")
        self.make_version(marker="FREQ=770", prompt="Four.")
        self.assertEqual(tree_snapshot(self.comp() / "version-0001"), first)
        self.assertEqual(tree_snapshot(self.comp() / "version-0002"), second)

    def test_archived_files_are_read_only(self):
        self.make_version()
        self.make_version(marker="FREQ=550", prompt="Two.")
        path = self.comp() / "version-0001" / "nu.csd"
        self.assertFalse(os.access(path, os.W_OK) and os.getuid() != 0)

    def test_verify_detects_modified_archive(self):
        self.make_version()
        self.make_version(marker="FREQ=550", prompt="Two.")
        target = self.comp() / "version-0001" / "nu.csd"
        target.chmod(0o644)
        target.write_text(csd_text("FREQ=999"))
        problems = versioning.verify(self.studio, "nu")
        self.assertTrue(any("version-0001" in p and "modified" in p for p in problems), problems)

    def test_verify_detects_added_removed_and_missing(self):
        self.make_version()
        self.make_version(marker="FREQ=550", prompt="Two.")
        (self.comp() / "version-0001" / "outputs" / "nu-extra.mid").write_bytes(b"MThd")
        self.assertTrue(versioning.verify(self.studio, "nu"))
        (self.comp() / "version-0001" / "outputs" / "nu-extra.mid").unlink()
        self.assertVerified()
        (self.comp() / "stray.txt").write_text("x")
        self.assertTrue(any("stray.txt" in p for p in versioning.verify(self.studio, "nu")))
        (self.comp() / "stray.txt").unlink()
        fsutil.remove_tree(self.comp() / "version-0001")
        self.assertTrue(any("version" in p for p in versioning.verify(self.studio, "nu")))

    def test_existing_version_directory_is_never_overwritten(self):
        self.make_version()
        rogue = self.comp() / "version-0001"
        rogue.mkdir()
        (rogue / "keep.txt").write_text("precious")
        directory = self.prepare(marker="FREQ=550", prompt="Two.")
        with self.assertRaises(StudioError):
            versioning.accept(self.studio, directory)
        self.assertEqual((rogue / "keep.txt").read_text(), "precious")
        self.assertIn("FREQ=440", (self.comp() / "nu.csd").read_text())

    def test_version_numbers_are_not_reused_after_a_gap(self):
        self.make_version()
        self.make_version(marker="FREQ=550", prompt="Two.")
        self.make_version(marker="FREQ=660", prompt="Three.")
        fsutil.remove_tree(self.comp() / "version-0001")
        directory = self.prepare(marker="FREQ=770", prompt="Four.")
        with self.assertRaises(IntegrityError):
            versioning.accept(self.studio, directory)
        self.assertEqual(versioning.list_versions(self.comp()), [2])

    def test_duplicate_composition_is_refused(self):
        self.make_version()
        with self.assertRaises(StudioError):
            staging.new_composition(self.studio, "nu")
        with self.assertRaises(StudioError):
            staging.new_composition(self.studio, "NU")

    def test_two_new_stagings_of_one_name(self):
        first = self.prepare()
        second = staging.new_composition(self.studio, "nu")
        staging.add_prompt(second, "Other.")
        (second / "nu.csd").write_text(csd_text("FREQ=880"))
        render.render(second)
        versioning.accept(self.studio, first)
        with self.assertRaises(versioning.ConflictError):
            versioning.accept(self.studio, second)
        self.assertIn("FREQ=440", (self.comp() / "nu.csd").read_text())


class PromptTests(StudioTestCase):
    def test_prompts_are_numbered_in_order_and_verbatim(self):
        directory = staging.new_composition(self.studio, "nu")
        texts = ["First request.\nWith two lines.", "  Second: keep *this* <exactly>  \n", "Third — ünïcode ♪"]
        for text in texts:
            staging.add_prompt(directory, text)
        for number, text in enumerate(texts, start=1):
            stored = (directory / f"nu-prompt-{number:03d}.md").read_text(encoding="utf-8")
            self.assertTrue(stored.startswith(f"# nu - prompt {number:03d}\n"))
            self.assertIn(text, stored)
            self.assertFalse(stored.startswith("---"))

    def test_prompts_accumulate_across_versions(self):
        self.make_version(prompt="One.")
        self.make_version(marker="FREQ=550", prompt="Two.")
        directory = self.prepare(marker="FREQ=660", prompt="Three.")
        staging.add_prompt(directory, "Three and a half.")
        render.render(directory)
        versioning.accept(self.studio, directory)
        current = manifest_module.load(self.comp(), "nu")
        self.assertEqual(current["prompts"], [f"nu-prompt-00{n}.md" for n in (1, 2, 3, 4)])
        self.assertEqual(current["new_prompts"], ["nu-prompt-003.md", "nu-prompt-004.md"])
        first = manifest_module.load(self.comp() / "version-0001", "nu")
        second = manifest_module.load(self.comp() / "version-0002", "nu")
        self.assertEqual(first["prompts"], ["nu-prompt-001.md"])
        self.assertEqual(second["prompts"], ["nu-prompt-001.md", "nu-prompt-002.md"])
        self.assertEqual(second["new_prompts"], ["nu-prompt-002.md"])
        # Each earlier prompt is byte-identical in every later version.
        for rel in ("nu-prompt-001.md", "nu-prompt-002.md"):
            self.assertEqual(
                (self.comp() / rel).read_bytes(), (self.comp() / "version-0002" / rel).read_bytes()
            )
        self.assertIn("One.", (self.comp() / "nu-prompt-001.md").read_text())
        self.assertIn("Three and a half.", (self.comp() / "nu-prompt-004.md").read_text())

    def test_revision_without_a_new_prompt_is_refused(self):
        self.make_version()
        directory = self.prepare(marker="FREQ=550", prompt=None)
        with self.assertRaises(staging.StagingError):
            versioning.accept(self.studio, directory)
        self.assertEqual(versioning.list_versions(self.comp()), [])

    def test_changing_or_deleting_an_earlier_prompt_is_refused(self):
        self.make_version()
        before = tree_snapshot(self.comp())
        directory = self.prepare(marker="FREQ=550", prompt="Two.")
        (directory / "nu-prompt-001.md").write_text("rewritten history")
        with self.assertRaises(staging.StagingError):
            versioning.accept(self.studio, directory)
        (directory / "nu-prompt-001.md").unlink()
        with self.assertRaises(staging.StagingError):
            versioning.accept(self.studio, directory)
        self.assertEqual(tree_snapshot(self.comp()), before)

    def test_empty_prompt_is_refused(self):
        directory = staging.new_composition(self.studio, "nu")
        with self.assertRaises(staging.StagingError):
            staging.add_prompt(directory, "   \n")


class ManifestTests(StudioTestCase):
    def test_manifest_contents(self):
        os.environ["STUDIO_AI_AGENT"] = "test-agent"
        directory = self.prepare(code={"nu-generate.py": "import CsoundAC\n"})
        staging.run_command(directory, ["python", "-c", "print('generated')"], seed=42,
                            parameters={"density": "3"})
        render.render(directory)
        versioning.accept(self.studio, directory, ai_model="test-model", notes="a note",
                          dependencies=["samples:piano.sf2"])
        manifest = manifest_module.load(self.comp(), "nu")
        self.assertEqual(manifest_module.problems(manifest, "nu"), [])
        self.assertEqual(manifest["ordinal"], 1)
        self.assertEqual(manifest["ai"], {"model": "test-model", "agent": "test-agent"})
        self.assertEqual(manifest["generator"]["kind"], "python-csoundac")
        self.assertEqual(manifest["generator"]["sources"], ["code/nu-generate.py"])
        self.assertEqual(manifest["generator"]["seeds"], [42])
        self.assertEqual(manifest["generator"]["runs"][0]["argv"][0], "python")
        self.assertEqual(manifest["generator"]["runs"][0]["parameters"], {"density": "3"})
        self.assertEqual(manifest["notes"], "a note")
        self.assertIn("piano.sf2", manifest["dependencies"]["samples"])
        self.assertEqual(manifest["validation"]["status"], "passed")
        self.assertTrue(manifest["render"]["commands"])
        self.assertTrue(manifest["environment"]["csound"]["version"])
        self.assertIn("duration_seconds", manifest["render"]["analysis"])
        # The manifest hashes every other file of the version.
        files = [f for f in fsutil.list_files(self.comp()) if f != "nu-manifest.json"]
        self.assertEqual(sorted(manifest["files"]), files)
        for rel in files:
            self.assertEqual(manifest["files"][rel]["sha256"], fsutil.sha256_file(self.comp() / rel))
        self.assertIn("generated", (self.comp() / "outputs" / "nu-generate.log").read_text())

    def test_manifest_holds_no_score_and_no_home_directory(self):
        self.make_version()
        text = (self.comp() / "nu-manifest.json").read_text()
        self.assertNotIn(os.path.expanduser("~"), text)
        self.assertNotIn("i 1 0", text)
        self.assertNotIn("CsScore", text)

    def test_history_links_every_archived_version(self):
        self.make_version()
        self.make_version(marker="FREQ=550", prompt="Two.")
        self.make_version(marker="FREQ=660", prompt="Three.")
        manifest = manifest_module.load(self.comp(), "nu")
        self.assertEqual([h["version"] for h in manifest["history"]], ["version-0001", "version-0002"])
        for entry in manifest["history"]:
            self.assertEqual(entry["tree_sha256"], fsutil.state_hash(self.comp() / entry["version"]))

    def test_invalid_manifests_are_detected(self):
        self.make_version()
        good = manifest_module.load(self.comp(), "nu")
        for mutate in (
            lambda m: m.pop("files"),
            lambda m: m.update(schema="other"),
            lambda m: m.update(composition="other"),
            lambda m: m.update(ordinal=3),
            lambda m: m.update(prompts=["nu-prompt-002.md"]),
            lambda m: m.update(new_prompts=[]),
            lambda m: m["files"].update({"nu-manifest.json": {"sha256": "x"}}),
        ):
            broken = json.loads(json.dumps(good))
            mutate(broken)
            self.assertTrue(manifest_module.problems(broken, "nu"))
        (self.comp() / "nu-manifest.json").write_text("{ not json")
        self.assertTrue(versioning.verify(self.studio, "nu"))


class ConcurrencyTests(StudioTestCase):
    def test_second_revision_from_the_same_base_conflicts(self):
        self.make_version()
        first = self.prepare(marker="FREQ=550", prompt="Agent A.")
        second = self.prepare(marker="FREQ=660", prompt="Agent B.")
        versioning.accept(self.studio, first)
        after_first = tree_snapshot(self.comp())
        with self.assertRaises(versioning.ConflictError):
            versioning.accept(self.studio, second)
        self.assertEqual(tree_snapshot(self.comp()), after_first)
        self.assertTrue(second.is_dir(), "the losing revision is kept for its author")
        self.assertEqual(versioning.list_versions(self.comp()), [1])
        self.assertVerified()

    def test_simultaneous_accepts_yield_exactly_one_new_version(self):
        self.make_version()
        stagings = [self.prepare(marker=f"FREQ={500 + i}", prompt=f"Agent {i}.") for i in range(4)]
        outcomes = []
        barrier = threading.Barrier(len(stagings))

        def worker(directory):
            barrier.wait()
            try:
                versioning.accept(self.studio, directory)
                outcomes.append("ok")
            except (versioning.ConflictError, versioning.LockedError) as error:
                outcomes.append(type(error).__name__)

        threads = [threading.Thread(target=worker, args=(d,)) for d in stagings]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()
        self.assertEqual(outcomes.count("ok"), 1, outcomes)
        self.assertEqual(versioning.list_versions(self.comp()), [1])
        self.assertVerified()

    def test_held_lock_blocks_and_is_released(self):
        self.make_version()
        directory = self.prepare(marker="FREQ=550", prompt="Two.")
        with versioning.CompositionLock(self.studio, "nu"):
            with self.assertRaises(versioning.LockedError):
                versioning.accept(self.studio, directory)
        versioning.accept(self.studio, directory)
        self.assertVerified()

    def test_lock_of_a_dead_process_is_taken_over(self):
        self.make_version()
        import socket
        lock = self.studio.locks_root / "nu.lock"
        lock.parent.mkdir(parents=True, exist_ok=True)
        lock.write_text(json.dumps({"pid": 2 ** 22 + 12345, "host": socket.gethostname(), "token": "dead"}))
        self.make_version(marker="FREQ=550", prompt="Two.")
        self.assertFalse(lock.exists())

    def test_lock_from_another_machine_needs_break_lock(self):
        self.make_version()
        lock = self.studio.locks_root / "nu.lock"
        lock.parent.mkdir(parents=True, exist_ok=True)
        lock.write_text(json.dumps({"pid": 1, "host": "some-other-machine", "token": "x"}))
        directory = self.prepare(marker="FREQ=550", prompt="Two.")
        with self.assertRaises(versioning.LockedError):
            versioning.accept(self.studio, directory)
        versioning.recover(self.studio, "nu", break_lock=True)
        versioning.accept(self.studio, directory)
        self.assertVerified()


if __name__ == "__main__":
    unittest.main()
