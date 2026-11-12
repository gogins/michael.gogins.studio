"""Interrupted version operations: fault injection and recovery."""
import os
import subprocess
import sys
import unittest

from support import TOOLS, StudioTestCase, tree_snapshot

from studio import fsutil, staging, versioning

#: Where the transaction can be interrupted, and where recovery must land.
CHECKPOINTS = [
    ("journal-prepared", "old"),
    ("archive-copied", "old"),
    ("archive-renamed", "new"),
    ("journal-archived", "new"),
    ("promote-first-entry", "new"),
    ("promoted", "new"),
    ("journal-removed", "new"),
]


def visible(snapshot):
    """A snapshot without the tools' hidden working files."""
    return {rel: digest for rel, digest in snapshot.items()
            if not any(part.startswith(".") for part in rel.split("/"))}


class RecoveryTests(StudioTestCase):
    def setUp(self):
        super().setUp()
        self.make_version(code={"nu-generate.py": "print('one')\n"})
        self.make_version(marker="FREQ=550", prompt="Two.", code={"nu-generate.py": "print('two')\n"})
        self.old = tree_snapshot(self.comp())
        self.old_hash = self.current_hash()
        # The revision drops the code directory and so exercises removal too.
        self.directory = self.prepare(marker="FREQ=660", prompt="Three.")
        fsutil.remove_tree(self.directory / "code")
        from studio import render
        render.render(self.directory)

    def interrupt_at(self, step):
        def hook(reached):
            if reached == step:
                raise versioning.InjectedFault(step)
        versioning.set_fault_hook(hook)
        with self.assertRaises(versioning.InjectedFault):
            versioning.accept(self.studio, self.directory)
        versioning.set_fault_hook(None)

    def assertOldState(self):
        self.assertEqual(visible(tree_snapshot(self.comp())), self.old)
        self.assertEqual(self.current_hash(), self.old_hash)
        self.assertEqual(versioning.list_versions(self.comp()), [1])

    def assertNewState(self):
        self.assertEqual(versioning.list_versions(self.comp()), [1, 2])
        self.assertEqual(fsutil.state_hash(self.comp() / "version-0002"), self.old_hash)
        self.assertIn("FREQ=660", (self.comp() / "nu.csd").read_text())
        self.assertFalse((self.comp() / "code").exists())
        self.assertEqual(
            {rel for rel in self.old if rel.startswith("version-0001/")},
            {rel for rel in tree_snapshot(self.comp()) if rel.startswith("version-0001/")},
        )

    def assertClean(self):
        leftovers = [p.name for p in self.comp().iterdir() if p.name.startswith(".")]
        self.assertEqual(leftovers, [])
        self.assertFalse((self.studio.locks_root / "nu.lock").exists())
        self.assertVerified()

    def test_recovery_from_every_interruption_point(self):
        for step, expected in CHECKPOINTS:
            with self.subTest(step=step):
                self.tearDown()
                self.setUp()
                self.interrupt_at(step)
                if step != "journal-removed":
                    # Until recovery, everything else is refused.
                    self.assertTrue(versioning.verify(self.studio, "nu"))
                    with self.assertRaises(versioning.PendingTransactionError):
                        staging.stage_revision(self.studio, "nu")
                    with self.assertRaises(versioning.PendingTransactionError):
                        versioning.accept(self.studio, self.directory)
                result = versioning.recover(self.studio, "nu")
                if expected == "old":
                    self.assertEqual(result["action"], "rolled-back")
                    self.assertOldState()
                else:
                    self.assertNewState()
                self.assertClean()
                # Recovery is idempotent.
                self.assertEqual(versioning.recover(self.studio, "nu")["action"], "none")
                self.assertClean()

    def test_work_continues_with_correct_numbers_after_recovery(self):
        for step, expected in CHECKPOINTS:
            with self.subTest(step=step):
                self.tearDown()
                self.setUp()
                self.interrupt_at(step)
                versioning.recover(self.studio, "nu")
                if expected == "old":
                    # The interrupted revision is still staged and can be accepted.
                    result = versioning.accept(self.studio, self.directory)
                    self.assertEqual(result["archived_as"], "version-0002")
                    self.assertNewState()
                else:
                    with self.assertRaises(versioning.ConflictError):
                        versioning.accept(self.studio, self.directory)
                    result = self.make_version(marker="FREQ=770", prompt="Four.")
                    self.assertEqual(result["archived_as"], "version-0003")
                self.assertClean()

    def test_explicit_rollback_from_every_interruption_point(self):
        for step, _expected in CHECKPOINTS[:-1]:
            with self.subTest(step=step):
                self.tearDown()
                self.setUp()
                self.interrupt_at(step)
                result = versioning.recover(self.studio, "nu", rollback=True)
                self.assertEqual(result["action"], "rolled-back")
                self.assertOldState()
                self.assertClean()

    def test_recovery_when_staged_copy_was_lost(self):
        self.interrupt_at("archive-renamed")
        for entry in self.comp().iterdir():
            if entry.name.startswith(".incoming-"):
                fsutil.remove_tree(entry)
        result = versioning.recover(self.studio, "nu")
        self.assertEqual(result["action"], "rolled-back")
        self.assertOldState()
        self.assertClean()

    def test_recovery_when_promotion_was_half_done_and_new_files_are_damaged(self):
        self.interrupt_at("promote-first-entry")
        (self.comp() / "nu.csd").write_text("damaged")
        for entry in self.comp().iterdir():
            if entry.name.startswith(".incoming-"):
                fsutil.remove_tree(entry)
        result = versioning.recover(self.studio, "nu")
        self.assertEqual(result["action"], "rolled-back")
        self.assertOldState()
        self.assertClean()

    def test_unrecoverable_damage_is_reported_not_hidden(self):
        self.interrupt_at("journal-prepared")
        (self.comp() / "nu.csd").write_text("damaged before anything was archived")
        with self.assertRaises(fsutil.IntegrityError):
            versioning.recover(self.studio, "nu")
        self.assertTrue((self.comp() / ".transaction.json").exists())

    def test_failure_before_the_transaction_changes_nothing(self):
        # A file that breaks the naming conventions makes accept fail early.
        (self.directory / "outputs" / "nu-stray name.txt").write_text("not allowed")
        with self.assertRaises(Exception):
            versioning.accept(self.studio, self.directory)
        self.assertOldState()
        self.assertClean()

    def test_killed_process_is_recovered(self):
        """A real process dies mid-transaction, leaving its lock behind."""
        for step, expected in (("archive-copied", "old"), ("promote-first-entry", "new")):
            with self.subTest(step=step):
                self.tearDown()
                self.setUp()
                env = dict(os.environ, STUDIO_FAULT_AT=step, STUDIO_FAULT_MODE="kill",
                           PYTHONPATH=str(TOOLS))
                done = subprocess.run(
                    [sys.executable, "-m", "studio.cli", "--root", str(self.root), "accept",
                     str(self.directory)],
                    env=env, capture_output=True, text=True,
                )
                self.assertEqual(done.returncode, 137, done.stderr)
                self.assertTrue((self.studio.locks_root / "nu.lock").exists())
                self.assertTrue((self.comp() / ".transaction.json").exists())
                result = versioning.recover(self.studio, "nu")
                if expected == "old":
                    self.assertEqual(result["action"], "rolled-back")
                    self.assertOldState()
                else:
                    self.assertEqual(result["action"], "rolled-forward")
                    self.assertNewState()
                self.assertClean()

    def test_interrupted_creation_leaves_no_composition(self):
        directory = self.prepare(name="xi", prompt="New.")
        def hook(reached):
            if reached == "create-copied":
                raise versioning.InjectedFault(reached)
        versioning.set_fault_hook(hook)
        with self.assertRaises(versioning.InjectedFault):
            versioning.accept(self.studio, directory)
        versioning.set_fault_hook(None)
        self.assertFalse(self.comp("xi").exists())
        result = versioning.accept(self.studio, directory)
        self.assertEqual(result["ordinal"], 1)
        self.assertVerified("xi")


if __name__ == "__main__":
    unittest.main()
