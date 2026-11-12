"""Integrating revisions made on separate Git branches (separate agents).

This runs the procedure documented in docs/cloud-agents.md in a temporary
Git repository: two branches revise the same composition, the merge
conflicts, and 'studio adopt' re-applies the other branch's revisions on top
so that every version of both branches survives under its own number.
"""
import shutil
import subprocess
import unittest

from support import StudioTestCase, csd_text

from studio import manifest, naming, render, staging, versioning

COMPOSITION = "music/ai-compositions/nu"


@unittest.skipUnless(shutil.which("git"), "git not found")
class BranchIntegrationTests(StudioTestCase):
    def git(self, *args, check=True):
        done = subprocess.run(
            ["git", "-C", str(self.root), "-c", "user.name=Test", "-c", "user.email=test@example.invalid",
             "-c", "commit.gpgsign=false", "-c", "core.hooksPath=/dev/null", *args],
            capture_output=True, text=True,
        )
        if check:
            self.assertEqual(done.returncode, 0, done.stdout + done.stderr)
        return done

    def commit(self, message):
        self.git("add", "-A")
        self.git("commit", "-q", "-m", message)

    def csd(self, version=None):
        return (versioning.state_dir(self.studio, "nu", version) / "nu.csd").read_text()

    def prompts(self):
        return [
            staging.parse_prompt((self.comp() / naming.prompt_name("nu", n)).read_text())["text"].strip()
            for n in staging.prompt_numbers("nu", self.comp())
        ]

    def setUp(self):
        super().setUp()
        self.git("init", "-q", "-b", "trunk")
        self.studio.archive.mkdir(parents=True)
        (self.studio.archive / ".gitignore").write_text(".staging/\n.locks/\n")
        self.make_version(prompt="Prompt one.")
        self.make_version(marker="FREQ=450", prompt="Prompt two.")
        self.commit("base: nu at ordinal 2")
        # Agent B, on its own branch: two revisions, the first with a program.
        self.git("checkout", "-q", "-b", "agent-b")
        self.make_version(marker="FREQ=500", prompt="B asks for 500.", code={"nu-generate.py": "# by B\n"})
        self.make_version(marker="FREQ=510", prompt="B asks for 510.", code={"nu-generate.py": "# by B, again\n"})
        self.commit("agent B: two revisions")
        # Agent A, on the trunk: one revision.
        self.git("checkout", "-q", "trunk")
        self.make_version(marker="FREQ=600", prompt="A asks for 600.")
        self.commit("agent A: one revision")
        self.other = self.tmp / "other"

    def extract_other_branch(self):
        self.other.mkdir()
        archive = subprocess.run(["git", "-C", str(self.root), "archive", "agent-b", COMPOSITION],
                                 capture_output=True, check=True)
        subprocess.run(["tar", "-x", "-C", str(self.other)], input=archive.stdout, check=True)
        return self.other / COMPOSITION

    def adopt(self, source, version=None):
        directory = staging.adopt(self.studio, "nu", source, version)
        render.render(directory)
        return versioning.accept(self.studio, directory)

    def test_plain_merge_conflicts_and_adopt_keeps_every_version(self):
        # Both branches numbered their revision 3: Git cannot merge that.
        merge = self.git("merge", "--no-commit", "--no-ff", "agent-b", check=False)
        self.assertNotEqual(merge.returncode, 0)
        self.assertIn("CONFLICT", merge.stdout + merge.stderr)

        # Documented procedure, step 1: keep the trunk's copy of the composition.
        self.git("restore", "--source=HEAD", "--staged", "--worktree", "--", COMPOSITION)
        self.assertVerified()
        self.assertIn("FREQ=600", self.csd())

        # Step 2: look at the other branch's copy.
        source = self.extract_other_branch()
        listing = staging.foreign_versions(self.studio, "nu", source)
        self.assertEqual([(e["version"], e["ordinal"], e["present_here"]) for e in listing],
                         [(1, 1, True), (2, 2, True), (3, 3, False), (None, 4, False)])

        # Step 3: adopt what is missing, oldest first.
        self.adopt(source, 3)
        self.adopt(source)
        self.assertVerified()

        # Every version of both branches is there, each under its own number.
        self.assertEqual(versioning.list_versions(self.comp()), [1, 2, 3, 4])
        for version, marker in ((1, "FREQ=440"), (2, "FREQ=450"), (3, "FREQ=600"), (4, "FREQ=500"), (None, "FREQ=510")):
            self.assertIn(marker, self.csd(version))
        self.assertEqual((self.comp() / "version-0004" / "code" / "nu-generate.py").read_text(), "# by B\n")
        self.assertEqual((self.comp() / "code" / "nu-generate.py").read_text(), "# by B, again\n")
        self.assertFalse((self.comp() / "version-0003" / "code").exists())
        # All prompts, in the order in which the versions now follow each other.
        self.assertEqual(self.prompts(),
                         ["Prompt one.", "Prompt two.", "A asks for 600.", "B asks for 500.", "B asks for 510."])
        document = manifest.load(self.comp(), "nu")
        self.assertEqual(document["ordinal"], 5)
        self.assertEqual(document["new_prompts"], ["nu-prompt-005.md"])
        self.assertEqual(document["adopted_from"]["ordinal"], 4)
        self.assertEqual(document["ai"]["model"], "test-model")
        # Now everything of the other copy is present here.
        self.assertTrue(all(e["present_here"] for e in staging.foreign_versions(self.studio, "nu", source)))
        with self.assertRaises(staging.StagingError) as caught:
            staging.adopt(self.studio, "nu", source)
        self.assertIn("already in this archive", str(caught.exception))

        # Step 4: the merge can be concluded.
        self.commit("merge agent-b, adopting its revisions of nu")
        self.assertEqual(self.git("status", "--porcelain").stdout.strip(), "")
        log = self.git("log", "--oneline", "--merges").stdout
        self.assertIn("merge agent-b", log)

    def test_versions_must_be_adopted_in_order(self):
        source = self.extract_other_branch()
        with self.assertRaises(staging.StagingError) as caught:
            staging.adopt(self.studio, "nu", source)
        self.assertIn("adopt its earlier versions first", str(caught.exception))
        self.assertEqual(staging.list_stagings(self.studio), [])

    def test_damaged_copy_is_refused_and_archive_is_untouched(self):
        source = self.extract_other_branch()
        before = self.current_hash()
        (source / "nu.csd").write_text(csd_text("FREQ=999"))
        with self.assertRaises(staging.StagingError) as caught:
            staging.adopt(self.studio, "nu", source)
        self.assertIn("does not match its manifest", str(caught.exception))
        for own in (self.comp(), self.other / ".." / "repo" / COMPOSITION):
            with self.assertRaises(staging.StagingError):
                staging.adopt(self.studio, "nu", own)
        self.assertEqual(self.current_hash(), before)
        self.assertVerified()

    def test_adopt_command(self):
        import os
        import sys
        from support import TOOLS
        source = self.extract_other_branch()
        command = [sys.executable, "-m", "studio.cli", "--root", str(self.root)]
        environment = dict(os.environ, PYTHONPATH=str(TOOLS))
        listing = subprocess.run([*command, "adopt", "nu", "--from", str(source), "--list"],
                                 capture_output=True, text=True, env=environment)
        self.assertEqual(listing.returncode, 0, listing.stderr)
        self.assertEqual(listing.stdout.count("already here"), 2)
        self.assertEqual(listing.stdout.count("NOT here"), 2)
        adopted = subprocess.run([*command, "adopt", "nu", "--from", str(source), "--version", "3"],
                                 capture_output=True, text=True, env=environment)
        self.assertEqual(adopted.returncode, 0, adopted.stderr)
        directory = adopted.stdout.strip().splitlines()[-1]
        self.assertIn("FREQ=500", open(os.path.join(directory, "nu.csd")).read())


if __name__ == "__main__":
    unittest.main()
