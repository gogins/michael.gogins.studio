"""The real repository: the studio only adds files in its four directories,
and the real archive and published files are consistent.

These tests read the repository; they never write to it.
"""
import os
import re
import subprocess
import unittest

from support import REPO, TOOLS

from studio import catalog, fsutil, publish, versioning
from studio.paths import AUTHORIZED_RELS, Studio

SECRET_PATTERNS = [
    re.compile(rb"-----BEGIN [A-Z ]*PRIVATE KEY-----"),
    re.compile(rb"\bgh[pousr]_[A-Za-z0-9]{30,}"),
    re.compile(rb"\bgithub_pat_[A-Za-z0-9_]{30,}"),
    re.compile(rb"\bsk-[A-Za-z0-9_-]{32,}"),
    re.compile(rb"\bAKIA[0-9A-Z]{16}\b"),
    re.compile(rb"\bxox[baprs]-[A-Za-z0-9-]{20,}"),
]


def git(*args):
    return subprocess.run(["git", "-C", str(REPO), *args], capture_output=True, text=True)


def base_branch():
    """The branch the studio work is based on (STUDIO_BASE_BRANCH, master or main)."""
    candidates = [os.environ.get("STUDIO_BASE_BRANCH"), "master", "main", "origin/master", "origin/main"]
    for name in candidates:
        if name and git("rev-parse", "--verify", "--quiet", f"{name}^{{commit}}").returncode == 0:
            return name
    return None


def authorized(path):
    return any(path.startswith(rel + "/") for rel in AUTHORIZED_RELS)


IN_GIT = git("rev-parse", "--is-inside-work-tree").stdout.strip() == "true"
BASE = base_branch() if IN_GIT else None


@unittest.skipUnless(IN_GIT and BASE, "not in a Git checkout with a base branch")
class PreexistingFilesTests(unittest.TestCase):
    def changes(self, *range_args):
        done = git("diff", "--name-status", "--no-renames", "-z", *range_args)
        self.assertEqual(done.returncode, 0, done.stderr)
        fields = done.stdout.split("\0")
        return [(fields[i], fields[i + 1]) for i in range(0, len(fields) - 1, 2)]

    def assertOnlyAdditions(self, changes):
        for status, path in changes:
            self.assertEqual(status, "A", f"{path}: a file that existed before was changed ({status})")
            self.assertTrue(authorized(path), f"{path}: outside the authorized directories")

    def test_commits_only_add_files_in_authorized_directories(self):
        merge_base = git("merge-base", BASE, "HEAD").stdout.strip()
        self.assertTrue(merge_base)
        self.assertOnlyAdditions(self.changes(merge_base, "HEAD"))

    def test_working_tree_only_adds_files_in_authorized_directories(self):
        merge_base = git("merge-base", BASE, "HEAD").stdout.strip()
        # Tracked files as they are on disk now, staged or not, against the base.
        self.assertOnlyAdditions(self.changes(merge_base))

    def test_no_preexisting_file_is_modified_deleted_or_renamed(self):
        done = git("status", "--porcelain", "--no-renames", "-z")
        self.assertEqual(done.returncode, 0, done.stderr)
        for entry in filter(None, done.stdout.split("\0")):
            status, path = entry[:2], entry[3:]
            if status == "??":
                continue
            self.assertTrue(authorized(path), f"{path}: changed outside the authorized directories ({status})")

    def test_existing_music_and_docs_are_untouched(self):
        merge_base = git("merge-base", BASE, "HEAD").stdout.strip()
        done = git("diff", "--quiet", merge_base, "--", "music", "docs",
                   ":(exclude)music/ai-compositions", ":(exclude)docs/compositions", ":(exclude)docs/studio")
        self.assertEqual(done.returncode, 0, "existing files under music/ or docs/ differ from the base branch")

    def test_no_root_level_agent_or_workflow_configuration(self):
        for path in ("AGENTS.md", ".cursor", ".github", "docs/.nojekyll", "docs/CNAME", "CNAME"):
            tracked = git("ls-files", "--", path).stdout.strip()
            in_base = git("ls-tree", "-r", "--name-only", BASE, "--", path).stdout.strip()
            self.assertEqual(tracked, in_base, f"{path} was added or changed by the studio work")

    def test_tools_contain_no_push_merge_or_deploy_command(self):
        for path in (TOOLS / "studio").glob("*.py"):
            text = path.read_text(encoding="utf-8")
            for forbidden in ('"push"', "'push'", "git push", '"merge"', "gh-pages"):
                self.assertNotIn(forbidden, text, f"{path.name} mentions {forbidden}")


class StudioFilesTests(unittest.TestCase):
    def setUp(self):
        self.studio = Studio(REPO)

    def studio_files(self):
        for rel in AUTHORIZED_RELS:
            base = REPO / rel
            if not base.is_dir():
                continue
            for root, dirs, files in os.walk(base):
                dirs[:] = [d for d in dirs if d not in ("__pycache__", ".staging", ".locks")]
                for name in files:
                    yield os.path.join(root, name)

    def test_no_secrets_in_studio_files(self):
        for path in self.studio_files():
            if os.path.getsize(path) > 2_000_000:
                continue
            with open(path, "rb") as handle:
                data = handle.read()
            for pattern in SECRET_PATTERNS:
                self.assertIsNone(pattern.search(data), f"{path} looks like it contains a credential")

    def test_no_symlinks_in_studio_directories(self):
        for path in self.studio_files():
            self.assertFalse(os.path.islink(path), path)

    def test_real_archive_verifies(self):
        for name in versioning.list_compositions(self.studio):
            self.assertEqual(versioning.verify(self.studio, name), [], name)

    def test_published_files_match_the_archive_and_catalog(self):
        state = publish.load_state(self.studio)
        for dest, entry in state.items():
            published = self.studio.published / dest
            self.assertTrue(published.is_file(), dest)
            self.assertEqual(fsutil.sha256_file(published), entry["sha256"], dest)
            source = REPO / entry["source"]
            if source.is_file():
                self.assertEqual(fsutil.sha256_file(source), entry["sha256"], dest)
        path = self.studio.published / catalog.CATALOG_NAME
        if path.is_file():
            on_disk = fsutil.read_json(path)
            on_disk.pop("generated_at", None)
            self.assertEqual(on_disk, catalog.build(self.studio), "catalog.json is out of date")

    def test_published_text_files_have_no_jekyll_front_matter(self):
        if not self.studio.published.is_dir():
            return
        for root, _dirs, files in os.walk(self.studio.published):
            for name in files:
                with open(os.path.join(root, name), "rb") as handle:
                    self.assertFalse(handle.read(4).startswith(b"---"), name)

    def test_viewer_uses_only_relative_local_resources(self):
        viewer = REPO / "docs" / "studio"
        if not viewer.is_dir():
            self.skipTest("viewer not present")
        names = sorted(p.name for p in viewer.iterdir() if p.is_file())
        self.assertIn("index.html", names)
        for name in names:
            self.assertTrue(name.endswith((".html", ".js", ".css")), name)
            text = (viewer / name).read_text(encoding="utf-8")
            self.assertFalse(text.startswith("---"), f"{name} would be processed by Jekyll")
            self.assertIsNone(re.search(r"https?://", text), f"{name} refers to an absolute URL")
            self.assertNotIn("innerHTML", text, name)
            self.assertNotIn("eval(", text, name)
        html = (viewer / "index.html").read_text(encoding="utf-8")
        for reference in re.findall(r'(?:src|href)="([^"#]+)"', html):
            if reference.startswith("../"):
                continue
            self.assertTrue((viewer / reference).is_file(), reference)
        for name in names:
            if name.endswith(".js"):
                for imported in re.findall(r"from '(\./[^']+)'", (viewer / name).read_text()):
                    self.assertTrue((viewer / imported).is_file(), f"{name} imports {imported}")


if __name__ == "__main__":
    unittest.main()
