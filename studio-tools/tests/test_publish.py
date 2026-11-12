"""Publication to docs/compositions/ and catalog generation."""
import json
import os
import unittest

from support import StudioTestCase, tree_snapshot

from studio import catalog, fsutil, publish, render, versioning
from studio.cli import main as cli_main
from studio.paths import PathSafetyError, StudioError


class PublishTestCase(StudioTestCase):
    def setUp(self):
        super().setUp()
        self.make_version(code={"nu-generate.py": "print('one')\n"})
        self.make_version(marker="FREQ=550", prompt="Two.", code={"nu-generate.py": "print('two')\n"})
        self.make_version(marker="FREQ=660", prompt="Three.", code={"nu-generate.py": "print('three')\n"})
        self.make_version(name="xi", prompt="A private sketch.")
        self.archive_before = tree_snapshot(self.studio.archive)

    def published(self):
        return sorted(rel for rel in tree_snapshot(self.studio.published)
                      if not rel.startswith(".") and rel != "catalog.json")

    def revise_without_code(self):
        """A revision that no longer has a generator program."""
        directory = self.prepare(marker="FREQ=770", prompt="Four.", do_render=False)
        fsutil.remove_tree(directory / "code")
        render.render(directory)
        versioning.accept(self.studio, directory)

    def assertArchiveUntouched(self):
        self.assertEqual(tree_snapshot(self.studio.archive), self.archive_before)


class PublishTests(PublishTestCase):
    def test_default_publication_maps_paths_one_to_one(self):
        result = publish.publish(self.studio, ["nu"])
        self.assertTrue(result["written"])
        expected = []
        for prefix, prompts in (("nu/", 3), ("nu/version-0001/", 1), ("nu/version-0002/", 2)):
            expected += [prefix + "nu.csd", prefix + "nu-manifest.json", prefix + "code/nu-generate.py",
                         prefix + "outputs/nu.mp3"]
            expected += [f"{prefix}nu-prompt-00{n}.md" for n in range(1, prompts + 1)]
        self.assertEqual(self.published(), sorted(expected))
        for rel in self.published():
            self.assertEqual(
                (self.studio.published / rel).read_bytes(), (self.studio.archive / rel).read_bytes(), rel
            )
        self.assertArchiveUntouched()

    def test_masters_logs_and_other_compositions_are_not_published_by_default(self):
        publish.publish(self.studio, ["nu"])
        names = self.published()
        self.assertFalse(any(rel.endswith((".flac", ".wav", ".log")) for rel in names))
        self.assertFalse(any(rel.startswith("xi/") for rel in names))
        text = (self.studio.published / "catalog.json").read_text()
        self.assertNotIn("xi", json.loads(text)["compositions"][0]["name"])
        self.assertNotIn("A private sketch", text)

    def test_nothing_is_published_without_naming_a_composition(self):
        with self.assertRaises(StudioError):
            publish.publish(self.studio, [])
        self.assertFalse(self.studio.published.exists())

    def test_version_selection(self):
        publish.publish(self.studio, ["nu"], versions=publish.parse_versions("1"))
        self.assertTrue(all(rel.startswith("nu/version-0001/") for rel in self.published()))
        publish.publish(self.studio, ["nu"], versions=publish.parse_versions("current"))
        self.assertFalse(any(rel.startswith("nu/version-0002/") for rel in self.published()))
        self.assertIn("nu/nu.csd", self.published())
        publish.publish(self.studio, ["nu"], versions=publish.parse_versions("version-0002,current"))
        self.assertIn("nu/version-0002/nu.csd", self.published())
        with self.assertRaises(StudioError):
            publish.publish(self.studio, ["nu"], versions=[7])
        for bad in ("0", "-1", "one", "1.5", "../1"):
            with self.assertRaises(StudioError):
                publish.parse_versions(bad)
        self.assertArchiveUntouched()

    def test_kind_selection(self):
        publish.publish(self.studio, ["nu"], kinds=publish.parse_kinds("csd,prompts"),
                        versions=publish.parse_versions("current"))
        self.assertEqual(self.published(), ["nu/nu-prompt-001.md", "nu/nu-prompt-002.md",
                                            "nu/nu-prompt-003.md", "nu/nu.csd"])
        publish.publish(self.studio, ["nu"], kinds=publish.parse_kinds("all"),
                        versions=publish.parse_versions("current"))
        self.assertIn("nu/outputs/nu.flac", self.published())
        self.assertIn("nu/outputs/nu-render.log", self.published())
        with self.assertRaises(StudioError):
            publish.parse_kinds("csd,everything")

    def test_several_compositions(self):
        publish.publish(self.studio, ["nu", "xi"])
        self.assertIn("xi/xi.csd", self.published())
        self.assertIn("nu/nu.csd", self.published())

    def test_dry_run_writes_nothing(self):
        result = publish.publish(self.studio, ["nu"], dry_run=True)
        self.assertFalse(result["written"])
        self.assertEqual({a["action"] for a in result["actions"]}, {"copy"})
        self.assertEqual(len(result["actions"]), 18)
        self.assertFalse(self.studio.published.exists())
        publish.publish(self.studio, ["nu"])
        before = tree_snapshot(self.studio.published)
        (self.studio.published / "nu" / "nu.csd").write_text("edited by hand")
        edited = tree_snapshot(self.studio.published)
        result = publish.publish(self.studio, ["nu"], dry_run=True, force=True)
        self.assertEqual(tree_snapshot(self.studio.published), edited)
        self.assertNotEqual(edited, before)
        self.assertEqual(cli_main(["--root", str(self.root), "publish", "nu", "--dry-run"]), 1)
        self.assertEqual(tree_snapshot(self.studio.published), edited)
        self.assertArchiveUntouched()

    def test_unchanged_files_are_skipped(self):
        publish.publish(self.studio, ["nu"])
        stamps = {p: p.stat().st_mtime_ns for p in self.studio.published.rglob("*") if p.is_file()}
        result = publish.publish(self.studio, ["nu"])
        self.assertEqual({a["action"] for a in result["actions"]}, {"skip"})
        self.assertFalse(result["catalog"]["changed"])
        self.assertEqual(
            {p: p.stat().st_mtime_ns for p in self.studio.published.rglob("*") if p.is_file()}, stamps
        )

    def test_new_revision_updates_current_and_adds_the_archived_version(self):
        publish.publish(self.studio, ["nu"])
        self.make_version(marker="FREQ=770", prompt="Four.", code={"nu-generate.py": "print('four')\n"})
        result = publish.publish(self.studio, ["nu"])
        actions = {a["dest"]: a["action"] for a in result["actions"]}
        self.assertEqual(actions["nu/nu.csd"], "update")
        self.assertEqual(actions["nu/version-0003/nu.csd"], "copy")
        self.assertEqual(actions["nu/version-0001/nu.csd"], "skip")
        self.assertEqual(actions["nu/nu-prompt-004.md"], "copy")
        self.assertIn("FREQ=770", (self.studio.published / "nu" / "nu.csd").read_text())
        self.assertIn("FREQ=660", (self.studio.published / "nu" / "version-0003" / "nu.csd").read_text())

    def test_modified_destination_is_a_conflict(self):
        publish.publish(self.studio, ["nu"])
        for rel in ("nu/nu.csd", "nu/version-0001/nu.csd"):
            target = self.studio.published / rel
            original = target.read_bytes()
            target.write_text("edited by hand")
            before = tree_snapshot(self.studio.published)
            with self.assertRaises(publish.PublishError) as caught:
                publish.publish(self.studio, ["nu"])
            self.assertIn(rel, str(caught.exception))
            self.assertEqual(tree_snapshot(self.studio.published), before)
            publish.publish(self.studio, ["nu"], force=True)
            self.assertEqual(target.read_bytes(), original)

    def test_foreign_destination_file_is_a_conflict(self):
        target = self.studio.published / "nu" / "nu.csd"
        target.parent.mkdir(parents=True)
        target.write_text("someone else's file")
        with self.assertRaises(publish.PublishError):
            publish.publish(self.studio, ["nu"])
        self.assertEqual(target.read_text(), "someone else's file")
        self.assertEqual(self.published(), ["nu/nu.csd"])

    def test_unrelated_files_in_destination_are_left_alone(self):
        publish.publish(self.studio, ["nu"])
        note = self.studio.published / "nu" / "README.txt"
        note.write_text("hand-written")
        other = self.studio.published / "other-site-file.html"
        other.write_text("<p>hello</p>")
        self.revise_without_code()
        publish.publish(self.studio, ["nu"], prune=True)
        self.assertEqual(note.read_text(), "hand-written")
        self.assertEqual(other.read_text(), "<p>hello</p>")
        names = [c["name"] for c in json.loads((self.studio.published / "catalog.json").read_text())["compositions"]]
        self.assertEqual(names, ["nu"])

    def test_stale_files_need_prune_and_only_unmodified_ones_go(self):
        publish.publish(self.studio, ["nu"])
        self.revise_without_code()
        with self.assertRaises(publish.PublishError) as caught:
            publish.publish(self.studio, ["nu"])
        self.assertIn("--prune", str(caught.exception))
        self.assertTrue((self.studio.published / "nu" / "code" / "nu-generate.py").exists())
        self.assertIn("FREQ=660", (self.studio.published / "nu" / "nu.csd").read_text())
        publish.publish(self.studio, ["nu"], prune=True)
        self.assertFalse((self.studio.published / "nu" / "code").exists())
        self.assertTrue((self.studio.published / "nu" / "version-0003" / "code" / "nu-generate.py").exists())

    def test_modified_stale_file_is_not_deleted(self):
        publish.publish(self.studio, ["nu"])
        self.revise_without_code()
        stale = self.studio.published / "nu" / "code" / "nu-generate.py"
        stale.write_text("edited by hand")
        with self.assertRaises(publish.PublishError):
            publish.publish(self.studio, ["nu"], prune=True)
        self.assertEqual(stale.read_text(), "edited by hand")

    def test_narrower_selection_does_not_unpublish(self):
        publish.publish(self.studio, ["nu"])
        before = self.published()
        publish.publish(self.studio, ["nu"], kinds=("csd",), versions=[None], prune=True)
        self.assertEqual(self.published(), before)

    def test_unpublish(self):
        publish.publish(self.studio, ["nu", "xi"])
        note = self.studio.published / "nu" / "README.txt"
        note.write_text("hand-written")
        result = publish.unpublish(self.studio, "nu", versions=[1], dry_run=True)
        self.assertEqual(len(result["removed"]), 5)
        self.assertTrue((self.studio.published / "nu" / "version-0001").exists())
        publish.unpublish(self.studio, "nu", versions=[1])
        self.assertFalse((self.studio.published / "nu" / "version-0001").exists())
        self.assertTrue((self.studio.published / "nu" / "version-0002" / "nu.csd").exists())
        publish.unpublish(self.studio, "nu")
        self.assertEqual([rel for rel in self.published() if rel.startswith("nu/")], ["nu/README.txt"])
        self.assertIn("xi/xi.csd", self.published())
        names = [c["name"] for c in json.loads((self.studio.published / "catalog.json").read_text())["compositions"]]
        self.assertEqual(names, ["xi"])
        self.assertArchiveUntouched()

    def test_corrupt_archive_is_not_published(self):
        target = self.comp() / "version-0001" / "nu.csd"
        target.chmod(0o644)
        target.write_text("tampered")
        with self.assertRaises(publish.PublishError):
            publish.publish(self.studio, ["nu"])
        self.assertFalse(self.studio.published.exists())

    def test_front_matter_is_refused(self):
        directory = self.prepare(marker="FREQ=770", prompt="Four.",
                                 code={"nu-notes.md": "---\ntitle: x\n---\nbody\n"})
        versioning.accept(self.studio, directory)
        with self.assertRaises(publish.PublishError) as caught:
            publish.publish(self.studio, ["nu"])
        self.assertIn("front matter", str(caught.exception))
        self.assertFalse(self.studio.published.exists())


class PublishSafetyTests(PublishTestCase):
    def test_destination_symlink_is_refused(self):
        outside = self.tmp / "outside"
        outside.mkdir()
        self.studio.published.parent.mkdir(parents=True)
        os.symlink(outside, self.studio.published)
        with self.assertRaises(PathSafetyError):
            publish.publish(self.studio, ["nu"])
        self.assertEqual(list(outside.iterdir()), [])

    def test_symlink_inside_destination_is_refused(self):
        outside = self.tmp / "outside"
        outside.mkdir()
        self.studio.published.mkdir(parents=True)
        os.symlink(outside, self.studio.published / "nu")
        with self.assertRaises(PathSafetyError):
            publish.publish(self.studio, ["nu"])
        self.assertEqual(list(outside.iterdir()), [])

    def test_symlinked_file_destination_is_not_written_through(self):
        secret = self.tmp / "secret.txt"
        secret.write_text("do not overwrite")
        target = self.studio.published / "nu" / "nu.csd"
        target.parent.mkdir(parents=True)
        os.symlink(secret, target)
        with self.assertRaises(StudioError):
            publish.publish(self.studio, ["nu"])
        self.assertEqual(secret.read_text(), "do not overwrite")

    def test_everything_published_is_inside_docs_compositions(self):
        publish.publish(self.studio, ["nu", "xi"], kinds=publish.ALL_KINDS)
        docs = self.root / "docs"
        for path in docs.rglob("*"):
            if path.is_file():
                self.assertTrue(path.resolve().is_relative_to(self.studio.published.resolve()), path)
        state = publish.load_state(self.studio)
        for dest, entry in state.items():
            self.assertFalse(dest.startswith("/") or ".." in dest.split("/"))
            self.assertTrue(entry["source"].startswith("music/ai-compositions/"))

    def test_tampered_state_file_cannot_delete_outside(self):
        publish.publish(self.studio, ["nu"])
        victim = self.tmp / "victim.txt"
        victim.write_text("keep me")
        state_path = self.studio.published / publish.STATE_NAME
        state = json.loads(state_path.read_text())
        state["files"]["nu/../../../../victim.txt"] = {"sha256": fsutil.sha256_file(victim)}
        state_path.write_text(json.dumps(state))
        for call in (lambda: publish.publish(self.studio, ["nu"], prune=True),
                     lambda: publish.unpublish(self.studio, "nu")):
            with self.assertRaises(StudioError):
                call()
        self.assertEqual(victim.read_text(), "keep me")


class CatalogTests(PublishTestCase):
    def load(self):
        return json.loads((self.studio.published / catalog.CATALOG_NAME).read_text())

    def test_empty_library(self):
        result = catalog.write(self.studio)
        self.assertEqual(result["compositions"], 0)
        data = self.load()
        self.assertEqual(data["compositions"], [])
        self.assertEqual(data["schema"], catalog.SCHEMA)

    def test_catalog_lists_versions_and_artifacts(self):
        publish.publish(self.studio, ["nu"])
        data = self.load()
        self.assertEqual([c["name"] for c in data["compositions"]], ["nu"])
        composition = data["compositions"][0]
        self.assertEqual([v["id"] for v in composition["versions"]],
                         ["current", "version-0002", "version-0001"])
        self.assertEqual([v["ordinal"] for v in composition["versions"]], [3, 2, 1])
        current = composition["versions"][0]
        self.assertTrue(current["is_current"])
        self.assertEqual(current["csd"]["path"], "nu/nu.csd")
        self.assertEqual([p["file"] for p in current["prompts"]],
                         ["nu-prompt-001.md", "nu-prompt-002.md", "nu-prompt-003.md"])
        self.assertEqual([p["new"] for p in current["prompts"]], [False, False, True])
        self.assertEqual(current["code"][0]["language"], "python")
        self.assertEqual(current["audio"][0]["path"], "nu/outputs/nu.mp3")
        self.assertEqual(current["audio"][0]["role"], "listening")
        self.assertEqual(current["info"]["ai_model"], "test-model")
        self.assertEqual(current["info"]["generator"], "python")
        oldest = composition["versions"][-1]
        self.assertEqual(oldest["csd"]["path"], "nu/version-0001/nu.csd")
        self.assertEqual(len(oldest["prompts"]), 1)
        text = composition["prompt_texts"][current["prompts"][2]["sha256"]]
        self.assertIn("Three.", text)

    def test_all_catalog_paths_are_relative_and_exist(self):
        publish.publish(self.studio, ["nu", "xi"], kinds=publish.ALL_KINDS)
        data = self.load()

        def paths(node):
            if isinstance(node, dict):
                for key, value in node.items():
                    if key == "path" and isinstance(value, str):
                        yield value
                    else:
                        yield from paths(value)
            elif isinstance(node, list):
                for item in node:
                    yield from paths(item)

        found = list(paths(data))
        self.assertGreater(len(found), 20)
        for rel in found:
            self.assertFalse(rel.startswith(("/", "http", "..")), rel)
            self.assertTrue((self.studio.published / rel).exists(), rel)
        self.assertNotIn(str(self.root), json.dumps(data))

    def test_missing_optional_artifacts(self):
        publish.publish(self.studio, ["nu"], kinds=("csd",), versions=[None, 1])
        data = self.load()
        versions = data["compositions"][0]["versions"]
        self.assertEqual([v["id"] for v in versions], ["current", "version-0001"])
        for version in versions:
            self.assertEqual((version["prompts"], version["code"], version["audio"], version["derived"]),
                             ([], [], [], []))
            self.assertIsNone(version["manifest"])
            self.assertEqual(version["info"], {})
        # Only an archived version published: there is no "current" entry.
        publish.unpublish(self.studio, "nu", versions=[None])
        data = self.load()
        self.assertEqual([v["id"] for v in data["compositions"][0]["versions"]], ["version-0001"])

    def test_derived_artifacts_are_listed(self):
        directory = self.prepare(marker="FREQ=770", prompt="Four.")
        (directory / "outputs" / "nu.mid").write_bytes(b"MThd\x00\x00\x00\x06")
        (directory / "outputs" / "nu.musicxml").write_text("<score-partwise/>")
        (directory / "outputs" / "nu.pdf").write_bytes(b"%PDF-1.4")
        versioning.accept(self.studio, directory)
        publish.publish(self.studio, ["nu"], prune=True)
        current = self.load()["compositions"][0]["versions"][0]
        self.assertEqual(sorted(d["kind"] for d in current["derived"]), ["midi", "musicxml", "pdf"])

    def test_catalog_is_stable_when_nothing_changes(self):
        publish.publish(self.studio, ["nu"])
        first = (self.studio.published / catalog.CATALOG_NAME).read_bytes()
        self.assertFalse(catalog.write(self.studio)["changed"])
        self.assertEqual((self.studio.published / catalog.CATALOG_NAME).read_bytes(), first)

    def test_catalog_ignores_files_that_are_not_studio_files(self):
        publish.publish(self.studio, ["nu"])
        (self.studio.published / "nu" / "README.txt").write_text("x")
        (self.studio.published / "not a composition").mkdir()
        (self.studio.published / "empty").mkdir()
        catalog.write(self.studio)
        text = (self.studio.published / catalog.CATALOG_NAME).read_text()
        self.assertNotIn("README", text)
        self.assertEqual(len(self.load()["compositions"]), 1)


if __name__ == "__main__":
    unittest.main()
