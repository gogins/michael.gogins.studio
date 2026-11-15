"""The command line, end to end, and the local static server."""
import http.client
import json
import os
import subprocess
import threading
import unittest

from support import TOOLS, StudioTestCase, csd_text, soundfiles

from studio import api, serve, versioning

GENERATOR = '''\
import os
seed = os.environ.get("STUDIO_SEED", "none")
open(os.environ["STUDIO_COMPOSITION"] + ".csd", "w").write(CSD.replace("FREQ=440", "FREQ=" + FREQ))
print("generated with seed", seed)
'''


class CommandLineTests(StudioTestCase):
    def cli(self, *args, expect=0, stdin=None):
        done = subprocess.run(
            [str(TOOLS / "bin" / "studio"), "--root", str(self.root), *args],
            capture_output=True, text=True, input=stdin,
        )
        self.assertEqual(done.returncode, expect, f"studio {' '.join(args)}\n{done.stdout}\n{done.stderr}")
        return done.stdout.strip()

    def generator(self, frequency):
        return f"CSD = {csd_text()!r}\nFREQ = {str(frequency)!r}\n" + GENERATOR

    def test_complete_session(self):
        # A new composition, written directly.
        staging = self.cli("new", "nu", "--title", "Nu")
        self.assertTrue(os.path.isdir(staging))
        self.cli("prompt", staging, "--file", "-", stdin="Make a piece.\nSecond line.")
        with open(os.path.join(staging, "nu.csd"), "w") as handle:
            handle.write(csd_text())
        self.cli("accept", staging, expect=1)  # not rendered yet
        # A rendering may be kept, but never inside the archive or the website.
        for inside in (self.studio.archive / "nu.wav", self.comp() / "outputs" / "nu.wav",
                       self.studio.docs / "compositions" / "nu" / "nu.wav"):
            self.cli("render", staging, "--wav", str(inside), expect=1)
        self.assertIn("no soundfile is kept", self.cli("render", staging))
        self.assertEqual(soundfiles(self.tmp), [])
        self.assertIn("created", self.cli("accept", staging, "--model", "model-x", "--agent", "agent-y"))
        self.assertFalse(os.path.exists(staging))

        # Two revisions made by a generator program.
        for number, frequency in ((2, 550), (3, 660)):
            staging = self.cli("stage", "nu")
            self.cli("prompt", staging, "--text", f"Revision {number}.")
            os.makedirs(os.path.join(staging, "code"), exist_ok=True)
            with open(os.path.join(staging, "code", "nu-generate.py"), "w") as handle:
                handle.write(self.generator(frequency))
            self.cli("run", staging, "--seed", str(number), "--param", "density=2", "--",
                        "python", "code/nu-generate.py")
            self.cli("render", staging)
            output = self.cli("accept", staging, "--model", "model-x")
            self.assertIn(f"version-000{number - 1}", output)

        self.assertIn("current is ordinal 3, 2 archived, 3 prompts", self.cli("list"))
        self.assertIn("nu: ok", self.cli("verify"))
        manifest = json.loads(self.cli("show", "nu", "--json"))
        self.assertEqual(manifest["ordinal"], 3)
        self.assertEqual(manifest["generator"]["seeds"], [3])
        self.assertEqual(manifest["generator"]["runs"][0]["argv"], ["python", "code/nu-generate.py"])
        self.assertEqual(manifest["ai"]["model"], "model-x")
        first = json.loads(self.cli("show", "nu", "--version", "1", "--json"))
        self.assertEqual((first["ordinal"], first["generator"]["kind"]), (1, "direct-csd"))
        self.assertIn("Second line.", (self.comp() / "version-0001" / "nu-prompt-001.md").read_text())

        # Every version can be rendered again; generated ones can be regenerated.
        self.assertIn("identical", self.cli("rerender", "nu", "--version", "1"))
        output = self.cli("rerender", "nu", "--version", "2", "--regenerate")
        self.assertIn("identical .csd", output)
        self.assertIn("identical to the render recorded in the manifest", output)
        self.assertIn("identical .csd", self.cli("rerender", "nu", "--regenerate"))
        self.assertIn("no soundfile is kept", self.cli("show", "nu"))

        # A soundfile of any version, written outside the repository on request.
        self.cli("rerender", "nu", "--version", "1", "--wav", str(self.comp() / "nu.wav"), expect=1)
        kept = self.tmp / "masters" / "nu-1.wav"
        self.assertIn(str(kept), self.cli("rerender", "nu", "--version", "1", "--wav", str(kept)))
        self.assertEqual(soundfiles(self.tmp), ["masters/nu-1.wav"])
        self.assertEqual(soundfiles(self.root), [])

        # Publication.
        self.assertIn("nothing written", self.cli("publish", "nu", "--dry-run"))
        self.assertFalse(self.studio.published.exists())

    def test_publish_and_catalog_commands(self):
        self.make_version()
        self.make_version(marker="FREQ=550", prompt="Two.")
        self.cli("publish", "nu", "--dry-run")
        self.assertFalse(self.studio.published.exists())
        self.assertIn("7 copy", self.cli("publish", "nu"))
        self.assertIn("7 skip", self.cli("publish", "nu"))
        self.assertIn("unchanged", self.cli("catalog"))
        self.assertIn("removed 3", self.cli("unpublish", "nu", "--versions", "1"))
        self.assertIn("4 skip", self.cli("publish", "nu", "--versions", "current"))
        self.cli("publish", "nu", "--kinds", "audio", expect=1)
        self.assertEqual(soundfiles(self.root), [])

    def test_errors_exit_with_status_one(self):
        self.cli("stage", "missing", expect=1)
        self.cli("verify", "missing", expect=1)
        self.cli("publish", "missing", expect=1)
        self.cli("new", "bad-name", expect=1)
        self.cli("render", "not-a-staging-dir", expect=1)
        self.cli("recover", "missing", expect=1)
        staging = self.cli("new", "nu")
        self.cli("run", staging, expect=1)
        self.cli("run", staging, "--", "python", "-c", "raise SystemExit(2)", expect=1)
        self.cli("prompt", staging, "--text", "  ", expect=1)
        self.cli("discard", staging)
        self.assertFalse(os.path.exists(staging))
        self.assertEqual(
            subprocess.run([str(TOOLS / "bin" / "studio")], capture_output=True).returncode, 2
        )

    def test_recover_command(self):
        self.make_version()
        staging = str(self.prepare(marker="FREQ=550", prompt="Two."))
        env = dict(os.environ, STUDIO_FAULT_AT="archive-renamed", STUDIO_FAULT_MODE="kill")
        done = subprocess.run([str(TOOLS / "bin" / "studio"), "--root", str(self.root), "accept", staging],
                              env=env, capture_output=True, text=True)
        self.assertEqual(done.returncode, 137)
        self.assertIn("INTERRUPTED", self.cli("list"))
        self.cli("verify", "nu", expect=1)
        self.cli("stage", "nu", expect=1)
        self.assertIn("completed; current is ordinal 2", self.cli("recover", "nu"))
        self.assertIn("nu: ok", self.cli("verify", "nu"))
        self.assertIn("nothing to recover", self.cli("recover", "nu"))


class ServerTests(StudioTestCase):
    def setUp(self):
        super().setUp()
        self.docs = self.root / "docs"
        (self.docs / "compositions").mkdir(parents=True)
        self.payload = bytes(range(256)) * 40
        (self.docs / "compositions" / "score.pdf").write_bytes(self.payload)
        (self.docs / "compositions" / "catalog.json").write_text('{"compositions": []}')
        (self.docs / "compositions" / "nu.csd").write_text("<CsoundSynthesizer>")
        (self.root / "secret.txt").write_text("outside docs")
        self.server = serve.make_server(self.docs, "127.0.0.1", 0, quiet=True)
        self.port = self.server.server_address[1]
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()

    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()
        super().tearDown()

    def get(self, path, headers=None):
        connection = http.client.HTTPConnection("127.0.0.1", self.port, timeout=10)
        connection.request("GET", path, headers=headers or {})
        response = connection.getresponse()
        body = response.read()
        connection.close()
        return response, body

    def test_api_disabled_without_studio(self):
        response, body = self.get("/api/v1/status")
        self.assertEqual(response.status, 503)
        self.assertFalse(json.loads(body)["ok"])

    def test_whole_files_and_types(self):
        response, body = self.get("/compositions/catalog.json")
        self.assertEqual((response.status, body), (200, b'{"compositions": []}'))
        self.assertEqual(response.getheader("Content-Type"), "application/json")
        self.assertEqual(response.getheader("Accept-Ranges"), "bytes")
        response, body = self.get("/compositions/score.pdf")
        self.assertEqual((response.status, body), (200, self.payload))
        self.assertEqual(response.getheader("Content-Type"), "application/pdf")
        response, _ = self.get("/compositions/nu.csd")
        self.assertTrue(response.getheader("Content-Type").startswith("text/plain"))

    def test_byte_ranges(self):
        size = len(self.payload)
        for header, start, end in (("bytes=100-199", 100, 199), ("bytes=5000-", 5000, size - 1),
                                   ("bytes=-300", size - 300, size - 1), ("bytes=0-0", 0, 0),
                                   (f"bytes=10-{size + 500}", 10, size - 1)):
            response, body = self.get("/compositions/score.pdf", {"Range": header})
            self.assertEqual(response.status, 206, header)
            self.assertEqual(body, self.payload[start:end + 1], header)
            self.assertEqual(response.getheader("Content-Range"), f"bytes {start}-{end}/{size}")
        response, _ = self.get("/compositions/score.pdf", {"Range": f"bytes={size + 10}-"})
        self.assertEqual(response.status, 416)

    def test_missing_files_and_escapes(self):
        self.assertEqual(self.get("/compositions/none.pdf")[0].status, 404)
        self.assertEqual(self.get("/compositions/none.pdf", {"Range": "bytes=0-10"})[0].status, 404)
        for path in ("/../secret.txt", "/compositions/../../secret.txt", "/%2e%2e/secret.txt"):
            response, body = self.get(path)
            self.assertNotIn(b"outside docs", body, path)


class ComposeApiTests(StudioTestCase):
    """Local compose API: prompt → render → accept → publish."""

    def setUp(self):
        super().setUp()
        (self.root / "docs" / "compositions").mkdir(parents=True)
        self.server = serve.make_server(self.studio.docs, "127.0.0.1", 0, quiet=True, studio=self.studio)
        self.port = self.server.server_address[1]
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()

    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()
        super().tearDown()

    def request(self, method, path, body=None):
        connection = http.client.HTTPConnection("127.0.0.1", self.port, timeout=60)
        payload = None if body is None else json.dumps(body).encode("utf-8")
        headers = {"Content-Type": "application/json"} if payload is not None else {}
        connection.request(method, path, body=payload, headers=headers)
        response = connection.getresponse()
        data = response.read()
        connection.close()
        return response, json.loads(data.decode("utf-8")) if data else {}

    def test_status_and_new_composition(self):
        response, status = self.request("GET", "/api/v1/status")
        self.assertEqual(response.status, 200)
        self.assertTrue(status["ok"])
        self.assertEqual(status["mode"], "local")
        self.assertIn("compose_ai", status)

        response, result = self.request("POST", "/api/v1/compose", {
            "name": "xi",
            "title": "Xi",
            "prompt": "A short stub piece for the viewer field.",
        })
        self.assertEqual(response.status, 200, result)
        self.assertTrue(result["ok"])
        self.assertEqual(result["composition"], "xi")
        self.assertEqual(result["version"], "current")
        self.assertTrue((self.studio.composition_dir("xi") / "xi.csd").is_file())
        catalog = json.loads((self.studio.published / "catalog.json").read_text())
        self.assertEqual([c["name"] for c in catalog["compositions"]], ["xi"])

    def test_revision_keeps_csd_without_hook(self):
        self.make_version(name="nu", prompt="First.")
        from studio import publish
        publish.publish(self.studio, ["nu"])
        before = (self.studio.composition_dir("nu") / "nu.csd").read_text(encoding="utf-8")
        response, result = self.request("POST", "/api/v1/compose", {
            "composition": "nu",
            "prompt": "Revision from the viewer; keep the music.",
        })
        self.assertEqual(response.status, 200, result)
        self.assertEqual(result["kind"], "revision")
        self.assertEqual(result["ordinal"], 2)
        after = (self.studio.composition_dir("nu") / "nu.csd").read_text(encoding="utf-8")
        self.assertEqual(before, after)
        prompts = list(self.studio.composition_dir("nu").glob("nu-prompt-*.md"))
        self.assertEqual(len(prompts), 2)

    def test_compose_rejects_empty_prompt(self):
        response, result = self.request("POST", "/api/v1/compose", {"name": "bad", "prompt": "  "})
        self.assertEqual(response.status, 409)
        self.assertFalse(result["ok"])

    def test_compose_requires_ai_without_allow_unchanged(self):
        os.environ.pop("STUDIO_COMPOSE_ALLOW_UNCHANGED", None)
        os.environ.pop("STUDIO_COMPOSE_HOOK", None)
        os.environ.pop("ANTHROPIC_API_KEY", None)
        os.environ.pop("STUDIO_COMPOSE_API_KEY", None)
        response, result = self.request("POST", "/api/v1/compose", {
            "name": "needsai",
            "prompt": "Write a short drone.",
        })
        self.assertEqual(response.status, 409, result)
        self.assertIn("ANTHROPIC_API_KEY", result.get("error", ""))

    def test_compose_hook_rewrites_csd(self):
        hook = self.tmp / "hook.py"
        hook.write_text(
            "import os\n"
            "from pathlib import Path\n"
            "name = os.environ['STUDIO_COMPOSITION']\n"
            "staging = Path(os.environ['STUDIO_STAGING'])\n"
            "text = (staging / '.compose-request.txt').read_text()\n"
            "assert 'hook tone' in text\n"
            f"staging.joinpath(name + '.csd').write_text({csd_text('FREQ=777')!r})\n",
            encoding="utf-8",
        )
        os.environ["STUDIO_COMPOSE_HOOK"] = f"{os.environ['STUDIO_PYTHON']} {hook}"
        response, result = self.request("POST", "/api/v1/compose", {
            "name": "hooked",
            "prompt": "Please make a hook tone.",
        })
        self.assertEqual(response.status, 200, result)
        csd = (self.studio.composition_dir("hooked") / "hooked.csd").read_text(encoding="utf-8")
        self.assertIn("FREQ=777", csd)

    def test_api_module_direct(self):
        result = api.compose(self.studio, name="direct", prompt="Direct call.")
        self.assertTrue(result["ok"])
        self.assertEqual(versioning.describe(self.studio, "direct")["ordinal"], 1)

    def test_cursor_and_viewer_new_pieces_share_catalog_shape(self):
        # Cursor path: new → prompt → csd → render → accept → publish.
        staging_dir = self.prepare(name="from_cursor", prompt="Made in Cursor.")
        versioning.accept(self.studio, staging_dir, ai_model="cursor-test", ai_agent="Cursor desktop")
        from studio import publish
        publish.publish(self.studio, ["from_cursor"])

        # Viewer path: compose API (same accept + publish).
        api.compose(self.studio, name="from_viewer", prompt="Made in the viewer.", title="From Viewer")

        catalog = json.loads((self.studio.published / "catalog.json").read_text(encoding="utf-8"))
        by_name = {c["name"]: c for c in catalog["compositions"]}
        self.assertEqual(set(by_name), {"from_cursor", "from_viewer"})
        for name, expected_title in (("from_cursor", "from_cursor"), ("from_viewer", "From Viewer")):
            composition = by_name[name]
            self.assertEqual(composition["title"], expected_title)
            self.assertTrue(composition["versions"])
            current = composition["versions"][0]
            self.assertEqual(current["id"], "current")
            self.assertTrue(current.get("is_current"))
            self.assertEqual(current["ordinal"], 1)
            self.assertTrue(current.get("csd"))
            self.assertEqual(len(current["prompts"]), 1)
            self.assertIn(current["prompts"][0]["sha256"], composition.get("prompt_texts") or {})


if __name__ == "__main__":
    unittest.main()
