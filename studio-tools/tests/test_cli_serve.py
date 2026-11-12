"""The command line, end to end, and the local static server."""
import http.client
import json
import os
import subprocess
import threading
import unittest

from support import TOOLS, StudioTestCase, csd_text

from studio import serve

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
        self.cli("render", staging)
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
        self.assertIn("identical to the stored render", output)
        self.assertIn("identical .csd", self.cli("rerender", "nu", "--regenerate"))

        # Publication.
        self.assertIn("nothing written", self.cli("publish", "nu", "--dry-run"))
        self.assertFalse(self.studio.published.exists())

    def test_publish_and_catalog_commands(self):
        self.make_version()
        self.make_version(marker="FREQ=550", prompt="Two.")
        self.cli("publish", "nu", "--dry-run")
        self.assertFalse(self.studio.published.exists())
        self.assertIn("9 copy", self.cli("publish", "nu"))
        self.assertIn("9 skip", self.cli("publish", "nu"))
        self.assertIn("unchanged", self.cli("catalog"))
        self.assertIn("removed 4", self.cli("unpublish", "nu", "--versions", "1"))
        self.assertIn("5 skip", self.cli("publish", "nu", "--versions", "current"))

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
        (self.docs / "compositions" / "audio.mp3").write_bytes(self.payload)
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

    def test_whole_files_and_types(self):
        response, body = self.get("/compositions/catalog.json")
        self.assertEqual((response.status, body), (200, b'{"compositions": []}'))
        self.assertEqual(response.getheader("Content-Type"), "application/json")
        self.assertEqual(response.getheader("Accept-Ranges"), "bytes")
        response, body = self.get("/compositions/audio.mp3")
        self.assertEqual((response.status, body), (200, self.payload))
        self.assertEqual(response.getheader("Content-Type"), "audio/mpeg")
        response, _ = self.get("/compositions/nu.csd")
        self.assertTrue(response.getheader("Content-Type").startswith("text/plain"))

    def test_byte_ranges_for_seeking(self):
        size = len(self.payload)
        for header, start, end in (("bytes=100-199", 100, 199), ("bytes=5000-", 5000, size - 1),
                                   ("bytes=-300", size - 300, size - 1), ("bytes=0-0", 0, 0),
                                   (f"bytes=10-{size + 500}", 10, size - 1)):
            response, body = self.get("/compositions/audio.mp3", {"Range": header})
            self.assertEqual(response.status, 206, header)
            self.assertEqual(body, self.payload[start:end + 1], header)
            self.assertEqual(response.getheader("Content-Range"), f"bytes {start}-{end}/{size}")
        response, _ = self.get("/compositions/audio.mp3", {"Range": f"bytes={size + 10}-"})
        self.assertEqual(response.status, 416)

    def test_missing_files_and_escapes(self):
        self.assertEqual(self.get("/compositions/none.mp3")[0].status, 404)
        self.assertEqual(self.get("/compositions/none.mp3", {"Range": "bytes=0-10"})[0].status, 404)
        for path in ("/../secret.txt", "/compositions/../../secret.txt", "/%2e%2e/secret.txt"):
            response, body = self.get(path)
            self.assertNotIn(b"outside docs", body, path)


if __name__ == "__main__":
    unittest.main()
