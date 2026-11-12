"""The static viewer, in a real browser.

A temporary site is assembled (the real docs/studio/ plus compositions
published by the tools), served by the studio's own static server, and
driven in headless Chrome by a small harness page. The harness loads the
viewer in a phone-sized frame, exercises it, and posts what it saw back to
the test.

Skipped when no Chrome/Chromium is found (set STUDIO_CHROME to its path).
"""
import json
import os
import shutil
import subprocess
import sys
import threading
import time
import unittest

from support import REPO, StudioTestCase

from studio import catalog, environment, publish, serve

CHROME_CANDIDATES = [
    os.environ.get("STUDIO_CHROME"),
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "google-chrome", "google-chrome-stable", "chromium", "chromium-browser",
]


def find_chrome():
    for candidate in CHROME_CANDIDATES:
        if candidate and (os.path.isfile(candidate) or shutil.which(candidate)):
            return candidate if os.path.isfile(candidate) else shutil.which(candidate)
    return None


HARNESS = r"""<!DOCTYPE html>
<meta charset="utf-8">
<title>studio viewer test harness</title>
<iframe id="frame" src="/studio/" style="width:412px;height:915px;border:0"></iframe>
<script type="module">
const mode = new URLSearchParams(location.search).get('mode');
const frame = document.getElementById('frame');
const doc = () => frame.contentDocument;
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
async function until(what, condition, timeout = 15000) {
  const start = Date.now();
  for (;;) {
    let value = null;
    try { value = condition(); } catch (error) { value = null; }
    if (value) return value;
    if (Date.now() - start > timeout) throw new Error('timed out waiting for ' + what);
    await sleep(40);
  }
}
const state = () => doc() && doc().documentElement.dataset.studioState;
const text = (selector) => { const node = doc().querySelector(selector); return node ? node.textContent : null; };
const all = (selector) => [...doc().querySelectorAll(selector)];
const overflow = () => doc().documentElement.scrollWidth - frame.contentWindow.innerWidth;
// True once the panel shows the given tab and its content has arrived.
const tab = (name, selector) => () => doc().getElementById('panel').dataset.tab === name && text(selector);
async function show(hash, what, condition) {
  frame.contentWindow.location.hash = hash;
  return until(`${hash}: ${what}`, condition);
}

async function run() {
  const result = { mode, overflow: {} };
  await until('viewer to settle', () => state() === 'ready' || state() === 'error');
  result.state = state();
  result.catalogError = text('#catalog-error');
  result.emptyLibrary = text('#empty-library');
  if (mode !== 'full') return result;

  // Library.
  result.library = all('.card').map((card) => card.dataset.composition);
  result.libraryText = text('.library');
  result.overflow.library = overflow();

  // A composition: versions and prompts.
  await show('#nu', 'version selector', () => doc().getElementById('version-select'));
  result.title = text('#composition-title');
  result.versions = all('#version-select option').map((option) => option.value);
  result.selected = doc().getElementById('version-select').value;
  result.prompts = all('.prompt pre').map((pre) => pre.textContent);
  result.newPrompts = all('.prompt.new h2').map((heading) => heading.textContent);
  result.overflow.prompts = overflow();

  // Csound file of the current version.
  await show('#nu/current/csound', 'csd text', tab('csound', '.source .code'));
  result.csd = text('.source .code');
  result.csdDownload = doc().querySelector('.source-file a.download').getAttribute('href');
  result.overflow.csound = overflow();

  // Generating program.
  await show('#nu/current/code', 'code text', tab('code', '.source .code'));
  result.code = text('.source .code');
  result.codeFiles = all('.source-file').map((item) => item.dataset.file);
  result.overflow.code = overflow();

  // Files and info.
  await show('#nu/current/files', 'file table', tab('files', 'table.files'));
  result.files = all('table.files tbody tr').map((row) => row.children[1].textContent);
  result.overflow.files = overflow();
  await show('#nu/current/info', 'info', tab('info', 'dl.info'));
  result.info = text('dl.info');
  result.overflow.info = overflow();

  // Comparison with the previous version.
  await show('#nu/current/compare', 'diff', tab('compare', '#diff-summary'));
  result.diffSummary = text('#diff-summary');
  result.diffAdded = all('.diff-line.add .diff-text').map((line) => line.textContent);
  result.diffRemoved = all('.diff-line.remove .diff-text').map((line) => line.textContent);
  result.compareOthers = all('#compare-other option').map((option) => option.value);
  result.overflow.compare = overflow();

  // Audio: play, then seek. The player must survive a change of tab.
  await show('#nu/current/prompts', 'player', tab('prompts', '.prompt pre'));
  const audio = doc().getElementById('player');
  audio.muted = true;
  await until('audio metadata', () => audio.readyState >= 1 && isFinite(audio.duration));
  await audio.play();
  await until('audio to advance', () => audio.currentTime > 0.05);
  await show('#nu/current/csound', 'csd text', tab('csound', '.source .code'));
  result.audio = {
    source: audio.currentSrc, duration: audio.duration, played: audio.currentTime,
    stillTheSameElement: doc().getElementById('player') === audio,
  };
  audio.pause();
  const target = audio.duration / 2;
  const seeked = new Promise((resolve) => audio.addEventListener('seeked', resolve, { once: true }));
  audio.currentTime = target;
  await Promise.race([seeked, sleep(5000)]);
  result.audio.seekTarget = target;
  result.audio.seekedTo = audio.currentTime;

  // An archived version, reached with the Older button.
  doc().getElementById('older').click();
  await until('older version', () => doc().getElementById('version-select').value === 'version-0002');
  doc().getElementById('older').click();
  await until('oldest version', () => doc().getElementById('version-select').value === 'version-0001');
  await until('old csd', () => (text('.source .code') || '').includes('FREQ=440'));
  result.oldest = {
    hash: frame.contentWindow.location.hash,
    csd: text('.source .code'),
    olderDisabled: doc().getElementById('older').disabled,
    audioSource: doc().getElementById('player').currentSrc || doc().querySelector('#player source').src,
  };
  await show('#nu/version-0001/code', 'no code message', tab('code', '#panel .status'));
  result.oldest.codeMessage = text('#panel .status');
  await show('#nu/version-0001/prompts', 'prompts', tab('prompts', '.prompt pre'));
  result.oldest.prompts = all('.prompt pre').map((pre) => pre.textContent);

  // A composition published without audio, code or manifest.
  await show('#xi', 'xi', () => text('#composition-title') === 'xi');
  result.sparse = {
    noAudio: text('#player-none'), versions: all('#version-select option').length,
    label: doc().querySelector('#version-select option').textContent,
  };
  await show('#xi/current/info', 'xi info', tab('info', 'dl.info'));
  result.sparse.info = text('dl.info');
  await show('#xi/current/compare', 'xi compare', tab('compare', '#panel .status'));
  result.sparse.compare = text('#panel .status');

  // Unknown composition, and back to the library.
  await show('#nope', 'not found', () => text('#not-found'));
  result.notFound = text('#not-found');
  await show('#', 'library', () => all('.card').length > 0);
  result.backToLibrary = all('.card').length;
  return result;
}

run().catch((error) => ({ mode, error: String(error), stack: error.stack }))
  .then((result) => fetch('/__result', { method: 'POST', body: JSON.stringify(result) }));
</script>
"""


@unittest.skipUnless(find_chrome(), "Chrome or Chromium not found")
class BrowserTests(StudioTestCase):
    def setUp(self):
        super().setUp()
        # Real ffmpeg, when there is one, gives a real MP3; otherwise the WAV is used.
        real_ffmpeg = shutil.which("ffmpeg")
        os.environ["STUDIO_FFMPEG"] = real_ffmpeg or "none"
        environment.reset_cache()
        self.has_mp3 = bool(real_ffmpeg)
        self.results = []
        shutil.copytree(REPO / "docs" / "studio", self.studio.viewer)
        (self.root / "docs" / "harness.html").write_text(HARNESS, encoding="utf-8")
        test = self

        class Handler(serve.StaticHandler):
            quiet = True

            def __init__(self, *args, **kwargs):
                super().__init__(*args, directory=str(test.root / "docs"), **kwargs)

            def do_POST(self):
                length = int(self.headers.get("Content-Length", "0"))
                test.results.append(json.loads(self.rfile.read(length)))
                self.send_response(204)
                self.send_header("Content-Length", "0")
                self.end_headers()

        self.server = serve.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self.server.daemon_threads = True
        self.port = self.server.server_address[1]
        threading.Thread(target=self.server.serve_forever, daemon=True).start()

    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()
        super().tearDown()

    def browse(self, mode, timeout=90):
        profile = self.tmp / f"chrome-{mode}"
        command = [
            find_chrome(), "--headless=new", "--disable-gpu", "--no-first-run",
            "--no-default-browser-check", "--disable-extensions", "--mute-audio",
            "--autoplay-policy=no-user-gesture-required", f"--user-data-dir={profile}",
            "--window-size=1200,1000",
        ]
        if sys.platform.startswith("linux"):
            command += ["--no-sandbox", "--disable-dev-shm-usage"]
        command.append(f"http://127.0.0.1:{self.port}/harness.html?mode={mode}")
        process = subprocess.Popen(command, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        try:
            deadline = time.time() + timeout
            while time.time() < deadline and not self.results and process.poll() is None:
                time.sleep(0.1)
        finally:
            process.terminate()
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                process.kill()
        self.assertTrue(self.results, "the browser did not report a result")
        result = self.results.pop()
        self.assertNotIn("error", result, result.get("stack"))
        return result

    def test_missing_catalog_is_reported(self):
        result = self.browse("missing")
        self.assertEqual(result["state"], "error")
        self.assertIn("could not be loaded", result["catalogError"])

    def test_empty_library(self):
        catalog.write(self.studio)
        result = self.browse("empty")
        self.assertEqual(result["state"], "ready")
        self.assertIn("No compositions have been published yet", result["emptyLibrary"])

    def test_browsing_versions_sources_and_audio(self):
        self.make_version(prompt="First prompt of nu.")
        self.make_version(marker="FREQ=550", prompt="Second prompt of nu.",
                          code={"nu-generate.py": "# generator two\nprint('two')\n"})
        self.make_version(marker="FREQ=660", prompt="Third prompt of nu.",
                          code={"nu-generate.py": "# generator three\nprint('three')\n"})
        self.make_version(name="xi", prompt="Only prompt of xi.")
        self.make_version(name="hidden", prompt="This one is never published.")
        publish.publish(self.studio, ["nu"], kinds=publish.ALL_KINDS)
        publish.publish(self.studio, ["xi"], kinds=("csd",))
        result = self.browse("full")

        # Catalog and selection.
        self.assertEqual(result["state"], "ready")
        self.assertEqual(result["library"], ["nu", "xi"])
        self.assertNotIn("hidden", json.dumps(result))
        self.assertEqual(result["title"], "nu")
        self.assertEqual(result["versions"], ["current", "version-0002", "version-0001"])
        self.assertEqual(result["selected"], "current")

        # Prompts in order, the new one marked.
        self.assertEqual([p.strip() for p in result["prompts"]],
                         ["First prompt of nu.", "Second prompt of nu.", "Third prompt of nu."])
        self.assertEqual(result["newPrompts"], ["Prompt 3"])

        # Sources.
        self.assertIn("FREQ=660", result["csd"])
        self.assertIn("<CsoundSynthesizer>", result["csd"])
        self.assertTrue(result["csdDownload"].endswith("/compositions/nu/nu.csd"))
        self.assertIn("generator three", result["code"])
        self.assertEqual(result["codeFiles"], ["nu-generate.py"])
        self.assertIn("nu.csd", result["files"])
        self.assertIn("nu-render.log", result["files"])
        self.assertIn("nu-manifest.json", result["files"])
        self.assertIn("test-model", result["info"])

        # Comparison of the current version with the one before.
        self.assertIn("+1", result["diffSummary"])
        self.assertTrue(any("FREQ=660" in line for line in result["diffAdded"]))
        self.assertTrue(any("FREQ=550" in line for line in result["diffRemoved"]))
        self.assertEqual(result["compareOthers"], ["version-0002", "version-0001"])

        # Audio really played, kept playing across a tab change, and seeked.
        audio = result["audio"]
        expected = "/compositions/nu/outputs/nu.mp3" if self.has_mp3 else "/compositions/nu/outputs/nu.wav"
        self.assertTrue(audio["source"].endswith(expected), audio["source"])
        self.assertAlmostEqual(audio["duration"], 0.5, delta=0.1)
        self.assertGreater(audio["played"], 0.05)
        self.assertTrue(audio["stillTheSameElement"])
        self.assertAlmostEqual(audio["seekedTo"], audio["seekTarget"], delta=0.06)

        # Archived versions.
        oldest = result["oldest"]
        self.assertTrue(oldest["hash"].startswith("#nu/version-0001"))
        self.assertIn("FREQ=440", oldest["csd"])
        self.assertTrue(oldest["olderDisabled"])
        self.assertIn("/compositions/nu/version-0001/outputs/", oldest["audioSource"])
        self.assertIn("written directly", oldest["codeMessage"])
        self.assertEqual([p.strip() for p in oldest["prompts"]], ["First prompt of nu."])

        # Missing optional artifacts are handled.
        self.assertIn("No audio", result["sparse"]["noAudio"])
        self.assertEqual(result["sparse"]["versions"], 1)
        self.assertEqual(result["sparse"]["label"].strip(), "Current")
        self.assertIn("not published", result["sparse"]["info"])
        self.assertIn("nothing to compare", result["sparse"]["compare"])
        self.assertIn("nope", result["notFound"])
        self.assertEqual(result["backToLibrary"], 2)

        # Phone-sized layout: nothing is wider than the 412 px frame.
        for view, extra in result["overflow"].items():
            self.assertLessEqual(extra, 0, f"the {view} view is {extra}px wider than a phone screen")


@unittest.skipUnless(shutil.which(os.environ.get("STUDIO_NODE", "node")), "node not found")
class DiffModuleTests(unittest.TestCase):
    """diff.js, run under Node: applying the diff must reproduce both texts."""

    SCRIPT = r"""
import { diffLines, hunks, summarize } from %(module)s;
let seed = 12345;
const random = () => { seed = (seed * 1103515245 + 12345) %% 2147483648; return seed / 2147483648; };
const lines = (count, alphabet) => Array.from({ length: count }, () => 'line ' + Math.floor(random() * alphabet));
let checked = 0;
const check = (oldText, newText) => {
  const diff = diffLines(oldText, newText);
  if (diff === null) throw new Error('unexpected null diff');
  const join = (list) => (list.length ? list.join('\n') + '\n' : '');
  const older = join(diff.filter((item) => item.type !== 'add').map((item) => item.line));
  const newer = join(diff.filter((item) => item.type !== 'remove').map((item) => item.line));
  const norm = (value) => (value === '' || value.endsWith('\n') ? value : value + '\n');
  if (older !== norm(oldText) || newer !== norm(newText)) throw new Error('diff does not reproduce its inputs');
  let a = 0; let b = 0;
  for (const item of diff) {
    if (item.type !== 'add' && item.a !== ++a) throw new Error('bad old line number');
    if (item.type !== 'remove' && item.b !== ++b) throw new Error('bad new line number');
  }
  const total = hunks(diff).reduce((sum, hunk) => sum + (hunk.type === 'skip' ? hunk.count : hunk.items.length), 0);
  if (total !== diff.length) throw new Error('hunks lose lines');
  checked += 1;
  return summarize(diff);
};
check('', ''); check('a\n', ''); check('', 'a\nb\n'); check('same\n', 'same\n');
const one = check('a\nb\nc\n', 'a\nB\nc\n');
if (one.added !== 1 || one.removed !== 1) throw new Error('wrong summary');
for (let round = 0; round < 300; round += 1) {
  const base = lines(Math.floor(random() * 60), 1 + Math.floor(random() * 12));
  const other = base.slice();
  for (let edit = 0; edit < Math.floor(random() * 12); edit += 1) {
    const at = Math.floor(random() * (other.length + 1));
    if (random() < 0.5) other.splice(at, 1); else other.splice(at, 0, 'new ' + Math.floor(random() * 5));
  }
  check(base.join('\n') + '\n', other.join('\n') + (random() < 0.5 ? '\n' : ''));
}
const big = Array.from({ length: 5000 }, (_, index) => 'x' + index);
if (diffLines(big.join('\n'), big.map((line) => line + '!').join('\n')) !== null) throw new Error('expected null');
console.log(JSON.stringify({ checked }));
"""

    def test_diff_reproduces_both_texts(self):
        module = json.dumps((REPO / "docs" / "studio" / "diff.js").as_uri())
        done = subprocess.run(
            [shutil.which(os.environ.get("STUDIO_NODE", "node")), "--input-type=module", "-e",
             self.SCRIPT % {"module": module}],
            capture_output=True, text=True, timeout=120,
        )
        self.assertEqual(done.returncode, 0, done.stderr[-2000:])
        self.assertGreater(json.loads(done.stdout)["checked"], 300)


if __name__ == "__main__":
    unittest.main()
