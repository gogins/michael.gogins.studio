"""Generating programs with the real CsoundAC: Python, JavaScript
(WebAssembly) and C++.

Each test is skipped when its tools are not configured:

    STUDIO_PYTHON             a Python that can import CsoundAC
    STUDIO_NODE, CSOUNDAC_WASM_DIR   Node.js and the CsoundAC WebAssembly build
    STUDIO_CSOUNDAC_CXXFLAGS, STUDIO_CSOUNDAC_LDFLAGS   to compile against CsoundAC

Csound itself is the real one when installed, the test double otherwise.
"""
import os
import shutil
import subprocess
import sys
import unittest

from support import StudioTestCase, real_csound_available

from studio import manifest, render, staging, versioning

# Captured before the test case replaces them.
REAL_PYTHON = os.environ.get("STUDIO_PYTHON") or sys.executable
WASM_DIR = os.environ.get("CSOUNDAC_WASM_DIR")
NODE = shutil.which(os.environ.get("STUDIO_NODE", "node"))
CXXFLAGS = os.environ.get("STUDIO_CSOUNDAC_CXXFLAGS")
LDFLAGS = os.environ.get("STUDIO_CSOUNDAC_LDFLAGS")


def python_has_csoundac() -> bool:
    try:
        done = subprocess.run([REAL_PYTHON, "-c", "import CsoundAC"], capture_output=True, timeout=60)
    except (OSError, subprocess.TimeoutExpired):
        return False
    return done.returncode == 0


def wasm_available() -> bool:
    return bool(NODE and WASM_DIR and os.path.isfile(os.path.join(WASM_DIR, "CsoundAC.wasm")))


CSD_HEAD = """<CsoundSynthesizer>
<CsOptions>
-d
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 32
nchnls = 2
0dbfs = 1
; FREQ=440
instr 1
  aEnvelope linseg 0, 0.01, 1, p3 - 0.02, 1, 0.01, 0
  aSignal poscil aEnvelope * 0.3 * (p5 / 127), cpsmidinn(p4)
  aLeft, aRight pan2 aSignal, p7
  outs aLeft, aRight
endin
</CsInstruments>
<CsScore>
"""
CSD_TAIL = """</CsScore>
</CsoundSynthesizer>
"""

# Four notes; key, pan and depth depend on the seed in the same way in
# every language, so that all three programs must write the same score.
PYTHON_GENERATOR = '''import os
import CsoundAC
seed = int(os.environ.get("STUDIO_SEED", "1"))
score = CsoundAC.Score()
for index in range(4):
    # time, duration, status, instrument, key, velocity, phase, pan, depth, height, pitches
    score.append(index * 0.25, 0.2, 144.0, 1.0, 60.0 + (seed * (index + 1)) % 12, 80.0,
                 0.0, index / 4.0, 0.5, 0.0, 4095.0)
with open("nu.csd", "w") as output:
    output.write(HEAD + score.getCsoundScore(12.0, False) + TAIL)
'''

JAVASCRIPT_GENERATOR = '''import { writeFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';
import path from 'node:path';
const { loadCsoundAC, appendNote, csoundScore } = await import(
  pathToFileURL(path.join(process.env.STUDIO_TOOLS, 'js', 'csoundac-node.mjs')).href);
const CsoundAC = await loadCsoundAC();
const seed = Number(process.env.STUDIO_SEED || '1');
const score = new CsoundAC.Score();
for (let index = 0; index < 4; index += 1) {
  appendNote(score, {
    time: index * 0.25, duration: 0.2, instrument: 1, key: 60 + (seed * (index + 1)) % 12, velocity: 80,
    pan: index / 4, depth: 0.5,
  });
}
writeFileSync('nu.csd', HEAD + csoundScore(score) + TAIL);
'''

CPP_GENERATOR = '''#include <Score.hpp>
#include <cstdlib>
#include <fstream>
#include <string>
int main() {
    const char *text = std::getenv("STUDIO_SEED");
    int seed = text ? std::atoi(text) : 1;
    csound::Score score;
    for (int index = 0; index < 4; ++index) {
        // time, duration, status, instrument, key, velocity, phase, pan, depth, height, pitches
        score.append(index * 0.25, 0.2, 144.0, 1.0, 60.0 + (seed * (index + 1)) % 12, 80.0,
                     0.0, index / 4.0, 0.5, 0.0, 4095.0);
    }
    std::ofstream output("nu.csd");
    output << HEAD << score.getCsoundScore(12.0, false) << TAIL;
    return 0;
}
'''

EXPECTED_SCORE = [
    "i 1 0 0.2 65 80 0.5 0 0 0 4095 1",
    "i 1 0.25 0.2 70 80 0.5 0.25 0 0 4095 1",
    "i 1 0.5 0.2 63 80 0.5 0.5 0 0 4095 1",
    "i 1 0.75 0.2 68 80 0.5 0.75 0 0 4095 1",
]


def python_literal(text: str) -> str:
    return repr(text)


def cpp_literal(text: str) -> str:
    return 'R"CSD(' + text + ')CSD"'


class GeneratorTests(StudioTestCase):
    use_fake_tools = not real_csound_available()

    def setUp(self):
        super().setUp()
        os.environ["STUDIO_PYTHON"] = REAL_PYTHON
        for name, value in (("CSOUNDAC_WASM_DIR", WASM_DIR), ("STUDIO_CSOUNDAC_CXXFLAGS", CXXFLAGS),
                            ("STUDIO_CSOUNDAC_LDFLAGS", LDFLAGS)):
            if value:
                os.environ[name] = value

    def generate(self, filename, source, command, kind):
        directory = staging.new_composition(self.studio, "nu")
        staging.add_prompt(directory, "Four notes, made by a program.")
        (directory / "code").mkdir(exist_ok=True)
        (directory / "code" / filename).write_text(source, encoding="utf-8")
        staging.run_command(directory, command, seed=5)
        text = (directory / "nu.csd").read_text(encoding="utf-8")
        score = [line.strip() for line in text.split("<CsScore>")[1].split("</CsScore>")[0].splitlines() if line.strip()]
        # p6 depth, p7 pan, p8 height, p9 phase, p10 pitch-class set, p11 homogeneity.
        self.assertEqual(score, EXPECTED_SCORE)
        render.render(directory)
        versioning.accept(self.studio, directory, ai_model="test-model")
        self.assertVerified()
        document = manifest.load(self.comp(), "nu")
        self.assertEqual(document["generator"]["kind"], kind)
        self.assertEqual(document["generator"]["sources"], [f"code/{filename}"])
        self.assertEqual(document["generator"]["seeds"], [5])
        self.assertEqual(document["generator"]["runs"][0]["argv"], command)
        self.assertEqual(manifest.problems(document, "nu"), [])
        # The stored program reproduces the stored .csd, and the .csd the audio.
        result = render.rerender(self.studio, "nu", regenerate=True)
        self.assertEqual(result["regenerated"], "identical .csd")
        self.assertEqual(result["audio"], "identical to the stored render")
        return document

    @unittest.skipUnless(python_has_csoundac(), "STUDIO_PYTHON cannot import CsoundAC")
    def test_python_with_csoundac(self):
        source = f"HEAD = {python_literal(CSD_HEAD)}\nTAIL = {python_literal(CSD_TAIL)}\n" + PYTHON_GENERATOR
        self.generate("nu-generate.py", source, ["python", "code/nu-generate.py"], "python-csoundac")

    @unittest.skipUnless(wasm_available(), "node or CSOUNDAC_WASM_DIR is not available")
    def test_javascript_with_csoundac_webassembly(self):
        source = (f"const HEAD = {python_literal(CSD_HEAD)};\nconst TAIL = {python_literal(CSD_TAIL)};\n"
                  + JAVASCRIPT_GENERATOR)
        self.generate("nu-generate.mjs", source, ["node", "code/nu-generate.mjs"], "javascript-csoundac-wasm")

    @unittest.skipUnless(CXXFLAGS and LDFLAGS and shutil.which(os.environ.get("CXX", "c++")),
                         "STUDIO_CSOUNDAC_CXXFLAGS and STUDIO_CSOUNDAC_LDFLAGS are not set")
    def test_cpp_with_csoundac(self):
        source = CPP_GENERATOR.replace("<< HEAD <<", f"<< {cpp_literal(CSD_HEAD)} <<").replace(
            "<< TAIL;", f"<< {cpp_literal(CSD_TAIL)};")
        self.generate("nu-generate.cpp", source, ["studio-cxx", "code/nu-generate.cpp"], "cpp-csoundac")


if __name__ == "__main__":
    unittest.main()
