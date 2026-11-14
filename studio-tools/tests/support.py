"""Shared fixtures for the studio test suite.

Every test works in a temporary directory that stands in for the
repository root, so nothing in the real repository is touched. Csound is
replaced by a small fake program (so the suite runs anywhere and can inject
failures), and the check in the viewer's Csound engine is switched off;
tests that need the real tools say so and are skipped when the tools are
missing.
"""
from __future__ import annotations

import os
import shutil
import stat
import sys
import tempfile
import unittest
from pathlib import Path
from typing import Dict, Optional

TOOLS = Path(__file__).resolve().parents[1]
REPO = TOOLS.parent
if str(TOOLS) not in sys.path:
    sys.path.insert(0, str(TOOLS))

from studio import environment, fsutil, render, staging, versioning  # noqa: E402
from studio.paths import Studio  # noqa: E402

FAKE_CSOUND = f"""#!{sys.executable}
# Fake csound for tests. Markers in the .csd select the behaviour.
import re, sys
sys.path.insert(0, {str(TOOLS)!r})
from studio import audio
args = sys.argv[1:]
print("--Csound version 7.0 (fake for tests)")
if "--version" in args:
    sys.exit(0)
text = open([a for a in args if a.endswith(".csd")][0]).read()
if "--syntax-check-only" in args:
    if "SYNTAX_ERROR" in text:
        print("error: syntax error, unexpected T_IDENT\\n1 errors in performance")
        sys.exit(1)
    print("Syntax check completed.")
    sys.exit(0)
if "RENDER_FAIL" in text:
    print("Segmentation fault (simulated)")
    sys.exit(1)
if "PERF_ERROR" in text:
    print("PERF ERROR in instr 1: simulated run-time error")
if "NO_OUTPUT" in text:
    sys.exit(0)
out = args[args.index("-o") + 1]
frequency = float((re.search(r"FREQ=([0-9.]+)", text) or [0, 440])[1])
amplitude = 0.0 if "SILENT" in text else (1.0 if "CLIP" in text else 0.5)
audio.write_test_wav(out, seconds=2.0, frequency=frequency, amplitude=amplitude, bits=16)
print("end of Performance")
"""

def csd_text(marker: str = "FREQ=440") -> str:
    return (
        "<CsoundSynthesizer>\n<CsOptions>\n-d\n</CsOptions>\n<CsInstruments>\n"
        "sr = 48000\nksmps = 32\nnchnls = 2\n0dbfs = 1\n"
        f"; {marker}\n"
        "instr 1\n  a1 poscil 0.3 * (p5 / 127), cpsmidinn(p4)\n  out a1, a1\nendin\n"
        "</CsInstruments>\n<CsScore>\ni 1 0 2 60 80\n</CsScore>\n</CsoundSynthesizer>\n"
    )


def write_executable(path: Path, text: str) -> Path:
    path.write_text(text, encoding="utf-8")
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    return path


def tree_snapshot(directory: Path) -> Dict[str, str]:
    """Every file under a directory (hidden ones included) with its hash."""
    snapshot = {}
    directory = Path(directory)
    if directory.is_dir():
        for path in sorted(directory.rglob("*")):
            if path.is_file() and not path.is_symlink():
                snapshot[path.relative_to(directory).as_posix()] = fsutil.sha256_file(path)
    return snapshot


class StudioTestCase(unittest.TestCase):
    """A temporary studio with a fake Csound and no browser-engine check."""

    use_fake_tools = True

    def setUp(self) -> None:
        self._saved_env = dict(os.environ)
        self.tmp = Path(tempfile.mkdtemp(prefix="studio-test-")).resolve()
        self.root = self.tmp / "repo"
        self.root.mkdir()
        self.studio = Studio(self.root)
        self.bin = self.tmp / "bin"
        self.bin.mkdir()
        for variable in ("STUDIO_FAULT_AT", "STUDIO_FAULT_MODE", "STUDIO_ROOT", "STUDIO_AI_MODEL",
                         "STUDIO_AI_AGENT", "STUDIO_SEED"):
            os.environ.pop(variable, None)
        if self.use_fake_tools:
            os.environ["STUDIO_CSOUND"] = str(write_executable(self.bin / "csound", FAKE_CSOUND))
        # Tests that exercise the viewer's engine turn this on themselves.
        os.environ["STUDIO_CSOUND_WASM"] = "none"
        os.environ["STUDIO_PYTHON"] = sys.executable
        # The tests configure everything themselves; no environment file.
        os.environ["STUDIO_ENV_FILE"] = ""
        os.environ.pop("STUDIO_ENV_LOADED", None)
        environment.reset_cache()
        versioning.set_fault_hook(None)

    def tearDown(self) -> None:
        versioning.set_fault_hook(None)
        os.environ.clear()
        os.environ.update(self._saved_env)
        environment.reset_cache()
        fsutil.remove_tree(self.tmp)

    # -- helpers -----------------------------------------------------------

    def prepare(
        self,
        name: str = "nu",
        marker: str = "FREQ=440",
        prompt: Optional[str] = "Make a piece.",
        code: Optional[Dict[str, str]] = None,
        do_render: bool = True,
    ) -> Path:
        """Stage a new composition or a revision, ready to accept."""
        if self.studio.composition_dir(name).is_dir():
            directory = staging.stage_revision(self.studio, name)
        else:
            directory = staging.new_composition(self.studio, name)
        if prompt is not None:
            staging.add_prompt(directory, prompt)
        (directory / f"{name}.csd").write_text(csd_text(marker), encoding="utf-8")
        for rel, text in (code or {}).items():
            path = directory / "code" / rel
            path.parent.mkdir(exist_ok=True)
            path.write_text(text, encoding="utf-8")
        if do_render:
            render.render(directory)
        return directory

    def make_version(self, name: str = "nu", marker: str = "FREQ=440", **kwargs) -> dict:
        directory = self.prepare(name, marker, **kwargs)
        return versioning.accept(self.studio, directory, ai_model="test-model")

    def comp(self, name: str = "nu") -> Path:
        return self.studio.composition_dir(name)

    def current_hash(self, name: str = "nu") -> str:
        return fsutil.state_hash(self.comp(name), skip_versions=True)

    def assertVerified(self, name: str = "nu") -> None:
        self.assertEqual(versioning.verify(self.studio, name), [])


def real_csound_available() -> bool:
    return shutil.which(os.environ.get("STUDIO_CSOUND", "csound")) is not None


#: The Csound WebAssembly engine installed in this repository's viewer.
BROWSER_ENGINE = REPO / "docs" / "studio" / "csound" / "csound.js"


def browser_engine_available() -> bool:
    """The viewer's engine is installed and there is a Node.js to run it."""
    return BROWSER_ENGINE.is_file() and shutil.which(os.environ.get("STUDIO_NODE", "node")) is not None


def soundfiles(directory: Path) -> list:
    """Every soundfile under a directory, hidden places included."""
    from studio import naming

    return sorted(
        path.relative_to(directory).as_posix()
        for path in Path(directory).rglob("*")
        if path.is_file() and naming.is_soundfile(path.name)
    )
