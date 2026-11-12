"""Discovery of the external tools the studio drives, and their versions.

Tools are located through environment variables so that the same commands
work on macOS, in a Cursor Cloud Agent, and in the test suite:

    STUDIO_CSOUND   csound executable            (default: csound)
    STUDIO_FFMPEG   ffmpeg executable or "none"  (default: ffmpeg)
    STUDIO_PYTHON   Python that can import CsoundAC (default: this Python)
    STUDIO_NODE     node executable              (default: node)
    CSOUNDAC_WASM_DIR     directory holding CsoundAC.js and CsoundAC.wasm
    CSOUNDAC_SOURCE_DIR   CsoundAC source checkout, to record its revision
    STUDIO_CSOUNDAC_REVISION  explicit CsoundAC revision string
"""
from __future__ import annotations

import json
import os
import platform
import re
import shutil
import subprocess
import sys
from typing import Any, Dict, List, Optional

from .fsutil import scrub_home

_CACHE: Dict[str, Any] = {}

#: Michael Gogins's Csound p-field conventions, which every instrument and
#: every generated score in the studio follows.
PFIELDS = [
    ("p1", "instrument number (may be fractional)"),
    ("p2", "onset time in seconds (beats under a tempo statement)"),
    ("p3", "duration in seconds (beats under a tempo statement)"),
    ("p4", "pitch as MIDI key number (may be fractional)"),
    ("p5", "loudness as MIDI velocity (may be fractional)"),
    ("p6", "space front to back (Ambisonic X; CsoundAC depth)"),
    ("p7", "space left to right (Ambisonic Y; CsoundAC pan)"),
    ("p8", "space bottom to top (Ambisonic Z; CsoundAC height)"),
    ("p9", "audio phase (CsoundAC phase)"),
    ("p10", "pitch-class set number (CsoundAC pitches)"),
    ("p11", "homogeneity (always 1 from CsoundAC)"),
]

_CSOUNDAC_PROBE = r"""
import json, platform
out = {"python": platform.python_version()}
try:
    import CsoundAC
    out["csoundac"] = True
    out["module"] = getattr(CsoundAC, "__file__", None)
    # time, duration, status, instrument, key, velocity, phase, pan, depth, height, pitches
    e = CsoundAC.Event(1.5, 2.5, 144.0, 3.0, 61.0, 71.0, 0.11, 0.22, 0.33, 0.44, 4095.0)
    out["istatement"] = e.toCsoundIStatement()
    out["default_event_nan"] = "nan" in CsoundAC.Event().toCsoundIStatement().lower()
    out["event_csound_methods"] = sorted(m for m in dir(e) if "sound" in m.lower() or "Blue" in m)
except Exception as error:
    out["csoundac"] = False
    out["error"] = repr(error)
print("STUDIO-PROBE " + json.dumps(out))
"""


def find_tool(variable: str, default: str) -> Optional[str]:
    value = os.environ.get(variable, default)
    if not value or value.lower() == "none":
        return None
    return shutil.which(value)


def _run(argv: List[str], timeout: int = 60) -> Optional[subprocess.CompletedProcess]:
    try:
        return subprocess.run(
            argv, capture_output=True, text=True, timeout=timeout, errors="replace"
        )
    except (OSError, subprocess.SubprocessError):
        return None


def csound_info() -> Dict[str, Any]:
    if "csound" in _CACHE:
        return _CACHE["csound"]
    info: Dict[str, Any] = {"available": False}
    path = find_tool("STUDIO_CSOUND", "csound")
    if path:
        result = _run([path, "--version"])
        text = ((result.stdout or "") + (result.stderr or "")) if result else ""
        match = re.search(r"Csound version\s+([0-9][^\s(]*)", text)
        commit = re.search(r"\[commit:\s*([0-9a-fA-F]+)\]", text)
        info = {
            "available": bool(match),
            "path": scrub_home(path),
            "version": match.group(1) if match else None,
            "commit": commit.group(1) if commit else None,
            "banner": next(
                (line.strip().lstrip("-") for line in text.splitlines() if "Csound version" in line), None
            ),
        }
        if match:
            try:
                info["major"] = int(match.group(1).split(".")[0])
            except ValueError:
                info["major"] = None
    _CACHE["csound"] = info
    return info


def classify_pfield_layout(istatement: Optional[str]) -> str:
    """Decide which p-field layout a CsoundAC build emits, from the probe event."""
    if not istatement:
        return "unknown"
    fields = istatement.split()
    if len(fields) < 10 or fields[0] != "i":
        return "unknown"
    try:
        values = [float(field) for field in fields[1:10]]
    except ValueError:
        return "unknown"
    head = values[:5]
    if [round(v, 6) for v in head] != [3.0, 1.5, 2.5, 61.0, 71.0]:
        return "unknown"
    tail = [round(v, 6) for v in values[5:9]]
    if tail == [0.33, 0.22, 0.44, 0.11]:
        return "gogins"  # p6 depth, p7 pan, p8 height, p9 phase
    if tail == [0.11, 0.22, 0.33, 0.44]:
        return "legacy"  # p6 phase, p7 pan, p8 depth, p9 height
    return "unknown"


def python_info() -> Dict[str, Any]:
    if "python" in _CACHE:
        return _CACHE["python"]
    executable = os.environ.get("STUDIO_PYTHON") or sys.executable
    resolved = shutil.which(executable) or executable
    info: Dict[str, Any] = {"path": scrub_home(resolved), "csoundac": False}
    result = _run([resolved, "-c", _CSOUNDAC_PROBE])
    if result is not None:
        for line in (result.stdout or "").splitlines():
            if line.startswith("STUDIO-PROBE "):
                probe = json.loads(line[len("STUDIO-PROBE "):])
                info["version"] = probe.get("python")
                info["csoundac"] = bool(probe.get("csoundac"))
                if probe.get("csoundac"):
                    info["csoundac_module"] = scrub_home(probe.get("module"))
                    info["csoundac_probe_istatement"] = (probe.get("istatement") or "").strip()
                    info["csoundac_pfield_layout"] = classify_pfield_layout(probe.get("istatement"))
                    info["csoundac_event_csound_methods"] = probe.get("event_csound_methods")
                    info["csoundac_default_event_nan"] = bool(probe.get("default_event_nan"))
                else:
                    info["csoundac_error"] = probe.get("error")
    info["csoundac_revision"] = csoundac_revision()
    _CACHE["python"] = info
    return info


def csoundac_revision() -> Optional[str]:
    explicit = os.environ.get("STUDIO_CSOUNDAC_REVISION")
    if explicit:
        return explicit
    source = os.environ.get("CSOUNDAC_SOURCE_DIR")
    if source and os.path.isdir(source):
        result = _run(["git", "-C", source, "describe", "--tags", "--always", "--dirty"])
        if result and result.returncode == 0 and result.stdout.strip():
            return result.stdout.strip() + " (source checkout; installed binary may differ)"
    return None


def node_info() -> Dict[str, Any]:
    if "node" in _CACHE:
        return _CACHE["node"]
    info: Dict[str, Any] = {"available": False}
    path = find_tool("STUDIO_NODE", "node")
    if path:
        result = _run([path, "--version"])
        if result and result.returncode == 0:
            info = {"available": True, "path": scrub_home(path), "version": result.stdout.strip()}
    wasm_dir = os.environ.get("CSOUNDAC_WASM_DIR")
    info["csoundac_wasm_dir"] = scrub_home(wasm_dir) if wasm_dir else None
    info["csoundac_wasm_present"] = bool(
        wasm_dir
        and os.path.isfile(os.path.join(wasm_dir, "CsoundAC.js"))
        and os.path.isfile(os.path.join(wasm_dir, "CsoundAC.wasm"))
    )
    _CACHE["node"] = info
    return info


def ffmpeg_info() -> Dict[str, Any]:
    if "ffmpeg" in _CACHE:
        return _CACHE["ffmpeg"]
    info: Dict[str, Any] = {"available": False}
    path = find_tool("STUDIO_FFMPEG", "ffmpeg")
    if path:
        result = _run([path, "-hide_banner", "-version"])
        if result and result.returncode == 0:
            first = (result.stdout or "").splitlines()[0] if result.stdout else ""
            match = re.search(r"ffmpeg version\s+(\S+)", first)
            info = {
                "available": True,
                "path": scrub_home(path),
                "version": match.group(1) if match else first,
            }
    _CACHE["ffmpeg"] = info
    return info


def collect() -> Dict[str, Any]:
    """Everything recorded in a manifest about the rendering environment."""
    return {
        "platform": platform.platform(),
        "machine": platform.machine(),
        "studio_python": platform.python_version(),
        "csound": csound_info(),
        "python": python_info(),
        "node": node_info(),
        "ffmpeg": ffmpeg_info(),
    }


def reset_cache() -> None:
    _CACHE.clear()


def problems(require_csoundac: bool = False) -> List[str]:
    """What is missing for the studio workflow in this environment."""
    found: List[str] = []
    csound = csound_info()
    if not csound.get("available"):
        found.append("csound not found (set STUDIO_CSOUND or install Csound 7)")
    elif csound.get("major") != 7:
        found.append(f"Csound {csound.get('version')} found, but Csound 7 is required")
    if not ffmpeg_info().get("available"):
        found.append("ffmpeg not found: FLAC masters and MP3 listening copies cannot be made")
    python = python_info()
    if not python.get("csoundac"):
        message = "CsoundAC is not importable by " + str(python.get("path"))
        if require_csoundac:
            found.append(message + " (set STUDIO_PYTHON)")
    elif python.get("csoundac_pfield_layout") != "gogins":
        found.append(
            "CsoundAC toCsoundIStatement() does not emit the studio p-field layout "
            f"(detected: {python.get('csoundac_pfield_layout')}); format i-statements "
            "explicitly from Event getters instead"
        )
    return found
