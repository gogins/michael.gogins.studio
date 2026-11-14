"""Validation and rendering of a staged composition.

    1. structural check of the .csd
    2. csound --syntax-check-only
    3. csound render to a temporary 24-bit WAV, outside the repository
    4. scan of the Csound log for run-time errors
    5. measurement of the audio (silence, clipping, levels)
    6. a performance in the viewer's Csound engine (WebAssembly, under Node)

No soundfile is kept: the temporary WAV is measured and deleted (or, on
request, copied to a path outside the archive). What a version sounds like
is defined by its .csd; the manifest records the measurements and the hash
of the rendered samples. The result is recorded in .render.json in the
staging directory. A failed render leaves the current composition alone.
"""
from __future__ import annotations

import json
import os
import re
import shutil
import subprocess
import tempfile
import time
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

from . import audio, environment, fsutil, naming, staging
from .paths import StudioError, safe_join

LOG_LIMIT = 400_000
_REQUIRED_TAGS = ("CsoundSynthesizer", "CsInstruments", "CsScore")
_ERRORS_LINE = re.compile(r"^\s*(\d+) errors in performance", re.MULTILINE)
_RUNTIME_ERRORS = re.compile(r"^.*\b(INIT ERROR|PERF ERROR)\b.*$", re.MULTILINE)
_OUT_OF_RANGE = re.compile(r"samples out of range:((?:[ \t]+\d+)+)")
_REALTIME_OUTPUT = re.compile(r"(?:^|\s)(?:-o\s*|--output=)(dac\S*)", re.MULTILINE)
_FILE_OUTPUT = re.compile(r"(?:^|\s)(?:-o\s*|--output=)(?!dac)\S+", re.MULTILINE)
#: How the temporary render directory is shown in logs and manifests.
TEMPORARY = "<temporary>"


class RenderError(StudioError):
    """Validation or rendering failed; details are in the render log."""


def structural_problems(csd_text: str) -> List[str]:
    found = []
    for tag in _REQUIRED_TAGS:
        opening = len(re.findall(rf"<{tag}[\s>]", csd_text))
        closing = csd_text.count(f"</{tag}>")
        if opening != 1 or closing != 1:
            found.append(f"expected exactly one <{tag}> ... </{tag}> element")
    if not found:
        instruments = csd_text.split("<CsInstruments>", 1)[1].split("</CsInstruments>", 1)[0]
        if not re.search(r"^\s*instr\s+\S", instruments, re.MULTILINE):
            found.append("<CsInstruments> defines no instrument")
    return found


def _trim(text: str) -> str:
    if len(text) <= LOG_LIMIT:
        return text
    half = LOG_LIMIT // 2
    return text[:half] + f"\n[... {len(text) - LOG_LIMIT} characters omitted ...]\n" + text[-half:]


def _execute(argv: List[str], cwd: Path, timeout: int) -> Tuple[Optional[int], str, float]:
    clock = time.monotonic()
    try:
        result = subprocess.run(
            argv, cwd=str(cwd), capture_output=True, text=True, errors="replace", timeout=timeout
        )
        code: Optional[int] = result.returncode
        output = (result.stdout or "") + (result.stderr or "")
    except subprocess.TimeoutExpired as error:
        code = None
        partial = error.stdout or b""
        if isinstance(partial, bytes):
            partial = partial.decode("utf-8", "replace")
        output = f"{partial}\n[timed out after {timeout} s]\n"
    except OSError as error:
        code = None
        output = f"[could not start {argv[0]}: {error}]\n"
    return code, output, round(time.monotonic() - clock, 3)


def log_problems(log_text: str) -> List[str]:
    """Run-time problems reported in a Csound log."""
    found = []
    for match in _ERRORS_LINE.finditer(log_text):
        if int(match.group(1)) > 0:
            found.append(f"Csound reported {match.group(1)} errors in performance")
    lines = [match.group(0).strip() for match in _RUNTIME_ERRORS.finditer(log_text)]
    if lines:
        found.append(f"{len(lines)} run-time error(s), first: {lines[0][:200]}")
    return found


def samples_out_of_range(log_text: str) -> int:
    total = 0
    for match in _OUT_OF_RANGE.finditer(log_text):
        total = max(total, sum(int(n) for n in match.group(1).split()))
    return total


def _display_argv(argv: List[str], scratch: Optional[Path]) -> List[str]:
    """A command as recorded: no home directory, no temporary directory."""
    shown = []
    for arg in argv:
        if scratch is not None:
            arg = arg.replace(str(scratch), TEMPORARY)
        shown.append(fsutil.scrub_home(arg))
    shown[0] = Path(argv[0]).name
    return shown


def browser_check(csd: Path, timeout: int = 3600) -> Dict[str, Any]:
    """Perform a .csd in the viewer's Csound engine (WebAssembly, under Node).

    Returns a record with ``checked``; when the check ran, also ``playable``
    and what the engine reported.
    """
    if os.environ.get("STUDIO_CSOUND_WASM", "").lower() == "none":
        return {"checked": False, "reason": "disabled (STUDIO_CSOUND_WASM=none)", "silent": True}
    engine = environment.browser_engine_path()
    if not engine:
        return {"checked": False, "reason": "the viewer's Csound engine is not installed "
                f"({environment.BROWSER_ENGINE_REL})"}
    node = environment.find_tool("STUDIO_NODE", "node")
    if not node:
        return {"checked": False, "reason": "node was not found"}
    info = environment.browser_engine_info()
    record: Dict[str, Any] = {
        "checked": True,
        "engine": {key: info.get(key) for key in ("package", "version", "sha256")},
    }
    code, output, seconds = _execute(
        [node, str(environment.BROWSER_CHECK_SCRIPT), str(engine), str(csd), str(timeout)],
        csd.parent, timeout + 60,
    )
    record["seconds"] = seconds
    result = None
    for line in output.splitlines():
        if line.startswith("STUDIO-BROWSER-CHECK "):
            try:
                result = json.loads(line[len("STUDIO-BROWSER-CHECK "):])
            except ValueError:
                result = None
    if not isinstance(result, dict):
        tail = " | ".join(output.strip().splitlines()[-3:])
        return {"checked": False, "reason": f"the check did not run (exit {code}): {tail[:300]}"}
    if result.get("engine_error"):
        return {"checked": False, "reason": f"the engine could not be loaded: {result['engine_error']}"}
    for key in ("csound_version", "commit", "compiled", "started", "performed", "timed_out",
                "duration_seconds", "sample_rate", "channels", "realtime_factor", "runtime_errors"):
        if result.get(key) is not None:
            record[key] = result[key]
    record["errors"] = result.get("errors") or []
    record["playable"] = bool(
        result.get("compiled") and result.get("started") and result.get("performed")
        and not result.get("runtime_errors")
    )
    record["messages_tail"] = result.get("messages_tail") or []
    return record


def render(
    staging_dir: Path,
    *,
    wav_to: Optional[Path] = None,
    allow_clipping: bool = False,
    allow_silence: bool = False,
    allow_unplayable: bool = False,
    check_browser: bool = True,
    timeout: int = 3600,
) -> Dict[str, Any]:
    """Validate and render the staged .csd; returns the render record.

    The audio is rendered to a temporary file, measured and deleted. With
    ``wav_to`` the rendered WAV is also copied to that path, which the
    caller must have checked to be outside the archive.

    Raises RenderError on any failure, after writing the record (status
    "failed") and the log, so that the problem can be diagnosed and fixed.
    """
    staging_dir = Path(staging_dir)
    meta = staging.load_meta(staging_dir)
    name = meta["composition"]
    scratch = Path(tempfile.mkdtemp(prefix="studio-render-")).resolve()
    try:
        return _render(
            staging_dir, name, scratch, wav_to=wav_to, allow_clipping=allow_clipping,
            allow_silence=allow_silence, allow_unplayable=allow_unplayable,
            check_browser=check_browser, timeout=timeout,
        )
    finally:
        shutil.rmtree(scratch, ignore_errors=True)


def _render(
    staging_dir: Path,
    name: str,
    scratch: Path,
    *,
    wav_to: Optional[Path],
    allow_clipping: bool,
    allow_silence: bool,
    allow_unplayable: bool,
    check_browser: bool,
    timeout: int,
) -> Dict[str, Any]:
    csd = staging_dir / naming.csd_name(name)
    outputs = safe_join(staging_dir, naming.OUTPUTS_DIR)
    outputs.mkdir(exist_ok=True)
    log_path = outputs / f"{name}-render.log"
    wav = scratch / f"{name}.wav"

    record: Dict[str, Any] = {
        "status": "failed",
        "started_at": fsutil.utc_now(),
        "commands": [],
        "checks": [],
        "warnings": [],
        "log": f"{naming.OUTPUTS_DIR}/{log_path.name}",
    }
    log_parts: List[str] = [f"Render of {name} started {record['started_at']}\n"]

    def check(label: str, passed: bool, detail: str = "") -> None:
        record["checks"].append({"check": label, "passed": passed, "detail": detail})

    def write_log() -> None:
        text = "".join(log_parts).replace(str(scratch), TEMPORARY)
        fsutil.atomic_write_text(log_path, _trim(fsutil.scrub_home(text) or ""))

    def fail(message: str) -> None:
        record["error"] = message
        record["finished_at"] = fsutil.utc_now()
        write_log()
        fsutil.atomic_write_json(staging_dir / staging.RENDER_META, record)
        raise RenderError(f"{message}\n(log: {log_path})")

    def run(argv: List[str], label: str, limit: int) -> Tuple[Optional[int], str]:
        code, output, seconds = _execute(argv, staging_dir, limit)
        shown = _display_argv(argv, scratch)
        record["commands"].append({"step": label, "argv": shown, "exit_code": code, "seconds": seconds})
        log_parts.append(f"\n$ {' '.join(shown)}\n{output}\n[exit {code}; {seconds} s]\n")
        return code, output

    # 1. Structure and naming.
    if not csd.is_file():
        fail(f"{csd.name} does not exist in the staging area")
    record["inputs_sha256"] = staging.inputs_hash(name, staging_dir)
    naming_problems = [
        problem
        for problem in naming.state_problems(
            name, fsutil.list_files(staging_dir), require_manifest=False
        )
        if not problem.startswith("no prompts")
    ]
    check("naming conventions", not naming_problems, "; ".join(naming_problems))
    if naming_problems:
        fail("naming problems: " + "; ".join(naming_problems))
    csd_text = csd.read_text(encoding="utf-8", errors="replace")
    structure = structural_problems(csd_text)
    check("complete .csd structure", not structure, "; ".join(structure))
    if structure:
        fail("the .csd is not a complete Csound file: " + "; ".join(structure))
    options = csd_text.split("<CsOptions>", 1)[1].split("</CsOptions>", 1)[0] if "<CsOptions>" in csd_text else ""
    if _FILE_OUTPUT.search(options) or _REALTIME_OUTPUT.search(options):
        record["warnings"].append(
            "<CsOptions> names an audio output; it is ignored: the studio renders to a temporary "
            "file and the viewer plays in real time. Leave the output out of <CsOptions>"
        )

    # 2. Syntax check.
    csound = environment.find_tool("STUDIO_CSOUND", "csound")
    if not csound:
        fail("csound was not found (install Csound 7 or set STUDIO_CSOUND)")
    code, output = run([csound, "--syntax-check-only", csd.name], "syntax-check", min(timeout, 600))
    check("csound --syntax-check-only", code == 0, "" if code == 0 else f"exit {code}")
    if code != 0:
        tail = "\n".join(output.strip().splitlines()[-25:])
        fail(f"Csound syntax check failed (exit {code}):\n{tail}")

    # 3. Render, to a temporary file outside the repository.
    code, output = run([csound, "-d", "-W", "-3", "-o", str(wav), csd.name], "render", timeout)
    check("csound render", code == 0, "" if code == 0 else f"exit {code}")
    if code != 0:
        tail = "\n".join(output.strip().splitlines()[-25:])
        fail(f"Csound render failed (exit {code}):\n{tail}")

    # 4. Run-time errors in the log.
    runtime = log_problems(output)
    check("no run-time errors in the Csound log", not runtime, "; ".join(runtime))
    if runtime:
        fail("Csound reported run-time errors: " + "; ".join(runtime))
    if not wav.is_file() or wav.stat().st_size < 64:
        check("audio rendered", False)
        fail("Csound finished but rendered no audio")
    check("audio rendered", True, "temporary file, measured and deleted")

    # 5. Measure the audio.
    try:
        analysis = audio.analyze(wav)
    except StudioError as error:
        check("audio readable", False, str(error))
        fail(f"rendered audio could not be read: {error}")
    record["analysis"] = {k: v for k, v in analysis.items() if k != "data_sha256"}
    record["wav_sha256"] = analysis["data_sha256"]
    log_parts.append("\nAudio: " + "; ".join(audio.summary(analysis)) + "\n")
    overs = max(analysis["clipped_samples"], samples_out_of_range(output))
    silent = analysis["peak_dbfs"] is None or analysis["peak_dbfs"] < -80.0
    check("audio is not silent", not silent, f"peak {analysis['peak_dbfs']} dBFS")
    if silent and not allow_silence:
        fail(f"the render is silent (peak {analysis['peak_dbfs']} dBFS)")
    check("no clipping", overs == 0, f"{overs} samples out of range")
    if overs and not allow_clipping:
        fail(
            f"the render clips: {overs} samples out of range. Lower the output level "
            "(or pass --allow-clipping if it is intended)"
        )
    if not silent and analysis["peak_dbfs"] < -20.0:
        record["warnings"].append(f"low level: peak is {analysis['peak_dbfs']} dBFS")
    if silent:
        record["warnings"].append("render is silent (accepted with --allow-silence)")
    if overs:
        record["warnings"].append(f"{overs} samples out of range (accepted with --allow-clipping)")

    # 6. The viewer's engine: the piece is heard by performing the .csd there.
    if check_browser:
        browser = browser_check(csd, timeout)
        silent_skip = browser.pop("silent", False)
        tail = browser.pop("messages_tail", [])
        record["browser"] = browser
        if browser["checked"]:
            engine = browser["engine"]
            label = f"{engine.get('package') or 'Csound WebAssembly'} {engine.get('version') or ''}".strip()
            log_parts.append(
                f"\nBrowser engine ({label}): "
                + json.dumps({k: v for k, v in browser.items() if k != "engine"}, sort_keys=True)
                + "\n" + "\n".join(tail) + "\n"
            )
            detail = "; ".join(browser["errors"][:2]) if not browser["playable"] else (
                f"{browser.get('duration_seconds')} s at {browser.get('realtime_factor')} times real time here"
            )
            check(f"performs in the viewer's Csound engine ({label})", browser["playable"], detail)
            if not browser["playable"]:
                reason = (
                    "; ".join(browser["errors"][:3])
                    or ("it did not finish within the time limit" if browser.get("timed_out") else "")
                    or "no error message was given"
                )
                message = (
                    f"the piece does not perform in the viewer's Csound engine ({label}): {reason}. "
                    "The viewer is how a version is heard, so use only what that engine provides"
                )
                if not allow_unplayable:
                    fail(message + " (or pass --allow-unplayable to keep a piece that cannot be heard there)")
                record["warnings"].append(message + " (accepted with --allow-unplayable)")
            else:
                duration = browser.get("duration_seconds")
                if duration is not None and abs(duration - analysis["duration_seconds"]) > 0.5:
                    record["warnings"].append(
                        f"the viewer's engine performed {duration} s but native Csound rendered "
                        f"{analysis['duration_seconds']} s"
                    )
                factor = browser.get("realtime_factor")
                if factor is not None and factor < 2.0:
                    record["warnings"].append(
                        f"in WebAssembly this machine performs the piece only {factor} times faster than "
                        "real time; it may not keep up in a browser, especially on a phone"
                    )
        elif not silent_skip:
            record["warnings"].append(
                f"not checked in the viewer's Csound engine: {browser['reason']}"
            )

    # Keep a copy of the audio only where the caller asked for one.
    if wav_to is not None:
        wav_to = Path(wav_to)
        wav_to.parent.mkdir(parents=True, exist_ok=True)
        partial = wav_to.with_name(wav_to.name + ".partial")
        shutil.copyfile(wav, partial)
        os.replace(partial, wav_to)

    record["status"] = "ok"
    record["finished_at"] = fsutil.utc_now()
    log_parts.append(f"\nRender finished {record['finished_at']}: ok\n")
    for warning in record["warnings"]:
        log_parts.append(f"warning: {warning}\n")
    write_log()
    fsutil.atomic_write_json(staging_dir / staging.RENDER_META, record)
    return record


def rerender(
    studio,
    name: str,
    version: Optional[int] = None,
    *,
    regenerate: bool = False,
    keep: bool = False,
    wav_to: Optional[Path] = None,
    timeout: int = 3600,
) -> Dict[str, Any]:
    """Render a stored version again, in a scratch area, and compare.

    The stored version is only read. With ``regenerate`` the recorded
    generator commands are run first and the regenerated .csd is compared
    with the stored one. The rendered samples are compared, by hash, with
    those of the render recorded in the manifest. With ``wav_to`` the
    rendered WAV is copied to that path.
    """
    from . import manifest as manifest_module
    from . import versioning

    source = versioning.state_dir(studio, name, version)
    manifest = manifest_module.load(source, name)
    files = fsutil.list_files(source, skip_versions=version is None)
    inputs = [rel for rel in files if (naming.classify(name, rel) or {}).get("kind") in ("csd", "code")]
    scratch = safe_join(
        studio.archive, f".staging/.rerender-{name}-{fsutil.new_token()[:8]}", allow_hidden=True
    )
    scratch.mkdir(parents=True)
    result: Dict[str, Any] = {
        "composition": name,
        "version": naming.version_dir_name(version) if version else "current",
        "scratch": str(scratch) if keep else None,
    }
    try:
        fsutil.copy_files(source, scratch, inputs)
        fsutil.make_tree_writable(scratch)
        fsutil.atomic_write_json(
            scratch / staging.STAGING_META,
            {
                "schema": staging.STAGING_SCHEMA,
                "composition": name,
                "kind": "rerender",
                "title": manifest["title"],
                "base_ordinal": manifest["ordinal"],
                "base_tree_sha256": None,
                "base_prompts": {},
                "created_at": fsutil.utc_now(),
            },
        )
        if regenerate:
            runs = manifest["generator"].get("runs") or []
            if not runs:
                result["regenerated"] = "no generator commands recorded"
            else:
                stored = fsutil.sha256_file(scratch / naming.csd_name(name))
                (scratch / naming.csd_name(name)).unlink()
                for run in runs:
                    staging.run_command(scratch, list(run["argv"]), seed=run.get("seed"), timeout=timeout)
                regenerated = scratch / naming.csd_name(name)
                if not regenerated.is_file():
                    raise RenderError("the recorded generator commands did not write the .csd")
                result["regenerated"] = (
                    "identical .csd" if fsutil.sha256_file(regenerated) == stored else "different .csd"
                )
        record = render(scratch, wav_to=wav_to, allow_clipping=True, allow_silence=True,
                        check_browser=False, timeout=timeout)
        result["analysis"] = record["analysis"]
        result["wav"] = str(wav_to) if wav_to is not None else None
        stored_hash = manifest["render"].get("wav_sha256")
        result["audio"] = (
            "identical to the render recorded in the manifest"
            if stored_hash and stored_hash == record["wav_sha256"]
            else "differs from the render recorded in the manifest"
        )
        result["stored_csound"] = (manifest["environment"].get("csound") or {}).get("banner")
        result["csound"] = environment.csound_info().get("banner")
        return result
    finally:
        if not keep:
            fsutil.remove_tree(scratch)
