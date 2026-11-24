"""Local compose API used by ``studio serve`` at ``/api/v1``.

The viewer prompt field posts here. The path is the same as Cursor's:

    stage → prompt → (hook or AI rewrite of the .csd) → render → accept → publish

No third-party packages: Anthropic is called with ``urllib`` when a key is set.
Without a key, a hook, or ``STUDIO_COMPOSE_ALLOW_UNCHANGED``, compose refuses
rather than republishing unchanged music.
"""
from __future__ import annotations

import json
import os
import shlex
import subprocess
import urllib.error
import urllib.request
from pathlib import Path
from typing import Any, Dict, Optional

from . import publish as publish_module
from . import render as render_module
from . import staging
from . import versioning
from .paths import Studio, StudioError

DEFAULT_MODEL = "claude-sonnet-4-20250514"
ANTHROPIC_URL = "https://api.anthropic.com/v1/messages"
REQUEST_FILE = ".compose-request.txt"

_STUB_CSD = """\
<CsoundSynthesizer>
<CsOptions>
-d
</CsOptions>
<CsInstruments>
sr = 48000
ksmps = 32
nchnls = 2
0dbfs = 1
instr 1
  aEnvelope linseg 0, 0.02, 1, p3 - 0.04, 1, 0.02, 0
  aSignal poscil aEnvelope * 0.3 * (p5 / 127), cpsmidinn(p4)
  aLeft, aRight pan2 aSignal, p7
  outs aLeft, aRight
endin
</CsInstruments>
<CsScore>
i 1 0 1 69 90 0 0.5
</CsScore>
</CsoundSynthesizer>
"""


def _truthy(name: str) -> bool:
    value = (os.environ.get(name) or "").strip().lower()
    return value not in ("", "0", "false", "no", "off")


def _api_key() -> Optional[str]:
    for name in ("STUDIO_COMPOSE_API_KEY", "ANTHROPIC_API_KEY"):
        value = (os.environ.get(name) or "").strip()
        if value:
            return value
    return None


def _compose_model() -> str:
    return (os.environ.get("STUDIO_COMPOSE_MODEL") or DEFAULT_MODEL).strip() or DEFAULT_MODEL


def _compose_agent() -> str:
    return (os.environ.get("STUDIO_COMPOSE_AGENT") or "studio serve compose API").strip()


def status() -> Dict[str, Any]:
    """Capabilities advertised to the viewer."""
    hook = (os.environ.get("STUDIO_COMPOSE_HOOK") or "").strip()
    key = _api_key()
    model = _compose_model() if key else None
    payload: Dict[str, Any] = {
        "ok": True,
        "mode": "local",
        "compose_hook": bool(hook),
        "compose_ai": bool(key),
        "compose_model": model,
        "allow_unchanged": _truthy("STUDIO_COMPOSE_ALLOW_UNCHANGED"),
    }
    if hook:
        payload["message"] = "Compose hook is configured."
    elif key:
        payload["message"] = f"Compose AI ready ({model})."
    elif payload["allow_unchanged"]:
        payload["message"] = "Compose allows unchanged music (testing / stub mode)."
    else:
        payload["message"] = (
            "Compose API is up, but no AI key is configured for studio serve. "
            "Export ANTHROPIC_API_KEY (or set STUDIO_COMPOSE_HOOK)."
        )
    return payload


def compose(
    studio: Studio,
    *,
    prompt: str,
    composition: Optional[str] = None,
    name: Optional[str] = None,
    title: Optional[str] = None,
) -> Dict[str, Any]:
    """Create or revise a composition from a viewer prompt and publish it."""
    text = (prompt or "").strip()
    if not text:
        raise StudioError("refusing to record an empty prompt")

    existing = (composition or "").strip() or None
    new_name = (name or "").strip() or None
    if existing and new_name and existing != new_name:
        raise StudioError("pass either composition (revision) or name (new), not both")
    if not existing and not new_name:
        raise StudioError("composition or name is required")

    if existing:
        staging_dir = staging.stage_revision(studio, existing)
        kind = "revision"
        composition_name = existing
    else:
        staging_dir = staging.new_composition(studio, new_name, title=title)
        kind = "new"
        composition_name = new_name

    staging.add_prompt(staging_dir, text, source="user")
    request_path = staging_dir / REQUEST_FILE
    request_path.write_text(text if text.endswith("\n") else text + "\n", encoding="utf-8")

    csd_path = staging_dir / f"{composition_name}.csd"
    hook = (os.environ.get("STUDIO_COMPOSE_HOOK") or "").strip()
    key = _api_key()
    allow_unchanged = _truthy("STUDIO_COMPOSE_ALLOW_UNCHANGED")

    if hook:
        _run_hook(hook, studio, staging_dir, composition_name)
    elif key:
        if not csd_path.is_file():
            csd_path.write_text(_STUB_CSD, encoding="utf-8")
        rewritten = _rewrite_with_anthropic(key, text, csd_path.read_text(encoding="utf-8"), composition_name)
        csd_path.write_text(rewritten if rewritten.endswith("\n") else rewritten + "\n", encoding="utf-8")
    elif allow_unchanged:
        if not csd_path.is_file():
            csd_path.write_text(_STUB_CSD, encoding="utf-8")
    else:
        raise StudioError(
            "viewer Submit needs a model to rewrite the .csd: export ANTHROPIC_API_KEY "
            "(or STUDIO_COMPOSE_API_KEY), or set STUDIO_COMPOSE_HOOK to a command that "
            "writes the staged .csd. For tests only, set STUDIO_COMPOSE_ALLOW_UNCHANGED=1."
        )

    if not csd_path.is_file():
        raise StudioError(f"compose produced no {csd_path.name}")

    # Hidden request file must not enter the archive.
    if request_path.exists():
        request_path.unlink()

    if hook:
        model_label = "compose-hook"
    elif key:
        model_label = _compose_model()
    else:
        model_label = "compose-unchanged"

    render_module.render(staging_dir)
    accepted = versioning.accept(
        studio,
        staging_dir,
        ai_model=model_label,
        ai_agent=_compose_agent(),
    )
    publish_module.publish(studio, [composition_name])
    described = versioning.describe(studio, composition_name)
    return {
        "ok": True,
        "composition": composition_name,
        "kind": kind,
        "version": "current",
        "ordinal": described["ordinal"],
        "archived_as": accepted.get("archived_as"),
        "message": (
            f"Published {composition_name} ({kind}, ordinal {described['ordinal']}"
            f", via {model_label})."
        ),
        "title": described.get("title"),
    }


def _run_hook(hook: str, studio: Studio, staging_dir: Path, composition_name: str) -> None:
    env = os.environ.copy()
    env["STUDIO_COMPOSITION"] = composition_name
    env["STUDIO_STAGING"] = str(staging_dir)
    env["STUDIO_ROOT"] = str(studio.root)
    try:
        command = shlex.split(hook)
    except ValueError as error:
        raise StudioError(f"invalid STUDIO_COMPOSE_HOOK: {error}") from error
    if not command:
        raise StudioError("STUDIO_COMPOSE_HOOK is empty")
    try:
        completed = subprocess.run(
            command,
            cwd=str(staging_dir),
            env=env,
            check=False,
            capture_output=True,
            text=True,
        )
    except OSError as error:
        raise StudioError(f"failed to run STUDIO_COMPOSE_HOOK: {error}") from error
    if completed.returncode != 0:
        detail = (completed.stderr or completed.stdout or "").strip()
        raise StudioError(
            f"STUDIO_COMPOSE_HOOK exited {completed.returncode}"
            + (f": {detail}" if detail else "")
        )


def _rewrite_with_anthropic(api_key: str, prompt: str, current_csd: str, name: str) -> str:
    system = (
        "You write complete Csound .csd files for the AI composition studio. "
        "Reply with only the .csd text: a single <CsoundSynthesizer> document "
        "containing <CsOptions>, <CsInstruments> and <CsScore>. "
        "Use 0dbfs = 1, sr = 48000, stereo output, and Michael Gogins's p-field "
        "conventions (p4 MIDI key, p5 MIDI velocity, p6 depth, p7 pan, …). "
        "Do not wrap the answer in markdown fences."
    )
    user = (
        f"Composition name: {name}\n\n"
        f"Prompt:\n{prompt}\n\n"
        f"Current .csd (may be a stub or prior version):\n{current_csd}\n"
    )
    body = {
        "model": _compose_model(),
        "max_tokens": 8192,
        "system": system,
        "messages": [{"role": "user", "content": user}],
    }
    request = urllib.request.Request(
        ANTHROPIC_URL,
        data=json.dumps(body).encode("utf-8"),
        headers={
            "content-type": "application/json",
            "x-api-key": api_key,
            "anthropic-version": "2023-06-01",
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(request, timeout=120) as response:
            payload = json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as error:
        detail = error.read().decode("utf-8", errors="replace")
        raise StudioError(f"Anthropic API error {error.code}: {detail}") from error
    except urllib.error.URLError as error:
        raise StudioError(f"Anthropic API request failed: {error}") from error

    parts = []
    for block in payload.get("content") or []:
        if isinstance(block, dict) and block.get("type") == "text":
            parts.append(block.get("text") or "")
    text = "\n".join(parts).strip()
    if text.startswith("```"):
        lines = text.splitlines()
        if lines and lines[0].startswith("```"):
            lines = lines[1:]
        if lines and lines[-1].strip() == "```":
            lines = lines[:-1]
        text = "\n".join(lines).strip()
    if "<CsoundSynthesizer>" not in text or "</CsoundSynthesizer>" not in text:
        raise StudioError("model reply was not a complete CsoundSynthesizer document")
    return text
