"""Viewer compose path: stage → prompt → rewrite → render → accept → publish.

This is what ``studio serve`` exposes under ``/api/v1``. The HTTP wiring
lives in ``serve.py``; this module is the work itself. Cursor agents still
drive the same archive through the ``studio`` commands; both paths finish
with accept and publish so the catalog stays one list.

Rewriting the ``.csd`` happens in one of three ways, chosen by environment:

* ``STUDIO_COMPOSE_HOOK`` — a command that writes ``NAME.csd`` in the staging
  directory (receives ``STUDIO_STAGING``, ``STUDIO_COMPOSITION``, and the
  prompt in ``.compose-request.txt``).
* ``ANTHROPIC_API_KEY`` or ``STUDIO_COMPOSE_API_KEY`` — call Anthropic's
  Messages API to rewrite the staged ``.csd``.
* ``STUDIO_COMPOSE_ALLOW_UNCHANGED=1`` — keep the prior ``.csd`` on a
  revision, or write a short stub for a new piece (tests and dry runs).

Without a hook, a key, or that allow flag, compose refuses rather than
republishing the same music.
"""
from __future__ import annotations

import json
import os
import re
import shlex
import subprocess
import urllib.error
import urllib.request
from pathlib import Path
from typing import Any, Dict, Optional

from . import publish, render, staging, versioning
from .paths import Studio, StudioError

DEFAULT_MODEL = "claude-sonnet-4-5"
ANTHROPIC_URL = "https://api.anthropic.com/v1/messages"
ANTHROPIC_VERSION = "2023-06-01"

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


def _truthy(value: Optional[str]) -> bool:
    return bool(value) and value.strip().lower() in ("1", "true", "yes", "on")


def _api_key() -> Optional[str]:
    return os.environ.get("ANTHROPIC_API_KEY") or os.environ.get("STUDIO_COMPOSE_API_KEY") or None


def _hook_command() -> Optional[str]:
    value = os.environ.get("STUDIO_COMPOSE_HOOK")
    return value.strip() if value and value.strip() else None


def _compose_model() -> str:
    return (
        os.environ.get("STUDIO_COMPOSE_MODEL")
        or os.environ.get("STUDIO_AI_MODEL")
        or DEFAULT_MODEL
    )


def _compose_agent() -> str:
    return (
        os.environ.get("STUDIO_COMPOSE_AGENT")
        or os.environ.get("STUDIO_AI_AGENT")
        or "studio serve (viewer)"
    )


def status() -> Dict[str, Any]:
    """What the viewer shows about compose readiness."""
    hook = _hook_command() is not None
    key = _api_key() is not None
    allow = _truthy(os.environ.get("STUDIO_COMPOSE_ALLOW_UNCHANGED"))
    model = _compose_model() if key else None
    result: Dict[str, Any] = {
        "ok": True,
        "mode": "local",
        "compose_hook": hook,
        "compose_ai": key,
        "compose_model": model,
        "allow_unchanged": allow,
    }
    if hook:
        result["message"] = "Compose hook is configured."
    elif key:
        result["message"] = f"Compose AI ready ({model})."
    elif allow:
        result["message"] = "Compose allows unchanged .csd (no AI key or hook)."
    else:
        result["message"] = (
            "Compose API is up, but no AI key is configured for studio serve."
        )
    return result


def compose(
    studio: Studio,
    *,
    prompt: str,
    composition: Optional[str] = None,
    name: Optional[str] = None,
    title: Optional[str] = None,
) -> Dict[str, Any]:
    """Run one viewer compose request end to end."""
    text = (prompt or "").strip()
    if not text:
        raise StudioError("refusing to record an empty prompt")

    composition_name = (composition or "").strip() or None
    new_name = (name or "").strip() or None
    if composition_name and new_name and composition_name != new_name:
        raise StudioError("pass either composition (revise) or name (new), not both")
    target = composition_name or new_name
    if not target:
        raise StudioError("composition or name is required")

    hook = _hook_command()
    key = _api_key()
    allow_unchanged = _truthy(os.environ.get("STUDIO_COMPOSE_ALLOW_UNCHANGED"))
    if not hook and not key and not allow_unchanged:
        raise StudioError(
            "compose needs a way to rewrite the .csd: set ANTHROPIC_API_KEY "
            "(or STUDIO_COMPOSE_API_KEY), or STUDIO_COMPOSE_HOOK, before submitting"
        )

    existing = studio.composition_dir(target)
    if composition_name or existing.is_dir():
        if not existing.is_dir():
            raise StudioError(f"no composition named {target!r}; use name= to create one")
        staging_dir = staging.stage_revision(studio, target)
        kind = "revision"
    else:
        staging_dir = staging.new_composition(studio, target, title=title or None)
        kind = "new"

    try:
        staging.add_prompt(staging_dir, text, source="user")
        request_path = staging_dir / ".compose-request.txt"
        request_path.write_text(text, encoding="utf-8")
        csd_path = staging_dir / f"{target}.csd"

        if hook:
            _run_hook(hook, staging_dir, target)
        elif key:
            _rewrite_with_anthropic(csd_path, prompt=text, composition=target, kind=kind)
        elif kind == "new":
            csd_path.write_text(_STUB_CSD, encoding="utf-8")
        # revision + allow_unchanged: keep the copied .csd

        if not csd_path.is_file():
            raise StudioError(f"compose did not produce {csd_path.name}")

        render.render(staging_dir)
        accepted = versioning.accept(
            studio,
            staging_dir,
            ai_model=_compose_model() if (hook or key) else "compose-unchanged",
            ai_agent=_compose_agent(),
            title=title if kind == "new" else None,
        )
        publish.publish(studio, [target])
    except Exception:
        try:
            staging.discard(studio, staging_dir)
        except Exception:  # noqa: BLE001 - best-effort cleanup
            pass
        raise

    return {
        "ok": True,
        "composition": target,
        "version": "current",
        "kind": kind,
        "ordinal": accepted["ordinal"],
        "message": (
            f"Published {target} as current "
            f"({'new composition' if kind == 'new' else 'revision'}, "
            f"ordinal {accepted['ordinal']})."
        ),
    }


def _run_hook(command: str, staging_dir: Path, name: str) -> None:
    env = os.environ.copy()
    env["STUDIO_STAGING"] = str(staging_dir)
    env["STUDIO_COMPOSITION"] = name
    try:
        argv = shlex.split(command)
    except ValueError as error:
        raise StudioError(f"invalid STUDIO_COMPOSE_HOOK: {error}") from error
    if not argv:
        raise StudioError("STUDIO_COMPOSE_HOOK is empty")
    done = subprocess.run(
        argv,
        cwd=str(staging_dir),
        env=env,
        capture_output=True,
        text=True,
        timeout=3600,
    )
    if done.returncode != 0:
        detail = (done.stderr or done.stdout or "").strip() or f"exit {done.returncode}"
        raise StudioError(f"compose hook failed: {detail}")


def _rewrite_with_anthropic(
    csd_path: Path, *, prompt: str, composition: str, kind: str
) -> None:
    key = _api_key()
    if not key:
        raise StudioError("ANTHROPIC_API_KEY is not set")
    prior = csd_path.read_text(encoding="utf-8") if csd_path.is_file() else ""
    system = (
        "You write complete Csound .csd files for the AI composition studio. "
        "Reply with only the .csd text: one <CsoundSynthesizer> document with "
        "<CsInstruments> and <CsScore>, 0dbfs = 1, and the studio p-field "
        "conventions (p4 MIDI key, p5 MIDI velocity, p6 depth, p7 pan, "
        "p8 height, p9 phase, p10 pitch-class set, p11 homogeneity). "
        "No markdown fences, no commentary."
    )
    user = (
        f"Composition name: {composition}\n"
        f"Request kind: {kind}\n\n"
        f"Composer prompt:\n{prompt}\n"
    )
    if prior.strip():
        user += f"\nCurrent .csd to revise:\n{prior}\n"
    body = {
        "model": _compose_model(),
        "max_tokens": 16000,
        "system": system,
        "messages": [{"role": "user", "content": user}],
    }
    request = urllib.request.Request(
        ANTHROPIC_URL,
        data=json.dumps(body).encode("utf-8"),
        headers={
            "content-type": "application/json",
            "x-api-key": key,
            "anthropic-version": ANTHROPIC_VERSION,
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(request, timeout=300) as response:
            payload = json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as error:
        detail = error.read().decode("utf-8", errors="replace")[:500]
        raise StudioError(f"Anthropic request failed ({error.code}): {detail}") from error
    except urllib.error.URLError as error:
        raise StudioError(f"Anthropic request failed: {error}") from error

    chunks = []
    for block in payload.get("content") or []:
        if isinstance(block, dict) and block.get("type") == "text":
            chunks.append(str(block.get("text") or ""))
    text = "\n".join(chunks).strip()
    if not text:
        raise StudioError("Anthropic returned no text for the .csd")
    text = _strip_markdown_fence(text)
    if "<CsoundSynthesizer>" not in text:
        raise StudioError("Anthropic reply was not a Csound .csd")
    csd_path.write_text(text if text.endswith("\n") else text + "\n", encoding="utf-8")


def _strip_markdown_fence(text: str) -> str:
    match = re.search(r"```(?:csound|csd)?\s*([\s\S]*?)```", text, re.IGNORECASE)
    if match:
        return match.group(1).strip()
    return text
