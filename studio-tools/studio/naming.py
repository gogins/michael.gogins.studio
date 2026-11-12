"""Naming conventions for compositions, versions, prompts and artifacts.

For a composition called ``nu``:

    nu.csd                 the canonical Csound file
    nu-manifest.json       provenance manifest
    nu-prompt-001.md       prompts, numbered in order
    code/nu-*.py|js|cpp    generated compositional programs
    outputs/nu.*           rendered audio and derived outputs
    outputs/nu-*.*         logs and other outputs
    version-0001/          archived versions, same layout inside

Nothing else is allowed in a composition state.
"""
from __future__ import annotations

import re
from pathlib import Path
from typing import Dict, List, Optional

from .paths import StudioError

# \Z, not $: a trailing newline must not slip through.
COMPOSITION_RE = re.compile(r"\A[A-Za-z0-9][A-Za-z0-9_]{0,63}\Z")
FILENAME_RE = re.compile(r"\A[A-Za-z0-9][A-Za-z0-9._-]{0,127}\Z")
VERSION_DIR_RE = re.compile(r"\Aversion-([0-9]{4,})\Z")

CODE_DIR = "code"
OUTPUTS_DIR = "outputs"

#: Kinds of artifacts, used for publication selection and the catalog.
AUDIO_LISTENING_EXT = {".mp3", ".opus", ".ogg", ".oga", ".m4a", ".aac"}
AUDIO_MASTER_EXT = {".wav", ".flac", ".aif", ".aiff", ".w64"}
DERIVED_KINDS = {
    ".mid": "midi",
    ".midi": "midi",
    ".musicxml": "musicxml",
    ".mxl": "musicxml",
    ".xml": "musicxml",
    ".pdf": "pdf",
    ".png": "image",
    ".svg": "image",
    ".jpg": "image",
    ".ly": "lilypond",
}
CODE_LANGUAGES = {
    ".py": "python",
    ".js": "javascript",
    ".mjs": "javascript",
    ".cpp": "cpp",
    ".cc": "cpp",
    ".cxx": "cpp",
    ".hpp": "cpp",
    ".h": "cpp",
    ".c": "c",
    ".lua": "lua",
    ".scm": "scheme",
    ".lisp": "lisp",
    ".orc": "csound",
    ".sco": "csound",
    ".inc": "csound",
    ".json": "json",
    ".txt": "text",
    ".md": "markdown",
    ".sh": "shell",
}


class NamingError(StudioError):
    """A name violates the studio naming conventions."""


def validate_composition_name(name: str) -> str:
    if not isinstance(name, str) or not COMPOSITION_RE.match(name):
        raise NamingError(
            f"invalid composition name {name!r}: use 1-64 letters, digits or "
            "underscores, starting with a letter or digit (no hyphens, spaces or dots)"
        )
    return name


def csd_name(name: str) -> str:
    return f"{name}.csd"


def manifest_name(name: str) -> str:
    return f"{name}-manifest.json"


def prompt_name(name: str, number: int) -> str:
    if number < 1:
        raise NamingError("prompt numbers start at 1")
    return f"{name}-prompt-{number:03d}.md"


def parse_prompt_number(name: str, filename: str) -> Optional[int]:
    match = re.match(rf"\A{re.escape(name)}-prompt-([0-9]{{3,}})\.md\Z", filename)
    return int(match.group(1)) if match else None


def version_dir_name(number: int) -> str:
    if number < 1:
        raise NamingError("version numbers start at 1")
    return f"version-{number:04d}"


def parse_version_dir(dirname: str) -> Optional[int]:
    match = VERSION_DIR_RE.match(dirname)
    if not match:
        return None
    number = int(match.group(1))
    # Canonical form only: version-0007, never version-00007.
    return number if number >= 1 and version_dir_name(number) == dirname else None


def is_hidden(relpath: str) -> bool:
    return any(part.startswith(".") for part in relpath.split("/"))


def classify(name: str, relpath: str) -> Optional[Dict[str, str]]:
    """Classify a file of a composition state by its relative path.

    Returns a dict with ``kind`` (csd, manifest, prompts, code, audio, master,
    derived, logs) and details, or None if the path is not allowed.
    """
    parts = relpath.split("/")
    base = parts[-1]
    if not all(FILENAME_RE.match(part) for part in parts):
        return None
    suffix = Path(base).suffix.lower()
    if len(parts) == 1:
        if base == csd_name(name):
            return {"kind": "csd"}
        if base == manifest_name(name):
            return {"kind": "manifest"}
        number = parse_prompt_number(name, base)
        if number is not None:
            return {"kind": "prompts", "number": str(number)}
        return None
    if parts[0] == CODE_DIR:
        if not base.startswith(f"{name}-"):
            return None
        return {"kind": "code", "language": CODE_LANGUAGES.get(suffix, "text")}
    if parts[0] == OUTPUTS_DIR:
        stem_ok = base.startswith(f"{name}-") or (
            base.startswith(f"{name}.") and len(base) > len(name) + 1
        )
        if not stem_ok:
            return None
        if suffix in AUDIO_LISTENING_EXT:
            return {"kind": "audio", "format": suffix[1:]}
        if suffix in AUDIO_MASTER_EXT:
            return {"kind": "master", "format": suffix[1:]}
        if suffix in DERIVED_KINDS:
            return {"kind": "derived", "derived": DERIVED_KINDS[suffix]}
        return {"kind": "logs"}
    return None


def state_problems(name: str, files: List[str], *, require_manifest: bool = True) -> List[str]:
    """Check the files of one composition state against the conventions.

    ``files`` are relative POSIX paths (hidden files and version directories
    already excluded). Returns a list of human-readable problems.
    """
    problems: List[str] = []
    prompt_numbers: List[int] = []
    seen_lower: Dict[str, str] = {}
    has_csd = False
    has_manifest = False
    for rel in sorted(files):
        lowered = rel.lower()
        if lowered in seen_lower:
            problems.append(f"{rel}: differs only by case from {seen_lower[lowered]}")
        seen_lower[lowered] = rel
        info = classify(name, rel)
        if info is None:
            problems.append(f"{rel}: not allowed by the naming conventions for {name!r}")
            continue
        if info["kind"] == "csd":
            has_csd = True
        elif info["kind"] == "manifest":
            has_manifest = True
        elif info["kind"] == "prompts":
            prompt_numbers.append(int(info["number"]))
    if not has_csd:
        problems.append(f"missing {csd_name(name)}")
    if require_manifest and not has_manifest:
        problems.append(f"missing {manifest_name(name)}")
    if not prompt_numbers:
        problems.append(f"no prompts ({prompt_name(name, 1)} is required)")
    else:
        expected = list(range(1, len(prompt_numbers) + 1))
        if sorted(prompt_numbers) != expected:
            problems.append(
                f"prompt numbers are not contiguous from 001: {sorted(prompt_numbers)}"
            )
    return problems
