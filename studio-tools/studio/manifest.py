"""Provenance manifests.

A manifest records where a composition version came from and how to
reconstruct it. It is metadata only: the music itself is the .csd file, and
nothing here is ever read back as a score.
"""
from __future__ import annotations

import re
from pathlib import Path
from typing import Any, Dict, List, Optional

from . import naming
from .fsutil import IntegrityError, read_json

SCHEMA = "ai-composition-studio/manifest/1"

_REQUIRED = {
    "schema": str,
    "composition": str,
    "title": str,
    "ordinal": int,
    "created_at": str,
    "prompts": list,
    "new_prompts": list,
    "files": dict,
    "history": list,
    "ai": dict,
    "generator": dict,
    "environment": dict,
    "render": dict,
    "validation": dict,
    "dependencies": dict,
}

_AUDIO_STRING = re.compile(r'"([^"\n]+\.(?:wav|aif|aiff|flac|ogg|mp3|sf2|sfz))"', re.IGNORECASE)
_INCLUDE = re.compile(r'^\s*#include\s+"([^"\n]+)"', re.MULTILINE)
_OPCODE_LIB = re.compile(r"--opcode-lib=(\S+)")


def detect_dependencies(csd_text: str) -> Dict[str, List[str]]:
    """External files a .csd appears to refer to (a heuristic scan)."""
    return {
        "includes": sorted(set(_INCLUDE.findall(csd_text))),
        "samples": sorted(set(_AUDIO_STRING.findall(csd_text))),
        "plugins": sorted(set(_OPCODE_LIB.findall(csd_text))),
    }


def infer_generator_kind(code_files: List[str], code_texts: Dict[str, str]) -> str:
    if not code_files:
        return "direct-csd"
    suffixes = {Path(rel).suffix.lower() for rel in code_files}
    # C++ programs name CsoundAC only through its namespace, csound::.
    uses_csoundac = any("CsoundAC" in text or "csound::" in text for text in code_texts.values())
    if ".py" in suffixes:
        return "python-csoundac" if uses_csoundac else "python"
    if suffixes & {".js", ".mjs"}:
        return "javascript-csoundac-wasm" if uses_csoundac else "javascript"
    if suffixes & {".cpp", ".cc", ".cxx"}:
        return "cpp-csoundac" if uses_csoundac else "cpp"
    return "program"


def build(
    name: str,
    *,
    title: str,
    ordinal: int,
    created_at: str,
    prompts: List[str],
    new_prompts: List[str],
    files: Dict[str, Dict[str, Any]],
    history: List[Dict[str, Any]],
    ai: Dict[str, Any],
    generator: Dict[str, Any],
    environment: Dict[str, Any],
    render: Dict[str, Any],
    validation: Dict[str, Any],
    dependencies: Dict[str, Any],
    notes: Optional[str] = None,
) -> Dict[str, Any]:
    manifest: Dict[str, Any] = {
        "schema": SCHEMA,
        "composition": name,
        "title": title,
        "ordinal": ordinal,
        "created_at": created_at,
        "prompts": prompts,
        "new_prompts": new_prompts,
        "files": files,
        "history": history,
        "ai": ai,
        "generator": generator,
        "environment": environment,
        "render": render,
        "validation": validation,
        "dependencies": dependencies,
    }
    if notes:
        manifest["notes"] = notes
    return manifest


def problems(manifest: Any, name: str) -> List[str]:
    found: List[str] = []
    if not isinstance(manifest, dict):
        return ["manifest is not a JSON object"]
    for key, kind in _REQUIRED.items():
        if key not in manifest:
            found.append(f"manifest: missing {key!r}")
        elif not isinstance(manifest[key], kind) or isinstance(manifest[key], bool):
            found.append(f"manifest: {key!r} should be {kind.__name__}")
    if found:
        return found
    if manifest["schema"] != SCHEMA:
        found.append(f"manifest: unknown schema {manifest['schema']!r}")
    if manifest["composition"] != name:
        found.append(f"manifest: composition is {manifest['composition']!r}, expected {name!r}")
    if manifest["ordinal"] < 1:
        found.append("manifest: ordinal must be >= 1")
    if len(manifest["history"]) != manifest["ordinal"] - 1:
        found.append(
            f"manifest: ordinal {manifest['ordinal']} needs {manifest['ordinal'] - 1} "
            f"history entries, found {len(manifest['history'])}"
        )
    for index, entry in enumerate(manifest["history"], start=1):
        if not isinstance(entry, dict) or entry.get("version") != naming.version_dir_name(index):
            found.append(f"manifest: history entry {index} is not {naming.version_dir_name(index)}")
        elif not isinstance(entry.get("tree_sha256"), str):
            found.append(f"manifest: history entry {index} has no tree_sha256")
    expected_prompts = [naming.prompt_name(name, i) for i in range(1, len(manifest["prompts"]) + 1)]
    if manifest["prompts"] != expected_prompts:
        found.append("manifest: prompts are not the contiguous numbered list")
    if not manifest["new_prompts"] or any(p not in manifest["prompts"] for p in manifest["new_prompts"]):
        found.append("manifest: new_prompts must name at least one of the prompts")
    if naming.manifest_name(name) in manifest["files"]:
        found.append("manifest: files must not list the manifest itself")
    for rel, entry in manifest["files"].items():
        if not isinstance(entry, dict) or not isinstance(entry.get("sha256"), str):
            found.append(f"manifest: files[{rel!r}] has no sha256")
    return found


def load(state_dir: Path, name: str) -> Dict[str, Any]:
    path = Path(state_dir) / naming.manifest_name(name)
    if not path.is_file():
        raise IntegrityError(f"missing manifest: {path}")
    try:
        manifest = read_json(path)
    except ValueError as error:
        raise IntegrityError(f"unreadable manifest {path}: {error}") from error
    found = problems(manifest, name)
    if found:
        raise IntegrityError(f"invalid manifest {path}: " + "; ".join(found))
    return manifest
