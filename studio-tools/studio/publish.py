"""Publication: copy selected files from the archive to docs/compositions/.

The archive (music/ai-compositions/) is only ever read. Files are copied to
the same relative paths under docs/compositions/, and catalog.json is
regenerated from what is published.

docs/compositions/.publish-state.json records the hash of every file this
tool has published. That is how it tells a file it may update or remove
(unchanged since it was published) from one it must not touch (edited by
hand, or put there by something else).
"""
from __future__ import annotations

import re
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Sequence, Tuple

from . import catalog, fsutil, naming, versioning
from .paths import PathSafetyError, Studio, StudioError, check_relative, is_within, safe_join

STATE_NAME = ".publish-state.json"
STATE_SCHEMA = "ai-composition-studio/publish-state/1"
ALL_KINDS = ("csd", "prompts", "code", "manifest", "derived", "logs")
#: Logs (machine details) are published only on request. There is no audio
#: to publish: the viewer performs the published .csd.
DEFAULT_KINDS = ("csd", "prompts", "code", "manifest", "derived")
_FRONT_MATTER = re.compile(rb"\A(?:\xef\xbb\xbf)?---[ \t]*\r?\n")


class PublishError(StudioError):
    """Publication was refused; nothing was written."""


def parse_kinds(text: Optional[str]) -> Tuple[str, ...]:
    if not text:
        return DEFAULT_KINDS
    if text.strip() == "all":
        return ALL_KINDS
    kinds = tuple(part.strip() for part in text.split(",") if part.strip())
    unknown = [kind for kind in kinds if kind not in ALL_KINDS]
    if unknown or not kinds:
        raise PublishError(f"unknown artifact kind(s) {unknown}; choose from {', '.join(ALL_KINDS)}")
    return kinds


def parse_versions(text: Optional[str]) -> Optional[List[Optional[int]]]:
    """None means every version; otherwise a list of None (current) and numbers."""
    if not text or text.strip() == "all":
        return None
    selected: List[Optional[int]] = []
    for part in text.split(","):
        part = part.strip()
        if not part:
            continue
        if part == "current":
            value: Optional[int] = None
        else:
            number = naming.parse_version_dir(part) if part.startswith("version-") else None
            if number is None:
                if not part.isdigit() or int(part) < 1:
                    raise PublishError(f"bad version {part!r}: use 'current', a number, or 'all'")
                number = int(part)
            value = number
        if value not in selected:
            selected.append(value)
    if not selected:
        raise PublishError("no versions selected")
    return selected


def _check_destination_root(studio: Studio) -> None:
    for directory in (studio.docs, studio.published):
        if directory.is_symlink():
            raise PathSafetyError(f"{directory} is a symbolic link")
    if not is_within(studio.published, studio.root / "docs"):
        raise PathSafetyError(f"{studio.published} is not inside docs/")


def load_state(studio: Studio) -> Dict[str, Dict[str, Any]]:
    path = studio.published / STATE_NAME
    if not path.is_file():
        return {}
    data = fsutil.read_json(path)
    if not isinstance(data, dict) or data.get("schema") != STATE_SCHEMA:
        raise PublishError(f"unrecognized {STATE_NAME}; remove it only if you know why it differs")
    files = data.get("files") or {}
    for dest_rel, entry in files.items():
        # The state file is data, not authority: its paths are checked like any other.
        try:
            parts = check_relative(dest_rel).parts
            naming.validate_composition_name(parts[0])
            if len(parts) < 2 or not isinstance(entry, dict):
                raise PublishError("not a published file entry")
        except StudioError as error:
            raise PublishError(f"{STATE_NAME} has an invalid entry {dest_rel!r}: {error}") from error
    return dict(files)


def _save_state(studio: Studio, files: Dict[str, Dict[str, Any]]) -> None:
    path = studio.published / STATE_NAME
    if path.is_file() and load_state(studio) == files:
        return  # unchanged: leave the file (and Git) alone
    studio.published.mkdir(parents=True, exist_ok=True)
    fsutil.atomic_write_json(
        studio.published / STATE_NAME, {"schema": STATE_SCHEMA, "files": dict(sorted(files.items()))}
    )


def _has_front_matter(path: Path) -> bool:
    with open(path, "rb") as handle:
        return bool(_FRONT_MATTER.match(handle.read(16)))


def plan(
    studio: Studio,
    names: Sequence[str],
    *,
    versions: Optional[List[Optional[int]]] = None,
    kinds: Iterable[str] = DEFAULT_KINDS,
    force: bool = False,
) -> Dict[str, Any]:
    """Work out what publishing would do, without writing anything."""
    _check_destination_root(studio)
    if not names:
        raise PublishError("name the composition(s) to publish; nothing is published by default")
    kinds = tuple(kinds)
    state = load_state(studio)
    actions: List[Dict[str, Any]] = []
    stale: List[Dict[str, Any]] = []
    for name in names:
        naming.validate_composition_name(name)
        comp = studio.composition_dir(name)
        if not comp.is_dir():
            raise PublishError(f"no composition named {name!r} in the archive")
        problems = versioning.verify(studio, name)
        if problems:
            raise PublishError(
                f"{name} fails verification and will not be published:\n  " + "\n  ".join(problems)
            )
        available: List[Optional[int]] = [None] + versioning.list_versions(comp)
        selected = available if versions is None else list(versions)
        for version in selected:
            if version not in available:
                raise PublishError(f"{name} has no {naming.version_dir_name(version)}")
        planned = set()
        for version in selected:
            source_dir = versioning.state_dir(studio, name, version)
            prefix = f"{name}/" if version is None else f"{name}/{naming.version_dir_name(version)}/"
            for rel in fsutil.list_files(source_dir, skip_versions=version is None):
                info = naming.classify(name, rel)
                if info is None or info["kind"] not in kinds:
                    continue
                dest_rel = prefix + rel
                source = safe_join(studio.archive, dest_rel)
                destination = safe_join(studio.published, dest_rel)
                planned.add(dest_rel)
                digest = fsutil.sha256_file(source)
                action: Dict[str, Any] = {
                    "dest": dest_rel,
                    "source": studio.relative(source),
                    "kind": info["kind"],
                    "sha256": digest,
                    "bytes": source.stat().st_size,
                    "archived": version is not None,
                }
                recorded = state.get(dest_rel, {}).get("sha256")
                if _has_front_matter(source):
                    action["action"] = "blocked"
                    action["reason"] = (
                        "begins with a '---' line, which GitHub Pages (Jekyll) treats as front "
                        "matter and would rewrite"
                    )
                elif destination.is_symlink() or (destination.exists() and not destination.is_file()):
                    action["action"] = "conflict"
                    action["reason"] = "destination exists and is not a regular file"
                elif not destination.exists():
                    action["action"] = "copy"
                else:
                    existing = fsutil.sha256_file(destination)
                    if existing == digest:
                        action["action"] = "skip"
                    elif version is not None:
                        action["action"] = "conflict"
                        action["reason"] = "published copy of an archived version differs from the archive"
                    elif recorded == existing:
                        action["action"] = "update"
                    else:
                        action["action"] = "conflict"
                        action["reason"] = "destination was not published by this tool or was modified since"
                if action["action"] == "conflict" and force:
                    action["action"] = "overwrite"
                actions.append(action)
        # Previously published files whose source no longer exists in the archive.
        for dest_rel, entry in sorted(state.items()):
            if not dest_rel.startswith(f"{name}/") or dest_rel in planned:
                continue
            if (studio.archive / dest_rel).is_file():
                continue  # still in the archive, just not selected this time
            destination = safe_join(studio.published, dest_rel)
            if not destination.is_file():
                stale.append({"dest": dest_rel, "action": "forget"})
            elif fsutil.sha256_file(destination) == entry.get("sha256") or force:
                stale.append({"dest": dest_rel, "action": "remove"})
            else:
                stale.append(
                    {"dest": dest_rel, "action": "conflict", "reason": "modified since it was published"}
                )
    return {"actions": actions, "stale": stale, "state": state}


def summarize(result: Dict[str, Any]) -> Dict[str, int]:
    counts: Dict[str, int] = {}
    for action in result["actions"]:
        counts[action["action"]] = counts.get(action["action"], 0) + 1
    for entry in result["stale"]:
        key = "stale-" + entry["action"]
        counts[key] = counts.get(key, 0) + 1
    return counts


def publish(
    studio: Studio,
    names: Sequence[str],
    *,
    versions: Optional[List[Optional[int]]] = None,
    kinds: Iterable[str] = DEFAULT_KINDS,
    dry_run: bool = False,
    prune: bool = False,
    force: bool = False,
) -> Dict[str, Any]:
    """Publish the selection. Refuses, writing nothing, on any conflict."""
    result = plan(studio, names, versions=versions, kinds=kinds, force=force)
    actions, stale, state = result["actions"], result["stale"], result["state"]
    refusals = [
        f"{a['dest']}: {a['reason']}" for a in actions if a["action"] in ("conflict", "blocked")
    ]
    refusals += [f"{s['dest']}: {s['reason']}" for s in stale if s["action"] == "conflict"]
    removable = [s for s in stale if s["action"] == "remove"]
    if removable and not prune:
        refusals += [
            f"{s['dest']}: was published earlier but is no longer in the archive; "
            "rerun with --prune to remove it"
            for s in removable
        ]
    result["refusals"] = refusals
    result["dry_run"] = dry_run
    result["written"] = False
    if dry_run:
        return result
    if refusals:
        raise PublishError(
            "nothing was published:\n  " + "\n  ".join(refusals)
            + "\n(--force overwrites conflicting files; --dry-run shows the full plan)"
        )
    for action in actions:
        if action["action"] in ("copy", "update", "overwrite"):
            destination = safe_join(studio.published, action["dest"])
            if destination.is_symlink():
                destination.unlink()
            fsutil.copy_file(studio.root / action["source"], destination)
        if action["action"] in ("copy", "update", "overwrite", "skip"):
            state[action["dest"]] = {
                "sha256": action["sha256"],
                "source": action["source"],
                "kind": action["kind"],
            }
    for entry in stale:
        if entry["action"] == "remove":
            _remove_published(studio, entry["dest"])
        if entry["action"] in ("remove", "forget"):
            state.pop(entry["dest"], None)
    _save_state(studio, state)
    result["catalog"] = catalog.write(studio)
    result["written"] = True
    return result


def _remove_published(studio: Studio, dest_rel: str) -> None:
    destination = safe_join(studio.published, dest_rel)
    if destination.is_file() or destination.is_symlink():
        destination.unlink()
    parent = destination.parent
    while parent != studio.published.resolve() and is_within(parent, studio.published):
        try:
            parent.rmdir()  # only succeeds when empty
        except OSError:
            break
        parent = parent.parent


def unpublish(
    studio: Studio,
    name: str,
    *,
    versions: Optional[List[Optional[int]]] = None,
    dry_run: bool = False,
    force: bool = False,
) -> Dict[str, Any]:
    """Remove published files of a composition. The archive is not touched.

    Only files recorded as published by this tool, and unchanged since, are
    removed; anything else is reported and left alone unless ``force``.
    """
    _check_destination_root(studio)
    naming.validate_composition_name(name)
    state = load_state(studio)
    prefixes = None
    if versions is not None:
        prefixes = [
            f"{name}/{naming.version_dir_name(v)}/" if v is not None else f"{name}/" for v in versions
        ]
    removed: List[str] = []
    kept: List[str] = []
    for dest_rel, entry in sorted(state.items()):
        if not dest_rel.startswith(f"{name}/"):
            continue
        inner = dest_rel[len(name) + 1:]
        in_version = naming.parse_version_dir(inner.split("/")[0]) is not None
        if prefixes is not None:
            wanted = any(
                dest_rel.startswith(prefix) and (prefix != f"{name}/" or not in_version)
                for prefix in prefixes
            )
            if not wanted:
                continue
        destination = safe_join(studio.published, dest_rel)
        if destination.is_file() and fsutil.sha256_file(destination) != entry.get("sha256") and not force:
            kept.append(dest_rel)
            continue
        removed.append(dest_rel)
        if not dry_run:
            _remove_published(studio, dest_rel)
            state.pop(dest_rel, None)
    result: Dict[str, Any] = {"removed": removed, "kept_modified": kept, "dry_run": dry_run}
    if not dry_run:
        _save_state(studio, state)
        result["catalog"] = catalog.write(studio)
    return result
