"""Numbered archival and transactional promotion of composition versions.

Layout of one composition (see naming.py):

    nu/                    the current version: unnumbered files
    nu/version-0001/       the first version, archived when it was replaced
    nu/version-0002/       ...

Accepting a staged revision runs this journaled transaction, under a
per-composition lock:

    1. prepared    the new state is copied into nu/.incoming-<tx>/ and
                   verified; the journal nu/.transaction.json is written.
                   Nothing visible has changed.
    2. archived    the current state is copied to nu/.version-NNNN.partial/,
                   verified, and renamed to nu/version-NNNN/ (one atomic
                   rename).
    3. promoting   each top-level entry of the new state is moved from
                   .incoming into place; replaced entries go to
                   nu/.outgoing-<tx>/.
    4. done        the journal is deleted (the commit point), then the
                   temporary directories are removed.

Whatever step is interrupted, ``recover`` brings the composition to exactly
the old state or exactly the new state, verified by tree hashes. While a
journal exists every other operation on the composition is refused.
"""
from __future__ import annotations

import os
import socket
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional

from . import environment, fsutil, manifest as manifest_module, naming, staging
from .fsutil import IntegrityError
from .paths import Studio, StudioError, safe_join

JOURNAL = ".transaction.json"
JOURNAL_SCHEMA = "ai-composition-studio/transaction/1"
#: GitHub rejects files of 100 MB and more.
MAX_FILE_BYTES = 95 * 1024 * 1024
LARGE_FILE_BYTES = 20 * 1024 * 1024


class ConflictError(StudioError):
    """The composition changed since the revision was staged."""


class LockedError(StudioError):
    """Another process holds the composition lock."""


class PendingTransactionError(StudioError):
    """An interrupted version operation must be recovered first."""


class InjectedFault(Exception):
    """Raised by the test suite to simulate an interruption."""


_fault_hook: Optional[Callable[[str], None]] = None


def set_fault_hook(hook: Optional[Callable[[str], None]]) -> None:
    global _fault_hook
    _fault_hook = hook


def checkpoint(step: str) -> None:
    """A point at which the test suite can interrupt the transaction."""
    if _fault_hook is not None:
        _fault_hook(step)
    if os.environ.get("STUDIO_FAULT_AT") == step:
        if os.environ.get("STUDIO_FAULT_MODE") == "kill":
            os._exit(137)  # simulate the process being killed outright
        raise InjectedFault(step)


# --------------------------------------------------------------------------
# Locking
# --------------------------------------------------------------------------


def _process_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except (PermissionError, OSError):
        return True
    return True


class CompositionLock:
    """Exclusive lock for one composition, as a lock file created with O_EXCL.

    A lock left by a dead process on this machine is taken over
    automatically. A lock from another machine has to be broken explicitly
    (``studio recover NAME --break-lock``).
    """

    def __init__(self, studio: Studio, name: str, *, break_lock: bool = False):
        naming.validate_composition_name(name)
        self.path = safe_join(studio.archive, f".locks/{name}.lock", allow_hidden=True)
        self.break_lock = break_lock
        self.token = fsutil.new_token()
        self.held = False

    def _describe(self) -> Dict[str, Any]:
        try:
            return fsutil.read_json(self.path)
        except (OSError, ValueError):
            return {}

    def _is_stale(self, info: Dict[str, Any]) -> bool:
        if self.break_lock:
            return True
        pid = info.get("pid")
        return info.get("host") == socket.gethostname() and isinstance(pid, int) and not _process_alive(pid)

    def acquire(self) -> "CompositionLock":
        self.path.parent.mkdir(parents=True, exist_ok=True)
        payload = fsutil.dump_json(
            {
                "pid": os.getpid(),
                "host": socket.gethostname(),
                "token": self.token,
                "acquired_at": fsutil.utc_now(),
            }
        ).encode("utf-8")
        for attempt in (1, 2):
            try:
                fd = os.open(str(self.path), os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o644)
            except FileExistsError:
                info = self._describe()
                if attempt == 1 and self._is_stale(info):
                    try:
                        self.path.unlink()
                    except FileNotFoundError:
                        pass
                    continue
                raise LockedError(
                    f"composition is locked by pid {info.get('pid')} on {info.get('host')} "
                    f"since {info.get('acquired_at')}; if that process is gone, run "
                    "'studio recover NAME --break-lock'"
                )
            with os.fdopen(fd, "wb") as handle:
                handle.write(payload)
                handle.flush()
                os.fsync(handle.fileno())
            self.held = True
            return self
        raise LockedError("could not acquire the composition lock")

    def release(self) -> None:
        if self.held:
            if self._describe().get("token") == self.token:
                try:
                    self.path.unlink()
                except FileNotFoundError:
                    pass
            self.held = False

    def __enter__(self) -> "CompositionLock":
        return self.acquire()

    def __exit__(self, *_exc: Any) -> None:
        self.release()


# --------------------------------------------------------------------------
# Reading compositions
# --------------------------------------------------------------------------


def list_compositions(studio: Studio) -> List[str]:
    if not studio.archive.is_dir():
        return []
    return sorted(
        entry.name
        for entry in studio.archive.iterdir()
        if entry.is_dir() and not entry.name.startswith(".") and naming.COMPOSITION_RE.match(entry.name)
    )


def list_versions(comp_dir: Path) -> List[int]:
    numbers = []
    if Path(comp_dir).is_dir():
        for entry in Path(comp_dir).iterdir():
            number = naming.parse_version_dir(entry.name)
            if number is not None and entry.is_dir():
                numbers.append(number)
    return sorted(numbers)


def state_dir(studio: Studio, name: str, version: Optional[int] = None) -> Path:
    """Directory of the current version (None) or of an archived version."""
    comp = studio.composition_dir(name)
    if not comp.is_dir():
        raise StudioError(f"no composition named {name!r}")
    if version is None:
        return comp
    directory = comp / naming.version_dir_name(version)
    if not directory.is_dir():
        raise StudioError(f"{name} has no {naming.version_dir_name(version)}")
    return directory


def state_files(directory: Path, *, is_current: bool) -> List[str]:
    return fsutil.list_files(directory, skip_versions=is_current)


def read_journal(comp_dir: Path) -> Optional[Dict[str, Any]]:
    path = Path(comp_dir) / JOURNAL
    if not path.is_file():
        return None
    try:
        return fsutil.read_json(path)
    except ValueError as error:
        raise IntegrityError(f"unreadable transaction journal {path}: {error}") from error


def require_no_transaction(studio: Studio, name: str) -> None:
    comp = studio.composition_dir(name)
    if read_journal(comp) is not None:
        raise PendingTransactionError(
            f"{name} has an interrupted version operation; run 'studio recover {name}' first"
        )


def _top_entries(table: Dict[str, Any]) -> List[str]:
    return sorted({rel.split("/")[0] for rel in table})


def _write_journal(comp_dir: Path, journal: Dict[str, Any]) -> None:
    fsutil.atomic_write_json(Path(comp_dir) / JOURNAL, journal)


def _next_version_number(comp_dir: Path, current_manifest: Dict[str, Any]) -> int:
    versions = list_versions(comp_dir)
    highest = versions[-1] if versions else 0
    if versions != list(range(1, highest + 1)):
        raise IntegrityError(f"version directories are not contiguous: {versions}")
    if current_manifest["ordinal"] != highest + 1:
        raise IntegrityError(
            f"current manifest says ordinal {current_manifest['ordinal']} but the highest "
            f"archived version is {highest}; run 'studio verify' and repair before revising"
        )
    return highest + 1


# --------------------------------------------------------------------------
# Accepting a staged composition or revision
# --------------------------------------------------------------------------


def _check_sizes(staging_dir: Path, files: List[str], allow_large: bool) -> List[str]:
    warnings = []
    for rel in files:
        size = (staging_dir / rel).stat().st_size
        if size > MAX_FILE_BYTES and not allow_large:
            raise staging.StagingError(
                f"{rel} is {size / 1e6:.0f} MB; GitHub rejects files of 100 MB or more. "
                "Shorten the render, keep only the FLAC/MP3, or pass --allow-large"
            )
        if size > LARGE_FILE_BYTES:
            warnings.append(f"{rel} is {size / 1e6:.0f} MB and will be committed to Git")
    return warnings


def _build_manifest(
    name: str,
    meta: Dict[str, Any],
    staging_dir: Path,
    *,
    ordinal: int,
    history: List[Dict[str, Any]],
    new_prompts: List[str],
    render: Dict[str, Any],
    options: Dict[str, Any],
) -> Dict[str, Any]:
    files = [rel for rel in fsutil.list_files(staging_dir) if rel != naming.manifest_name(name)]
    table = fsutil.hash_files(staging_dir, files)
    code_files = [rel for rel in files if (naming.classify(name, rel) or {}).get("kind") == "code"]
    code_texts = {}
    for rel in code_files:
        try:
            code_texts[rel] = (staging_dir / rel).read_text(encoding="utf-8", errors="replace")
        except OSError:
            code_texts[rel] = ""
    csd_text = (staging_dir / naming.csd_name(name)).read_text(encoding="utf-8", errors="replace")
    # Failed attempts stay in the generation log; the manifest records what worked.
    runs = [run for run in staging.load_runs(staging_dir) if run.get("exit_code") == 0]
    dependencies = manifest_module.detect_dependencies(csd_text)
    for extra in options.get("dependencies") or []:
        kind, _, value = extra.partition(":")
        dependencies.setdefault(kind.strip() or "other", [])
        if value.strip() and value.strip() not in dependencies[kind.strip() or "other"]:
            dependencies[kind.strip() or "other"].append(value.strip())
    prompts = sorted(rel for rel in files if (naming.classify(name, rel) or {}).get("kind") == "prompts")
    # A revision adopted from another copy keeps the credit of whoever made it.
    adopted = meta.get("adopted_from") or {}
    made_by = adopted.get("ai") or {}
    document = manifest_module.build(
        name,
        title=options.get("title") or meta.get("title") or name,
        ordinal=ordinal,
        created_at=fsutil.utc_now(),
        prompts=prompts,
        new_prompts=new_prompts,
        files=table,
        history=history,
        ai={
            "model": options.get("ai_model") or made_by.get("model")
            or os.environ.get("STUDIO_AI_MODEL") or "unspecified",
            "agent": options.get("ai_agent") or made_by.get("agent")
            or os.environ.get("STUDIO_AI_AGENT") or "unspecified",
        },
        generator={
            "kind": options.get("generator_kind")
            or manifest_module.infer_generator_kind(code_files, code_texts),
            "sources": code_files,
            "runs": runs,
            "seeds": [run["seed"] for run in runs if "seed" in run],
        },
        environment=environment.collect(),
        render={
            key: render.get(key)
            for key in ("commands", "started_at", "finished_at", "analysis", "wav_sha256", "outputs", "log")
        },
        validation={
            "status": "passed",
            "checks": render.get("checks", []),
            "warnings": render.get("warnings", []),
        },
        dependencies=dependencies,
        notes=options.get("notes"),
    )
    if adopted:
        document["adopted_from"] = adopted
    return document


def accept(studio: Studio, staging_dir: Path, **options: Any) -> Dict[str, Any]:
    """Promote a validated, rendered staging area to be the current version.

    For a revision, the previous current version is first archived under the
    next version number. On any failure before the journal is written the
    current composition is untouched; after that, ``recover`` finishes or
    undoes the operation.
    """
    staging_dir = Path(staging_dir)
    meta = staging.load_meta(staging_dir)
    name = meta["composition"]
    comp = studio.composition_dir(name)

    render = staging.load_render(staging_dir)
    if not render or render.get("status") != "ok":
        raise staging.StagingError(
            "this staging area has no successful render; run 'studio render' first"
        )
    if render.get("inputs_sha256") != staging.inputs_hash(name, staging_dir):
        raise staging.StagingError(
            "the .csd or code changed after the last render; run 'studio render' again"
        )
    new_prompts = staging.check_prompts(meta, staging_dir)

    with CompositionLock(studio, name):
        is_new = meta["kind"] == "new"
        if is_new:
            if comp.exists() or staging._existing_case_insensitive(studio, name):
                raise ConflictError(
                    f"composition {name!r} was created by someone else since this was staged; "
                    f"use 'studio stage {name}' and carry your changes over"
                )
            history: List[Dict[str, Any]] = []
            ordinal = 1
            old_hash = None
            old_table: Dict[str, Any] = {}
            version = None
        else:
            if not comp.is_dir():
                raise ConflictError(f"composition {name!r} no longer exists")
            if read_journal(comp) is not None:
                raise PendingTransactionError(
                    f"{name} has an interrupted version operation; run 'studio recover {name}' first"
                )
            old_table = fsutil.hash_files(comp, state_files(comp, is_current=True))
            old_hash = fsutil.tree_hash(old_table)
            if old_hash != meta["base_tree_sha256"]:
                raise ConflictError(
                    f"{name} changed since this revision was staged (it was based on "
                    f"version ordinal {meta['base_ordinal']}). Stage a fresh revision with "
                    f"'studio stage {name}' and carry your changes over; nothing was modified."
                )
            current = manifest_module.load(comp, name)
            version = _next_version_number(comp, current)
            ordinal = version + 1
            history = list(current["history"]) + [
                {
                    "version": naming.version_dir_name(version),
                    "ordinal": version,
                    "tree_sha256": old_hash,
                    "archived_at": fsutil.utc_now(),
                }
            ]

        manifest = _build_manifest(
            name,
            meta,
            staging_dir,
            ordinal=ordinal,
            history=history,
            new_prompts=new_prompts,
            render=render,
            options=options,
        )
        found = manifest_module.problems(manifest, name)
        if found:
            raise IntegrityError("refusing to accept: " + "; ".join(found))
        fsutil.atomic_write_json(staging_dir / naming.manifest_name(name), manifest)

        new_files = fsutil.list_files(staging_dir)
        found = naming.state_problems(name, new_files)
        if found:
            raise staging.StagingError("naming problems:\n  " + "\n  ".join(found))
        warnings = _check_sizes(staging_dir, new_files, bool(options.get("allow_large")))
        new_table = fsutil.hash_files(staging_dir, new_files)
        new_hash = fsutil.tree_hash(new_table)

        if is_new:
            _create_composition(studio, name, staging_dir, new_files, new_hash)
        else:
            _run_transaction(
                comp,
                name,
                staging_dir,
                version=version,
                old_table=old_table,
                old_hash=old_hash,
                new_files=new_files,
                new_table=new_table,
                new_hash=new_hash,
            )

    staging.discard(studio, staging_dir)
    return {
        "composition": name,
        "ordinal": ordinal,
        "archived_as": naming.version_dir_name(version) if version else None,
        "tree_sha256": new_hash,
        "warnings": warnings,
    }


def _create_composition(
    studio: Studio, name: str, staging_dir: Path, files: List[str], expected_hash: str
) -> None:
    """A new composition appears with one atomic directory rename."""
    if studio.staging_root.is_dir():
        for leftover in studio.staging_root.glob(f".final-{name}-*"):
            fsutil.remove_tree(leftover)  # from an earlier, interrupted attempt
    final = safe_join(studio.archive, f".staging/.final-{name}-{fsutil.new_token()}", allow_hidden=True)
    try:
        fsutil.copy_files(staging_dir, final, files)
        if fsutil.state_hash(final) != expected_hash:
            raise IntegrityError("copy of the staged composition does not match its hash")
        checkpoint("create-copied")
        os.rename(final, studio.composition_dir(name))
        fsutil.fsync_dir(studio.archive)
    finally:
        if final.exists():
            fsutil.remove_tree(final)


def _run_transaction(
    comp: Path,
    name: str,
    staging_dir: Path,
    *,
    version: int,
    old_table: Dict[str, Any],
    old_hash: str,
    new_files: List[str],
    new_table: Dict[str, Any],
    new_hash: str,
) -> None:
    txid = fsutil.new_token()
    incoming = comp / f".incoming-{txid}"
    partial = comp / f".{naming.version_dir_name(version)}.partial"
    version_dir = comp / naming.version_dir_name(version)
    if version_dir.exists():
        raise IntegrityError(f"{version_dir} already exists; version numbers are never reused")

    # Step 1: stage the new state next to its destination, and journal it.
    try:
        fsutil.copy_files(staging_dir, incoming, new_files)
        if fsutil.state_hash(incoming) != new_hash:
            raise IntegrityError("copy of the staged revision does not match its hash")
    except BaseException:
        fsutil.remove_tree(incoming)
        raise
    journal = {
        "schema": JOURNAL_SCHEMA,
        "txid": txid,
        "composition": name,
        "state": "prepared",
        "version": version,
        "old_tree_sha256": old_hash,
        "new_tree_sha256": new_hash,
        "old_entries": _top_entries(old_table),
        "new_entries": _top_entries(new_table),
        "started_at": fsutil.utc_now(),
        "host": socket.gethostname(),
        "pid": os.getpid(),
    }
    _write_journal(comp, journal)
    checkpoint("journal-prepared")

    # Step 2: archive the current state under the new version number.
    if partial.exists():
        fsutil.remove_tree(partial)
    fsutil.copy_files(comp, partial, sorted(old_table))
    if fsutil.state_hash(partial) != old_hash:
        raise IntegrityError("archive copy of the current version does not match its hash")
    checkpoint("archive-copied")
    os.rename(partial, version_dir)
    fsutil.fsync_dir(comp)
    checkpoint("archive-renamed")
    journal["state"] = "archived"
    _write_journal(comp, journal)
    checkpoint("journal-archived")

    # Step 3: move the new state into place.
    journal["state"] = "promoting"
    _write_journal(comp, journal)
    _roll_forward(comp, journal)
    if fsutil.state_hash(comp, skip_versions=True) != new_hash:
        raise IntegrityError(
            f"promotion of {name} did not produce the expected state; run 'studio recover {name}'"
        )
    checkpoint("promoted")

    # Step 4: commit.
    _finish(comp, journal)


def _roll_forward(comp: Path, journal: Dict[str, Any]) -> None:
    """Move every remaining entry of the new state into place. Idempotent."""
    incoming = comp / f".incoming-{journal['txid']}"
    outgoing = comp / f".outgoing-{journal['txid']}"
    outgoing.mkdir(exist_ok=True)
    for entry in journal["old_entries"]:
        if entry in journal["new_entries"]:
            continue
        stale = comp / entry
        if stale.exists():
            if (outgoing / entry).exists():
                fsutil.remove_tree(outgoing / entry)
            os.rename(stale, outgoing / entry)
    moved = 0
    for entry in journal["new_entries"]:
        source = incoming / entry
        if not source.exists():
            continue  # already in place
        destination = comp / entry
        if destination.exists() and (destination.is_dir() or source.is_dir()):
            if (outgoing / entry).exists():
                fsutil.remove_tree(outgoing / entry)
            os.rename(destination, outgoing / entry)
        os.replace(source, destination)
        moved += 1
        if moved == 1:
            checkpoint("promote-first-entry")
    fsutil.fsync_dir(comp)


def _finish(comp: Path, journal: Dict[str, Any]) -> None:
    (comp / JOURNAL).unlink()
    fsutil.fsync_dir(comp)
    checkpoint("journal-removed")
    _remove_temporaries(comp)
    version_dir = comp / naming.version_dir_name(journal["version"])
    if version_dir.is_dir():
        fsutil.make_files_readonly(version_dir)


def _temporaries(comp: Path) -> List[Path]:
    return sorted(
        entry
        for entry in Path(comp).iterdir()
        if entry.name.startswith((".incoming-", ".outgoing-"))
        or (entry.name.startswith(".version-") and entry.name.endswith(".partial"))
    )


def _remove_temporaries(comp: Path) -> List[str]:
    removed = []
    for entry in _temporaries(comp):
        fsutil.remove_tree(entry)
        removed.append(entry.name)
    return removed


# --------------------------------------------------------------------------
# Recovery
# --------------------------------------------------------------------------


def recover(
    studio: Studio, name: str, *, rollback: bool = False, break_lock: bool = False
) -> Dict[str, Any]:
    """Finish or undo an interrupted version operation.

    Rolls forward when the new state is completely available, otherwise (or
    with ``rollback``) restores the old state. Both outcomes are verified
    against the tree hashes recorded in the journal.
    """
    comp = studio.composition_dir(name)
    if not comp.is_dir():
        raise StudioError(f"no composition named {name!r}")
    with CompositionLock(studio, name, break_lock=break_lock):
        journal = read_journal(comp)
        if journal is None:
            removed = _remove_temporaries(comp)
            for number in list_versions(comp):
                fsutil.make_files_readonly(comp / naming.version_dir_name(number))
            return {"action": "none", "removed": removed}
        if journal.get("schema") != JOURNAL_SCHEMA or journal.get("composition") != name:
            raise IntegrityError(f"unrecognized transaction journal in {comp}")
        version = journal["version"]
        version_dir = comp / naming.version_dir_name(version)
        old_hash = journal["old_tree_sha256"]
        new_hash = journal["new_tree_sha256"]
        archived = version_dir.is_dir() and fsutil.state_hash(version_dir) == old_hash
        current_hash = fsutil.state_hash(comp, skip_versions=True)

        if not rollback and archived:
            try:
                _roll_forward(comp, journal)
                rolled = fsutil.state_hash(comp, skip_versions=True) == new_hash
            except OSError:
                rolled = False
            if rolled:
                _finish(comp, journal)
                return {"action": "rolled-forward", "version": version, "ordinal": version + 1}
            current_hash = fsutil.state_hash(comp, skip_versions=True)

        # Roll back to the old state.
        if current_hash != old_hash:
            if not archived:
                raise IntegrityError(
                    f"cannot recover {name}: the current files are damaged and "
                    f"{version_dir.name} is not a complete copy of the previous version. "
                    "Restore the composition directory from Git."
                )
            for entry in sorted(comp.iterdir()):
                if entry.name.startswith(".") or naming.parse_version_dir(entry.name) is not None:
                    continue
                fsutil.remove_tree(entry)
            fsutil.copy_files(version_dir, comp, fsutil.list_files(version_dir))
            _make_current_writable(comp)
            if fsutil.state_hash(comp, skip_versions=True) != old_hash:
                raise IntegrityError(f"restoring {name} from {version_dir.name} failed verification")
        if version_dir.exists():
            fsutil.remove_tree(version_dir)
        (comp / JOURNAL).unlink()
        fsutil.fsync_dir(comp)
        _remove_temporaries(comp)
        return {"action": "rolled-back", "version": version, "ordinal": version}


def _make_current_writable(comp: Path) -> None:
    for rel in fsutil.list_files(comp, skip_versions=True):
        path = comp / rel
        try:
            path.chmod(path.stat().st_mode | 0o200)
        except OSError:
            pass


# --------------------------------------------------------------------------
# Verification
# --------------------------------------------------------------------------


def _check_state(
    name: str, directory: Path, *, is_current: bool, label: str
) -> List[str]:
    found: List[str] = []
    files = state_files(directory, is_current=is_current)
    found += [f"{label}: {problem}" for problem in naming.state_problems(name, files)]
    try:
        manifest = manifest_module.load(directory, name)
    except IntegrityError as error:
        return found + [f"{label}: {error}"]
    actual = fsutil.hash_files(directory, [rel for rel in files if rel != naming.manifest_name(name)])
    recorded = manifest["files"]
    for rel in sorted(set(actual) | set(recorded)):
        if rel not in actual:
            found.append(f"{label}: {rel} is recorded in the manifest but missing")
        elif rel not in recorded:
            found.append(f"{label}: {rel} is not recorded in the manifest")
        elif actual[rel]["sha256"] != recorded[rel]["sha256"]:
            found.append(f"{label}: {rel} does not match the hash in the manifest")
    return found


def verify(studio: Studio, name: str) -> List[str]:
    """Check one composition; returns a list of problems (empty when sound).

    Verifies the naming conventions, every manifest, that every archived
    version still has exactly the content it had when it was archived, and
    that prompts were only ever appended.
    """
    comp = studio.composition_dir(name)
    if not comp.is_dir():
        return [f"no composition named {name!r}"]
    found: List[str] = []
    try:
        if read_journal(comp) is not None:
            found.append(f"interrupted version operation pending; run 'studio recover {name}'")
    except IntegrityError as error:
        found.append(str(error))
    for entry in _temporaries(comp):
        found.append(f"leftover temporary directory {entry.name}; run 'studio recover {name}'")
    found += _check_state(name, comp, is_current=True, label="current")
    try:
        current = manifest_module.load(comp, name)
    except IntegrityError:
        return found
    versions = list_versions(comp)
    expected = list(range(1, current["ordinal"]))
    if versions != expected:
        found.append(
            f"archived versions on disk {versions} do not match the history "
            f"{expected} recorded in the current manifest"
        )
    previous_prompts: Dict[str, str] = {}
    for entry in current["history"]:
        number = entry["ordinal"]
        label = naming.version_dir_name(number)
        directory = comp / label
        if not directory.is_dir():
            found.append(f"{label}: archived version is missing")
            continue
        if fsutil.state_hash(directory) != entry["tree_sha256"]:
            found.append(f"{label}: archived version was modified after it was archived")
        found += _check_state(name, directory, is_current=False, label=label)
        try:
            archived_manifest = manifest_module.load(directory, name)
        except IntegrityError:
            continue
        if archived_manifest["ordinal"] != number:
            found.append(f"{label}: manifest ordinal is {archived_manifest['ordinal']}")
        if archived_manifest["history"] != current["history"][: number - 1]:
            found.append(f"{label}: manifest history does not match the current history")
        prompts = {
            rel: archived_manifest["files"][rel]["sha256"]
            for rel in archived_manifest["prompts"]
            if rel in archived_manifest["files"]
        }
        for rel, digest in previous_prompts.items():
            if prompts.get(rel) != digest:
                found.append(f"{label}: earlier prompt {rel} was not preserved")
        previous_prompts = prompts
    current_prompts = {
        rel: current["files"][rel]["sha256"] for rel in current["prompts"] if rel in current["files"]
    }
    for rel, digest in previous_prompts.items():
        if current_prompts.get(rel) != digest:
            found.append(f"current: earlier prompt {rel} was not preserved")
    return found


def describe(studio: Studio, name: str) -> Dict[str, Any]:
    comp = studio.composition_dir(name)
    current = manifest_module.load(comp, name)
    return {
        "composition": name,
        "title": current["title"],
        "ordinal": current["ordinal"],
        "created_at": current["created_at"],
        "versions": list_versions(comp),
        "prompts": current["prompts"],
        "pending_transaction": read_journal(comp) is not None,
    }
