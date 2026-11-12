"""Filesystem primitives: hashing, durable atomic writes, tree copies."""
from __future__ import annotations

import hashlib
import json
import os
import shutil
import stat
import uuid
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional

from . import naming
from .paths import StudioError


class IntegrityError(StudioError):
    """Files on disk do not match what the studio recorded for them."""


def utc_now() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def new_token() -> str:
    return uuid.uuid4().hex[:12]


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def list_files(directory: Path, *, skip_versions: bool = False) -> List[str]:
    """Relative POSIX paths of all regular files under ``directory``.

    Hidden files and directories (tool-internal state, .DS_Store) are
    skipped. With ``skip_versions`` the ``version-NNNN`` directories at the
    top level are skipped too, leaving only the current state.
    Symbolic links are never followed and are reported as an error.
    """
    directory = Path(directory)
    result: List[str] = []
    if not directory.is_dir():
        return result
    for root, dirs, files in os.walk(directory, followlinks=False):
        rel_root = Path(root).relative_to(directory)
        at_top = rel_root == Path(".")
        kept = []
        for name in sorted(dirs):
            if name.startswith("."):
                continue
            if at_top and skip_versions and naming.parse_version_dir(name) is not None:
                continue
            if (Path(root) / name).is_symlink():
                raise IntegrityError(f"symbolic link not allowed: {Path(root) / name}")
            kept.append(name)
        dirs[:] = kept
        for name in sorted(files):
            if name.startswith("."):
                continue
            full = Path(root) / name
            if full.is_symlink():
                raise IntegrityError(f"symbolic link not allowed: {full}")
            result.append((rel_root / name).as_posix() if not at_top else name)
    return sorted(result)


def hash_files(directory: Path, files: Iterable[str]) -> Dict[str, Dict[str, Any]]:
    table: Dict[str, Dict[str, Any]] = {}
    for rel in sorted(files):
        full = Path(directory) / rel
        table[rel] = {"sha256": sha256_file(full), "bytes": full.stat().st_size}
    return table


def tree_hash(table: Dict[str, Dict[str, Any]]) -> str:
    """One hash for a whole set of files: order-independent, content+path."""
    digest = hashlib.sha256()
    for rel in sorted(table):
        digest.update(f"{table[rel]['sha256']}  {rel}\n".encode("utf-8"))
    return digest.hexdigest()


def state_hash(directory: Path, *, skip_versions: bool = False) -> str:
    files = list_files(directory, skip_versions=skip_versions)
    return tree_hash(hash_files(directory, files))


def fsync_dir(directory: Path) -> None:
    try:
        fd = os.open(str(directory), os.O_RDONLY)
    except OSError:
        return
    try:
        os.fsync(fd)
    except OSError:
        pass
    finally:
        os.close(fd)


def atomic_write_bytes(path: Path, data: bytes) -> None:
    """Write a file so that readers see either the old or the new content."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    temp = path.parent / f".{path.name}.{new_token()}.tmp"
    try:
        with open(temp, "wb") as handle:
            handle.write(data)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(temp, path)
    finally:
        if temp.exists():
            temp.unlink()
    fsync_dir(path.parent)


def atomic_write_text(path: Path, text: str) -> None:
    atomic_write_bytes(path, text.encode("utf-8"))


def dump_json(data: Any) -> str:
    return json.dumps(data, indent=2, sort_keys=True, ensure_ascii=False) + "\n"


def atomic_write_json(path: Path, data: Any) -> None:
    atomic_write_text(path, dump_json(data))


def read_json(path: Path) -> Any:
    with open(path, "r", encoding="utf-8") as handle:
        return json.load(handle)


def copy_file(source: Path, destination: Path) -> None:
    """Copy one file durably, via a temporary name in the destination dir."""
    destination = Path(destination)
    destination.parent.mkdir(parents=True, exist_ok=True)
    temp = destination.parent / f".{destination.name}.{new_token()}.tmp"
    try:
        with open(source, "rb") as src, open(temp, "wb") as dst:
            shutil.copyfileobj(src, dst, 1024 * 1024)
            dst.flush()
            os.fsync(dst.fileno())
        os.replace(temp, destination)
    finally:
        if temp.exists():
            temp.unlink()


def copy_files(source_dir: Path, destination_dir: Path, files: Iterable[str]) -> None:
    for rel in files:
        copy_file(Path(source_dir) / rel, Path(destination_dir) / rel)
    for root, _dirs, _files in os.walk(destination_dir):
        fsync_dir(Path(root))


def make_tree_writable(directory: Path) -> None:
    for root, _dirs, files in os.walk(directory):
        for name in files:
            full = Path(root) / name
            try:
                full.chmod(full.stat().st_mode | stat.S_IWUSR)
            except OSError:
                pass


def make_files_readonly(directory: Path) -> None:
    """Soft guard for archived versions: clear the write bits of every file.

    Directories stay writable so that Git can still switch branches. Git does
    not record these bits; ``studio verify`` is the real integrity check.
    """
    for root, _dirs, files in os.walk(directory):
        for name in files:
            full = Path(root) / name
            try:
                mode = full.stat().st_mode
                full.chmod(mode & ~(stat.S_IWUSR | stat.S_IWGRP | stat.S_IWOTH))
            except OSError:
                pass


def remove_tree(directory: Path) -> None:
    directory = Path(directory)
    if directory.is_symlink() or directory.is_file():
        directory.unlink()
        return
    if directory.exists():
        make_tree_writable(directory)
        shutil.rmtree(directory)


def scrub_home(text: Optional[str]) -> Optional[str]:
    """Replace the user's home directory with ``~`` in recorded strings."""
    if not text:
        return text
    home = os.path.expanduser("~")
    return text.replace(home, "~") if home and home != "/" else text
