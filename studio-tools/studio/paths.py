"""Repository layout and write-safety guards.

Every path the tools write to is derived through this module, so that a bad
composition name, a crafted relative path, or a symlink can never cause a
write outside the directories the studio is authorized to use.
"""
from __future__ import annotations

import os
from pathlib import Path, PurePosixPath
from typing import Optional, Union

ARCHIVE_REL = "music/ai-compositions"
PUBLISHED_REL = "docs/compositions"
VIEWER_REL = "docs/studio"
TOOLS_REL = "studio-tools"

#: The only directory trees (relative to the repository root) the studio may
#: create files in.
AUTHORIZED_RELS = (ARCHIVE_REL, PUBLISHED_REL, VIEWER_REL, TOOLS_REL)


class StudioError(Exception):
    """Base class for all errors with a user-facing message."""


class PathSafetyError(StudioError):
    """A path would escape the directory it is required to stay inside."""


class Studio:
    """Locations of the studio directories for one repository checkout.

    The root defaults to the repository containing this file. Tests point it
    at a temporary directory, either with the ``root`` argument or with the
    ``STUDIO_ROOT`` environment variable.
    """

    def __init__(self, root: Optional[Union[str, os.PathLike]] = None):
        if root is None:
            root = os.environ.get("STUDIO_ROOT") or Path(__file__).resolve().parents[2]
        self.root = Path(root).resolve()
        self.archive = self.root / ARCHIVE_REL
        self.published = self.root / PUBLISHED_REL
        self.viewer = self.root / VIEWER_REL
        self.tools = self.root / TOOLS_REL
        self.docs = self.root / "docs"
        self.staging_root = self.archive / ".staging"
        self.locks_root = self.archive / ".locks"

    def composition_dir(self, name: str) -> Path:
        from . import naming

        naming.validate_composition_name(name)
        return safe_join(self.archive, name)

    def published_dir(self, name: str) -> Path:
        from . import naming

        naming.validate_composition_name(name)
        return safe_join(self.published, name)

    def relative(self, path: Union[str, os.PathLike]) -> str:
        """Path relative to the repository root, for messages and manifests."""
        try:
            return Path(path).resolve().relative_to(self.root).as_posix()
        except ValueError:
            return str(path)


def check_relative(rel: str) -> PurePosixPath:
    """Validate a relative POSIX path: no absolute paths, no ``..``, no tricks."""
    if not isinstance(rel, str) or not rel:
        raise PathSafetyError(f"empty or non-string path: {rel!r}")
    if "\x00" in rel or "\\" in rel:
        raise PathSafetyError(f"illegal character in path: {rel!r}")
    if rel.startswith("/") or rel.startswith("~"):
        raise PathSafetyError(f"absolute path not allowed: {rel!r}")
    parts = rel.split("/")
    for part in parts:
        if part in ("", ".", ".."):
            raise PathSafetyError(f"illegal path segment {part!r} in {rel!r}")
    return PurePosixPath(rel)


def is_within(path: Path, base: Path) -> bool:
    """True if ``path`` (resolved) is ``base`` (resolved) or inside it."""
    path_r = Path(path).resolve()
    base_r = Path(base).resolve()
    return path_r == base_r or base_r in path_r.parents


def ensure_within(path: Path, base: Path) -> Path:
    if not is_within(path, base):
        raise PathSafetyError(f"{path} is outside {base}")
    return Path(path)


def safe_join(base: Path, rel: str, *, allow_hidden: bool = False) -> Path:
    """Join ``rel`` onto ``base`` and prove the result stays inside ``base``.

    Rejects absolute paths, ``..`` segments, hidden segments (unless
    ``allow_hidden``), and any existing symlink along the way, so that the
    result cannot be redirected outside ``base``.
    """
    posix = check_relative(rel)
    base_r = Path(base).resolve()
    current = base_r
    for part in posix.parts:
        if part.startswith(".") and not allow_hidden:
            raise PathSafetyError(f"hidden path segment {part!r} in {rel!r}")
        current = current / part
        if current.is_symlink():
            raise PathSafetyError(f"symbolic link in path: {current}")
    if not is_within(current, base_r):
        raise PathSafetyError(f"{rel!r} escapes {base}")
    return current
