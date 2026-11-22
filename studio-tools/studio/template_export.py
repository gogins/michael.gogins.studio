"""Write a starter repository: the studio, an empty library, and the patches.

The starter is a separate directory. It does not contain this repository's
compositions, website, or other files. See studio-tools/docs/sharing.md.
"""
from __future__ import annotations

import shutil
from pathlib import Path

from . import catalog
from .paths import Studio, StudioError, is_within

_SKIP = {"__pycache__", ".DS_Store", ".staging", ".locks"}
_HOME_LINK = ">Michael Gogins</a>"
_HOME_LINK_FOR_STARTER = ">Home</a>"
_PERSONAL_PLAYER = "https://gogins.github.io/michael.gogins.studio/studio/"


def _ignore(directory: str, names: list) -> set:
    skipped = set()
    for name in names:
        if name in _SKIP or name.endswith(".pyc"):
            skipped.add(name)
    return skipped


def _copy_tree(source: Path, destination: Path) -> None:
    if not source.is_dir():
        raise StudioError(f"missing {source}")
    shutil.copytree(source, destination, ignore=_ignore)


def _replace(path: Path, old: str, new: str) -> None:
    text = path.read_text(encoding="utf-8")
    if old not in text:
        raise StudioError(f"{path.name} has no {old!r} to rewrite for the starter")
    path.write_text(text.replace(old, new), encoding="utf-8")


def export_template(studio: Studio, destination: Path) -> dict:
    """Create ``destination`` as a starter. It must not already exist."""
    destination = Path(destination).expanduser().resolve()
    if is_within(destination, studio.root) or destination == studio.root:
        raise StudioError(f"{destination} is inside this repository; choose a directory outside it")
    if destination.exists():
        raise StudioError(f"{destination} already exists")

    patches = studio.root / "docs" / "silencio" / "patches"
    template = studio.tools / "template"
    destination.mkdir(parents=True)
    try:
        _copy_tree(studio.tools, destination / "studio-tools")
        _copy_tree(studio.player, destination / "docs" / "studio")
        _copy_tree(patches, destination / "patches")
        archive = destination / "music" / "ai-compositions"
        archive.mkdir(parents=True)
        shutil.copyfile(studio.archive / ".gitignore", archive / ".gitignore")
        (archive / ".gitkeep").write_text("", encoding="utf-8")

        for page in ("index.html", "cheatsheet.html"):
            _replace(destination / "docs" / "studio" / page, _HOME_LINK, _HOME_LINK_FOR_STARTER)
        _replace(
            destination / "studio-tools" / "README.md",
            f"[gogins.github.io/michael.gogins.studio/studio/]({_PERSONAL_PLAYER})",
            "[the player](../docs/studio/index.html)",
        )
        cheat = destination / "studio-tools" / "docs" / "cheatsheet.md"
        _replace(cheat, f"[Player]({_PERSONAL_PLAYER})", "[Player](../../docs/studio/index.html)")
        _replace(
            cheat,
            f"[https://gogins.github.io/michael.gogins.studio/studio/]({_PERSONAL_PLAYER})",
            "[the player](../../docs/studio/index.html)",
        )

        shutil.copyfile(template / "README.md", destination / "README.md")
        shutil.copyfile(template / "index.html", destination / "docs" / "index.html")
        shutil.copyfile(template / "gitignore", destination / ".gitignore")
        shutil.copyfile(studio.tools / "cursor" / "AGENTS.md.template", destination / "AGENTS.md")
        environment = destination / ".cursor"
        (environment / "rules").mkdir(parents=True)
        shutil.copyfile(
            studio.tools / "cursor" / "environment.json.template",
            environment / "environment.json",
        )
        shutil.copyfile(
            studio.tools / "cursor" / "ai-composition-studio.mdc.template",
            environment / "rules" / "ai-composition-studio.mdc",
        )
        written = catalog.write(Studio(destination))
    except Exception:
        shutil.rmtree(destination, ignore_errors=True)
        raise
    return {
        "destination": destination,
        "catalog": written["path"],
        "compositions": written["compositions"],
    }
