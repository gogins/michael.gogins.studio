"""Staging areas: where a new composition or a revision is prepared.

Nothing in a staging area affects the current composition. A staged
revision only becomes current through ``versioning.accept`` after it has
been validated and rendered.

    music/ai-compositions/.staging/<name>-<timestamp>-<token>/
        .staging.json     what this staging area is based on
        .runs.json        generator commands that were run here
        .render.json      result of the last render
        <name>.csd, <name>-prompt-NNN.md, code/, outputs/
"""
from __future__ import annotations

import os
import subprocess
import time
from pathlib import Path
from typing import Any, Dict, List, Optional

from . import fsutil, naming
from .fsutil import IntegrityError
from .paths import Studio, StudioError, is_within, safe_join

STAGING_META = ".staging.json"
RUNS_META = ".runs.json"
RENDER_META = ".render.json"
STAGING_SCHEMA = "ai-composition-studio/staging/1"
PROMPT_SOURCES = ("user", "agent")


class StagingError(StudioError):
    """The staging area is missing something or is not ready."""


def _existing_case_insensitive(studio: Studio, name: str) -> Optional[str]:
    if not studio.archive.is_dir():
        return None
    for entry in studio.archive.iterdir():
        if entry.is_dir() and entry.name.lower() == name.lower():
            return entry.name
    return None


def _create_dir(studio: Studio, name: str) -> Path:
    stamp = time.strftime("%Y%m%dT%H%M%SZ", time.gmtime())
    directory = safe_join(
        studio.archive, f".staging/{name}-{stamp}-{fsutil.new_token()[:6]}", allow_hidden=True
    )
    directory.mkdir(parents=True, exist_ok=False)
    return directory


def resolve(studio: Studio, staging: str) -> Path:
    """Find a staging directory from a path or its directory name."""
    candidate = Path(staging)
    if not candidate.is_absolute():
        by_cwd = Path.cwd() / candidate
        candidate = by_cwd if by_cwd.is_dir() else studio.staging_root / candidate.name
    if not candidate.is_dir() or not is_within(candidate, studio.staging_root):
        raise StagingError(f"not a staging directory under {studio.relative(studio.staging_root)}: {staging}")
    if candidate.resolve().parent != studio.staging_root.resolve():
        raise StagingError(f"not a staging directory: {staging}")
    if not (candidate / STAGING_META).is_file():
        raise StagingError(f"{staging} has no {STAGING_META}")
    return candidate.resolve()


def load_meta(staging_dir: Path) -> Dict[str, Any]:
    meta = fsutil.read_json(Path(staging_dir) / STAGING_META)
    if meta.get("schema") != STAGING_SCHEMA:
        raise StagingError(f"unknown staging schema in {staging_dir}")
    naming.validate_composition_name(meta.get("composition"))
    return meta


def new_composition(studio: Studio, name: str, title: Optional[str] = None) -> Path:
    """Create a staging area for a composition that does not exist yet."""
    naming.validate_composition_name(name)
    existing = _existing_case_insensitive(studio, name)
    if existing is not None:
        raise StagingError(
            f"composition {existing!r} already exists; use 'studio stage {existing}' to revise it"
        )
    directory = _create_dir(studio, name)
    fsutil.atomic_write_json(
        directory / STAGING_META,
        {
            "schema": STAGING_SCHEMA,
            "composition": name,
            "kind": "new",
            "title": title or name,
            "base_ordinal": 0,
            "base_tree_sha256": None,
            "base_prompts": {},
            "created_at": fsutil.utc_now(),
        },
    )
    return directory


def stage_revision(studio: Studio, name: str) -> Path:
    """Create a staging area holding a working copy of the current version."""
    from . import manifest as manifest_module
    from . import versioning

    comp = studio.composition_dir(name)
    if not comp.is_dir():
        raise StagingError(f"no composition named {name!r}; use 'studio new {name}'")
    versioning.require_no_transaction(studio, name)
    current = manifest_module.load(comp, name)
    files = fsutil.list_files(comp, skip_versions=True)
    table = fsutil.hash_files(comp, files)
    carried = [
        rel
        for rel in files
        if (naming.classify(name, rel) or {}).get("kind") in ("csd", "prompts", "code")
    ]
    directory = _create_dir(studio, name)
    fsutil.copy_files(comp, directory, carried)
    fsutil.make_tree_writable(directory)
    fsutil.atomic_write_json(
        directory / STAGING_META,
        {
            "schema": STAGING_SCHEMA,
            "composition": name,
            "kind": "revision",
            "title": current["title"],
            "base_ordinal": current["ordinal"],
            "base_tree_sha256": fsutil.tree_hash(table),
            "base_prompts": {
                rel: table[rel]["sha256"]
                for rel in files
                if (naming.classify(name, rel) or {}).get("kind") == "prompts"
            },
            "created_at": fsutil.utc_now(),
        },
    )
    return directory


def prompt_numbers(name: str, staging_dir: Path) -> List[int]:
    numbers = []
    for entry in Path(staging_dir).iterdir():
        number = naming.parse_prompt_number(name, entry.name)
        if number is not None and entry.is_file():
            numbers.append(number)
    return sorted(numbers)


def format_prompt(name: str, number: int, text: str, source: str, recorded_at: str) -> str:
    """Prompt file: a heading, one metadata comment, then the verbatim text.

    The file deliberately has no YAML front matter: GitHub Pages (Jekyll)
    would otherwise convert a published prompt into an HTML page.
    """
    body = text if text.endswith("\n") else text + "\n"
    return (
        f"# {name} - prompt {number:03d}\n\n"
        f"<!-- recorded: {recorded_at}; source: {source} -->\n\n"
        f"{body}"
    )


def add_prompt(staging_dir: Path, text: str, source: str = "user") -> Path:
    """Save the next prompt, verbatim, beside the .csd it will produce."""
    if source not in PROMPT_SOURCES:
        raise StagingError(f"prompt source must be one of {PROMPT_SOURCES}")
    if not text or not text.strip():
        raise StagingError("refusing to record an empty prompt")
    meta = load_meta(staging_dir)
    name = meta["composition"]
    numbers = prompt_numbers(name, staging_dir)
    number = (numbers[-1] if numbers else 0) + 1
    path = safe_join(Path(staging_dir), naming.prompt_name(name, number))
    if path.exists():
        raise StagingError(f"{path.name} already exists")
    fsutil.atomic_write_text(path, format_prompt(name, number, text, source, fsutil.utc_now()))
    return path


def run_command(
    staging_dir: Path,
    argv: List[str],
    *,
    seed: Optional[int] = None,
    parameters: Optional[Dict[str, Any]] = None,
    timeout: int = 3600,
) -> Dict[str, Any]:
    """Run a generator program inside the staging area and record it.

    The command runs with the staging directory as its working directory.
    Its output is appended to outputs/<name>-generate.log, and the command,
    seed, parameters and exit status are recorded for the manifest.
    """
    if not argv:
        raise StagingError("no command given")
    meta = load_meta(staging_dir)
    name = meta["composition"]
    staging_dir = Path(staging_dir)
    log_path = safe_join(staging_dir, f"{naming.OUTPUTS_DIR}/{name}-generate.log")
    log_path.parent.mkdir(parents=True, exist_ok=True)
    environment = dict(os.environ)
    environment["STUDIO_COMPOSITION"] = name
    environment["STUDIO_STAGING"] = str(staging_dir)
    if seed is not None:
        environment["STUDIO_SEED"] = str(seed)
    started = fsutil.utc_now()
    clock = time.monotonic()
    # "python" and "node" mean the interpreters configured for the studio, so
    # that recorded commands stay portable between machines.
    actual = list(argv)
    override = {"python": "STUDIO_PYTHON", "python3": "STUDIO_PYTHON", "node": "STUDIO_NODE"}.get(argv[0])
    if override and os.environ.get(override):
        actual[0] = os.environ[override]
    try:
        result = subprocess.run(
            actual,
            cwd=str(staging_dir),
            env=environment,
            capture_output=True,
            text=True,
            errors="replace",
            timeout=timeout,
        )
        exit_code: Optional[int] = result.returncode
        output = (result.stdout or "") + (result.stderr or "")
    except subprocess.TimeoutExpired as error:
        exit_code = None
        output = f"{error.stdout or ''}{error.stderr or ''}\n[timed out after {timeout} s]\n"
    except OSError as error:
        exit_code = None
        output = f"[could not start: {error}]\n"
    record: Dict[str, Any] = {
        "argv": [fsutil.scrub_home(arg) for arg in argv],
        "exit_code": exit_code,
        "started_at": started,
        "seconds": round(time.monotonic() - clock, 3),
    }
    if seed is not None:
        record["seed"] = seed
    if parameters:
        record["parameters"] = parameters
    with open(log_path, "a", encoding="utf-8") as log:
        log.write(f"$ {' '.join(record['argv'])}\n[started {started}; exit {exit_code}]\n")
        log.write(fsutil.scrub_home(output) or "")
        if output and not output.endswith("\n"):
            log.write("\n")
        log.write("\n")
    runs_path = staging_dir / RUNS_META
    runs = fsutil.read_json(runs_path) if runs_path.is_file() else []
    runs.append(record)
    fsutil.atomic_write_json(runs_path, runs)
    if exit_code != 0:
        raise StagingError(
            f"generator command failed (exit {exit_code}); see {log_path.name}:\n"
            + "\n".join(output.strip().splitlines()[-15:])
        )
    return record


def load_runs(staging_dir: Path) -> List[Dict[str, Any]]:
    path = Path(staging_dir) / RUNS_META
    return fsutil.read_json(path) if path.is_file() else []


def load_render(staging_dir: Path) -> Optional[Dict[str, Any]]:
    path = Path(staging_dir) / RENDER_META
    return fsutil.read_json(path) if path.is_file() else None


def inputs_hash(name: str, staging_dir: Path) -> str:
    """Hash of everything that determines the rendered sound: .csd and code."""
    files = [
        rel
        for rel in fsutil.list_files(staging_dir)
        if (naming.classify(name, rel) or {}).get("kind") in ("csd", "code")
    ]
    return fsutil.tree_hash(fsutil.hash_files(staging_dir, files))


def check_prompts(meta: Dict[str, Any], staging_dir: Path) -> List[str]:
    """Names of the prompts added in this staging area; raises on tampering."""
    name = meta["composition"]
    numbers = prompt_numbers(name, staging_dir)
    if numbers != list(range(1, len(numbers) + 1)):
        raise StagingError(f"prompt numbers are not contiguous from 001: {numbers}")
    base = meta.get("base_prompts") or {}
    for rel, digest in sorted(base.items()):
        path = Path(staging_dir) / rel
        if not path.is_file():
            raise StagingError(f"earlier prompt {rel} was removed; prompts must be preserved")
        if fsutil.sha256_file(path) != digest:
            raise StagingError(f"earlier prompt {rel} was modified; prompts must be preserved")
    new = [naming.prompt_name(name, n) for n in numbers if naming.prompt_name(name, n) not in base]
    if not new:
        raise StagingError(
            "no new prompt recorded; save the instructions that produced this version "
            "with 'studio prompt' before accepting"
        )
    return new


def discard(studio: Studio, staging_dir: Path) -> None:
    staging_dir = Path(staging_dir).resolve()
    if staging_dir.parent != studio.staging_root.resolve():
        raise IntegrityError(f"refusing to delete {staging_dir}: not a staging directory")
    fsutil.remove_tree(staging_dir)


def list_stagings(studio: Studio) -> List[Dict[str, Any]]:
    found: List[Dict[str, Any]] = []
    if not studio.staging_root.is_dir():
        return found
    for entry in sorted(studio.staging_root.iterdir()):
        if entry.is_dir() and not entry.name.startswith(".") and (entry / STAGING_META).is_file():
            try:
                meta = load_meta(entry)
            except (StudioError, ValueError):
                continue
            render = load_render(entry)
            found.append(
                {
                    "path": entry,
                    "composition": meta["composition"],
                    "kind": meta["kind"],
                    "created_at": meta.get("created_at"),
                    "render": (render or {}).get("status", "not rendered"),
                }
            )
    return found
