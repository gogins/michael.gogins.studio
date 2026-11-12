"""Command line interface: studio-tools/bin/studio <command> ..."""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any, List, Optional

from . import (
    __version__,
    audio,
    catalog,
    environment,
    naming,
    publish as publish_module,
    render as render_module,
    serve as serve_module,
    staging,
    versioning,
)
from .paths import Studio, StudioError

EPILOG = """\
Typical session:
  S=$(studio new nu)                      # or: S=$(studio stage nu) to revise
  studio prompt "$S" --file request.txt   # save the instructions, verbatim
  ... write $S/nu.csd, or $S/code/nu-generate.py and:
  studio run "$S" -- python code/nu-generate.py
  studio render "$S"                      # validate, render, measure, encode
  studio accept "$S" --model "<AI model>" # archive the old version, promote
  studio publish nu                       # copy to docs/compositions/
See studio-tools/README.md.
"""


def _out(text: str = "") -> None:
    print(text)


def _note(text: str) -> None:
    print(text, file=sys.stderr)


def cmd_env(studio: Studio, args: argparse.Namespace) -> int:
    info = environment.collect()
    if args.json:
        _out(json.dumps(info, indent=2, sort_keys=True))
    else:
        csound, python, node, ffmpeg = info["csound"], info["python"], info["node"], info["ffmpeg"]
        _out(f"repository   {studio.root}")
        _out(f"platform     {info['platform']}")
        _out(f"csound       {csound.get('banner') or 'NOT FOUND'}  [{csound.get('path', '-')}]")
        _out(f"python       {python.get('version', '?')}  [{python.get('path')}]")
        if python.get("csoundac"):
            _out(f"CsoundAC     importable; p-field layout: {python.get('csoundac_pfield_layout')}"
                 f"; revision: {python.get('csoundac_revision') or 'not recorded'}")
            _out(f"             probe: {python.get('csoundac_probe_istatement')}")
        else:
            _out(f"CsoundAC     NOT importable ({python.get('csoundac_error', 'unknown error')})")
        _out(f"node         {node.get('version', 'NOT FOUND')}; CsoundAC WASM: "
             f"{'present in ' + str(node.get('csoundac_wasm_dir')) if node.get('csoundac_wasm_present') else 'not configured (CSOUNDAC_WASM_DIR)'}")
        _out(f"ffmpeg       {ffmpeg.get('version', 'NOT FOUND')}")
    problems = environment.problems(require_csoundac=args.require_csoundac)
    for problem in problems:
        _note(f"problem: {problem}")
    return 1 if problems else 0


def cmd_new(studio: Studio, args: argparse.Namespace) -> int:
    directory = staging.new_composition(studio, args.name, args.title)
    _note(f"staging area for new composition {args.name}:")
    _out(str(directory))
    return 0


def cmd_stage(studio: Studio, args: argparse.Namespace) -> int:
    directory = staging.stage_revision(studio, args.name)
    _note(f"staging area for a revision of {args.name} (working copy of the current version):")
    _out(str(directory))
    return 0


def cmd_prompt(studio: Studio, args: argparse.Namespace) -> int:
    directory = staging.resolve(studio, args.staging)
    if args.text is not None:
        text = args.text
    elif args.file == "-":
        text = sys.stdin.read()
    else:
        text = Path(args.file).read_text(encoding="utf-8")
    path = staging.add_prompt(directory, text, args.source)
    _out(str(path))
    return 0


def cmd_run(studio: Studio, args: argparse.Namespace) -> int:
    directory = staging.resolve(studio, args.staging)
    command = list(args.command)
    if command and command[0] == "--":
        command = command[1:]
    parameters = {}
    for item in args.param or []:
        key, separator, value = item.partition("=")
        if not separator:
            raise StudioError(f"--param needs KEY=VALUE, got {item!r}")
        parameters[key] = value
    record = staging.run_command(
        directory, command, seed=args.seed, parameters=parameters or None, timeout=args.timeout
    )
    _out(f"ok ({record['seconds']} s): {' '.join(record['argv'])}")
    return 0


def cmd_render(studio: Studio, args: argparse.Namespace) -> int:
    directory = staging.resolve(studio, args.staging)
    record = render_module.render(
        directory,
        keep_wav=args.keep_wav,
        allow_clipping=args.allow_clipping,
        allow_silence=args.allow_silence,
        timeout=args.timeout,
    )
    for line in audio.summary({**record["analysis"], "data_sha256": ""}):
        _out(line)
    _out("outputs: " + ", ".join(record["outputs"]))
    for warning in record["warnings"]:
        _note(f"warning: {warning}")
    return 0


def cmd_accept(studio: Studio, args: argparse.Namespace) -> int:
    directory = staging.resolve(studio, args.staging)
    result = versioning.accept(
        studio,
        directory,
        ai_model=args.model,
        ai_agent=args.agent,
        title=args.title,
        notes=args.notes,
        generator_kind=args.generator,
        dependencies=args.dependency,
        allow_large=args.allow_large,
    )
    name = result["composition"]
    if result["archived_as"]:
        _out(f"{name}: previous version archived as {result['archived_as']}; "
             f"the revision is now current (ordinal {result['ordinal']})")
    else:
        _out(f"{name}: created (ordinal 1)")
    for warning in result["warnings"]:
        _note(f"warning: {warning}")
    _out(f"to commit:  git add {studio.relative(studio.composition_dir(name))} && "
         f"git commit -m \"{name}: version {result['ordinal']}\"")
    return 0


def cmd_discard(studio: Studio, args: argparse.Namespace) -> int:
    directory = staging.resolve(studio, args.staging)
    staging.discard(studio, directory)
    _out(f"discarded {directory.name}")
    return 0


def cmd_list(studio: Studio, args: argparse.Namespace) -> int:
    names = versioning.list_compositions(studio)
    if not names:
        _out("no compositions yet")
    for name in names:
        try:
            info = versioning.describe(studio, name)
            pending = "  [INTERRUPTED: run 'studio recover']" if info["pending_transaction"] else ""
            _out(f"{name}: current is ordinal {info['ordinal']}, "
                 f"{len(info['versions'])} archived, {len(info['prompts'])} prompts{pending}")
        except StudioError as error:
            _out(f"{name}: PROBLEM: {error}")
    for item in staging.list_stagings(studio):
        _out(f"staging: {studio.relative(item['path'])} ({item['kind']} of {item['composition']}, "
             f"{item['render']}, created {item['created_at']})")
    return 0


def cmd_show(studio: Studio, args: argparse.Namespace) -> int:
    from . import manifest as manifest_module

    directory = versioning.state_dir(studio, args.name, args.version)
    manifest = manifest_module.load(directory, args.name)
    if args.json:
        _out(json.dumps(manifest, indent=2, sort_keys=True))
        return 0
    analysis = manifest["render"].get("analysis") or {}
    _out(f"{args.name} - {manifest['title']}")
    _out(f"  directory   {studio.relative(directory)}")
    _out(f"  ordinal     {manifest['ordinal']} (created {manifest['created_at']})")
    _out(f"  AI          {manifest['ai'].get('model')} / {manifest['ai'].get('agent')}")
    _out(f"  generator   {manifest['generator'].get('kind')} {manifest['generator'].get('sources')}")
    _out(f"  csound      {(manifest['environment'].get('csound') or {}).get('banner')}")
    _out(f"  audio       {analysis.get('duration_seconds')} s, peak {analysis.get('peak_dbfs')} dBFS")
    _out(f"  prompts     {', '.join(manifest['prompts'])}")
    _out(f"  files       {', '.join(sorted(manifest['files']))}")
    return 0


def cmd_verify(studio: Studio, args: argparse.Namespace) -> int:
    names = args.names or versioning.list_compositions(studio)
    failed = False
    for name in names:
        problems = versioning.verify(studio, name)
        if problems:
            failed = True
            _out(f"{name}: {len(problems)} problem(s)")
            for problem in problems:
                _out(f"  {problem}")
        else:
            info = versioning.describe(studio, name)
            _out(f"{name}: ok (current ordinal {info['ordinal']}, {len(info['versions'])} archived)")
    if not names:
        _out("no compositions to verify")
    return 1 if failed else 0


def cmd_recover(studio: Studio, args: argparse.Namespace) -> int:
    result = versioning.recover(studio, args.name, rollback=args.rollback, break_lock=args.break_lock)
    if result["action"] == "none":
        _out(f"{args.name}: nothing to recover" +
             (f" (removed leftovers: {', '.join(result['removed'])})" if result["removed"] else ""))
    elif result["action"] == "rolled-forward":
        _out(f"{args.name}: interrupted revision completed; current is ordinal {result['ordinal']}")
    else:
        _out(f"{args.name}: interrupted revision undone; current is ordinal {result['ordinal']}")
    problems = versioning.verify(studio, args.name)
    for problem in problems:
        _out(f"  {problem}")
    return 1 if problems else 0


def cmd_rerender(studio: Studio, args: argparse.Namespace) -> int:
    result = render_module.rerender(
        studio, args.name, args.version, regenerate=args.regenerate, keep=args.keep, timeout=args.timeout
    )
    analysis = result["analysis"]
    _out(f"{args.name} {result['version']}: rendered {analysis['duration_seconds']} s, "
         f"peak {analysis['peak_dbfs']} dBFS; audio {result['audio']}")
    if "regenerated" in result:
        _out(f"  generator: {result['regenerated']}")
    if result["stored_csound"] != result["csound"]:
        _out(f"  note: stored render used {result['stored_csound']}; this is {result['csound']}")
    if result.get("scratch"):
        _out(f"  kept in {result['scratch']}")
    return 0


def _print_plan(result: dict) -> None:
    for action in result["actions"]:
        reason = f"  ({action['reason']})" if action.get("reason") else ""
        _out(f"  {action['action']:9} {action['dest']}{reason}")
    for entry in result["stale"]:
        reason = f"  ({entry['reason']})" if entry.get("reason") else ""
        _out(f"  stale:{entry['action']:8} {entry['dest']}{reason}")


def cmd_publish(studio: Studio, args: argparse.Namespace) -> int:
    result = publish_module.publish(
        studio,
        args.names,
        versions=publish_module.parse_versions(args.versions),
        kinds=publish_module.parse_kinds(args.kinds),
        dry_run=args.dry_run,
        prune=args.prune,
        force=args.force,
    )
    if args.dry_run or args.verbose:
        _print_plan(result)
    counts = publish_module.summarize(result)
    summary = ", ".join(f"{count} {action}" for action, count in sorted(counts.items())) or "nothing selected"
    if args.dry_run:
        _out(f"dry run, nothing written: {summary}")
        for refusal in result["refusals"]:
            _out(f"  would refuse: {refusal}")
        return 1 if result["refusals"] else 0
    _out(f"published to {studio.relative(studio.published)}: {summary}")
    _out(f"catalog: {result['catalog']['compositions']} composition(s), "
         f"{'updated' if result['catalog']['changed'] else 'unchanged'}")
    return 0


def cmd_unpublish(studio: Studio, args: argparse.Namespace) -> int:
    result = publish_module.unpublish(
        studio,
        args.name,
        versions=publish_module.parse_versions(args.versions),
        dry_run=args.dry_run,
        force=args.force,
    )
    verb = "would remove" if args.dry_run else "removed"
    for dest in result["removed"]:
        _out(f"  {verb} {dest}")
    for dest in result["kept_modified"]:
        _out(f"  kept (modified since publication; use --force) {dest}")
    _out(f"{verb} {len(result['removed'])} published file(s); the archive is untouched")
    return 0


def cmd_catalog(studio: Studio, args: argparse.Namespace) -> int:
    result = catalog.write(studio)
    _out(f"{studio.relative(result['path'])}: {result['compositions']} composition(s), "
         f"{'updated' if result['changed'] else 'unchanged'}")
    return 0


def cmd_serve(studio: Studio, args: argparse.Namespace) -> int:
    serve_module.serve(studio.docs, args.host, args.port)
    return 0


def _version_number(text: str) -> int:
    number = naming.parse_version_dir(text) if text.startswith("version-") else None
    if number is None:
        if not text.isdigit() or int(text) < 1:
            raise argparse.ArgumentTypeError("use a version number such as 1 or version-0001")
        number = int(text)
    return number


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="studio",
        description="AI composition studio: versioned Csound compositions, rendering and publication.",
        epilog=EPILOG,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("--version", action="version", version=f"studio {__version__}")
    parser.add_argument("--root", help="repository root (default: the repository containing these tools)")
    sub = parser.add_subparsers(dest="command", metavar="COMMAND")

    def add(name: str, function: Any, help_text: str) -> argparse.ArgumentParser:
        command = sub.add_parser(name, help=help_text, description=help_text)
        command.set_defaults(function=function)
        return command

    p = add("env", cmd_env, "show the tools found in this environment and what is missing")
    p.add_argument("--json", action="store_true")
    p.add_argument("--require-csoundac", action="store_true", help="fail if CsoundAC cannot be imported")

    p = add("new", cmd_new, "start a new composition; prints its staging directory")
    p.add_argument("name")
    p.add_argument("--title")

    p = add("stage", cmd_stage, "start a revision of a composition; prints its staging directory")
    p.add_argument("name")

    p = add("prompt", cmd_prompt, "save a prompt, verbatim, as the next numbered prompt file")
    p.add_argument("staging")
    group = p.add_mutually_exclusive_group(required=True)
    group.add_argument("--text")
    group.add_argument("--file", help="file holding the prompt text, or - for standard input")
    p.add_argument("--source", choices=staging.PROMPT_SOURCES, default="user",
                   help="who wrote the prompt (default: user)")

    p = add("run", cmd_run, "run a generator program in the staging directory and record it")
    p.add_argument("staging")
    p.add_argument("--seed", type=int, help="random seed; passed as STUDIO_SEED and recorded")
    p.add_argument("--param", action="append", metavar="KEY=VALUE", help="generation parameter to record")
    p.add_argument("--timeout", type=int, default=3600)
    p.add_argument("command", nargs=argparse.REMAINDER, help="-- program and arguments")

    p = add("render", cmd_render, "validate and render the staged .csd with Csound")
    p.add_argument("staging")
    p.add_argument("--keep-wav", action="store_true", help="keep the WAV beside the FLAC master")
    p.add_argument("--allow-clipping", action="store_true")
    p.add_argument("--allow-silence", action="store_true")
    p.add_argument("--timeout", type=int, default=3600, help="seconds (default 3600)")

    p = add("accept", cmd_accept, "archive the current version and promote the staged one")
    p.add_argument("staging")
    p.add_argument("--model", help="AI model and version (or set STUDIO_AI_MODEL)")
    p.add_argument("--agent", help="agent/product, e.g. 'Cursor desktop' (or set STUDIO_AI_AGENT)")
    p.add_argument("--title")
    p.add_argument("--notes")
    p.add_argument("--generator", help="override the detected generator kind")
    p.add_argument("--dependency", action="append", metavar="KIND:VALUE",
                   help="external dependency to record, e.g. samples:piano.sf2")
    p.add_argument("--allow-large", action="store_true", help="allow files over 95 MB")

    p = add("discard", cmd_discard, "delete a staging directory")
    p.add_argument("staging")

    add("list", cmd_list, "list compositions and staging directories")

    p = add("show", cmd_show, "show the provenance of a version")
    p.add_argument("name")
    p.add_argument("--version", type=_version_number, dest="version")
    p.add_argument("--json", action="store_true")

    p = add("verify", cmd_verify, "check naming, manifests and that archived versions are unmodified")
    p.add_argument("names", nargs="*")

    p = add("recover", cmd_recover, "finish or undo an interrupted version operation")
    p.add_argument("name")
    p.add_argument("--rollback", action="store_true", help="undo instead of completing")
    p.add_argument("--break-lock", action="store_true", help="take over a lock left by another machine")

    p = add("rerender", cmd_rerender, "render a stored version again and compare with the stored audio")
    p.add_argument("name")
    p.add_argument("--version", type=_version_number, dest="version")
    p.add_argument("--regenerate", action="store_true", help="also re-run the recorded generator commands")
    p.add_argument("--keep", action="store_true", help="keep the scratch directory")
    p.add_argument("--timeout", type=int, default=3600)

    p = add("publish", cmd_publish, "copy selected files to docs/compositions/ and update the catalog")
    p.add_argument("names", nargs="+", metavar="name")
    p.add_argument("--versions", help="all (default), or a list such as current,1,2")
    p.add_argument("--kinds", help=f"comma list of {','.join(publish_module.ALL_KINDS)} or all "
                                   f"(default {','.join(publish_module.DEFAULT_KINDS)})")
    p.add_argument("--dry-run", action="store_true", help="show what would happen; write nothing")
    p.add_argument("--prune", action="store_true",
                   help="remove published files whose source left the archive")
    p.add_argument("--force", action="store_true", help="overwrite conflicting destination files")
    p.add_argument("--verbose", action="store_true")

    p = add("unpublish", cmd_unpublish, "remove published files of a composition (not the archive)")
    p.add_argument("name")
    p.add_argument("--versions")
    p.add_argument("--dry-run", action="store_true")
    p.add_argument("--force", action="store_true")

    add("catalog", cmd_catalog, "regenerate docs/compositions/catalog.json")

    p = add("serve", cmd_serve, "serve docs/ locally to try the viewer")
    p.add_argument("--host", default="127.0.0.1", help="use 0.0.0.0 to reach it from a phone on the LAN")
    p.add_argument("--port", type=int, default=8000)
    return parser


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if not getattr(args, "function", None):
        parser.print_help()
        return 2
    try:
        studio = Studio(args.root)
        return args.function(studio, args)
    except StudioError as error:
        _note(f"studio: {error}")
        return 1
    except KeyboardInterrupt:
        _note("studio: interrupted")
        return 130


if __name__ == "__main__":
    sys.exit(main())
