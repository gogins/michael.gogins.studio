# AI composition studio

Tools for composing Csound pieces by prompting an AI agent, keeping every
version, and publishing selected versions to a small web viewer.

A composition is a complete Csound `.csd` file. The agent writes it directly
or writes a program (Python, JavaScript or C++, usually with CsoundAC) that
writes it. The studio validates it, renders it with Csound, keeps the `.csd`,
the prompts, the program, the audio and a provenance manifest together, and
archives the previous version under a number before a new one takes its place.

Everything belonging to the studio is in four directories of this repository:

| Directory | Contents |
| --- | --- |
| `music/ai-compositions/` | The archive: every composition and all of its versions. Authoritative. |
| `docs/compositions/` | Published copies of selected files, and `catalog.json`. Derived from the archive. |
| `docs/studio/` | The viewer: static HTML, CSS and JavaScript modules. |
| `studio-tools/` | These tools, their tests, documentation, Cursor templates and cloud setup. |

## Requirements

- Python 3.9 or later (standard library only) for the tools themselves.
- Csound 7 to render.
- Optional: ffmpeg (FLAC master and MP3 listening copy), a Python with
  CsoundAC, Node.js with the CsoundAC WebAssembly build, a C++ compiler.

`studio env` reports what is found and what is missing. See
[docs/dependencies.md](docs/dependencies.md).

## A session

```sh
export PATH="$PWD/studio-tools/bin:$PATH"

S=$(studio new nu)                       # or: S=$(studio stage nu) to revise nu
studio prompt "$S" --file request.txt    # save the instructions, verbatim
# write "$S/nu.csd" directly, or write "$S/code/nu-generate.py" and run it:
studio run "$S" --seed 7 -- python code/nu-generate.py
studio render "$S"                       # validate, render, measure, encode
studio accept "$S" --model "<AI model and version>" --agent "<Cursor desktop | Cursor Cloud Agent>"
studio publish nu                        # copy to docs/compositions/, update the catalog
studio serve                             # http://127.0.0.1:8000/studio/
```

Nothing about the current composition changes until `studio accept`, and
`accept` refuses a staging area that has not been rendered successfully.

## Commands

| Command | Purpose |
| --- | --- |
| `studio env [--json] [--require-csoundac]` | Tools found in this environment, and what is missing. |
| `studio new NAME [--title T]` | Start a new composition; prints its staging directory. |
| `studio stage NAME` | Start a revision: a working copy of the current `.csd`, prompts and code. |
| `studio adopt NAME --from DIR [--version N] [--list]` | Stage a revision made in another copy (Git branch) of the composition. |
| `studio prompt STAGING (--text T \| --file F) [--source user\|agent]` | Save a prompt, verbatim, as the next numbered prompt file. |
| `studio run STAGING [--seed N] [--param K=V]... -- PROGRAM ARGS...` | Run a generating program in the staging directory and record it. |
| `studio render STAGING [--keep-wav] [--allow-clipping] [--allow-silence]` | Validate and render with Csound; encode FLAC and MP3. |
| `studio accept STAGING [--model M] [--agent A] [--title T] [--notes N]` | Archive the current version under the next number; promote the staged one. |
| `studio discard STAGING` | Delete a staging directory. |
| `studio list` | Compositions and staging directories. |
| `studio show NAME [--version N] [--json]` | Provenance of a version. |
| `studio verify [NAME...]` | Check naming, manifests, and that archived versions are unmodified. |
| `studio recover NAME [--rollback] [--break-lock]` | Finish or undo an interrupted `accept`. |
| `studio rerender NAME [--version N] [--regenerate]` | Render a stored version again and compare with the stored audio. |
| `studio publish NAME... [--versions V] [--kinds K] [--dry-run] [--prune] [--force]` | Publish selected files. |
| `studio unpublish NAME [--versions V] [--dry-run] [--force]` | Remove published files (never the archive). |
| `studio catalog` | Regenerate `docs/compositions/catalog.json`. |
| `studio serve [--host H] [--port P]` | Serve `docs/` locally to use the viewer. |

`studio --root DIR` (or `STUDIO_ROOT`) points the tools at another repository
root; the tests and `cloud/check.sh` use it to work in temporary directories.

## Documentation

- [docs/architecture.md](docs/architecture.md): how the parts fit together.
- [docs/versioning.md](docs/versioning.md): naming, versions, manifests, and the recovery protocol.
- [docs/csound-conventions.md](docs/csound-conventions.md): p-fields, and what was verified about CsoundAC.
- [docs/publishing.md](docs/publishing.md): what is published, and the safeguards.
- [docs/usage.md](docs/usage.md): working from the MacBook and from Android; running the viewer locally.
- [docs/cloud-agents.md](docs/cloud-agents.md): Cursor Cloud Agents, and integrating work from several branches.
- [docs/dependencies.md](docs/dependencies.md): what has to be installed, on macOS and on Linux.
- [docs/testing.md](docs/testing.md): the automated tests and how to run them.
- [docs/deployment.md](docs/deployment.md): reviewing, approving and deploying; open questions.
- [AGENT_INSTRUCTIONS.md](AGENT_INSTRUCTIONS.md): the rules an AI agent follows when composing here.
- [cursor/INSTALL.md](cursor/INSTALL.md): Cursor configuration templates. Nothing is installed outside `studio-tools/` without approval.

## Tests

```sh
studio-tools/run-tests.sh        # all tests
studio-tools/cloud/check.sh      # does this machine render? (works in a temporary directory)
```
