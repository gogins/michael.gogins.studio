# Naming, versions, manifests and recovery

## Layout of a composition

For a composition named `nu`:

```
music/ai-compositions/nu/
    nu.csd                    the composition: the canonical artifact
    nu-prompt-001.md          every prompt so far, in order
    nu-prompt-002.md
    nu-prompt-003.md
    nu-manifest.json          provenance of this version
    code/nu-generate.py       programs that generated nu.csd, if any
    outputs/nu.flac           master
    outputs/nu.mp3            listening copy
    outputs/nu-render.log     Csound's log and the measurements
    outputs/nu-generate.log   output of the generating programs
    version-0001/             the first version, in the same layout
    version-0002/             the second version
```

The files directly in `nu/` are the **current** version. There is no
`current/` directory. Each `version-NNNN/` directory is a complete earlier
version with its own `.csd`, prompts, manifest, `code/` and `outputs/`.

## Names

- Composition names: letters, digits and underscores, beginning with a
  letter or digit, at most 64 characters. Two names differing only in case
  are refused.
- `<name>.csd` is the canonical file. Every other file begins with
  `<name>-`; files in `outputs/` may also be `<name>.<extension>`.
- Prompts are `<name>-prompt-NNN.md`, numbered from 001 without gaps.
- Programs are in `code/`, derived artifacts in `outputs/`.
- Anything else (unknown names, stray directories, symbolic links) is
  reported by `studio render`, `studio accept` and `studio verify`.

Files are classified by kind, which the publishing tool uses:
`csd`, `prompts`, `manifest`, `code`, `audio` (MP3, Opus, Ogg, M4A),
`master` (FLAC, WAV, AIFF), `derived` (MIDI, MusicXML, PDF, images and
other outputs), `logs`.

## How a version is made

1. `studio new NAME` or `studio stage NAME` creates a staging directory under
   `music/ai-compositions/.staging/` (ignored by Git). For a revision it
   holds a working copy of the current `.csd`, prompts and code.
2. The prompt is saved, the `.csd` is written or generated, and
   `studio render` validates and renders it there.
3. `studio accept` archives the current version as the next `version-NNNN/`
   and puts the staged files in its place.

Version numbers are sequential and never reused. The next archive gets one
more than the highest existing number. Before using it, `accept` checks
that the version directories run from 1 without gaps and that the current
manifest's ordinal is one more than the highest; if a directory has gone
missing it refuses, rather than hand the number out again. An existing
`version-NNNN/` is never overwritten: the files are made read-only when
archived, `accept` refuses to archive onto an existing directory, and
`studio verify` compares every archived version with the hash recorded when
it was archived.

Prompts accumulate. A revision starts with all earlier prompt files and
adds at least one; `accept` refuses if an earlier prompt was changed or
removed, or if no new prompt was saved. So the current version always holds
the complete sequence of prompts, and each archived version holds the
sequence as it stood then. Prompt files hold the text verbatim after a
two-line header (a heading and a comment with the time and whether the
user or the agent wrote it).

## The manifest

`<name>-manifest.json` is written by `accept`. It records provenance and
nothing else:

| Key | Content |
| --- | --- |
| `composition`, `title`, `ordinal`, `created_at` | Which version this is. The ordinal counts versions from 1; the current version of a composition with two archived versions has ordinal 3. |
| `prompts`, `new_prompts` | All prompt files, and those added in this version. |
| `files` | Every file of the version with its SHA-256 and size. |
| `history` | For each archived version: its directory, ordinal, tree hash and archival time. |
| `ai` | Model and agent, as given to `accept`. |
| `generator` | `kind` (`direct-csd`, `python-csoundac`, `javascript-csoundac-wasm`, `cpp-csoundac`, ...), source files, the commands that were run, seeds and parameters. |
| `environment` | Csound version and commit, CsoundAC revision and p-field layout, Python, Node.js and ffmpeg versions, platform. |
| `render` | The Csound and ffmpeg commands, times, measurements (duration, peak, RMS, clipped samples), a hash of the rendered samples, the output files and the log. |
| `validation` | The checks that passed, and warnings. |
| `dependencies` | `#include`d files, sample files and plugins referred to by the `.csd`, and any given with `--dependency`. |
| `adopted_from` | Present when the revision was made on another branch; see [cloud-agents.md](cloud-agents.md). |
| `notes` | Free text, when given. |

Home-directory paths are shortened to `~` before being written, so
manifests do not carry a user name.

## Rendering checks

`studio render` refuses, and leaves the current composition alone, when:
the `.csd` lacks `<CsoundSynthesizer>`, `<CsInstruments>` or `<CsScore>`;
`csound --syntax-check-only` fails; Csound exits with an error; the log
reports errors in performance, `INIT ERROR` or `PERF ERROR`; no audio was
written; the audio is silent (peak below -80 dBFS); or samples are out of
range. The last two can be overridden deliberately with `--allow-silence`
and `--allow-clipping`.

The render is a 24-bit WAV. With ffmpeg it is encoded to FLAC (the master)
and MP3 (the listening copy) and the WAV is deleted; without ffmpeg the WAV
is kept as the master. Real-time output options such as `-odac` in
`<CsOptions>` are overridden.

`accept` also refuses when the `.csd` or code changed after the last
render, so the stored audio always belongs to the stored `.csd`.

## Checking and reproducing

- `studio verify [NAME...]` checks names, manifests, the hash of every file
  against its manifest, and every archived version against the history.
- `studio rerender NAME [--version N]` renders a stored version again in a
  scratch directory and compares the samples with the stored render.
  With `--regenerate` it first re-runs the recorded generator commands and
  compares the regenerated `.csd` with the stored one. The stored version
  is only read.

One limitation: a `.csd` that refers to external files by a path relative
to itself resolves that path one directory deeper once it is archived in
`version-NNNN/`. Record such files with `--dependency` and prefer paths
that do not depend on the location of the `.csd`.

## Safeguards against interruption and concurrent work

**Staging.** All work happens in a staging directory. A failed generator or
render changes nothing in the composition.

**Stale staging.** A staged revision records the hash of the version it was
based on. If the composition has changed by the time of `accept` (another
agent got there first), `accept` refuses with a conflict and changes
nothing. Stage a fresh revision and carry the changes over.

**Lock.** `accept` and `recover` hold a per-composition lock,
`music/ai-compositions/.locks/<name>.lock`, created exclusively. A second
process is refused. A lock left by a dead process on the same machine is
taken over automatically; a lock from another machine needs
`studio recover NAME --break-lock`.

**Transaction.** For a revision, `accept` does this under the lock:

1. *Prepared.* The new state is copied to `nu/.incoming-<id>/` and verified
   against the staging area. The journal `nu/.transaction.json` is written
   with the hashes of the old and the new state. Nothing visible has
   changed yet.
2. *Archived.* The current state is copied to `nu/.version-NNNN.partial/`,
   verified, made read-only, and renamed to `nu/version-NNNN/` in one
   atomic step.
3. *Promoting.* Each top-level entry of the new state is moved into place;
   what it replaces is moved to `nu/.outgoing-<id>/`.
4. *Done.* The result is verified, the journal is deleted (the commit
   point), and the temporary directories are removed.

A new composition is simpler: it is assembled and verified beside the
archive and appears with a single atomic rename.

All copies are written to a temporary name, flushed to disk and renamed,
so no file is ever seen half-written under its final name.

## Recovery protocol

When a command reports an interrupted version operation, or
`.transaction.json` exists in a composition directory:

```sh
studio recover NAME      # complete the operation if possible, otherwise undo it
studio verify NAME       # must print "ok"
```

`recover` reads the journal and decides:

- If `version-NNNN/` exists and matches the old state's hash, the archive
  step finished. Recovery **rolls forward**: it moves the remaining entries
  into place, verifies the result against the new state's hash, and
  deletes the journal. The revision is accepted.
- Otherwise it **rolls back**: it restores the old current state (from the
  untouched files, or from `version-NNNN/` if promotion had begun), verifies
  it against the old state's hash, removes the incomplete archive
  directory and the journal. The composition is exactly as it was, and
  the staging directory is still there to be accepted again.
- `studio recover NAME --rollback` forces the second outcome.

Both outcomes are verified by hash before the journal is removed. While a
journal exists, `stage`, `accept` and `publish` refuse to work on that
composition. `recover` on a composition with no journal only removes
leftover temporary directories and is safe to run at any time.

If recovery itself cannot verify either state (the files were damaged by
something other than the tools), it stops and says so. Restore the
composition's directory from Git in that case:
`git restore --source=HEAD --staged --worktree -- music/ai-compositions/NAME`.

The tests interrupt `accept` at every step, both by raising an error inside
the process and by killing the process, and check that recovery gives
exactly the old or exactly the new state each time; see
[testing.md](testing.md).

## Git

The archive is ordinary files, so it is committed like anything else:

```sh
git add -- music/ai-compositions docs/compositions
git commit -m "nu: version 4"
```

`.staging/` and `.locks/` are ignored. Rendered audio is committed: the
FLAC master and the MP3 of every version. `accept` warns about any file
over 20 MB and refuses files over 95 MB (GitHub rejects 100 MB) unless
`--allow-large` is given. For long pieces consider keeping only the MP3 in
Git, or Git LFS; that is a decision for the composer, see
[deployment.md](deployment.md).
