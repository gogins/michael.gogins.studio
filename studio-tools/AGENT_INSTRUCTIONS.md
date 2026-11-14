# Instructions for AI agents composing in this studio

These instructions apply when you are asked to create or revise a composition
under `music/ai-compositions/`. They are for Cursor on the desktop and for
Cursor Cloud Agents alike. The human-readable documentation is in
`studio-tools/README.md` and `studio-tools/docs/`.

## What you may touch

- Create and change files only in `music/ai-compositions/`, `docs/compositions/`,
  `docs/studio/` and `studio-tools/`.
- Do not modify, rename, move or delete anything else in the repository: not
  the existing website, not other files under `music/` or `docs/`, not
  workflows or settings.
- Do not modify the Csound or CsoundAC repositories.
- Change the archive and the published files only through the `studio`
  commands. Never edit, move or delete a `version-NNNN/` directory, an
  accepted prompt file, or a manifest by hand.
- Never put secrets, tokens or credentials into any file.
- Do not push to the default branch, merge, deploy, or change repository or
  service settings unless the user has explicitly asked for that step.

## The composition

- The canonical artifact is one complete Csound file, `<name>.csd`, with
  `<CsInstruments>` and `<CsScore>`. Do not introduce another score
  representation. MIDI, MusicXML or notation, if wanted, are derived outputs
  written to `outputs/`.
- Names: letters, digits and underscores, starting with a letter or digit.
  Every file of a composition starts with its name: `nu.csd`, `nu-prompt-001.md`,
  `nu-manifest.json`, `code/nu-generate.py`, `outputs/nu-render.log` (no soundfiles).
- Follow Michael Gogins's p-field conventions exactly; see
  `studio-tools/docs/csound-conventions.md`:

  | p1 | p2 | p3 | p4 | p5 | p6 | p7 | p8 | p9 | p10 | p11 |
  | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
  | instrument | time | duration | MIDI key | MIDI velocity | depth (front to back) | pan (left to right) | height | phase | pitch-class set | homogeneity |

- Set `0dbfs = 1`. Do not rely on real-time output; the studio renders to a file.
- Keep demonstration and test pieces short. Rendered audio is committed to
  Git: a FLAC master and an MP3 for each version.

## Workflow

Use `studio-tools/bin/studio`. Run `studio env` first in a new environment.

1. Start: `S=$(studio new NAME)` for a new composition, `S=$(studio stage NAME)`
   to revise one. `S` is a staging directory; the current composition is not
   affected by anything you do there.
2. Save the user's prompt, verbatim and complete, before doing the work:
   `studio prompt "$S" --file FILE` (or `--text TEXT`, or `--file -` for standard input).
   Do not paraphrase, shorten or correct it. If you record instructions that
   you wrote yourself, use `--source agent`.
3. Write `$S/NAME.csd`, either directly or with a program kept in `$S/code/`
   (named `NAME-...`). Run programs through the studio so that the command,
   seed and parameters are recorded:
   `studio run "$S" --seed 7 -- python code/NAME-generate.py`.
   The program runs with `$S` as its working directory and must write the
   complete `NAME.csd` there. Take the random seed from `STUDIO_SEED`, so
   that the same seed reproduces the same file.
4. Render: `studio render "$S"`. This checks the structure and the syntax,
   renders to a temporary file, measures the audio, discards it, and checks
   that the piece performs in the viewer's Csound engine (`@csound/browser`).
   Do not write soundfiles into the archive.
   If it fails, read the message and `$S/outputs/NAME-render.log`, correct
   the `.csd` or the program, and render again. Do not pass
   `--allow-clipping` or `--allow-silence` to get past a real problem; fix
   the levels instead.
5. Accept: `studio accept "$S" --model "<your model name and version>" --agent "<Cursor desktop (macOS) | Cursor Cloud Agent>"`.
   The previous current version is archived as the next `version-NNNN/` and
   the staged one becomes current. State your model truthfully.
6. Check: `studio verify NAME`.
7. Publish only when the user asks for it: `studio publish NAME`
   (try `--dry-run` first). Published files become public once the branch
   is deployed.
8. Commit only the studio's directories, and say what you committed:
   `git add -- music/ai-compositions docs/compositions docs/studio studio-tools`.
   Never use `git add -A` or `git add .` in this repository.

If `accept` is interrupted, or any command reports an interrupted operation,
run `studio recover NAME` and then `studio verify NAME`. Do not repair the
directories by hand.

## CsoundAC

- Use only methods that exist. The ones verified for this studio are listed
  in `studio-tools/docs/csound-conventions.md`. When you need another one,
  check the header in the CsoundAC source (`CsoundAC/*.hpp`) or
  `dir(CsoundAC.<Class>)` first. Do not invent method names.
- Prefer explicitly named methods over overloads.
- Python: create notes with all eleven arguments,
  `score.append(time, duration, 144.0, instrument, key, velocity, phase, pan, depth, height, 4095.0)`,
  and write the score with `score.getCsoundScore(12.0, False)`. A
  default-constructed `CsoundAC.Event()` has uninitialized fields.
- JavaScript: load CsoundAC with `studio-tools/js/csoundac-node.mjs` and pass
  every argument of every call; JavaScript does not fill in C++ defaults.
- C++: `studio run "$S" -- studio-cxx code/NAME-generate.cpp`.

## Reporting

- Report what you actually ran and what it printed. Do not say that
  something renders, plays or passes unless you ran it and it did.
- If a dependency is missing, say which one, do the work that does not
  need it, and describe what remains.
- When you finish, state the composition, the new version's number, the
  level and duration that `studio render` reported, and whether you
  published and committed.
