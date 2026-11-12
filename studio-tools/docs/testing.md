# Testing

## Running

```sh
studio-tools/run-tests.sh               # all tests
studio-tools/run-tests.sh -v            # one line per test
studio-tools/run-tests.sh test_publish  # one module
studio-tools/cloud/check.sh             # does this machine render? (not part of the suite)
```

The tests are `unittest` tests in `studio-tools/tests/`, using only the
Python standard library. Every test that writes anything does so in a
fresh temporary directory, with its own archive, staging area and
published directory. The repository is read by a few tests and written by
none.

Most tests use small stand-ins for `csound` and `ffmpeg`
(`tests/support.py`) that behave like the real programs as far as the
studio can tell, and that can be told to fail in specific ways: a syntax
error, a crash, a run-time error in the log, no output, silence, clipping.
That makes the failure paths testable on any machine. Tests that need the
real tools use them when present and are reported as skipped otherwise:

| Tests | Need | Otherwise |
| --- | --- | --- |
| `test_render.RealCsoundTests` | `csound` on the PATH (and `ffmpeg` for the encoding part) | skipped |
| `test_generators` | `STUDIO_PYTHON` with CsoundAC; Node.js with `CSOUNDAC_WASM_DIR`; `STUDIO_CSOUNDAC_CXXFLAGS`/`LDFLAGS` | each skipped separately |
| `test_viewer.BrowserTests` | Chrome or Chromium (`STUDIO_CHROME` to name it) | skipped |
| `test_viewer.DiffModuleTests` | Node.js | skipped |
| `test_branches` | Git | skipped |
| `test_repository.PreexistingFilesTests` | A Git checkout with the base branch (`STUDIO_BASE_BRANCH`, default `master` or `main`) | skipped |

In a sandbox that forbids creating Git repositories or starting a browser,
run the suite outside the sandbox; those tests fail there, they do not skip.

## What is tested

| Requirement | Tests |
| --- | --- |
| Naming conventions | `test_naming_paths.NamingTests`: valid and invalid composition names, file names, version directories, classification of files, problems in a state. |
| Path traversal; no writes outside the authorized directories | `test_naming_paths.PathSafetyTests`: `..`, absolute paths, hidden segments and symbolic links are refused; malicious names create nothing; after a whole session every file written is inside the four directories. |
| Sequential allocation of versions | `test_versioning.VersioningTests`: versions are 1, 2, 3 in order; a number is not reused after a gap; an existing version directory is never overwritten. |
| Immutability of archived versions | `test_versioning.VersioningTests`: an archived version equals the previous current state, is read-only, and is unchanged after further revisions; `verify` detects modified, added, removed and missing files. |
| Prompt order and preservation | `test_versioning.PromptTests`: numbered in order, text verbatim, accumulated across versions; changing or deleting an earlier prompt, an empty prompt, and a revision without a new prompt are refused. |
| Preservation of code and outputs | `test_versioning`, `test_render`, `test_generators`: code and outputs are archived with their version and re-rendering reproduces the stored audio. |
| Manifests | `test_versioning.ManifestTests`: contents, history linking every archived version, detection of invalid manifests, no score data and no home directory in a manifest. |
| Render failures | `test_render.RenderTests`: syntax error, crash, run-time errors in the log, no audio, silence, clipping, incomplete `.csd`, missing Csound, failing generator; a failed revision leaves the current version intact; a failed render can be corrected and accepted; an unrendered or stale staging area cannot be accepted. |
| Audio measurement | `test_render.AudioAnalysisTests`: levels of known signals, silence, clipping, sample hash, rejection of non-audio. |
| Real rendering | `test_render.RealCsoundTests`: Csound 7 renders a 24-bit, 48 kHz stereo file of the right length; FLAC and MP3 are produced; re-rendering is identical; a real syntax error is caught. |
| Recovery from interruption | `test_recovery.RecoveryTests`: `accept` is interrupted at every checkpoint, by an error in the process and by killing the process; recovery yields exactly the old or exactly the new state; explicit rollback from every point; damaged or lost intermediate copies; damage that cannot be recovered is reported; numbering continues correctly afterwards. |
| Concurrent revisions | `test_versioning.ConcurrencyTests`: a held lock blocks; a dead process's lock is taken over; another machine's lock needs `--break-lock`; a second revision from the same base conflicts; simultaneous accepts yield exactly one new version. `test_branches`: two Git branches revising one composition are integrated with `studio adopt` without losing a version. |
| Catalog generation | `test_publish.CatalogTests`: versions and artifacts, relative paths that exist, stability when nothing changes, empty library, missing optional artifacts, derived artifacts, foreign files ignored. |
| Publication selection and path mapping | `test_publish.PublishTests`: one-to-one path mapping; selection of compositions, versions and kinds; masters, logs and other compositions not published by default; unchanged files skipped; conflicts; stale files and `--prune`; `unpublish`; a corrupt archive is not published; front matter refused. |
| Dry run | `test_publish.PublishTests.test_dry_run_writes_nothing`. |
| No publication outside `docs/compositions/` | `test_publish.PublishSafetyTests`: symbolic links in and at the destination, a tampered state file, and a check that everything published is inside the directory. |
| The archive is not modified by publishing | `test_publish`: the archive's tree hash is compared before and after. |
| Browser loading of the catalog; audio playback | `test_viewer.BrowserTests`: headless Chrome loads the viewer in a 412 x 915 frame against a site published by the tools; checks the library, version selection, prompts, `.csd`, code, files, provenance, comparison, playback and seeking of real audio, an archived version, missing optional artifacts, an unknown composition, an empty library, a missing catalog, and that no view is wider than the frame. |
| Viewer code | `test_viewer.DiffModuleTests` (the diff reproduces both texts for 300 random cases); `test_repository.StudioFilesTests`: no absolute URLs, no `innerHTML`, all references resolve. |
| Command line and local server | `test_cli_serve`: a complete session through `bin/studio`; exit status on errors; `publish`, `catalog`, `recover`; whole files, content types, byte ranges and refusal of escapes by the server. |
| Generating programs | `test_generators`: Python, JavaScript (WebAssembly) and C++ programs using the real CsoundAC write the same score with the studio's p-field layout, are recorded in the manifest, and regenerate the identical `.csd`. |
| No modification of pre-existing files | `test_repository.PreexistingFilesTests`: against the base branch, every committed and every working-tree change is an added file inside the four directories; nothing under `music/` or `docs/` outside the studio's directories differs; no root `AGENTS.md`, `.cursor/`, `.github/`, `CNAME` or `.nojekyll` was added; the tools contain no push, merge or deploy command. |
| The real archive | `test_repository.StudioFilesTests`: every composition verifies; published files match the archive and the catalog; no secrets; no symbolic links; no front matter in published files. |

## Results

Run on 12 November 2026 on macOS 27.2 (Apple silicon), Python 3.15.2 for
the suite, outside the sandbox:

| Environment | Result |
| --- | --- |
| Configured with `config/studio-env.macos.example.sh` (Csound 7.0, ffmpeg 9.0.2, CsoundAC under Python 3.12.16, Node.js 26.13.1 with the WebAssembly build, C++ flags, Chrome, Git) | **129 tests, all passed, none skipped** (109 s) |
| No configuration (Csound, ffmpeg, Node.js, Chrome and Git on the PATH; no CsoundAC) | 129 tests: 126 passed, 3 skipped (the three `test_generators` tests) |
| Csound, ffmpeg and Node.js removed from the PATH | 129 tests: 123 passed, 6 skipped (generators, real Csound, the diff module) |
| No configuration, suite and tools run by the system Python 3.9.6 | 129 tests: 126 passed, 3 skipped (the three `test_generators` tests) |

`cloud/check.sh` on the same machine: all twelve checks passed with the
example configuration; without configuration the nine Csound checks passed
and the three CsoundAC routes were reported as not configured; with
`STUDIO_CSOUND` pointing nowhere it failed with exit status 1.

Not run anywhere: the suite and `check.sh` on Linux, and
`cloud/install.sh`.

Test counts by module: `test_naming_paths` 13, `test_versioning` 25,
`test_recovery` 9, `test_render` 22, `test_publish` 30, `test_cli_serve` 7,
`test_generators` 3, `test_branches` 4, `test_viewer` 4,
`test_repository` 12.

## Manual checks

Things the suite does not cover and that were checked by hand while
building the studio:

- The viewer in Cursor's built-in browser against the published `nu`:
  catalog loaded, three versions listed, audio played and seeked,
  comparison shown.
- `studio rerender` of all three versions of `nu`, with `--regenerate` for
  the two generated ones: identical audio and identical `.csd`.

Things not checked at all:

- The viewer on a physical Android phone. The browser test uses a
  phone-sized frame in desktop Chrome.
- The deployed site: how GitHub Pages (Jekyll) serves the new files.
