# Dependencies

`studio env` shows what the studio finds on a machine, and
`studio-tools/cloud/check.sh` shows what actually works, by rendering a
small piece in a temporary directory.

| Part | Needed for | Required |
| --- | --- | --- |
| Python 3.9 or later, standard library | The tools themselves | yes |
| Csound 7 (`csound` command) | Validation and rendering | yes, to render |
| ffmpeg with FLAC and LAME | FLAC master and MP3 listening copy | no; without it the WAV is kept and there is no MP3 |
| Python 3.12 with the CsoundAC module | Python generating programs | no |
| Node.js, `CsoundAC.js` and `CsoundAC.wasm` | JavaScript generating programs | no |
| C++17 compiler, CsoundAC headers and library, Boost, Eigen | C++ generating programs | no |
| Git | Committing; the repository tests | no |
| Chrome or Chromium | The browser test of the viewer | no |

The tools use no Python packages outside the standard library, and the
viewer uses no JavaScript libraries.

## Configuration

Everything is found through environment variables, with sensible defaults:

| Variable | Meaning | Default |
| --- | --- | --- |
| `STUDIO_CSOUND` | Csound executable | `csound` on the PATH |
| `STUDIO_FFMPEG` | ffmpeg executable, or `none` to do without | `ffmpeg` on the PATH |
| `STUDIO_PYTHON` | Python that can import CsoundAC | `python3` |
| `STUDIO_NODE` | Node.js executable | `node` on the PATH |
| `CSOUNDAC_WASM_DIR` | Directory holding `CsoundAC.js` and `CsoundAC.wasm` | not set |
| `CSOUNDAC_SOURCE_DIR` | CsoundAC source checkout, to record its revision | not set |
| `STUDIO_CSOUNDAC_REVISION` | CsoundAC revision, stated explicitly | not set |
| `STUDIO_CSOUNDAC_CXXFLAGS`, `STUDIO_CSOUNDAC_LDFLAGS` | Compiler and linker flags for `studio-cxx` | not set |
| `STUDIO_AI_MODEL`, `STUDIO_AI_AGENT` | Defaults for `studio accept --model/--agent` | not set |
| `STUDIO_ROOT` | Repository root to work in | the repository containing the tools |
| `STUDIO_ENV_FILE` | Shell file that `studio` loads first | `~/.local/ai-composition-studio/studio-env.sh`, if it exists |

## macOS (verified)

The studio was developed and tested on a MacBook (macOS 27.2, Apple
silicon) with:

- Csound 7.0 (commit `f9434ffc`), installed in `/usr/local/bin`, framework
  in `/Library/Frameworks/CsoundLib64.framework`.
- ffmpeg 9.0.2 and Node.js 26.13 from Homebrew.
- CsoundAC built from `~/csound-ac` (`v9.1.1-beta-26-gd884dea62`) with its
  Python module installed in the virtual environment `~/venv/csound`
  (Python 3.12.16).
- The CsoundAC WebAssembly build in `~/csound-wasm/dist`.
- Apple clang for C++, Boost and Eigen from Homebrew.

`studio-tools/config/studio-env.macos.example.sh` holds these settings.
Source it, or copy it to `~/.local/ai-composition-studio/studio-env.sh` so
that `studio` loads it automatically. Then:

```sh
studio env
studio-tools/cloud/check.sh
```

Csound and CsoundAC themselves are built as described in their own
repositories (`csound/BUILD.md`, `csound-ac/build-macos.sh`); the studio
does not build or modify them.

## Linux and Cursor Cloud Agents (not yet verified)

`studio-tools/cloud/install.sh` builds the same environment on Ubuntu:

1. Installs the Ubuntu packages that the CsoundAC continuous integration
   installs for its Linux build, plus ffmpeg, Node.js, curl and unzip.
2. Builds Csound 7 from `https://github.com/csound/csound.git` with the
   options of the CsoundAC CI (no Java or Python interfaces, no tests).
3. Builds CsoundAC and its Python module from
   `https://github.com/gogins/csound-ac.git`, with submodules, with the
   options of the "Configure - Linux" step of the CsoundAC CI.
4. Downloads the `csound-wasm` release (`v4.1.2-beta`,
   `csound-wasm-4.0.0-beta.zip`) and extracts `CsoundAC.js` and
   `CsoundAC.wasm`.
5. Writes `~/.local/ai-composition-studio/studio-env.sh`, which `studio`
   loads automatically, and `studio-versions.txt` with the commits built.

Everything is installed under `~/.local/ai-composition-studio` and built
under `~/.cache/ai-composition-studio`; nothing is written into the
repository. The script can be run again; it updates the checkouts and
rebuilds what changed.

**This script has not been run.** It was written on the MacBook, where
there was neither Docker nor a Cloud Agent to run it in. It follows the
projects' own build recipes, but the following are untested and are the
likely places for a first run to need correction:

- The package list on the Ubuntu release of the Cloud Agent image.
- CsoundAC requires Python 3.12 or later with development headers
  (`find_package(Python3 3.12 REQUIRED ...)`). Ubuntu 24.04 has it; older
  releases do not. The script stops with a message in that case; it can
  still set up Csound, ffmpeg and the WebAssembly route with
  `STUDIO_INSTALL_STEPS="packages csound wasm env"`.
- The CsoundAC repository has a branch named `copilot/fix-build-linux-job`,
  which suggests that its Linux build has needed repair. If the build
  fails, the direct `.csd` and WebAssembly routes do not depend on it.
- The locations assumed for the installed Python module
  (`lib/python3.X/site-packages` under the prefix) and for the C++ headers
  (`include/csound/CsoundAC`) are taken from the CMake files and the macOS
  layout, not from a Linux install.
- Build time and memory on the Cloud Agent's default machine.

What was verified here: both scripts pass `bash -n`; `cloud/check.sh`
runs and passes on macOS, and fails as it should when Csound is missing;
the WebAssembly release named above was downloaded and its `CsoundAC.js`
and `CsoundAC.wasm` work with `js/csoundac-node.mjs` under Node.js 26.

After the first real run, `cloud/check.sh` is the test: do not rely on the
cloud environment until it prints "All checks that could run passed" there.

## What is not in this repository

- Csound and CsoundAC sources and binaries.
- `CsoundAC.wasm` (about 33 MB) and `CsoundAC.js`; they come from the
  `csound-wasm` repository's build or releases.
- Samples, SoundFonts and plugins. A composition that needs them must
  record them (`studio accept --dependency samples:NAME`), and the
  environment must provide them; none are needed by the demonstration.
