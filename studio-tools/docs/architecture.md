# Architecture

## Purpose

The studio is a way of composing by conversation. The composer prompts in
Cursor, on the desktop or in a Cloud Agent. That writes or revises a
complete Csound piece, renders it, accepts it into the archive, and
publishes it. The page at `docs/studio/` only plays, compares, and seeks.
It does not take a prompt. The composer listens there and prompts again
in Cursor. It is not a digital audio workstation.

## Parts

```
  composer ──► Cursor (desktop or Cloud Agent)
     ▲              │  "Create a composition named …"  →  studio new
     │              │  "Revise …"                       →  studio stage
     │              ▼
     │         music/ai-compositions/            the archive (authoritative)
     │           nu/  nu.csd, prompts, manifest, code/, outputs/
     │           nu/version-0001/ ...             earlier versions, never changed
     │                      │  studio publish (copies; never changes the archive)
     │                      ▼
     │         docs/compositions/                published copies + catalog.json
     │                      │  git push to master
     │                      ▼
     └──────── docs/studio/                      plays the .csd in the browser
```

1. **Archive**, `music/ai-compositions/`. One directory per composition. The
   files directly in it are the current version; `version-NNNN/` directories
   hold the earlier ones. This is the only authoritative copy.
2. **Tools**, `studio-tools/`. A Python package (`studio/`, standard library
   only) behind one command, `bin/studio`. It creates versions, renders,
   verifies, recovers and publishes. Also here: tests, documentation,
   Cursor templates, and the cloud setup scripts.
3. **Published files**, `docs/compositions/`. Copies of selected files from
   the archive, at the same relative paths, plus `catalog.json`, which the
   tools generate by scanning this directory.
4. **Player**, `docs/studio/`. Plain HTML, CSS and native JavaScript modules.
   It reads `../compositions/catalog.json` and the published files. No build
   step, no framework, no server-side code.

GitHub Pages already publishes `docs/`. The studio adds two directories
beneath it and changes nothing else, so after approval and deployment the
player and the published compositions appear at `/studio/` and
`/compositions/` of the site. All URLs in the player and the catalog are
relative, so they work under any site address and from `studio serve`.

## Design decisions

- **The `.csd` is the composition.** There is no intermediate score format.
  Programs that generate the `.csd` are kept as artifacts beside it, and the
  manifest records the commands and seeds that produced it.
- **The filesystem is the version store.** Versions are ordinary directories
  with readable names, usable without Git. Git records the archive's
  history as a whole, but the studio does not depend on it.
- **Manifests are provenance only.** `<name>-manifest.json` describes how a
  version was made and what it contains. It does not drive anything
  musical; deleting it would lose the record, not the music.
- **Nothing becomes current unrendered.** Work happens in a staging
  directory. `studio accept` refuses a staging area whose last render failed
  or is out of date with respect to the `.csd` and code.
- **Promotion is a transaction.** The previous version is archived and the
  new one put in place under a lock, with a journal, so that an
  interruption can always be completed or undone. See
  [versioning.md](versioning.md).
- **The archive and the published copies are separate.** Publishing copies
  selected files and never writes to the archive. What has not been
  published is not in `docs/` and is not served.
- **No database.** The catalog is a generated JSON file. The player is
  static. `studio serve` only serves it locally. Creating and revising
  stay in Cursor; the commands are in [cheatsheet.md](cheatsheet.md).

## Tools package

| Module | Responsibility |
| --- | --- |
| `paths.py` | The four authorized directories; refusal of path traversal, absolute paths, hidden segments and symbolic links. |
| `naming.py` | Composition, file and version names; classification of files by kind. |
| `fsutil.py` | Hashing, atomic writes, copying, read-only archival. |
| `environment.py` | Discovery of Csound, `@csound/browser`, Python with CsoundAC, Node.js; versions for the manifest. |
| `staging.py` | Staging areas, verbatim prompts, recorded generator runs, adoption of revisions from another branch. |
| `render.py` | Validation, rendering with Csound, log scanning, encoding, re-rendering of stored versions. |
| `audio.py` | Measurement of WAV files: duration, peak, RMS, clipping, a hash of the samples. |
| `manifest.py` | Building and checking manifests. |
| `versioning.py` | Locks, the promotion transaction, recovery, verification. |
| `publish.py` | Selection, planning, copying, conflict detection, pruning. |
| `catalog.py` | `catalog.json`, generated from the published files. |
| `serve.py` | A local static server with byte-range support, so the page can be played on this machine. |
| `cli.py` | The `studio` command. |

## Player modules

| File | Responsibility |
| --- | --- |
| `index.html` | The page; loads `studio.js` as a module. |
| `studio.js` | Library, composition and version views; tabs for prompts, Csound, code, files, comparison and provenance; routing by URL fragment (`#nu/version-0001/csound`). |
| `player.js` | Performs the published `.csd` in the browser and draws the level meter. |
| `messages.js` | The Csound message dock. |
| `status.js` | The one-line status bar. |
| `cheatsheet.html` | The command list. The Player links to it, and it links back to the Player. Same text as [cheatsheet.md](cheatsheet.md). |
| `catalog.js` | Loading the catalog, resolving published paths, fetching text. |
| `diff.js` | Line comparison of two versions of a text. |
| `dom.js` | Element construction and formatting helpers. All text is inserted as text, never as HTML. |
| `studio.css` | Layout for phone and desktop, light and dark. |
