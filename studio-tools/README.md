# AI composition studio

Tools for composing Csound pieces by prompting an AI agent, keeping every
version, and publishing selected versions to a small web viewer.

A composition is a complete Csound `.csd` file. The agent writes it directly
or writes a program (Python, JavaScript or C++, usually with CsoundAC) that
writes it. The studio validates it, renders it with Csound (to a temporary
file that is measured and discarded), keeps the `.csd`, the prompts, the
program and a provenance manifest together, and archives the previous version
under a number before a new one takes its place. No soundfile is stored:
listening is real-time performance of the published `.csd` in the viewer,
with the official Csound 7 for WebAssembly (`@csound/browser`).

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
- The viewer's Csound engine under `docs/studio/csound/` (`@csound/browser`;
  install or update with `studio-tools/js/update-csound-browser.sh`).
- Node.js to check, at render time, that a piece performs in that engine
  (and for optional JavaScript generators).
- Optional: a Python with CsoundAC, the CsoundAC WebAssembly build, a C++
  compiler.

`studio env` reports what is found and what is missing. See
[docs/dependencies.md](docs/dependencies.md).

## Getting started

Listening is the same on every device: the viewer performs the published
`.csd` with official Csound 7 for WebAssembly (`@csound/browser`). No
soundfile is stored. The work is always compose → render → accept →
publish; what changes is whether an agent on your Mac or in the cloud does
it, and where you open the viewer.

Use the branch that has the studio (today: `feature/ai-composition-studio`).
Begin agent prompts with “Follow studio-tools/AGENT_INSTRUCTIONS.md”
unless the Cursor templates under `studio-tools/cursor/` have been
installed (see [cursor/INSTALL.md](cursor/INSTALL.md)).

### Desktop Cursor IDE (MacBook)

1. Open this repository in the Cursor desktop app.
2. Once per machine, put the tools on your `PATH` and load the macOS
   example environment if you use CsoundAC (a terminal in Cursor is fine):

   ```sh
   export PATH="$PWD/studio-tools/bin:$PATH"
   . studio-tools/config/studio-env.macos.example.sh   # optional; or your own copy
   studio env
   ```

3. In the Cursor chat, prompt the agent, for example:

   > Follow studio-tools/AGENT_INSTRUCTIONS.md. Create a composition named
   > `xi`: three minutes, slow bowed-string drones in just intonation.
   > Write the score with Python and CsoundAC. Render, accept, and publish it.

4. Listen: ask the agent to run `studio serve`, or run it yourself, and open
   `http://127.0.0.1:8000/studio/` in a browser on the Mac.

You can drive the same commands in the terminal without the agent
(`studio new` / `stage` / `prompt` / `run` / `render` / `accept` /
`publish`). Nothing enters the archive until `accept`, and `accept`
refuses a staging area that has not rendered successfully. More detail:
[docs/usage.md](docs/usage.md).

### Phone (Chrome on Android)

The phone is for prompting and listening. Rendering runs in a Cursor Cloud
Agent (Ubuntu in Cursor’s cloud); the MacBook can be off.

**One-time cloud setup** (not yet verified on a real agent — full steps in
[docs/cloud-agents.md](docs/cloud-agents.md)):

1. Connect GitHub to Cursor Cloud Agents for this repository.
2. Push this feature branch; for a lasting install, merge to `master`
   (environments build from the default branch — see
   [docs/deployment.md](docs/deployment.md)).
3. In the Cloud Agents dashboard, create an environment whose install
   script is:

   ```sh
   if [ -f studio-tools/cloud/install.sh ]; then bash studio-tools/cloud/install.sh; fi
   ```

4. Start an agent once and ask it to run `studio-tools/cloud/check.sh --tests`
   and report the output. Treat the environment as working only after that
   passes.

**Each session on the phone:**

1. In Chrome, open `cursor.com/agents` (install as an app from Chrome’s
   menu if you like). Choose this repository.
2. Prompt, for example:

   > Follow studio-tools/AGENT_INSTRUCTIONS.md. Revise `nu`: make the bells
   > sparser in the second half. Render, accept, publish, commit, and push.

3. When the agent has pushed, review the branch on GitHub. After that branch
   is merged to the deployed branch, open the site’s `/studio/` address and
   Play — the `.csd` performs in the phone’s browser.

Until the cloud environment works, you can still listen on the phone to a
viewer the Mac is serving (`studio serve --host 0.0.0.0`) while prompting
in desktop Cursor; see [docs/usage.md](docs/usage.md).

### Cloud Agents from desktop Cursor

Same cloud environment as on the phone. In desktop Cursor you can start a
Cloud Agent (instead of a local agent) so the work runs on Ubuntu and
pushes a branch; useful when the Mac should not do the render. Prompting
and the `studio` cycle are the same as above; listening is again the
viewer after publish (local `studio serve` on a machine that has the
branch, or `/studio/` on the site after merge).

### A simple example

Goal: a new twenty-second study named `bells`, then one revision. Do this
in desktop Cursor (local agent) once `studio env` looks right; the same
prompts work on the phone after the cloud environment passes `check.sh`.

**1. Create the first version.** Paste into the Cursor chat:

> Follow studio-tools/AGENT_INSTRUCTIONS.md.
> Create a composition named `bells`, about 20 seconds long: a slow
> bell-like melody in D Dorian over a low drone, with reverberation.
> Write it directly as one complete Csound `.csd` (instruments and notes),
> using the studio p-field conventions. Render it, accept it, publish it,
> and tell me the duration and peak level.

What each part asks for:

| Phrase | Why it is there |
| --- | --- |
| `Follow studio-tools/AGENT_INSTRUCTIONS.md` | Loads the studio rules (staging, naming, no soundfiles, p-fields). |
| `Create a composition named bells` | `studio new bells` — the archive directory name. |
| `about 20 seconds…` | Musical brief; saved verbatim as `bells-prompt-001.md`. |
| `Write it directly as one complete Csound .csd` | No generator program; the agent writes `bells.csd` itself. |
| `studio p-field conventions` | p4 key, p5 velocity, p6–p8 space, etc. (see `docs/csound-conventions.md`). |
| `Render it` | `studio render` — Csound offline measure + `@csound/browser` check. |
| `accept it` | `studio accept` — becomes the current version (ordinal 1). |
| `publish it` | `studio publish bells` — copy into `docs/compositions/` for the viewer. |
| `duration and peak level` | So you can confirm the render without opening files. |

**2. Listen.** In a terminal (or ask the agent):

```sh
studio serve
```

Open `http://127.0.0.1:8000/studio/`, choose **bells**, press **Play**. The
browser runs the published `.csd`; there is no MP3. On the phone, after a
Cloud Agent has published and the branch is on the deployed site, use the
site’s `/studio/` address the same way.

**3. Revise.** A second prompt; the previous version is kept as
`version-0001/`:

> Follow studio-tools/AGENT_INSTRUCTIONS.md.
> Revise `bells`: make the drone a fifth lower and the melody quieter by
> about 3 dB. Render, accept, publish, and summarize what changed.

| Phrase | Why it is there |
| --- | --- |
| `Revise bells` | `studio stage bells` — working copy of the current `.csd` and prompts. |
| `make the drone…` | New instructions; saved as `bells-prompt-002.md` (prompt 1 kept). |
| `Render, accept, publish` | Same pipeline; current becomes version 2, old current → `version-0001/`. |

**4. Compare.** In the viewer, open **bells** → **Compare**, pick version 1,
and play either version (only one plays at a time). The **Prompts** tab
shows both briefs; the new one is marked.

If you prefer the terminal instead of prompting, the same example is:

```sh
S=$(studio new bells --title "Bells")
studio prompt "$S" --file - <<'EOF'
About 20 seconds: slow bell-like melody in D Dorian over a low drone, with
reverberation. One complete .csd, studio p-field conventions.
EOF
# write $S/bells.csd, then:
studio render "$S"
studio accept "$S" --model "manual" --agent "terminal"
studio publish bells
studio serve
```

## Commands

| Command | Purpose |
| --- | --- |
| `studio env [--json] [--require-csoundac]` | Tools found in this environment, and what is missing. |
| `studio new NAME [--title T]` | Start a new composition; prints its staging directory. |
| `studio stage NAME` | Start a revision: a working copy of the current `.csd`, prompts and code. |
| `studio adopt NAME --from DIR [--version N] [--list]` | Stage a revision made in another copy (Git branch) of the composition. |
| `studio prompt STAGING (--text T \| --file F) [--source user\|agent]` | Save a prompt, verbatim, as the next numbered prompt file. |
| `studio run STAGING [--seed N] [--param K=V]... -- PROGRAM ARGS...` | Run a generating program in the staging directory and record it. |
| `studio render STAGING [--wav FILE] [--allow-clipping] [--allow-silence] [--allow-unplayable]` | Validate and render with Csound; measure; check in `@csound/browser`. |
| `studio accept STAGING [--model M] [--agent A] [--title T] [--notes N]` | Archive the current version under the next number; promote the staged one. |
| `studio discard STAGING` | Delete a staging directory. |
| `studio list` | Compositions and staging directories. |
| `studio show NAME [--version N] [--json]` | Provenance of a version. |
| `studio verify [NAME...]` | Check naming, manifests, and that archived versions are unmodified. |
| `studio recover NAME [--rollback] [--break-lock]` | Finish or undo an interrupted `accept`. |
| `studio rerender NAME [--version N] [--regenerate] [--wav FILE]` | Render a stored version again and compare with the recorded render. |
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
