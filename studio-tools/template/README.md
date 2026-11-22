# AI composition studio

A Csound studio for composing with Cursor. A piece is one `.csd` file. The
tools keep every version. The player performs the published `.csd` in the
browser. No soundfile is stored.

This repository starts empty of pieces. It includes the instrument and
effect definitions in `patches/`.

## What you need

- GitHub, and GitHub Pages serving the `docs/` folder of this repository.
- Python 3.9 or later.
- Csound 7, to render on your computer.
- Node.js, so a render can check that the piece plays in the browser engine.
- Cursor. Composing on a computer uses the desktop app. Composing on a
  phone uses your own Cloud Agent, set up once for this repository.

`studio-tools/bin/studio env` reports what is installed.

## Pieces

From the repository root:

```sh
export PATH="$PWD/studio-tools/bin:$PATH"
studio env
```

In Cursor:

> Follow studio-tools/AGENT_INSTRUCTIONS.md. Create a composition named
> `xi`: …. Render, accept, publish, and push to `master`.

Listen with `studio serve` at <http://127.0.0.1:8000/studio/>, or on your
GitHub Pages site at `/studio/` after that commit is on `master`.

The sentences and commands are in `docs/studio/cheatsheet.html`. The manual
is `studio-tools/README.md`.

An orchestra may `#include` a file from `patches/`. Those files are a
shared library. The notes in `patches/README.md` describe how they connect.

## Your own Cloud Agent

A phone does not render. Cursor runs that work in the cloud, in an agent
on your account, against this repository. Someone else's agent cannot see
this repository.

Once:

1. Use a Cursor plan that includes Cloud Agents.
2. At `cursor.com/agents`, connect GitHub and give Cursor access to this
   repository.
3. In the Cloud Agents dashboard, create an environment for this
   repository. Set its configuration to `.cursor/environment.json`, or
   paste this install command:

   ```sh
   if [ -f studio-tools/cloud/install.sh ]; then bash studio-tools/cloud/install.sh; fi
   ```

4. Start an agent and ask it to run `studio-tools/cloud/check.sh --tests`
   and report the output. The environment is ready when that passes.

After that, open `cursor.com/agents` on the phone, choose this repository,
and send the same kind of prompt. The player updates after the result is
on `master` and GitHub Pages rebuilds.

Details are in `studio-tools/docs/cloud-agents.md`. The Linux install
script has to be confirmed on the first build.
