# Using the studio

## On the MacBook (Cursor desktop)

Set up once: see [dependencies.md](dependencies.md). Then, in Cursor with
this repository open, prompt the agent. Begin with "Follow
studio-tools/AGENT_INSTRUCTIONS.md" unless the Cursor rule from
`studio-tools/cursor/` has been installed.

Examples:

> Follow studio-tools/AGENT_INSTRUCTIONS.md. Create a composition named
> `xi`: three minutes, slow bowed-string drones in just intonation with
> occasional plucked figures. Write the score with a Python program using
> CsoundAC. Render it and accept it.

> Follow studio-tools/AGENT_INSTRUCTIONS.md. Revise `xi`: the plucked
> figures are too loud and too regular. Then publish it.

What the agent does, and what you can do yourself in a terminal:

```sh
. studio-tools/config/studio-env.macos.example.sh   # or your own copy

S=$(studio stage xi)                 # a working copy of the current version
studio prompt "$S" --file -          # paste the prompt, end with Control-D
$EDITOR "$S/xi.csd"                  # or edit and run "$S/code/xi-generate.py"
studio run "$S" --seed 3 -- python code/xi-generate.py
studio render "$S"                   # prints duration and levels; checks @csound/browser
studio accept "$S" --model "manual edit" --agent "Michael Gogins"
studio publish xi
studio serve                         # listen: Play performs the .csd in the browser
```

Looking at what exists:

```sh
studio list                          # compositions, versions, staging areas
studio show xi                       # provenance of the current version
studio show xi --version 1
studio verify                        # everything in the archive
studio rerender xi --version 1       # does it still render the same?
```

Abandoning work: `studio discard "$S"`. The composition is unaffected.

## The viewer, locally

```sh
studio publish xi                    # copy to docs/compositions/, update the catalog
studio serve                         # http://127.0.0.1:8000/studio/
```

`studio serve` is a small static server for `docs/`. Any static server that
serves `docs/` will do. The viewer performs each published `.csd` with the
official Csound 7 for WebAssembly (`@csound/browser`) in this browser; the
same player is used on the deployed site.

To use the local viewer from the phone on the same network:

```sh
studio serve --host 0.0.0.0 --port 8000
```

and open `http://<the MacBook's address>:8000/studio/` on the phone. This
exposes `docs/` to the local network while it runs.

In the viewer:

- The first page lists the published compositions.
- A composition's page has a version selector (and Older and Newer
  buttons), a real-time Csound player with seeking, and tabs:
  **Prompts** (all prompts up to that version; the new ones are marked),
  **Csound** (the `.csd`, with a download link),
  **Code** (the generating programs),
  **Files** (everything published for the version, with sizes),
  **Compare** (line differences of the `.csd` or code against another
  version, with a second player for the other version),
  **Info** (provenance from the manifest).
- The address records the composition, version and tab
  (`.../studio/#nu/version-0001/csound`), so a view can be bookmarked or
  shared.

## From Android (Cursor Cloud Agents)

Once the cloud environment exists (see [cloud-agents.md](cloud-agents.md)):

1. In Chrome, open `cursor.com/agents`, choose the repository, and prompt
   as on the desktop.
2. When the agent reports that it has accepted and pushed a version, look
   at its branch on GitHub, or, after the branch has been merged and
   deployed, open the viewer at the site's `/studio/` address.
3. Prompt again. Each accepted prompt becomes a new version; the previous
   ones stay available in the viewer's version selector.

The phone needs nothing but a browser. The MacBook can be off.

Until the cloud environment has been set up and checked, the phone can be
used with the MacBook as the machine that does the work: the viewer served
with `studio serve --host 0.0.0.0` is the listening side; prompting still
happens in Cursor on the MacBook.

## Listening levels and length

`studio render` prints the duration, the peak and the RMS level. It refuses
silence and clipping, and checks that the piece performs in the viewer's
Csound engine. A peak around -6 to -3 dBFS leaves headroom when that engine
plays the piece in the browser.

No soundfile is kept in the archive or published. To keep a one-off WAV
outside the repository, use `studio render --wav ~/Desktop/nu.wav` or
`studio rerender nu --wav ~/Desktop/nu.wav`.
