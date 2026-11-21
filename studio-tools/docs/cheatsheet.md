# Cheat sheet

[Player](https://gogins.github.io/michael.gogins.studio/studio/)

The Player links back to this sheet at `docs/studio/cheatsheet.html`. Keep
that page the same as the text below.

Create and revise pieces in Cursor. The Player only plays them.

Say the same sentences on the computer (Cursor chat) and on the phone
(`cursor.com/agents`, this repository). Begin with
`Follow studio-tools/AGENT_INSTRUCTIONS.md` unless that file is already
installed as the Cursor rule.

## Say this

New piece:

> Follow studio-tools/AGENT_INSTRUCTIONS.md. Create a composition named
> `xi`: …. Render, accept, publish, and push to `master`.

Next version of a piece that already exists:

> Follow studio-tools/AGENT_INSTRUCTIONS.md. Revise `xi`: …. Render,
> accept, publish, and push to `master`.

Create a composition named NAME runs `'studio new NAME'`. That command stops
if the name already exists. Revise NAME runs `'studio stage NAME'`. That
command stops if the name does not exist. The name is the context.

On the computer the agent can push from the local checkout. On the phone
the cloud agent clones the repository, runs the same commands, and pushes
to `master`. The page plays the version after GitHub Pages rebuilds.

## Speak

Click the microphone in the Cursor chat, or press Cmd+Shift+Space. Speak
the sentence, read the text, then send it. In the Agents window, hold
Ctrl+M while you talk. On the phone, use the microphone in the prompt box
at `cursor.com/agents`.

The Player header link Prompt opens the Cursor app on the computer with
this already in the chat:

[Prompt](cursor://anysphere.cursor-deeplink/prompt?text=Follow%20studio-tools%2FAGENT_INSTRUCTIONS.md.%20)

Finish the sentence and send it. On a phone, Prompt opens
[cursor.com/agents](https://cursor.com/agents). The Cursor app opens that
address when the app is installed.

## Listen

[https://gogins.github.io/michael.gogins.studio/studio/](https://gogins.github.io/michael.gogins.studio/studio/)

On the MacBook, `studio serve` then [http://127.0.0.1:8000/studio/](http://127.0.0.1:8000/studio/).

## Commands

Run these from the repository root, with `studio-tools/bin` on `PATH`.
Replace `NAME` with the piece's name: `studio new MyName`.

`studio new` and `studio stage` print a staging directory. To hand that
directory to the next commands, save it:

```sh
S=$(studio new MyName)
studio prompt "$S" --file request.txt
```

| You want | Command |
| --- | --- |
| See what is installed | `studio env` |
| New piece | `studio new NAME` |
| Next version | `studio stage NAME` |
| Save the prompt, word for word | `studio prompt "$S" --file FILE` |
| Run a generator | `studio run "$S" --seed N -- python code/NAME-generate.py` |
| Check, render, measure | `studio render "$S"` |
| Keep this version | `studio accept "$S" --model "…" --agent "…"` |
| Throw away the staging area | `studio discard "$S"` |
| Copy it to the page | `studio publish NAME` |
| Put it on GitHub | `git push` |
| What exists | `studio list` |
| One version's record | `studio show NAME` |
| Check the archive | `studio verify` |
| Finish a broken accept | `studio recover NAME` |
| Play locally | `studio serve` |
| Take a version from another branch | `studio adopt NAME --from DIR` |
| Remove it from the page | `studio unpublish NAME` |
| Render a stored version again | `studio rerender NAME` |
| Rebuild the catalog | `studio catalog` |

`accept` refuses a staging area that has not rendered successfully.
`publish` never writes soundfiles. `discard` does not touch the current version.
`studio COMMAND --help` prints the flags. Flags worth knowing: `render --wav FILE`
(a soundfile outside the repository), `publish --dry-run`, `recover --rollback`.
