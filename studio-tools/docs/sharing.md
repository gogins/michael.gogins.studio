# Sharing the studio

Someone else uses a starter repository, not a fork of this one. A fork
would copy this catalog, the rest of the site, and the git history of
both. The published starter is
[gogins/ai-composition-studio](https://github.com/gogins/ai-composition-studio).
It has the tools, the player, an empty library, and the Csound patches
from `docs/silencio/patches/`. **Use this template** on that repository
starts a new repository with one commit and none of this history.

```sh
studio export-template ~/ai-composition-studio
```

The destination must be outside this repository and must not already
exist. The command copies:

| From here | In the starter |
| --- | --- |
| `studio-tools/` | `studio-tools/` |
| `docs/studio/` | `docs/studio/` |
| `docs/silencio/patches/` | `patches/` |
| `studio-tools/template/` | `README.md`, `docs/index.html`, `.gitignore` |
| `studio-tools/cursor/` | `AGENTS.md`, `.cursor/environment.json`, `.cursor/rules/` |

It writes an empty `music/ai-compositions/` and an empty
`docs/compositions/catalog.json`. The player header says Home. The
personal player address in the copied manual points at the starter's own
player.

Give the new directory its own git history and publish it as a GitHub
template repository, so **Use this template** starts a repository with one
commit and none of this history. Enable GitHub Pages for the `docs/`
folder. Each person then creates a Cloud Agent for their own repository,
as `studio-tools/docs/cloud-agents.md` describes. A computer with Cursor
and Csound does not need one.

Regenerate the starter from this repository after the tools, the player,
or the patches change, and commit the result in the starter's repository.
Do not copy `music/ai-compositions/` or `docs/compositions/` into it.
