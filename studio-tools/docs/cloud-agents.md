# Cursor Cloud Agents

A Cursor Cloud Agent runs in an Ubuntu virtual machine in Cursor's cloud. It
clones the repository from GitHub, works on a branch of its own, and pushes
that branch back. It needs neither the MacBook nor any local Cursor
settings, which is what makes it usable from a phone.

**Status.** Nothing in this document has been exercised in a real Cloud
Agent. The studio's tools were built and tested on macOS. The Linux setup
script has not been run (see [dependencies.md](dependencies.md)), and no
cloud environment, repository connection or dashboard setting was created
or changed while building the studio. The steps below are what remains for
the composer to do, or to authorize.

What a Cloud Agent does not have: the MacBook's files, its shell
configuration, its Cursor settings and rules, its Csound and CsoundAC
installations, and anything in a temporary directory from an earlier run.
Everything it needs must come from the repository and from the
environment's build.

## One-time setup

These steps change external accounts and services. They need the
composer's decision and are not done by the studio.

1. **A plan with Cloud Agents.** Cursor's documentation says Cloud Agents
   need a plan that includes them, and asks for a spend limit on first use.
2. **Connect GitHub.** In Cursor (web: `cursor.com/agents`, or the Cloud
   Agents dashboard), connect the GitHub account and give Cursor read and
   write access to `gogins/michael.gogins.studio`. The agent pushes
   branches to that repository.
3. **Put the studio on the default branch.** A Cloud Agent starts from the
   repository on GitHub, and Cursor builds environments from the default
   branch (`master`). Until `feature/ai-composition-studio` is pushed and
   merged, the studio's tools do not exist where the agent can see them.
   See [deployment.md](deployment.md).
4. **Create the environment.** In the Cloud Agents dashboard, create an
   environment for the repository and give it this install script:

   ```sh
   if [ -f studio-tools/cloud/install.sh ]; then bash studio-tools/cloud/install.sh; fi
   ```

   That is all the configuration the studio needs, and it lives in the
   dashboard, not in the repository. Cursor runs the install script when it
   builds the environment, and agents then start from the finished build.
   The script must be safe to run repeatedly; `install.sh` is written to be.

   Alternatively, the same setting can be committed as
   `.cursor/environment.json`; a template is in
   `studio-tools/cursor/environment.json.template`. That file is outside
   the studio's directories and is not installed without approval; see
   [`cursor/INSTALL.md`](../cursor/INSTALL.md).
5. **Check the first build.** Start an agent and ask it to run

   ```sh
   studio-tools/cloud/check.sh --tests
   ```

   and to report the output exactly. Expect the first run of `install.sh`
   to need corrections. Until `check.sh` passes there, do not treat the
   cloud environment as working.
6. **Agent instructions.** A Cloud Agent reads `AGENTS.md` from the
   repository. A template that points to
   `studio-tools/AGENT_INSTRUCTIONS.md` is in `studio-tools/cursor/`.
   Until it is installed, begin each prompt with: "Follow
   studio-tools/AGENT_INSTRUCTIONS.md."

No secrets are needed: the repositories and the WebAssembly release that
`install.sh` fetches are public. If a composition ever needs a private
resource, put the credential in the Cloud Agents dashboard's Secrets, never
in a file in the repository.

## How the environment persists

- The build keeps what `install.sh` wrote to disk: the packages, and
  `~/.local/ai-composition-studio` with Csound, CsoundAC, the WebAssembly
  files and `studio-env.sh`.
- It does not keep exported shell variables or running processes. For that
  reason the `studio` command loads `studio-env.sh` itself, every time.
- A run's working files are not kept after the run. Whatever matters must be
  committed and pushed: the composition, its versions, its audio. Staging
  directories and `/tmp` are scratch space.

## Working from a phone

1. Open `cursor.com/agents` in Chrome on Android (it can be installed as an
   app from Chrome's menu). Choose the repository.
2. Prompt, for example:

   > Follow studio-tools/AGENT_INSTRUCTIONS.md. Revise the composition
   > `nu`: make the bells sparser in the second half and end on D. Render
   > it, accept it as a new version, publish it, commit, and report the
   > levels.

3. The agent stages a revision, saves the prompt, edits or regenerates the
   `.csd`, renders, accepts, publishes, commits, and pushes its branch.
4. Review the result. The agent's branch is on GitHub, where the `.csd`,
   the prompts and the MP3 can be opened from the phone. The viewer at
   `/studio/` shows a version only after its branch has been merged into the
   branch that GitHub Pages deploys.

How merging and deployment are to be authorized is an open decision; see
[deployment.md](deployment.md).

## Several agents, several branches

Each Cloud Agent works on its own branch. Agents that run one after
another, each starting from a branch that contains the previous one's
work, simply add versions in sequence and merge without conflict.

Two agents that revise **the same composition** from the same starting
point both create the next version number. Their branches cannot be merged
by Git: the current files differ, and the same `version-NNNN` would mean
two different things. Do not resolve such a conflict by hand and do not
pick one side with a merge strategy; either loses a version. Use
`studio adopt`, which re-applies the other branch's revisions on top of
this one, so that every version of both branches is kept under its own
number.

On the branch that is to receive the work (here called the trunk), with
the other branch named `agent-b` and the composition `nu`:

```sh
# 1. Start the merge. Git reports conflicts in music/ai-compositions/nu.
git merge --no-commit --no-ff agent-b

# 2. Keep the trunk's copy of the composition, exactly.
git restore --source=HEAD --staged --worktree -- music/ai-compositions/nu
studio verify nu

# 3. Put the other branch's copy somewhere outside the repository.
other=$(mktemp -d)
git archive agent-b music/ai-compositions/nu | tar -x -C "$other"

# 4. See which of its versions are missing here.
studio adopt nu --from "$other/music/ai-compositions/nu" --list

# 5. Adopt each missing version, oldest first, the other copy's current last.
S=$(studio adopt nu --from "$other/music/ai-compositions/nu" --version 3)
studio render "$S" && studio accept "$S"
S=$(studio adopt nu --from "$other/music/ai-compositions/nu")
studio render "$S" && studio accept "$S"

# 6. Check, and conclude the merge.
studio verify nu
git add -- music/ai-compositions
git commit
```

Each adoption is an ordinary revision: the version current on the trunk is
archived under the next number, and the adopted `.csd`, code and prompts
become current. The adopted prompts keep their text and recording time and
get the next prompt numbers; the manifest records `adopted_from` and keeps
the model and agent that made the revision. `adopt` refuses to skip a
version (its prompts would be lost), refuses a copy that does not match its
own manifest, and refuses a version that is already here.

After adoption the order of versions is: the trunk's, then the other
branch's. If the musical intention was to combine the two revisions, that
is a further prompt and a further version.

This procedure is run by `tests/test_branches.py` in a temporary Git
repository, including the failing merge.

**Published files.** `docs/compositions/catalog.json` and
`.publish-state.json` are rewritten whenever anything is published, so two
branches that both publish will conflict there as well. The simplest rule
is that agents on side branches do not publish; publish on the trunk after
integrating. If both did publish, keep the trunk's `docs/compositions`
(`git restore --source=HEAD --staged --worktree -- docs/compositions`),
then run `studio publish` again for the compositions concerned.

**The same working tree.** Two agents or two terminals working in one
checkout are protected differently: each `accept` takes the composition's
lock, and a staged revision that is no longer based on the current version
is refused with a conflict and nothing changed. Stage a fresh revision and
carry the changes over.

**Worktrees.** With `git worktree`, each worktree has its own
`music/ai-compositions` and its own staging directory, so worktrees behave
like branches: integrate them with `studio adopt`, using the other
worktree's composition directory directly as `--from`.

## Desktop and cloud together

The archive is the same on both. On the MacBook, pull before starting a
revision, and push (when authorized) after accepting one, so that a Cloud
Agent started afterwards begins from the latest version. Manifests record
which machine, Csound and CsoundAC produced each version;
`studio rerender` on the other machine shows whether a render reproduces
there. Bit-identical audio across macOS and Linux is not guaranteed and has
not been tested.
