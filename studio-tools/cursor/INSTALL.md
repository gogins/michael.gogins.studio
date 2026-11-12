# Cursor configuration templates

Cursor looks for its configuration in places outside the studio's four
directories: `AGENTS.md` at the repository root, and files under `.cursor/`.
The studio does not create or change anything there. This directory holds
templates, and this file says how to install them. **Installing any of them
is the composer's decision; an agent must not do it without explicit
approval.**

None of them is required. Without them, begin each prompt with
"Follow studio-tools/AGENT_INSTRUCTIONS.md", and configure the Cloud Agent
environment in the dashboard as described in
[`../docs/cloud-agents.md`](../docs/cloud-agents.md).

| Template | Installed as | Effect |
| --- | --- | --- |
| `AGENTS.md.template` | `AGENTS.md` (repository root) | Cursor desktop and Cloud Agents read it; it points them to the studio's instructions when a task concerns the studio. |
| `ai-composition-studio.mdc.template` | `.cursor/rules/ai-composition-studio.mdc` | A Cursor rule that applies the studio's instructions when files under the studio's directories are involved. |
| `environment.json.template` | `.cursor/environment.json` | Tells a Cloud Agent environment to run the studio's install script. |

The repository had no `AGENTS.md`, no `.cursor/` and no `.github/` when the
studio was built, so installing these adds files and replaces nothing.
Check again before installing:

```sh
ls -a AGENTS.md .cursor 2>/dev/null
```

If `AGENTS.md` or a rule already exists by then, merge the template's text
into it by hand instead of copying over it.

## Installing

From the repository root, after approval:

```sh
# Instructions for agents (desktop and cloud)
cp studio-tools/cursor/AGENTS.md.template AGENTS.md

# Cursor rule (desktop and cloud)
mkdir -p .cursor/rules
cp studio-tools/cursor/ai-composition-studio.mdc.template .cursor/rules/ai-composition-studio.mdc

# Cloud Agent environment, only if it is to be kept in the repository
# rather than in the dashboard
mkdir -p .cursor
cp studio-tools/cursor/environment.json.template .cursor/environment.json

git add AGENTS.md .cursor
git commit -m "Cursor configuration for the AI composition studio"
```

Notes:

- `AGENTS.md` and the rule only take effect for Cloud Agents once they are
  on the branch the agent starts from.
- According to Cursor's documentation, a saved Cloud Agent environment uses
  `.cursor/environment.json` only if the environment's "environment.json
  location" points at that file; environments created in the dashboard
  start with that set to none. Entering the install command in the
  dashboard has the same effect and needs no file. Environment builds use
  the configuration on the repository's default branch.
- After installing, the repository test
  `test_no_root_level_agent_or_workflow_configuration` in
  `studio-tools/tests/test_repository.py` will report these files as added
  relative to the base branch. That is its purpose; once the files are on
  the base branch it passes again.

## Removing

```sh
git rm AGENTS.md .cursor/rules/ai-composition-studio.mdc .cursor/environment.json
```

## What the templates do not contain

No secrets, tokens or account names. A Cloud Agent's access to GitHub is
granted in Cursor's dashboard, and any credentials belong in the
dashboard's Secrets.
