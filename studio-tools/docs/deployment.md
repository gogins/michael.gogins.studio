# Review, approval and deployment

The studio was built on the local branch `feature/ai-composition-studio`,
based on `master`. Nothing has been pushed, merged or deployed, and no
setting of GitHub, GitHub Pages, Cursor or any other service was changed.
Every step in this document that reaches outside the local repository
needs the composer's approval.

## Reviewing

```sh
git switch feature/ai-composition-studio

# Only new files, only in the four directories:
git diff --stat master...feature/ai-composition-studio
git diff --name-status --no-renames master...feature/ai-composition-studio | grep -v '^A' || echo "only additions"
git diff --name-only master...feature/ai-composition-studio | cut -d/ -f1-2 | sort | uniq -c

# The tests, including the checks that nothing else changed:
studio-tools/run-tests.sh

# The archive and the demonstration:
studio-tools/bin/studio verify
studio-tools/bin/studio show nu
studio-tools/bin/studio rerender nu --version 1

# The viewer:
studio-tools/bin/studio serve      # http://127.0.0.1:8000/studio/
```

## Decisions to make before deploying

1. **The site's address.** The specification names
   `https://michael.gogins.studio/studio/` and `.../compositions/`. On
   12 November 2026 the host `michael.gogins.studio` did not resolve in
   DNS, the repository has no `CNAME` file, and `docs/_config.yml` gives
   the address as `https://gogins.github.io/michael.gogins.studio`, which
   is where GitHub Pages was serving the site. With the configuration as
   it is, the viewer would appear at
   `https://gogins.github.io/michael.gogins.studio/studio/`. The viewer
   and the catalog use only relative addresses and work under either.
   Setting up the custom domain is a change to DNS and to the GitHub Pages
   settings, which the studio does not make.
2. **What becomes public.** Merging to `master` publishes everything under
   `docs/compositions/` on the branch: at present the three demonstration
   versions of `nu` (`.csd`, prompts, generating program and manifest; no
   soundfiles). Withdraw anything first with `studio unpublish`. The
   archive under `music/ai-compositions/` is not served by GitHub Pages,
   but it is in the repository, and the repository is public on GitHub;
   "unpublished" means not on the website, not secret.
3. **No soundfiles in Git.** Versions are heard by performing their `.csd`
   in the viewer with `@csound/browser`, locally or on the site. The
   archive and `docs/compositions/` refuse soundfiles. A temporary WAV
   can be written outside them with `studio render --wav` or
   `studio rerender --wav`. The tools still warn about any other file
   above 20 MB and refuse above 95 MB per file unless told otherwise.
4. **The demonstration.** `nu` and its prompts were written by the
   implementing agent to exercise the studio; the prompt files say so.
   Keep it as an example, or remove it before merging:

   ```sh
   studio unpublish nu
   chmod -R u+w music/ai-compositions/nu && rm -r music/ai-compositions/nu
   ```

   The viewer then shows an empty library, and the tests that read the
   real archive accept an empty one.
5. **How Cloud Agent work reaches the site.** A Cloud Agent pushes a
   branch. Something has to merge it into the deployed branch: the
   composer, from the phone, through a pull request, is the conservative
   choice. Letting agents push to `master` directly is faster and removes
   the review. See [cloud-agents.md](cloud-agents.md).
6. **Cursor configuration.** Whether to install `AGENTS.md` and the files
   under `.cursor/` from `studio-tools/cursor/`; see
   [`../cursor/INSTALL.md`](../cursor/INSTALL.md).

## Deploying

After approval, and only then:

```sh
git push -u origin feature/ai-composition-studio
```

Open a pull request into `master` on GitHub and review the same diff
there. When it is merged, GitHub Pages rebuilds from `docs/` as it does
for any other change; no setting has to be touched. Then check, on the
deployed site:

1. `<site>/studio/` loads and lists `nu`.
2. A version plays, and seeking works.
3. The Prompts, Csound, Code and Compare tabs show their content.
4. `<site>/compositions/catalog.json` is served as JSON.
5. The rest of the site is unchanged.

Then, for the Cloud Agents, follow [cloud-agents.md](cloud-agents.md) from
step 4 and run `studio-tools/cloud/check.sh --tests` in the first agent.

## What has not been verified

- **The deployed site.** How GitHub Pages builds and serves the new
  directories has not been observed, since nothing was deployed. One
  observation in its favour: on 12 November 2026 the existing
  `docs/README.md` was served unconverted, as Markdown, at
  `https://gogins.github.io/michael.gogins.studio/README.md`, so published
  prompt files will probably be reachable as they are. The viewer does not
  depend on that; it takes prompt texts from the catalog. Jekyll does not
  publish files whose names begin with a dot, which is intended for
  `.publish-state.json`.
- **Linux and the Cloud Agent environment.** `cloud/install.sh` has not
  been run; see [dependencies.md](dependencies.md).
- **A physical Android phone.** The viewer was tested in a phone-sized
  frame in desktop Chrome and in Cursor's browser, not on a phone.
- **Identical audio across machines.** Re-rendering reproduces the stored
  samples exactly on the MacBook. Whether Csound on Linux produces the
  same samples has not been tested; the manifest records which machine
  rendered each version.

## Known limitations

- CsoundAC's default-constructed `Event` is uninitialized, and JavaScript
  calls must pass every argument; see
  [csound-conventions.md](csound-conventions.md). CsoundAC itself was not
  changed.
- A `.csd` that refers to external files relative to its own location
  resolves them one directory deeper once archived in `version-NNNN/`.
- For `nu` version 3 the manifest's recorded parameters `canon_delay` and
  `canon_steps` describe constants in the generating program; the program
  does not read them from the command line.
- The viewer compares text files line by line. It does not compare audio
  other than by offering both players side by side.
- MIDI, MusicXML and PDF are listed and downloadable when a composition
  publishes them in `outputs/`; the studio does not produce them itself.
