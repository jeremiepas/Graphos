# mnemo in Graphos

The project's memory lives in the **`graphos`** space of the mnemo memory graph
(`http://localhost:7477`), shared with every other space and told apart by the
`space` property. The configuration is `~/.config/mnemo/graphos.yaml`; the
wrapper is `mnemo-graphos` (console on `127.0.0.1:7481`).

```bash
mnemo-graphos status
mnemo-graphos doctor
mnemo-graphos web
```

## `openspec-release`

`workflows/openspec-release.yaml` is the proposal-to-release workflow of this
repository. It registers an OpenSpec change (or reuses a complete one), has a
person accept the proposal, creates the worktree in `.worktrees/<change>` on
`openspec/<change>`, opens the draft pull request with the proposal, develops
and commits it atomically, reviews it on GitHub, gates on the review and green
CI, archives the change, squash-merges into `main`, bumps the `version:` line of
`graphos.cabal`, tags `v<version>` and lets `.github/workflows/release.yml`
publish the GitHub release, then removes the worktree and the branch.

Run it from this checkout, inside `devenv shell`, with the space configuration:

```bash
export MNEMO_CONFIG=$HOME/.config/mnemo/graphos.yaml
mnemo workflow import .mnemo/workflows/openspec-release.yaml
mnemo run openspec-release --param change=my-change --param idea='what to build'
```

The run stops at `accept-proposal` and at `approve-merge` (exit 3); approve each
with `mnemo approve RUN STEP --by NAME` and resume with `mnemo run --resume RUN`.
To start from an existing proposal, pass a change whose `openspec status` is
`4/4`; the proposal step leaves it untouched and you approve the gate once.

The mechanical work is in `scripts/mnemo/openspec-release.sh`; the delegate
target `opencode-deepseek` runs `scripts/mnemo/agent.sh`, which enters the
worktree recorded by the workflow and then runs `opencode run`. The model is the
one `~/.config/mnemo/graphos.yaml` names (`litellm/montech/Ornith-1.5-9B-Q8_0`
by default; override with `MNEMO_GRAPHOS_MODEL`).

### Conventions of this port

- **Worktree and branch.** `.worktrees/<change>` on `openspec/<change>`. The
  branch naming rule of `AGENTS.md` (`<type>/AVI-<issue>-<slug>`) applies to
  issue-scoped work; one OpenSpec change can cover several issues (for example
  `lsp-project-session` covers #293, #294 and #295), so the workflow names its
  branch after the change. No force-push, ever.
- **Checks.** `check` and `gate` run what CI runs for Haskell:
  `cabal build --flag dev`, `cabal test`, and
  `openspec validate <change> --strict`. The GitHub checks are the ones of
  `.github/workflows/haskell.yml`.
- **Release.** The version is the `version:` line of `graphos.cabal`, bumped on
  the patch component (`0.3.0.0` → tag `v0.3.0`); the tag is `v<version>`. The
  workflow pushes the tag and `verify-release` waits for
  `.github/workflows/release.yml` to publish the release.
- **Merge.** The workflow's GitHub account authors the pull request, and GitHub
  refuses a self-approval, so the squash merge falls back to `--admin` when
  branch protection asks for a review. The human gate at `approve-merge` is the
  review of record; the automated `review` step still posts its status comment,
  which `review-gate` requires.
- **Identity.** `prepare` sets a repository-local `git user.name` (taken from the
  last commit) when the checkout has none, so the commits of the run do not
  fail.
