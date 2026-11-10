# Contributing to Graphos

Thanks for your interest in contributing to Graphos! This document covers the
practicalities: development environment, conventions, and how to get your work
reviewed and merged.

## Ways to contribute

- **Bug reports** — open an issue using the bug report form; include reproduction steps and your environment.
- **Feature ideas** — open an issue using the feature request form, or start a [discussion](https://github.com/jeremiepas/Graphos/discussions) first.
- **Code, docs, and specs** — everything below applies.

## Development environment

Graphos is a Haskell project built with **cabal**, wrapped in a [devenv](https://devenv.sh)
Nix shell. GHC 9.10, cabal-install, and haskell-language-server are pinned via
`devenv.nix`.

```bash
git clone https://github.com/jeremiepas/Graphos.git
cd Graphos

# Enter the development shell (requires Nix)
devenv shell

# Build
cabal build

# Run the test suite
cabal test

# Build and install the CLI locally
cabal install graphos

# Format check (apply with `apply-refactor -i` if needed)
cabal run hlint src
```

If you don't use Nix, a plain cabal setup with GHC 9.10.x also works:

```bash
cabal build
cabal test
```

### Runtime notes

- Extraction talks to language servers via LSP. For end-to-end manual testing,
  install the language server for a language you're working with (see
  [README](README.md#language-server-requirements)).
- Some pipeline stages call an LLM (docs/papers/images extraction, inference).
  Those need the relevant API keys; unit tests do not.

## Project layout

```
src/Graphos/
├── Domain/          -- Pure types and logic, no IO
├── UseCase/          -- Orchestration, still pure
└── Infrastructure/   -- IO boundary, all side effects here
```

Respect the dependency direction: `Infrastructure` may depend on `UseCase` and
`Domain`; `UseCase` may depend on `Domain`; `Domain` depends on nothing
project-internal.

## Commit convention

Prefix commits with the tracking issue id, matching the existing history:

```
AVI-1072: add SECURITY.md and community files
```

If a change has no tracking issue, use a short conventional-commit style
subject instead (e.g. `fix(ci): make build offline-first`).

## Spec-affecting changes

Behavioural changes go through [OpenSpec](openspec/):

1. Create a change folder under `openspec/changes/<change-name>/` with
   `proposal.md`, design notes, and delta specs.
2. Track implementation progress in `tasks.md`.
3. Spec conformance is enforced in review — a PR that changes behaviour without
   a corresponding spec change will be asked to add one.

Pure docs, CI, and tooling changes don't need an OpenSpec change.

## Pull requests

- Keep PRs **small and focused** — one logical change each.
- Add or update **tests** for any behaviour change; pure refactorings should
   keep the existing suite green.
- Add a `CHANGELOG.md` entry for user-visible changes.
- For behavioural changes, include or reference the OpenSpec change (see above).
- Describe **what** and **why** in the PR description; the template has a
  checklist.

## Release process

Graphos follows a lightweight **GitFlow** model. A change flows from a tracking
issue through a short-lived feature branch, into an integration branch, and
finally onto `main`, where an annotated `vX.Y.Z` tag materializes the release.

### Branch model

```text
                          ┌─► main ──► vX.Y.Z   (annotated tag = release)
feature/AVI-XXX  ─────────┤
                          └── fix/AVI-XXX (hotfix)
```

| Branch | Purpose | Lifecycle |
| --- | --- | --- |
| `main` | Production. Only release merges and hotfixes land here. | long-lived |
| `develop` | Integration. Feature branches land here first for green CI before reaching `main`. | long-lived |
| `feature/AVI-XXX` | One per OpenSpec change or task, branched from `develop` (or `main`). Merged back with a reviewed PR and green CI. | short-lived |
| `fix/AVI-XXX` | Hotfix, branched directly from `main`. See [Hotfixes](#hotfixes). | short-lived |

Branch names follow `<type>/AVI-<issue-number>-<slug>` (e.g.
`feature/AVI-974-review-cosmetics`).

> _Note:_ `develop` is the canonical integration branch in this model but is not
> yet created on this repository. Adopting it — or any branch-model change — is
> an architecture decision owned by head-of-dev. Until then, feature branches
> are opened from `main`.

### From issue to `main`

1. Open `feature/AVI-XXX` from `develop` (or `main`) and implement the work.
2. Open a PR `feature/AVI-XXX` → `develop`; it must pass green CI (see
   [Controls](#controls--real-enforcement-is-review-plus-green-ci)).
3. After review, the merge-gatekeeper merges it into `develop`.
4. When a set of changes is ready to ship, a reviewed PR merges `develop` →
   `main`.
5. Tag `main` at that commit as `vX.Y.Z` (see [Versioning](#versioning-semver))
   to create the release.

### Versioning (SemVer)

Versions follow [Semantic Versioning](https://semver.org): `MAJOR.MINOR.PATCH`.

- **PATCH** — bug fixes, documentation, and dependency/tooling churn that does
  not change observable behaviour.
- **MINOR** — new, backwards-compatible functionality.
- **MAJOR** — backwards-incompatible changes.

The canonical version lives in `graphos.cabal` (`version:` field); keep it in
sync with the tag you create, and add a `## X.Y.Z` entry to
[`CHANGELOG.md`](CHANGELOG.md) before tagging. Pre-release candidates use an
`-rc.N` suffix (e.g. `v0.1.2-rc.1`) and publish as pre-releases; stable tags
(`v0.1.2`) publish as full releases.

### Creating a release — the tag materializes the release

A release is marked by an **annotated tag** on `main`. Pushing the tag runs the
release workflow (`.github/workflows/release.yml`), which builds and tests the
tagged sources and publishes a **GitHub Release** (static Linux binary plus a
SHA256 checksum) that references `CHANGELOG.md`:

```bash
# from a clean, up-to-date main
git checkout main
git pull origin main

# tag the release commit (version from cabal + CHANGELOG)
git tag -a v1.2.3 -m "Graphos v1.2.3"
git push origin v1.2.3
```

The tag is what materializes the release; the workflow only builds, tests, and
publishes the GitHub Release assets — it does **not** deploy to any environment.

### Hotfixes

For a release-critical fix, branch a hotfix straight from `main`:

```bash
git checkout main
git checkout -b fix/AVI-XXX          # hotfix from main
# ... fix, build, test, open a reviewed PR back to main ...
git push -u origin fix/AVI-XXX
```

Ship it as a **PATCH** (`vX.Y.(Z+1)`) via the steps above, then cherry-pick the
fix onto `develop` so it is included in the next minor release.

### Controls — real enforcement is review plus green CI

GitHub **branch protection is not available** on this private repository
(GitHub free plan), so branch rules cannot enforce this model. Control is
instead provided by:

- the **merge-gatekeeper**, who reviews and merges every `develop` → `main` and
  hotfix PR, and
- **green CI** on every feature PR (`.github/workflows/haskell.yml`).

### Deployment target — deferred to the first release

The deployment destination (for example a VPS hub versus a homelab/ArgoCD
target) is a decision to be made at the **first release**, under CTO review.
Nothing is deployed automatically: the release workflow only publishes the
GitHub Release assets and stops.

## Issue triage expectations

When you open an issue:

- Use the provided forms (bug report / feature request).
- Bug reports: include the `graphos` version, OS, the language/LSP server
  involved, and a minimal reproduction.
- Feature requests: describe the problem before the solution — alternatives
  you've considered matter.

## Security

Do **not** open public issues for security problems — see
[SECURITY.md](SECURITY.md) for reporting channels.