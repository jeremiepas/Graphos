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