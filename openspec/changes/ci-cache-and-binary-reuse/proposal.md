## Why

The CI workflows re-derive everything from scratch on every run:

- `haskell.yml` caches only the nix/devenv toolchain (Cachix `devenv`);
  the **cabal store** — every Hackage dependency plus the graphos library —
  is rebuilt from zero each push, then the built binary is **discarded**.
- `graphos-analyze.yml` (the repo-analyzer dispatch) wants a prebuilt
  binary, but the repo has **no GitHub releases**, so every dispatch falls
  back to a **full source build per matrix job** — the same multi-minute
  devenv/cabal build repeated once per analyzed repository.

A binary is built on every push to `main` and then thrown away, while the
analyzer rebuilds it over and over. Caching the cabal store + saving the
binary as a reusable artifact closes both gaps with no behavior change to
the produced graph or the read-only analyzer contract.

## What Changes

- **Cabal store caching in CI:** `haskell.yml` caches the cabal store and
  `dist-newstyle` (via `actions/cache`) keyed on the cabal file hashes,
  restored across builds. The Cachix `devenv` cache continues to cover the
  nix/GHC toolchain.
- **CI binary artifact:** after the build, `haskell.yml` saves the
  `graphos` executable as a workflow artifact (`graphos-bin`) with a short
  retention, so recent successful builds' binaries are fetchable.
- **Analyzer reuses the binary:** `graphos-analyze.yml` tries, in order:
  (1) the `graphos_version` release asset when a release with the binary
  exists, (2) the most recent successful `main` run's `graphos-bin`
  artifact, (3) source build via devenv (unchanged fallback). With (1) or
  (2) hit, the nix/devenv install steps are skipped entirely for that job.
- **New devenv task `ci:bin`** exporting the built binary path (repo
  convention: CI operations are devenv tasks), used by both workflows to
  locate the executable without duplicating `cabal list-bin` logic in YAML.

## Capabilities

### New Capabilities

- `ci-binary-reuse`: The CI contract for cabal build caching, the binary
  workflow artifact, and the analyzer's binary acquisition order (release
  asset → CI artifact → source build), including when the nix/devenv setup
  is skipped and the fallback guarantees.

### Modified Capabilities

(none — no existing main spec covers CI workflows; `devenv-shell` gains
  no new requirement because the added `ci:bin` task is a CI implementation
  detail whose behavior is covered by `ci-binary-reuse`.)

## Impact

- `.github/workflows/haskell.yml` — add `actions/cache` for the cabal
  store + `dist-newstyle`; add `graphos-bin` artifact upload after build.
- `.github/workflows/graphos-analyze.yml` — download-first acquisition:
  release asset, then latest successful main artifact via the GitHub API
  (`gh api` + `gh run download`), then the existing devenv source-build
  fallback; nix/devenv setup becomes conditional.
- `devenv.nix` — add `ci:bin` task printing the binary path.
- No code, dependency, or graph-format changes; read-only analyzer
  contract (`graphos-analyze` outputs) unchanged.
- Permissions: `haskell.yml` keeps `contents: read` (artifact upload needs
  none); `graphos-analyze.yml` gains `actions: read` to list workflow runs
  when resolving the latest artifact.