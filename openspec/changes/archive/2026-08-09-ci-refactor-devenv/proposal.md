## Why

The current GitHub CI workflows (`haskell.yml`, `release.yml`) manually replicate what `devenv.nix` already declares: GHC 9.10, cabal, system dependencies, and build steps. This duplication causes drift — the CI uses `haskell-actions/setup@v2` with env vars while devenv uses `pkgs.haskell.packages.ghc910`, leading to version mismatches and duplicated maintenance. Every change to `devenv.nix` must be manually mirrored in YAML.

devenv provides a `tasks` system and `devenv tasks run`/`devenv test` commands that allow the same task definitions to run both locally and in CI. The `cloud.ci.github` config option exposes GitHub context (branch, refs) to task logic, enabling branch-conditional behavior. By defining build/test/release tasks in `devenv.nix` and running them via `devenv tasks run` in GitHub Actions, we eliminate duplication and ensure CI always matches the dev environment.

## What Changes

1. Add `tasks` to `devenv.nix` for all CI operations: `ci:build`, `ci:test`, `ci:haddock`, `ci:release-build`, `ci:release-test`
2. Refactor `.github/workflows/haskell.yml` to install Nix + devenv and run `devenv tasks run ci:test` instead of manual cabal steps
3. Refactor `.github/workflows/release.yml` to use `devenv tasks run ci:release-test` and `ci:release-build`
4. Add `cachix/cachix-action` for Nix store caching (replaces manual cabal cache)
5. Configure `cloud.ci.github` in `devenv.nix` for branch-aware task logic
6. Remove redundant `haskell-actions/setup` steps — devenv provides GHC and cabal

## Capabilities

### New Capabilities
- `ci-devenv-tasks`: Task definitions in devenv.nix for build, test, haddock, and release, runnable locally and in CI

### Modified Capabilities
- (none — this is infrastructure, not a product capability)

## Impact

- `devenv.nix`: New `tasks` block and `cloud.ci.github` config
- `.github/workflows/haskell.yml`: Replaced with devenv-based steps
- `.github/workflows/release.yml`: Replaced with devenv-based steps
- Dependencies: Requires `cachix/install-nix-action` and `cachix/cachix-action` (already public GitHub Actions)
- No changes to application code, domain, or use-case layers

## PDCA Cycle

- **Plan**: CI uses the same devenv environment as local dev. Build/test tasks defined once in `devenv.nix`, invoked identically locally (`devenv tasks run ci:test`) and in CI. Target: zero duplication between devenv.nix and CI YAML, CI builds pass on first try.
- **Do**: Add task definitions to devenv.nix, refactor both workflow files to use `devenv tasks run`, add Cachix caching.
- **Check**: CI passes with same GHC/cabal versions as local devenv. `devenv tasks run ci:test` produces same result locally and in CI. Release workflow produces identical binary.
- **Act**: If CI passes, the pattern is standardized — any new CI task is added to `devenv.nix` only. Future changes to GHC version or dependencies need one edit in one file.