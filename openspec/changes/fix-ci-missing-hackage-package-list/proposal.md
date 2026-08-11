## Why

CI (`devenv tasks run ci:build`) fails before compilation even starts with `Could not resolve dependencies: unknown package: zip-archive (dependency of graphos)`. The root cause: `ci:build` runs `cabal configure && cabal build` without ever running `cabal update`, so the local Hackage package list (`~/.cabal/packages/hackage.haskell.org/`) is empty in CI and cabal literally doesn't know that `zip-archive` (or any dependency) exists. Any newly-added Hackage dependency would hit the same wall; the build is only "working" locally because a prior `cabal update` populated the index on the dev machine.

## What Changes

- Add a `cabal update` step to the `ci:build` devenv task before `cabal configure`, so the Hackage index is always fresh in CI and on first-time local runs.
- Guard the `cabal update` so transient Hackage/network failures don't fail the build when the index already exists (best-effort in CI, required on clean slate).
- Update the existing `devenv-shell` CI contract to require an up-to-date Hackage index before any `cabal configure`.
- Add a regression scenario asserting that a clean-slate CI run (no `~/.cabal` cache) can resolve `zip-archive` and all other declared dependencies.

## Capabilities

### New Capabilities

_None_ — this is a CI/build-infrastructure fix, not a new product capability.

### Modified Capabilities

- `devenv-shell`: The `ci:build` task SHALL refresh the Hackage package index before running `cabal configure`, so dependency resolution does not depend on a pre-populated `~/.cabal` cache.

## Impact

- **Code**: `devenv.nix` (`tasks.ci:build` exec string) is the only production file changed.
- **CI**: `.github/workflows/haskell.yml` `Build` step (`devenv tasks run ci:build`) now passes on a clean runner with no cached cabal state.
- **Dependencies**: No Haskell dependency changes — `zip-archive` and all other deps stay as declared in `graphos.cabal`. The fix only ensures cabal knows they exist.
- **Performance**: `cabal update` adds ~5-15s to cold CI runs; warm runs with `cachix` and a populated `~/.cabal` are unaffected because the index already exists (the update is idempotent and cheap when unchanged).
- **Compatibility**: No API or behavior change for `graphos` users; this only affects the build pipeline.

## PDCA Cycle

- **Plan**: CI build (`devenv tasks run ci:build`) resolves all `graphos.cabal` dependencies on a clean Ubuntu runner with no pre-existing `~/.cabal` cache. Success = `cabal build all` reaches the compilation phase (no `unknown package` errors) and `zip-archive` resolves to a Hackage version.
- **Do**: Prepend a `cabal update` (best-effort, non-fatal if index exists) to the `ci:build` task in `devenv.nix`; update the `devenv-shell` spec to codify the invariant.
- **Check**: (1) Re-run the failing CI job — confirm `zip-archive` resolves and the build proceeds to GHC compilation. (2) Locally simulate a clean slate (`rm -rf ~/.cabal/packages` inside a nix-shell) and run `devenv tasks run ci:build` — confirm dependencies resolve. (3) `cabal test` still runs after build.
- **Act**: If `cabal update` is flaky in CI, add retry logic or pin the Hackage snapshot via `cabal`'s `index-state` field. Standardize the "cabal update before configure" rule in the `devenv-shell` spec so future task definitions don't regress.