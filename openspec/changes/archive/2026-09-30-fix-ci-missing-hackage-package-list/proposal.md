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