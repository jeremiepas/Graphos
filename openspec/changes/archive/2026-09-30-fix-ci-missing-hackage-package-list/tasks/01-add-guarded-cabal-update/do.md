# Task 1 — Add guarded `cabal update` to `ci:build` and `ci:release-build` tasks — DO

**Task slug**: `01-add-guarded-cabal-update`
**Attempt**: 1
**Status**: in-progress

## Summary

Implemented the guarded `cabal update` pattern in both `ci:build` and `ci:release-build` devenv tasks in `devenv.nix`. The guard ensures Hackage index is refreshed on clean-slate CI runs while tolerating transient failures on warm-cache runs.

## Detail

### What was implemented

Modified `devenv.nix` lines 42-55:

- **`ci:build`**: Replaced single-line exec with multi-line Nix string containing:
  1. Guarded `cabal update` — fails only if both `cabal update` fails AND no cached index exists
  2. `cabal configure --enable-tests --flag dev -j4 && cabal build all -j4` (preserved original flags)

- **`ci:release-build`**: Same pattern — guarded `cabal update` prepended to original `cabal configure --enable-tests && cabal build all`

### Key decisions

- Used `|| [ -d "$HOME/.cabal/packages/hackage.haskell.org" ]` pattern: if `cabal update` fails but the cache directory exists, the guard succeeds and build proceeds.
- Used Nix multi-line string (`''`) for the exec to allow inline comments and multi-line shell.
- Preserved all original build flags (`--enable-tests --flag dev -j4` for ci:build, plain for ci:release-build).

### Concrete changes

| File | Change |
|------|--------|
| `devenv.nix:42-46` | `ci:build` exec updated with guarded cabal update |
| `devenv.nix:53-57` | `ci:release-build` exec updated with guarded cabal update |
