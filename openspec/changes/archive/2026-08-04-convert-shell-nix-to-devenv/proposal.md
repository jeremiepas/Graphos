## Why

The current `shell.nix` uses `builtins.getFlake` with a live `nixpkgs-unstable` reference — no lock file, no reproducibility. Every `nix-shell` invocation can resolve a different nixpkgs snapshot. Devenv provides a `devenv.lock` for pinned, reproducible environments, plus a cleaner declarative module system for language support, scripts, and env vars.

Additionally, the shell hook includes stale PATH entries for `bun` and `npm-global` that are no longer used and should be removed.

## What Changes

- Replace `shell.nix` with `devenv.nix` + `devenv.yaml`
- Pin nixpkgs via `devenv.lock` for reproducibility (rolling `nixpkgs-unstable` channel, locked)
- Use `languages.haskell` module with GHC 9.10, ensuring all Haskell tooling (ghc, cabal, hls, hpack, hspec-discover) comes from the same `ghc910` package set
- Convert `mgconsole` from `writeShellScriptBin` to a devenv `scripts` entry
- Let devenv manage `LD_LIBRARY_PATH` automatically from library packages (zlib, openssl)
- Remove unused `$HOME/.cache/.bun/bin` and `$HOME/.npm-global/bin` PATH entries
- Preserve `EXTRA_LIBRARY_PATH` and `OPENCODE_EXPERIMENTAL_LSP_TOOL` as explicit env vars
- Update `.envrc` from `use nix` to `use devenv`

## Capabilities

### New Capabilities
- `devenv-shell`: Reproducible, pinned development environment using devenv module system

### Modified Capabilities
(none — this is infra-only, no app behavior changes)

## Impact

- Dev shell entry point changes from `shell.nix` to `devenv.nix` + `devenv.yaml`
- `.envrc` updated for devenv integration
- `shell.nix` removed
- All developers must have `devenv` installed (already present on this machine)
- Nixpkgs pinning adds `devenv.lock` to version control

## PDCA Cycle

- **Plan**: Switch to devenv for reproducible, declarative dev shells. Success = `devenv shell` activates an equivalent environment with same GHC version, all tools available, and `devenv.lock` present.
- **Do**: Create `devenv.yaml` (nixpkgs-unstable input), `devenv.nix` (Haskell module, packages, scripts, env), update `.envrc`, remove `shell.nix`.
- **Check**: Verify `devenv shell` activates, `ghc --version` reports 9.10.x, `mgconsole` works, `LD_LIBRARY_PATH` includes zlib/openssl paths, `devenv.lock` is generated and committed.
- **Act**: If successful, remove `shell.nix`. If issues arise with GHC package set consistency or LD_LIBRARY_PATH, adjust and iterate.