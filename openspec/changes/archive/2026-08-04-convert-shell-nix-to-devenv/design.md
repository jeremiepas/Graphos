## Context

The project uses a `shell.nix` that fetches nixpkgs-unstable via `builtins.getFlake` — a live reference with no lock file. This means every `nix-shell` invocation can resolve a different nixpkgs snapshot, breaking reproducibility. The shell also includes unused PATH entries for `bun` and `npm-global` home directories.

Devenv provides a declarative module system (`devenv.nix` + `devenv.yaml` + `devenv.lock`) that gives us pinned dependencies, Haskell language support, and a cleaner way to manage scripts and environment variables.

Current `shell.nix` structure:

| Component | Current Implementation | Lines |
|-----------|----------------------|-------|
| Nixpkgs | `builtins.getFlake "github:NixOS/nixpkgs/nixpkgs-unstable"` (unpinned) | 2-5 |
| Haskell | `haskell.packages.ghc910` set: ghc, cabal, hls, hpack, hspec-discover | 7-15 |
| System libs | zlib, openssl, poppler-utils (in `LD_LIBRARY_PATH` + `EXTRA_LIBRARY_PATH`) | 17-21 |
| Tooling | jq, pyright, pyyaml, bun, uv, nixd, vscode-langservers-extracted | 23-31 |
| mgconsole | `writeShellScriptBin` port-remapping wrapper around docker exec | 33-54 |
| Env vars | `LD_LIBRARY_PATH`, `EXTRA_LIBRARY_PATH`, `OPENCODE_EXPERIMENTAL_LSP_TOOL` | 56-66 |
| Shell hook | PATH additions (bun, npm-global), version echo | 67-74 |

## Goals / Non-Goals

**Goals:**
- Achieve reproducible dev shells via `devenv.lock` pinning
- Use devenv's `languages.haskell` module to ensure all Haskell tooling comes from the same `ghc910` package set
- Convert `mgconsole` to a devenv `scripts` entry
- Let devenv auto-manage `LD_LIBRARY_PATH` from library packages
- Remove unused PATH entries (`$HOME/.cache/.bun/bin`, `$HOME/.npm-global/bin`)
- Preserve all currently functional environment capabilities

**Non-Goals:**
- Changing the nixpkgs channel (staying on nixpkgs-unstable, just pinned)
- Adding new tools or packages beyond what shell.nix already provides
- Modifying any application code or build configuration
- Setting up CI with devenv (future work)

## Decisions

### D1: Use `languages.haskell` module with explicit GHC 910 package set

**Decision:** Enable `languages.haskell` and pin compiler, cabal, and HLS to the `ghc910` package set.

**Alternatives considered:**
- List all Haskell packages in `packages` only → Loses devenv's Haskell integration (auto PATH setup, cabal wrapper)
- Overlay to replace `pkgs.ghc` → More complex, fragile

**Rationale:** The `languages.haskell` module provides cabal integration and proper PATH setup. By setting `languages.haskell.package`, `languages.haskell.cabal.package`, and `languages.haskell.lsp.package` all to `ghc910` set members, we guarantee version consistency (the same concern as the original shell.nix's `hp = pkgs'.haskell.packages.ghc910` binding).

### D2: Use `scripts.mgconsole` instead of `writeShellScriptBin`

**Decision:** Convert `mgconsole` to a devenv `scripts.mgconsole` entry.

**Alternatives considered:**
- Keep `pkgs.writeShellScriptBin "mgconsole" ...` in `packages` → Works but less idiomatic, harder to document
- Use `files` module to create the script → Overkill for a shell wrapper

**Rationale:** Devenv scripts are self-documenting, appear in `devenv info`, and are idiomatic. The port-remapping logic transfers directly into `scripts.mgconsole.exec`.

### D3: Let devenv manage `LD_LIBRARY_PATH`, keep `EXTRA_LIBRARY_PATH` explicit

**Decision:** Add `zlib` and `openssl` to `packages`. Devenv auto-adds library packages to `LD_LIBRARY_PATH`. Keep `EXTRA_LIBRARY_PATH` as an explicit `env` entry computed from the same library path.

**Alternatives considered:**
- Manually set `env.LD_LIBRARY_PATH` → Redundant, devenv handles it
- Remove `EXTRA_LIBRARY_PATH` entirely → Unknown if anything relies on it; preserve for safety

**Rationale:** Devenv's automatic `LD_LIBRARY_PATH` management is tested and reliable. `EXTRA_LIBRARY_PATH` is a project-specific variable that must be set explicitly.

### D4: Remove unused PATH entries

**Decision:** Drop `$HOME/.cache/.bun/bin` and `$HOME/.npm-global/bin` from the shell hook entirely.

**Alternatives considered:**
- Keep them with a comment → Still pollutes PATH with non-existent directories
- Conditionalize them → YAGNI; bun is already in nix packages

**Rationale:** These directories are not used. `bun` is already provided as a nix package. No known tooling relies on the npm-global path.

### D5: Pin nixpkgs via `devenv.yaml` + `devenv.lock`

**Decision:** Use `devenv.yaml` with `nixpkgs` input pointing to `github:NixOS/nixpkgs/nixpkgs-unstable`, then run `devenv` to generate `devenv.lock`.

**Alternatives considered:**
- Use `github:cachix/devenv-nixpkgs/rolling` → Different channel, may not have ghc910
- Pin to a specific nixpkgs commit → Too restrictive for a development environment

**Rationale:** Rolling unstable matches the current behavior but gains reproducibility through the lock file. The lock can be updated with `devenv update` when desired.

### D6: Use `enterShell` for version echo

**Decision:** Move the ghc/cabal version echo to `enterShell`.

**Rationale:** This is the devenv idiom for shell activation hooks, equivalent to `shellHook` in `mkShell`.

## Risks / Trade-offs

| Risk | Impact | Mitigation |
|------|--------|------------|
| GHC 910 package set mismatch between `languages.haskell.package` and extra packages | Build failures if `hpack`/`hspec-discover` come from a different GHC set | All Haskell packages explicitly reference `pkgs.haskell.packages.ghc910.*` |
| `LD_LIBRARY_PATH` auto-management misses a path | Runtime linker failures for Haskell binaries needing zlib/openssl | Verify `LD_LIBRARY_PATH` in activated shell contains both lib paths |
| devenv lock file drifts from nixpkgs-unstable head | Security or compatibility lag | Run `devenv update` periodically |
| `mgconsole` script behavior differs slightly | Docker exec wrapper fails if devenv script wrapper changes argument passing | Test `mgconsole` invocation after migration |
| `devenv` not installed on other developer machines | Cannot enter dev shell | Document devenv installation in project README |

## Verification Strategy (Check)

1. `devenv shell` activates without errors
2. `ghc --version` reports 9.10.x
3. `cabal --version` reports expected version
4. `which mgconsole` finds the script; `mgconsole --help` connects to docker
5. `echo $LD_LIBRARY_PATH` includes zlib and openssl lib paths
6. `echo $EXTRA_LIBRARY_PATH` matches the same paths
7. `devenv.lock` file exists and is committed
8. `cabal build` succeeds inside the devenv shell
9. `cabal test` succeeds inside the devenv shell

## Iteration & Rollback (Act)

- If GHC 910 tooling fails: try `languages.haskell.package = pkgs.haskell.packages.ghc910.ghc` with explicit cabal/hls overrides
- If `LD_LIBRARY_PATH` is missing libs: fall back to explicit `env.LD_LIBRARY_PATH`
- If devenv causes CI issues: keep `shell.nix` as fallback until CI is migrated
- Rollback: revert to `shell.nix` by restoring `.envrc` to `use nix`

## Migration Plan

1. Create `devenv.yaml` with nixpkgs-unstable input
2. Create `devenv.nix` with all packages, Haskell config, scripts, env vars
3. Run `devenv shell` to generate `devenv.lock`
4. Test all verification criteria
5. Update `.envrc` from `use nix` to `use devenv`
6. Remove `shell.nix`
7. Commit `devenv.yaml`, `devenv.nix`, `devenv.lock`, `.envrc`; delete `shell.nix`