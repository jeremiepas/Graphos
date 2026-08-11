## MODIFIED Requirements

### Requirement: Reproducible dev environment via devenv
The project SHALL use `devenv.nix` and `devenv.yaml` as the development environment definition. The nixpkgs input SHALL be pinned via `devenv.lock` for reproducibility. The `shell.nix` file SHALL be removed.

Previously: Same as above — this requirement is unchanged; it is re-listed here only because the `devenv-shell` spec is being extended by the ADDED requirement below and the existing baseline must be visible in the delta.

- **Plan**: Replace unpinned `builtins.getFlake` shell.nix with devenv's pinned module system.
- **Do**: Create devenv.yaml (nixpkgs-unstable input), devenv.nix (Haskell, packages, scripts, env), generate devenv.lock, update .envrc, remove shell.nix.
- **Check**: The scenarios below verify the migration.
- **Act**: If lock drift causes issues, run `devenv update`. If devenv is unsuitable, revert to shell.nix.

#### Scenario: devenv shell activates
- **WHEN** a developer runs `devenv shell`
- **THEN** the shell activates without errors and all packages are on PATH

#### Scenario: devenv.lock provides reproducibility
- **WHEN** `devenv.lock` exists and is committed
- **THEN** two checkouts at the same commit produce identical nix store paths for all dev shell dependencies

#### Scenario: shell.nix removed
- **WHEN** the migration is complete
- **THEN** `shell.nix` does not exist in the repository root

## ADDED Requirements

### Requirement: CI build task refreshes the Hackage package index before configure
The `ci:build` devenv task SHALL run `cabal update` before `cabal configure`, so dependency resolution does not depend on a pre-existing `~/.cabal` package cache. The `cabal update` step SHALL be best-effort when the index already exists (non-fatal on transient Hackage errors) but MUST produce a usable index on a clean slate (no `~/.cabal` directory).

Previously: `ci:build` ran `cabal configure --enable-tests --flag dev -j4 && cabal build all -j4` with no `cabal update`, causing `unknown package: zip-archive` on any runner without a pre-populated Hackage index.

- **Plan**: CI build (`devenv tasks run ci:build`) resolves all dependencies declared in `graphos.cabal` — including `zip-archive` — on a clean Ubuntu runner with no cached `~/.cabal` state.
- **Do**: Prepend `cabal update` (best-effort, non-fatal if index exists) to the `ci:build` exec string in `devenv.nix`.
- **Check**: The scenarios below verify dependency resolution on clean and warm caches.
- **Act**: If `cabal update` is flaky in CI, add retry logic or pin a Hackage `index-state` in `graphos.cabal` for reproducibility.

#### Scenario: Clean-slate CI run resolves all dependencies
- **WHEN** `devenv tasks run ci:build` runs on a runner with no `~/.cabal/packages/hackage.haskell.org` directory
- **THEN** `cabal update` downloads the Hackage package index
- **AND** `cabal configure` resolves every dependency in `graphos.cabal` (including `zip-archive`)
- **AND** the build proceeds to GHC compilation without any `unknown package` error

#### Scenario: Warm cache run is unaffected
- **WHEN** `devenv tasks run ci:build` runs on a runner with an existing, up-to-date Hackage index
- **THEN** `cabal update` completes quickly (idempotent, small diff)
- **AND** `cabal configure && cabal build all` behaves identically to before this change

#### Scenario: Transient Hackage failure does not block warm-cache builds
- **WHEN** `cabal update` fails due to a transient network/Hackage error AND a usable package index already exists in `~/.cabal`
- **THEN** the `ci:build` task SHALL NOT fail solely because of the `cabal update` error
- **AND** `cabal configure` proceeds using the existing cached index

#### Scenario: Missing dependency is reported clearly
- **WHEN** a dependency declared in `graphos.cabal` does not exist on Hackage at configure time
- **THEN** `cabal configure` fails with a clear `unknown package: <name>` error naming the missing package
- **AND** the `ci:build` task exits non-zero (this is a real dependency error, not a stale-index artifact)