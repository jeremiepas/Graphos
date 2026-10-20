## ADDED Requirements

### Requirement: CI build task refreshes the Hackage package index before configure
The `ci:build` devenv task SHALL run `cabal update` before `cabal configure`, so dependency resolution does not depend on a pre-existing Hackage package cache. The `cabal update` step SHALL be best-effort when the index already exists (non-fatal on transient Hackage errors) but MUST produce a usable index on a clean slate (no Hackage index directory). The cached-index fallback SHALL recognize the index in either `~/.cache/cabal` (cabal >= 3.10) or `~/.cabal` (older cabal).

Previously: `ci:build` ran `cabal configure --enable-tests --flag dev -j4 && cabal build all -j4` with no `cabal update`, causing `unknown package: zip-archive` on any runner without a pre-populated Hackage index.

- **Plan**: CI build (`devenv tasks run ci:build`) resolves all dependencies declared in `graphos.cabal` — including `zip-archive` — on a clean Ubuntu runner with no cached Hackage index state.
- **Do**: Prepend `cabal update` (best-effort, non-fatal if index exists) to the `ci:build` exec string in `devenv.nix`.
- **Check**: The scenarios below verify dependency resolution on clean and warm caches.
- **Act**: If `cabal update` is flaky in CI, add retry logic or pin a Hackage `index-state` in `graphos.cabal` for reproducibility.

#### Scenario: Clean-slate CI run resolves all dependencies
- **WHEN** `devenv tasks run ci:build` runs on a runner with no Hackage index directory (neither `~/.cache/cabal/packages/hackage.haskell.org` nor `~/.cabal/packages/hackage.haskell.org`)
- **THEN** `cabal update` downloads the Hackage package index
- **AND** `cabal configure` resolves every dependency in `graphos.cabal` (including `zip-archive`)
- **AND** the build proceeds to GHC compilation without any `unknown package` error

#### Scenario: Warm cache run is unaffected
- **WHEN** `devenv tasks run ci:build` runs on a runner with an existing, up-to-date Hackage index
- **THEN** `cabal update` completes quickly (idempotent, small diff)
- **AND** `cabal configure && cabal build all` behaves identically to before this change

#### Scenario: Transient Hackage failure does not block warm-cache builds
- **WHEN** `cabal update` fails due to a transient network/Hackage error AND a usable package index already exists (in `~/.cache/cabal` or `~/.cabal`)
- **THEN** the `ci:build` task SHALL NOT fail solely because of the `cabal update` error
- **AND** `cabal configure` proceeds using the existing cached index

#### Scenario: Missing dependency is reported clearly
- **WHEN** a dependency declared in `graphos.cabal` does not exist on Hackage at configure time
- **THEN** `cabal configure` fails with a clear `unknown package: <name>` error naming the missing package
- **AND** the `ci:build` task exits non-zero (this is a real dependency error, not a stale-index artifact)