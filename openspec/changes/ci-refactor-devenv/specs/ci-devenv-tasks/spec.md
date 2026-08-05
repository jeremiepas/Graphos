## ADDED Requirements

### Requirement: CI task definitions in devenv.nix

The `devenv.nix` file SHALL define tasks under the `tasks` attribute with the `ci:` prefix for all CI operations. Each task SHALL use `exec` to run the corresponding cabal command. The task definitions SHALL be the single source of truth for build, test, haddock, and release commands.

- **Plan**: Eliminate duplication between local dev commands and CI YAML steps by declaring all CI operations as devenv tasks.
- **Do**: Add `tasks` block to `devenv.nix` with `ci:build`, `ci:test`, `ci:haddock`, `ci:release-build`, `ci:release-test`.
- **Check**: Scenarios below verify each task produces correct output.
- **Act**: If tasks fail locally, the CI workflow will also fail — consistent feedback loop.

#### Scenario: ci:build task builds all targets
- **WHEN** `devenv tasks run ci:build` is executed
- **THEN** cabal builds all targets with `cabal build all -j4` and exits with code 0

#### Scenario: ci:test task runs all tests
- **WHEN** `devenv tasks run ci:test` is executed
- **THEN** cabal runs all test suites with `cabal test all` and exits with code 0

#### Scenario: ci:haddock task generates documentation
- **WHEN** `devenv tasks run ci:haddock` is executed
- **THEN** cabal generates haddock documentation with `cabal haddock all` and exits with code 0

#### Scenario: ci:release-build task builds release binary
- **WHEN** `devenv tasks run ci:release-build` is executed
- **THEN** cabal builds all targets without the `dev` flag using `cabal build all` and exits with code 0

#### Scenario: ci:release-test task runs release tests
- **WHEN** `devenv tasks run ci:release-test` is executed
- **THEN** cabal runs all tests without the `dev` flag using `cabal test all` and exits with code 0

---

### Requirement: GitHub Actions workflows use devenv tasks

The `.github/workflows/haskell.yml` and `.github/workflows/release.yml` workflows SHALL use `cachix/install-nix-action`, `cachix/cachix-action`, and `nix profile add nixpkgs#devenv` to set up the environment, then run `devenv tasks run` for all build/test/haddock/release steps. The workflows SHALL NOT use `haskell-actions/setup` or manual `GHC_VERSION`/`CABAL_VERSION` environment variables.

- **Plan**: CI workflows derive their toolchain entirely from `devenv.nix`, not from separate YAML configuration.
- **Do**: Replace `haskell-actions/setup` and env vars with Nix + devenv installation steps; replace `cabal build/test/haddock` steps with `devenv tasks run ci:*`.
- **Check**: Scenarios verify CI steps use devenv tasks and no manual GHC/cabal setup remains.
- **Act**: If CI fails due to devenv issues, revert YAML files; devenv tasks still work locally.

#### Scenario: haskell.yml uses devenv for build-and-test job
- **WHEN** the `haskell.yml` workflow runs on push or PR to main
- **THEN** the `build-and-test` job installs Nix via `cachix/install-nix-action@v31`, configures Cachix via `cachix/cachix-action@v16`, installs devenv via `nix profile add nixpkgs#devenv`, and runs `devenv tasks run ci:build` and `devenv tasks run ci:test`

#### Scenario: haskell.yml uses devenv for haddock job
- **WHEN** the `haskell.yml` workflow runs the `haddock` job
- **THEN** the job installs Nix, Cachix, and devenv, and runs `devenv tasks run ci:haddock`

#### Scenario: release.yml uses devenv for release job
- **WHEN** the `release.yml` workflow runs on a version tag push
- **THEN** the job installs Nix, Cachix, and devenv, runs `devenv tasks run ci:release-build` and `devenv tasks run ci:release-test`, then generates and uploads the release binary

#### Scenario: no manual GHC/cabal version env vars in workflows
- **WHEN** the workflow YAML files are inspected
- **THEN** there are no `GHC_VERSION` or `CABAL_VERSION` environment variables and no `haskell-actions/setup` steps

---

### Requirement: Nix store caching via Cachix

The CI workflows SHALL use `cachix/cachix-action@v16` to cache the Nix store. The Cachix cache name SHALL be configurable and default to a project-specific cache. This replaces the manual `actions/cache` for cabal-store and dist-newstyle.

- **Plan**: Leverage Nix's content-addressable store and Cachix for reliable, cross-run caching.
- **Do**: Add `cachix/cachix-action@v16` step after `cachix/install-nix-action@v31` in both workflows.
- **Check**: Second CI run is faster than first (cache hit). No `actions/cache` steps remain.
- **Act**: If Cachix is unavailable, CI still works (just slower); no hard dependency.

#### Scenario: Cachix cache speeds up subsequent CI runs
- **WHEN** a second CI run occurs after a successful first run
- **THEN** the Nix store cache is hit and build steps complete faster than the first run

#### Scenario: No manual cabal cache steps in workflows
- **WHEN** the workflow YAML files are inspected
- **THEN** there are no `actions/cache` steps targeting cabal-store or dist-newstyle directories