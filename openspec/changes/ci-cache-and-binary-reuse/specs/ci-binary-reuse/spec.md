## ADDED Requirements

### Requirement: CI caches the cabal store across builds

The CI build workflow SHALL cache the cabal store and `dist-newstyle`
build directory between runs, keyed on the project's cabal definition
hashes (e.g. `graphos.cabal`, `cabal.project`), so unchanged dependencies
are not rebuilt from Hackage sources on every run. The nix/devenv
toolchain cache (Cachix) SHALL continue to cover GHC and shell
dependencies.

#### Scenario: unchanged cabal definition restores the cache

- **WHEN** a push to `main` builds with an unchanged `graphos.cabal`/`cabal.project` relative to the previous successful build
- **THEN** the cabal store and `dist-newstyle` are restored from the cache before the build and the build skips already-built dependencies

#### Scenario: changed cabal definition rebuilds dependencies

- **WHEN** a push to `main` modifies `graphos.cabal` or `cabal.project`
- **THEN** the cache key changes, no stale store is restored, and the build recompiles against the new dependency resolution

#### Scenario: cache miss still builds

- **WHEN** no cache entry exists (first run, or cache evicted)
- **THEN** the workflow builds from scratch and succeeds, then saves a new cache entry

### Requirement: CI saves the built binary as a workflow artifact

The CI build workflow SHALL, after a successful build, locate the
`graphos` executable via the shared `ci:bin` devenv task and upload it as
a workflow artifact named `graphos-bin`, with a bounded retention period.
The artifact SHALL be a single executable file named `graphos-linux-x86_64`.

#### Scenario: successful build uploads the binary artifact

- **WHEN** the CI build job succeeds on a push to `main`
- **THEN** the run has a `graphos-bin` artifact containing an executable `graphos-linux-x86_64`

#### Scenario: failed build uploads nothing

- **WHEN** the CI build or test job fails
- **THEN** no `graphos-bin` artifact is published for that run

### Requirement: devenv task ci:bin verifies the built binary

`devenv.nix` SHALL define a `ci:bin` task that verifies the `graphos`
executable has been built (via `cabal list-bin`) and prints its path,
failing with a clear message when it has not. Workflows SHALL locate the
binary via `devenv shell -- cabal list-bin graphos` (devenv task output is
not reliably capturable as stdout) and MAY run `ci:bin` as a fail-fast
preflight before copying the binary.

#### Scenario: ci:bin prints the binary path after a build

- **WHEN** `devenv tasks run ci:bin` is executed after a successful `ci:build`
- **THEN** the task output (shown by devenv) is the absolute path of the `graphos` executable

#### Scenario: ci:bin fails without a build

- **WHEN** `devenv tasks run ci:bin` is executed with no prior build output
- **THEN** the task fails with a non-zero exit code naming the missing build prerequisite

#### Scenario: workflows capture the binary path via devenv shell

- **WHEN** a workflow needs the binary path for copying/uploading
- **THEN** it captures `devenv shell -- cabal list-bin graphos` and validates the resulting file exists before use

### Requirement: Repo analyzer acquires the graphos binary by preference order

The repo-analyzer dispatch workflow SHALL acquire the graphos binary for
each matrix job by the following order: (1) the GitHub release asset
`graphos-linux-x86_64` matching the requested `graphos_version` (or the
latest release when `latest` is requested), if such a release exists;
(2) the `graphos-bin` artifact of the most recent successful build run on
`main`; (3) a source build via the existing devenv tasks. When (1) or (2)
succeeds, the job SHALL skip the nix/devenv installation steps.

#### Scenario: release asset takes precedence

- **WHEN** the analyzer is dispatched while a release with the `graphos-linux-x86_64` asset exists (matching the requested version, or latest)
- **THEN** the matrix job downloads that asset and does not install nix/devenv or build from source

#### Scenario: CI artifact is used when no release exists

- **WHEN** the analyzer is dispatched while no release exists and the most recent successful `main` build produced a `graphos-bin` artifact
- **THEN** the matrix job downloads that artifact, makes it executable, and skips the nix/devenv setup

#### Scenario: source build remains the fallback

- **WHEN** neither a matching release asset nor a usable `graphos-bin` artifact is available
- **THEN** the matrix job falls back to the existing devenv source build (nix + cachix + `devenv tasks run ci:build` + `ci:bin`) and still succeeds

#### Scenario: analyzed outputs are identical regardless of acquisition path

- **WHEN** the same repo is analyzed with a release binary, an artifact binary, and a source-built binary at the same commit
- **THEN** the analyzer's graph outputs (graph.json contents up to generation timestamps, metrics, time/memory report) are equivalent

### Requirement: Artifact download does not require elevated permissions

The repo-analyzer workflow SHALL acquire release assets and the latest
CI artifacts using the workflow's own token with `actions: read` and
`contents: read` permissions, without any stored secret.

#### Scenario: dispatch works with default permissions

- **WHEN** the analyzer is dispatched on a repository where the workflow token has `actions: read` and `contents: read`
- **THEN** release asset and CI artifact resolution succeed without configuration or credentials beyond the default `GITHUB_TOKEN`