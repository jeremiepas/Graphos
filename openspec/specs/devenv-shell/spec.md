## Purpose

Provide a reproducible, pinned development environment via devenv with all tooling (Haskell GHC 9.10, Memgraph mgconsole, LSP, libraries) on PATH without stale entries or cloud dependencies.
## Requirements
### Requirement: Reproducible dev environment via devenv
The project SHALL use `devenv.nix` and `devenv.yaml` as the development environment definition. The nixpkgs input SHALL be pinned via `devenv.lock` for reproducibility. The `shell.nix` file SHALL be removed.

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

### Requirement: Haskell GHC 910 tooling from consistent package set
All Haskell development tools (ghc, cabal-install, haskell-language-server, hpack, hspec-discover) SHALL come from `pkgs.haskell.packages.ghc910` to ensure version compatibility.

- **Plan**: Use `languages.haskell` module with explicit package set pinning.
- **Do**: Set `languages.haskell.package`, `cabal.package`, and `lsp.package` to ghc910 set members; list extra Haskell tools from the same set in `packages`.
- **Check**: GHC, cabal, HLS versions are consistent and come from ghc910.

#### Scenario: GHC reports 9.10
- **WHEN** a developer runs `ghc --version` inside the devenv shell
- **THEN** the output contains `9.10`

#### Scenario: cabal from ghc910 set
- **WHEN** a developer runs `cabal --version` inside the devenv shell
- **THEN** cabal-install is available and functional

#### Scenario: hpack and hspec-discover available
- **WHEN** a developer runs `hpack --version` and `hspec-discover --version`
- **THEN** both tools are available on PATH

### Requirement: mgconsole as devenv script
The `mgconsole` command SHALL be defined as a devenv `scripts` entry with the same port-remapping behavior as the original `writeShellScriptBin`.

- **Plan**: Convert writeShellScriptBin to scripts.mgconsole.
- **Do**: Move the port-remapping shell logic into `scripts.mgconsole.exec`.

#### Scenario: mgconsole available and functional
- **WHEN** a developer runs `mgconsole` inside the devenv shell
- **THEN** the script is on PATH and remaps port 7688 to 7687 before executing `docker exec -i graphos-memgraph mgconsole`

### Requirement: Library paths managed by devenv
`LD_LIBRARY_PATH` SHALL be automatically populated by devenv from library packages (zlib, openssl). `EXTRA_LIBRARY_PATH` SHALL be set explicitly as an env variable with the same library paths.

- **Plan**: Let devenv handle LD_LIBRARY_PATH; set EXTRA_LIBRARY_PATH manually.
- **Do**: Add zlib and openssl to packages; set `env.EXTRA_LIBRARY_PATH` using `lib.makeLibraryPath`.

#### Scenario: LD_LIBRARY_PATH contains library paths
- **WHEN** a developer runs `echo $LD_LIBRARY_PATH` inside the devenv shell
- **THEN** the output includes paths to zlib and openssl lib directories

#### Scenario: EXTRA_LIBRARY_PATH matches LD_LIBRARY_PATH libs
- **WHEN** a developer runs `echo $EXTRA_LIBRARY_PATH` inside the devenv shell
- **THEN** the output includes the same zlib and openssl library paths

### Requirement: No stale PATH entries
The development shell SHALL NOT add `$HOME/.cache/.bun/bin` or `$HOME/.npm-global/bin` to PATH.

- **Plan**: Remove unused PATH manipulations from shell hook.
- **Do**: Do not include these PATH entries in `enterShell`.

#### Scenario: no bun/npm-global home paths in PATH
- **WHEN** a developer runs `echo $PATH` inside the devenv shell
- **THEN** the output does not contain `$HOME/.cache/.bun/bin` or `$HOME/.npm-global/bin`

### Requirement: Environment variables preserved
`OPENCODE_EXPERIMENTAL_LSP_TOOL` SHALL be set to `"true"` in the devenv shell environment.

#### Scenario: OPENCODE_EXPERIMENTAL_LSP_TOOL set
- **WHEN** a developer runs `echo $OPENCODE_EXPERIMENTAL_LSP_TOOL` inside the devenv shell
- **THEN** the output is `true`

### Requirement: .envrc updated for devenv
The `.envrc` file SHALL use `use devenv` instead of `use nix`.

#### Scenario: direnv uses devenv
- **WHEN** direnv loads in the project directory
- **THEN** it activates the devenv shell via `use devenv`

### Requirement: Shell greeting preserved
The `enterShell` hook SHALL print the graphos dev shell name and GHC/cabal versions, matching the original `shellHook` behavior.

#### Scenario: version echo on shell entry
- **WHEN** a developer enters the devenv shell
- **THEN** the output includes `graphos dev shell`, `ghc` version, and `cabal` version

### Requirement: Orchestrator service entry in devenv shell
The devenv shell SHALL expose `openspec-orch` (alias: `oporch`) commands — `start`, `stop`, and `status` — as devenv `scripts` entries. These commands manage the `openspec-orchestrator` dev-env service in the background.

- **Plan**: Integrate the orchestrator service into the Nix dev shell so it is available alongside existing tools when running `nix-shell shell.nix` (or `devenv shell`).
- **Do**: Add `scripts.openspec-orch.exec` in `devenv.nix` that delegates to the orchestrator binary or driver script, logging output to `graphos-out/orchestrator/*.log`.
- **Check**: The scenarios below verify the service entry is available and functional.
- **Act**: If the orchestrator binary path changes, update the script's `exec` to point to the new location.

#### Scenario: openspec-orch start launches the service
- **WHEN** a developer runs `openspec-orch start` inside the devenv shell
- **THEN** the orchestrator service launches in the background, writes its PID to a known location, and begins processing the designated change(s)

#### Scenario: openspec-orch stop terminates the service
- **WHEN** a developer runs `openspec-orch stop` inside the devenv shell
- **THEN** the orchestrator service terminates and the PID file is cleaned up

#### Scenario: openspec-orch status reports state
- **WHEN** a developer runs `openspec-orch status` inside the devenv shell
- **THEN** the command outputs the current state (running, stopped, or paused-on-question), the active change name, and the log file path

#### Scenario: Orchestrator command is on PATH via devenv
- **WHEN** the devenv shell is activated
- **THEN** `openspec-orch` is available on PATH without any additional setup

### Requirement: Orchestrator log directory created
The devenv shell SHALL ensure that `graphos-out/orchestrator/` exists before the orchestrator service writes logs. The directory SHALL be created automatically if it does not exist.

- **Plan**: Avoid log-write failures by pre-creating the output directory as part of the dev shell setup.
- **Do**: Add a devenv `enterShell` hook or script that runs `mkdir -p graphos-out/orchestrator`.
- **Check**: The scenarios below verify directory creation.

#### Scenario: Orchestrator log directory exists after shell activation
- **WHEN** a developer activates the devenv shell
- **THEN** `graphos-out/orchestrator/` exists

#### Scenario: Orchestrator logs written to correct directory
- **WHEN** the orchestrator is running
- **THEN** all log output is written to files under `graphos-out/orchestrator/`

