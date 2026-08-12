## ADDED Requirements

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
