<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [x] N.P …`, `- [x] N.D …`, `- [x] N.C …`, `- [x] N.A …`.

  PASS rule:  a task PASSES only when its Check passes AND its Act is OK.
  RETRY rule: if Act is NOT OK, record the failed attempt under
  "### Attempt history (N)", then start a NEW attempt. Never delete prior notes.
-->

## 1. Create llamacpp model provider configuration

- [x] 1.P Plan: Create `.opencode/providers/llamacpp.json` with `baseUrl` pointing to `http://localhost:8080/v1/chat/completions`, `model` set to `qwen3.6-35b-a3b`, and no cloud provider keys. Check: opencode uses this provider when specified, all requests go to localhost:8080, and no external endpoints are contacted. Affected: new file `.opencode/providers/llamacpp.json`.
- [x] 1.D Do: Write `.opencode/providers/llamacpp.json` with the OpenAI-compatible endpoint configuration for the local llama.cpp server. Ensure the provider name is `llamacpp` and the model field matches `qwen3.6-35b-a3b`.
- [x] 1.C Check: (1) File exists at `.opencode/providers/llamacpp.json`, (2) valid JSON with correct `baseUrl` and `model` fields, (3) no `OPENAI_API_KEY` or other cloud keys present in the provider config, (4) opencode can parse the provider file without errors.
- [x] 1.A Act: If the provider file format differs from what opencode expects, adjust the schema to match the actual `.opencode/providers/` convention.

### Attempt history (1)

<!-- empty -->

## 2. Add .envrc defaults for orchestrator environment variables

- [x] 2.P Plan: Create `.envrc` with default environment variables for the orchestrator: `LLAMA_BASEURL`, `OPENCODE_MODEL`, `ORCHESTRATOR_LOG_DIR`, `ORCHESTRATOR_MAX_REMEDIATION`, `ORCHESTRATOR_TIMEOUT`. Check: `direnv allow` loads these vars, and `orchestrate.py` picks them up correctly at runtime. Affected: new file `.envrc` (or update existing).
- [x] 2.D Do: Write `.envrc` with `export` statements for all orchestrator config variables with sensible defaults matching `orchestrate.py` defaults. Use `direnv`-compatible syntax.
- [x] 2.C Check: (1) `.envrc` exists and is valid direnvrc syntax, (2) all required env vars have defaults, (3) `direnv allow` succeeds and vars are available in a new shell.
- [x] 2.A Act: If existing `.envrc` content conflicts, merge the new variables without overwriting user-specific settings.

### Attempt history (1)

<!-- empty -->

## 3. Add openspec-orch lifecycle scripts to devenv.nix

- [x] 3.P Plan: Add `scripts.openspec-orch.exec` in `devenv.nix` with `start`/`stop`/`status` subcommands. `start` launches `orchestrate.py` in background via nohup, writes PID to `graphos-out/orchestrator/orchestrator.pid`, logs to `graphos-out/orchestrator/*.log`. `stop` reads PID and sends SIGTERM. `status` checks PID liveness and question file state. Check: all three subcommands work correctly from a devenv shell. Affected: `devenv.nix`.
- [x] 3.D Do: Write the shell script in `devenv.nix` under `scripts.openspec-orch.exec` that handles the three subcommands. Reuse the existing `scripts.orchestrator.exec` pattern for the nohup launch, but add the PID file management and question-file inspection for status.
- [x] 3.C Check: (1) `openspec-orch start` launches and creates PID file, (2) `openspec-orch stop` terminates the process, (3) `openspec-orch status` reports running/stopped/paused-on-question correctly, (4) log files appear under `graphos-out/orchestrator/`.
- [x] 3.A Act: If the script needs additional subcommands (e.g., `--all`, `--change`), add them to the argument passthrough.

### Attempt history (1)

<!-- empty -->

## 4. Add enterShell hook to create orchestrator output directory

- [x] 4.P Plan: Add a devenv `enterShell` hook that runs `mkdir -p graphos-out/orchestrator` to ensure the log directory exists before the orchestrator service writes any output. Check: the directory is created when the devenv shell activates. Affected: `devenv.nix`.
- [x] 4.D Do: Insert an `enterShell` hook into `devenv.nix` that creates the orchestrator output directory. Place it near existing setup hooks.
- [x] 4.C Check: (1) Activating the devenv shell creates `graphos-out/orchestrator/`, (2) the directory persists across shell restarts.
- [x] 4.A Act: If another hook already creates this directory, consolidate to avoid duplicates.

### Attempt history (1)

<!-- empty -->

## 5. Add orchestrator:status devenv task and oporch alias

- [x] 5.P Plan: Add a `orchestrator:status` devenv task that runs `openspec-orch status`, and add `oporch` as an alias for `openspec-orch`. Check: `devenv tasks run orchestrator:status` prints the current state, and `oporch` is on PATH. Affected: `devenv.nix`.
- [x] 5.D Do: Register the `orchestrator:status` task in `devenv.nix` and create the `oporch` alias in the devenv shell PATH or profile.
- [x] 5.C Check: (1) `oporch` runs successfully from the devenv shell, (2) `devenv tasks run orchestrator:status` outputs the orchestrator state.
- [x] 4.A Act: If the alias needs to be environment-specific rather than global, move it to `env.sh` or equivalent.

### Attempt history (1)

<!-- empty -->

## 6. End-to-end integration test

- [x] 6.P Plan: Create a minimal test change in `openspec/changes/` with a trivial spec, then run `openspec-orch start <test-change>` end-to-end. Check: (a) logs in `graphos-out/orchestrator/*.log` contain artifact timestamps, (b) `metrics.jsonl` records events, (c) exit code 0 on clean archive or 10 on question pause. Affected: new test change + verification.
- [x] 6.D Do: Create a simple pilot change with one spec, run the full orchestrator loop (start → artifact advance → validate → archive → stop), and collect metrics/logs. Also test the existing `devenv tasks run orchestrator:run` to confirm backward compatibility.
- [x] 6.C Check: (1) `openspec-orch start` drives a change through all PDCA artifacts to archive, (2) `metrics.jsonl` contains at least one event per phase, (3) `openspec-orch stop` works after archive, (4) `devenv tasks run orchestrator:run` still works as before.
- [x] 6.A Act: If the orchestrator stalls on the test change, debug the artifact dispatch loop and adjust the opencode prompt.

### Attempt history (1)

<!-- empty -->

## 7. Model isolation verification

- [x] 7.P Plan: Run `openspec-orch start <test-change>` with `OPENAI_API_KEY` set in the environment. Monitor network traffic to verify zero requests leave localhost:8080. Check: all requests target `http://localhost:8080`, no external endpoints are contacted. Affected: network monitoring setup.
- [x] 7.D Do: Execute the orchestrator with a cloud API key present, use network monitoring (e.g., `tcpdump` or `socat` on port 443) to confirm no external API calls. Also verify that `orchestrate.py` fails fast if `http://localhost:8080` is unreachable (per spec requirement).
- [x] 6.C Check: (1) Zero requests to non-localhost endpoints during the run, (2) `OPENAI_API_KEY` is ignored by the orchestrator's provider config, (3) orchestrator emits clear error when llama.cpp is not available.
- [x] 7.A Act: If any cloud endpoint is contacted, add a pre-flight check or firewall rule to block it.

### Attempt history (1)

<!-- empty -->
