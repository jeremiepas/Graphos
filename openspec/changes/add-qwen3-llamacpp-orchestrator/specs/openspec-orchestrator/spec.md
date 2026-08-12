## ADDED Requirements

### Requirement: Orchestrator drives PDCA artifact cycle
The `openspec-orchestrator` SHALL be a long-running dev-env service that autonomously advances the OpenSpec PDCA artifact cycle (proposal → specs → design → tasks → plan → do → check → act → archive) for a named change using opencode driven by Qwen 3.6 served via llama.cpp.

- **Plan**: Replace manual hand-driven artifact invocation with an automated loop that parses status, dispatches artifacts via opencode, and waits for completion.
- **Do**: Implement the orchestrator loop in the `orchestrator/` directory (Infrastructure/dev-env tooling — not part of the Haskell Graphos library), consuming the `openspec` CLI and opencode's headless interface.
- **Check**: The scenarios below verify the loop advances artifacts correctly and pauses on questions.
- **Act**: If the loop stalls on certain artifact types, refine the opencode dispatch prompt or artifact parser to handle edge cases, then re-test.

#### Scenario: Orchestrator advances to the first ready artifact
- **WHEN** a change exists in `openspec/changes/<name>/` with artifacts in the proposal stage
- **THEN** the orchestrator parses `openspec status --change <name> --json`, identifies the first artifact with status `ready`, and begins generation via opencode

#### Scenario: Orchestrator writes generated artifact content
- **WHEN** opencode produces output for a `ready` artifact
- **THEN** the orchestrator writes the output to the correct artifact file under `openspec/changes/<name>/spec/<artifact-name>/` (or the appropriate artifact directory)

#### Scenario: Orchestrator progresses through all PDCA artifacts
- **WHEN** the current artifact reaches `complete` status
- **THEN** the orchestrator advances to the next `ready` artifact in the sequence and repeats until all artifacts are `complete`

#### Scenario: Orchestrator halts on a question
- **WHEN** opencode surfaces a question during artifact generation (via the opencode `question` tool or an explicit `<QUESTION>` block in the artifact output)
- **THEN** the orchestrator halts the loop, emits the question to the dev-env console, writes it to `questions/<change>.md`, and waits for a user answer before resuming

#### Scenario: Orchestrator resumes after question answer
- **WHEN** a user provides an answer to a previously paused question
- **THEN** the orchestrator resumes artifact generation from the point of pause, passing the user's answer to opencode

#### Scenario: Orchestrator exits when all artifacts are complete
- **WHEN** all PDCA artifacts for the change reach `complete` status
- **THEN** the orchestrator stops advancing artifacts and proceeds to the auto-verify gate

### Requirement: Auto-verify gate before archive
After all artifacts are complete, the orchestrator SHALL run `openspec validate --change <name>` and `openspec verify --change <name>` (where available). If both pass and no remediation items remain, the orchestrator SHALL auto-invoke `openspec archive --change <name>`.

- **Plan**: Prevent invalid or incomplete changes from being archived by requiring a clean validate + verify before archive.
- **Do**: After the last artifact completes, execute validate and verify in sequence; only call archive on success.
- **Check**: The scenarios below verify the gate behavior.

#### Scenario: Auto-archive on clean verify
- **WHEN** `openspec validate --change <name>` and `openspec verify --change <name>` both succeed with no issues
- **THEN** the orchestrator invokes `openspec archive --change <name>` automatically

#### Scenario: No auto-archive on failed validation
- **WHEN** `openspec validate --change <name>` reports violations
- **THEN** the orchestrator does NOT archive and instead enters the remediation loop

#### Scenario: No auto-archive on failed verification
- **WHEN** `openspec verify --change <name>` reports discrepancies between artifacts and implementation
- **THEN** the orchestrator does NOT archive and instead enters the remediation loop

### Requirement: Bounded remediation loop
If verify or check flags issues, the orchestrator SHALL feed each finding back into the relevant artifact as a fix-up pass, with a bounded retry count (default: 3 rounds). Only archive after a clean verify within the retry budget.

- **Plan**: Ensure recoverability from common spec/implementation mismatches without infinite retry.
- **Do**: Parse validate/verify output, identify affected artifacts, re-dispatch those artifacts to opencode with the findings as context, up to the maximum retry count.
- **Check**: The scenarios below verify the remediation behavior and budget enforcement.

#### Scenario: Remediation retries on verify failure
- **WHEN** `openspec verify --change <name>` reports issues
- **THEN** the orchestrator re-dispatches the affected artifact(s) to opencode with the findings, increments the retry counter, and re-verifies

#### Scenario: Remediation respects the retry budget
- **WHEN** the remediation retry count reaches the maximum (default: 3)
- **THEN** the orchestrator stops retrying, does NOT archive, emits a final failure report to `graphos-out/orchestrator/*.log`, and pauses for human intervention

#### Scenario: Remediation archives on clean verify within budget
- **WHEN** a remediation round produces a clean verify result
- **THEN** the orchestrator invokes `openspec archive --change <name>` regardless of the retry count

#### Scenario: Remediation feeds findings as context
- **WHEN** the orchestrator re-dispatches an artifact during remediation
- **THEN** the validate/verify findings are included as structured context in the opencode dispatch prompt

### Requirement: Dev-env lifecycle management
The orchestrator SHALL expose `start` / `stop` / `status` commands so the service runs in the background of the dev shell, logs to `graphos-out/orchestrator/*.log`, and is restartable.

- **Plan**: Allow operators to manage the orchestrator lifecycle without leaving the dev shell environment.
- **Do**: Implement lifecycle commands as devenv scripts in `shell.nix`, log all output to `graphos-out/orchestrator/`, and track the process PID for stop/restart.
- **Check**: The scenarios below verify lifecycle management.

#### Scenario: Start command launches the orchestrator
- **WHEN** a user runs the `start` command (e.g., `openspec-orch start`)
- **THEN** the orchestrator service launches in the background, writes its PID, and begins processing the designated change(s)

#### Scenario: Stop command terminates the orchestrator
- **WHEN** a user runs the `stop` command (e.g., `openspec-orch stop`)
- **THEN** the orchestrator service terminates gracefully, and the PID file is cleaned up

#### Scenario: Status command reports current state
- **WHEN** a user runs the `status` command (e.g., `openspec-orch status`)
- **THEN** the command reports the current state: running, stopped, or paused-on-question, along with the active change name(s) and log path

#### Scenario: Service logs to graphos-out/orchestrator
- **WHEN** the orchestrator is running
- **THEN** all stdout and stderr output is appended to files under `graphos-out/orchestrator/` with names matching the pattern `*.log`

#### Scenario: Service is restartable
- **WHEN** the orchestrator is stopped and then started again
- **THEN** the new instance launches with a fresh PID, overwriting the old PID file, and resumes operation

### Requirement: Multi-change fan-out
The orchestrator SHALL accept a change name OR the `--all` flag to process every in-progress change in `openspec/changes/` that is not yet archived, serially (one at a time).

- **Plan**: Support batch processing of multiple changes while keeping Qwen 3.6 context coherent per change.
- **Do**: Enumerate changes from `openspec/changes/`, filter for non-archived and non-complete, iterate sequentially.
- **Check**: The scenarios below verify multi-change behavior.

#### Scenario: Orchestrator processes a single named change
- **WHEN** the orchestrator is invoked with a specific change name
- **THEN** it processes only that change from proposal to archive (or pause)

#### Scenario: Orchestrator processes all changes with --all
- **WHEN** the orchestrator is invoked with the `--all` flag
- **THEN** it enumerates all in-progress changes in `openspec/changes/` that are not yet archived, and processes them serially

#### Scenario: Orchestrator skips archived changes
- **WHEN** the orchestrator scans `openspec/changes/` for work
- **THEN** it excludes any change directory already present under `openspec/changes/archive/`

#### Scenario: Orchestrator preserves serial order
- **WHEN** the orchestrator processes multiple changes with `--all`
- **THEN** it completes all artifacts and verify for one change before beginning the next

### Requirement: Local model provider isolation
The orchestrator SHALL consume a local llama.cpp server providing an OpenAI-compatible `/v1/chat/completions` endpoint at `http://localhost:8080`. All LLM calls for the orchestrator agent SHALL target this endpoint exclusively — no cloud API keys or remote model endpoints SHALL be used.

- **Plan**: Guarantee fully offline, zero-cost model serving for the orchestrator loop.
- **Do**: Configure the orchestrator's opencode dispatch to route all chat completions through the local llama.cpp endpoint via a dedicated model provider configuration.
- **Check**: The scenarios below verify model isolation.

#### Scenario: Orchestrator uses local endpoint only
- **WHEN** the orchestrator dispatches an artifact to opencode
- **THEN** all chat completion requests target `http://localhost:8080/v1/chat/completions`

#### Scenario: Orchestrator fails to start without llama.cpp
- **WHEN** the orchestrator starts but `http://localhost:8080` is unreachable
- **THEN** the orchestrator emits an error, does NOT proceed with artifact generation, and waits for the llama.cpp server to become available

#### Scenario: No cloud API keys consumed
- **WHEN** the orchestrator runs all PDCA artifacts
- **THEN** zero requests are made to external (non-localhost) model endpoints
