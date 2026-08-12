## Context

Graphos uses OpenSpec's PDCA workflow (proposal → specs → design → tasks → plan → do → check → act → archive) to drive changes. Currently, advancing a change requires a human to manually invoke `openspec instructions`, `openspec validate`, `openspec verify`, and `openspec archive` for each artifact. The `fix-mvar-shutdown-crash` change added per-component timeouts so the pipeline exits cleanly; the `docker-otel-stack` change demonstrated adding infrastructure tooling to the project.

The project already has a Python orchestrator (`orchestrate.py`, 983 lines) that implements the core PDCA loop — parsing `openspec status --json`, dispatching artifacts to `opencode run --format json`, handling `<QUESTION>` blocks, running `openspec validate`, and executing bounded remediation retries. The `llama:server` devenv task starts llama.cpp hosting Qwen 3.6. An `orchestrator` script entry exists in `devenv.nix` that delegates to the Python script.

The missing pieces are: (1) the `openspec-orch` command wrapper providing `start`/`stop`/`status` lifecycle management, (2) opencode model provider configuration routing the orchestrator's LLM calls exclusively through the local llama.cpp endpoint, and (3) the integration glue tying these together under the `graphos-out/orchestrator/` directory.

## Goals / Non-Goals

**Goals:**
- Define how the `openspec-orch` lifecycle wrapper (start/stop/status) is structured as devenv shell scripts, using PID files under `graphos-out/orchestrator/` and logging to `graphos-out/orchestrator/*.log`
- Define the opencode `.opencode/` model provider configuration for llama.cpp, ensuring the orchestrator agent exclusively targets `http://localhost:8080/v1/chat/completions` with model `qwen3.6-35b-a3b`
- Specify the integration architecture: how `orchestrate.py` receives its configuration (environment variables vs config file), how question files are persisted, and how metrics are collected
- Provide a clear file layout that separates lifecycle management (shell scripts), orchestrator logic (Python), and model provider configuration (`.opencode/providers/`)

**Non-Goals:**
- Haskell library changes — the orchestrator is pure Infrastructure/dev-env tooling, not part of the Graphos Haskell library
- New external Haskell dependencies
- Changes to the `openspec` CLI interface or OpenSpec schema
- Production deployment of the orchestrator (this is a dev-env tool only)
- Multi-model support — the design targets llama.cpp exclusively, with no fallback to cloud providers

## Decisions

### D1: Orchestrator lifecycle management via shell wrapper, not Python subprocess daemon

**Decision**: Provide `openspec-orch` start/stop/status as devenv shell scripts that manage the Python process externally using PID files. The `start` command launches `orchestrate.py` in the background with `nohup`, writing stderr/stdout to `graphos-out/orchestrator/*.log` and the PID to `graphos-out/orchestrator/orchestrator.pid`. The `stop` command reads the PID file and sends SIGTERM. The `status` command reads the PID file, checks process liveness, and inspects `questions/<change>.md` for pause state.

**Alternatives considered:**
- A) Embed start/stop/status in the Python script itself — adds complexity to `orchestrate.py`, requires parsing additional subcommands, couples lifecycle to Python runtime
- B) **Shell wrapper with PID files** — simple, works with any process, integrates naturally with devenv scripts, matches existing devenv task patterns
- C) Use a process supervisor (pm2, supervisord) — overkill for a dev tool, adds external dependency

**Rationale**: Shell scripts are the right abstraction for process lifecycle management in a Nix/devenv context. The Python orchestrator focuses on PDCA logic; the shell wrapper handles OS-level concerns (daemonization, signal handling, PID management). This separation matches how devenv tasks already structure `llama:server` and `orchestrator:run`.

### D2: Model provider configuration via `.opencode/providers/` directory

**Decision**: Create an opencode provider configuration under `.opencode/providers/llamacpp.json` (or equivalent provider file format) that sets `baseUrl` to `http://localhost:8080/v1/chat/completions`, `model` to `qwen3.6-35b-a3b`, and disables fallback to any other provider. The orchestrator's Python dispatch (`opencode run --model ...`) passes the model name explicitly, ensuring no ambiguity.

**Alternatives considered:**
- A) Use environment variables (`OPENAI_BASE_URL`, `OPENAI_API_KEY`) — risk of accidental fallback if cloud keys are set; requires the orchestrator to clear environment variables
- B) **`.opencode/providers/` configuration** — explicit, version-controlled, isolated from user's global opencode config, makes the local-only intent visible
- C) Hard-code the endpoint in `orchestrate.py` — inflexible, requires code changes to change the model or endpoint

**Rationale**: A dedicated provider file makes the local-only intent explicit and version-controlled. The `OPENAI_API_KEY` isolation requirement in the spec means we cannot rely on environment variable suppression — a provider config that only defines the llama.cpp endpoint is the correct isolation boundary.

### D3: Configuration via environment variables with `.envrc` defaults

**Decision**: The orchestrator reads its configurable parameters from environment variables with sensible defaults in `orchestrate.py`: `LLAMA_BASEURL` (default `http://localhost:8080/v1`), `OPENCODE_MODEL` (default `llama/qwen3.6-35b-a3b`), `ORCHESTRATOR_CHANGE` (target change name, or `all`), `ORCHESTRATOR_MAX_REMEDIATION` (default 3), `ORCHESTRATOR_TIMEOUT` (default 1800), `ORCHESTRATOR_LOG_DIR` (default `graphos-out/orchestrator`). These are set via `.envrc` in the devenv shell.

**Alternatives considered:**
- A) YAML/TOML config file — requires file parsing, versioning, and path resolution; devenv already provides env var injection
- B) **Environment variables with `.envrc` defaults** — already the devenv pattern (see `env.LLAMA_BASEURL` in `devenv.nix:131`), zero parsing overhead, easily overridable per-change
- C) CLI flags for all config — verbose, error-prone for background processes that need persistent config

**Rationale**: The devenv shell already uses environment variables for configuration (`LLAMA_BASEURL`, `ORCHESTRATOR_POLL_INTERVAL` in `devenv.nix`). Extending this pattern keeps the configuration model consistent. The `.envrc` file provides defaults; users override via `direnv` or explicit `export` in their shell.

### D4: Questions persistence as Markdown files with answer markers

**Decision**: When the orchestrator surfaces a question (via opencode's `question` tool or `<QUESTION>` block), it writes the question to `openspec/changes/<change-name>/questions/<change>.md` with structured markdown: numbered questions, timestamps, and `<!-- answer below -->` markers. The `status` command checks for the existence and non-emptiness of answer sections to report `paused-on-question` state.

**Alternatives considered:**
- A) SQLite database — overkill, requires dependency, breaks the no-new-dependencies constraint
- B) **Markdown file with answer markers** — human-readable, editable, version-control friendly, matches existing OpenSpec artifact conventions
- C) JSON format — less human-editable, harder for users to provide answers via `vi`/`emacs`

**Rationale**: Questions are a human-in-the-loop mechanism. Markdown is the most accessible format for both machine parsing and human editing. The existing `orchestrate.py` already implements this pattern (see `write_question_file` at line 479). The `openspec-orch status` wrapper simply inspects these files.

### D5: Metrics as JSONL append-only log

**Decision**: All orchestrator events (artifact generation, validate, remediation, archive, question pauses) are written as JSONL records to `graphos-out/orchestrator/metrics.jsonl`, one JSON object per line. Each record includes a `ts` timestamp, `change` name, `event` type, and event-specific fields. This format is compatible with `jq` for ad-hoc analysis and `devenv tasks run ci:orchestrator-metrics` for structured queries.

**Alternatives considered:**
- A) Prometheus metrics endpoint — requires HTTP server, aggregation, Grafana dashboard; overkill for dev tool
- B) **JSONL append-only log** — already implemented in `orchestrate.py` (line 178), queryable via `jq`, parsable by any downstream tool, no infrastructure dependency
- C) SQLite — relational queries but adds dependency, not needed for this scope

**Rationale**: The orchestrator already writes metrics as JSONL (`Logger.metric()` at `orchestrate.py:178`). This design keeps the approach — it's simple, queryable, and persists across restarts since it's append-only.

### D6: Service state reporting via PID file + question file inspection

**Decision**: The `openspec-orch status` command reports state by examining three signals: (1) `orchestrator.pid` file existence and process liveness → `running` or `stopped`, (2) presence of `questions/<change>.md` with unanswered questions → `paused-on-question`, (3) `graphos-out/orchestrator/orchestrator.log` tail for recent events. State is reported as a simple one-line summary with PID, change name, and log path.

**Alternatives considered:**
- A) JSON state file updated by the orchestrator — adds IPC complexity, requires the Python process to update state on every transition
- B) **PID file + question file + log tail** — no IPC needed, leverages existing file artifacts, status is always consistent with the actual OS state
- C) Systemd service — not available in Nix dev shell context, adds infrastructure dependency

**Rationale**: PID-file-based state is the simplest approach that works reliably across environments (terminal, tmux, direnv shell, devenv task). The question file and log already exist as side effects of the orchestrator — `status` simply inspects them.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  devenv shell (devenv.nix)                              │
│                                                         │
│  scripts.orchestrator.exec  ──►  orchestrate.py         │
│  scripts.openspec-orch.exec ──►  openspec-orch.sh       │
│    start / stop / status                                  │
│                                                         │
│  env.LLAMA_BASEURL = http://localhost:8080/v1            │
│  env.OPENCODE_EXPERIMENTAL_LSP_TOOL = true               │
└──────────────────────┬──────────────────────────────────┘
                       │
          ┌────────────▼─────────────┐
          │  openspec-orch start     │
          │  ┌─────────────────────┐ │
          │  │ nohup python3       │ │
          │  │   orchestrate.py    │ │
          │  │   --all             │ │
          │  │   > log &           │ │
          │  │   PID > pid file    │ │
          │  └─────────────────────┘ │
          └────────────┬─────────────┘
                       │
          ┌────────────▼─────────────┐
          │  orchestrate.py          │
          │  ┌─────────────────────┐ │
          │  │ PDCA artifact loop  │ │
          │  │   openspec status   │ │
          │  │   openspec instr    │ │
          │  │   opencode run      │ │
          │  │   openspec validate │ │
          │  │   openspec archive  │ │
          │  └─────────────────────┘ │
          │                          │
          │  writes:                 │
          │    graphos-out/           │
          │      orchestrator/        │
          │        *.log              │
          │        *.jsonl            │
          │        orchestrator.pid   │
          │    openspec/changes/      │
          │      <name>/              │
          │        questions/         │
          │          <name>.md        │
          └────────────┬─────────────┘
                       │
          ┌────────────▼─────────────┐
          │  opencode run --format   │
          │    json --model <model>  │
          │  ┌─────────────────────┐ │
          │  │ .opencode/          │ │
          │  │   providers/        │ │
          │  │     llamacpp.json   │ │
          │  └─────────────────────┘ │
          │  ┌─────────────────────┐ │
          │  │ localhost:8080      │ │
          │  │ /v1/chat/completions│ │
          │  └─────────────────────┘ │
          └──────────────────────────┘
```

## File Layout

```
graphos/
├── .opencode/
│   └── providers/
│       └── llamacpp.json           # Model provider config for llama.cpp
├── devenv.nix                      # Add openspec-orch scripts (start/stop/status)
├── .envrc                          # Default environment variables for orchestrator
├── orchestrator/
│   └── orchestrate.py              # Existing orchestrator (no changes required)
├── openspec/changes/
│   └── <change-name>/
│       └── questions/
│           └── <change>.md         # Question files (created by orchestrator)
└── graphos-out/orchestrator/       # Logs, metrics, PID (created by orchestrator)
    ├── <change>-<timestamp>.log
    ├── metrics.jsonl
    └── orchestrator.pid
```

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| llama.cpp server goes down mid-artifact | `orchestrate.py` already has `--no-health-check` flag and `check_llama_server()` (line 827). The `openspec-orch start` wrapper includes a pre-flight check and fails fast if the endpoint is unreachable. |
| Unbounded LLM cost if misconfigured | Provider config restricts to `localhost:8080` only. The `.opencode/providers/llamacpp.json` has no cloud keys. The spec requires zero cloud API consumption (verified by test scenario). |
| Autonomous archive is irreversible | The verify gate (`openspec validate` + `openspec verify`) runs before every archive call. Remediation loop retries up to 3 rounds. A question-pause always stops before archive if genuine decisions surface. |
| Question file editing race during multi-change processing | Orchestrator processes changes serially (spec requirement). Each change writes to its own `questions/<change>.md`. The `openspec-orch status` only reports the current active change. |
| Log files grow unbounded during long remediation cycles | Logs use timestamp-rotated filenames (`<change>-<YYYYMMDD-HHMMSS>.log`). A future `openspec-orch clean-logs` command (not in scope) can trim old logs. |
| PID file stale after crash | `status` checks process liveness via `/proc/<pid>`. `stop` ignores stale PIDs. A `start` that detects an existing stale PID file waits up to 5s for graceful shutdown before overwriting. |
| `opencode run --format json` parsing fragile | `parse_opencode_stream()` (line 383) uses a permissive parser — it ignores unknown event types, skips JSON decode errors, and falls back to regex for `<QUESTION>` blocks. |

## Verification Strategy

1. **Unit tests for orchestrator components**: `python -m pytest orchestrator/` — tests for `parse_opencode_stream`, `build_opencode_prompt`, `collect_validation_issues`, `extract_questions_from_text`. Verify parsing correctness against sample JSONL event streams.

2. **Integration test — single change end-to-end**:
   ```
   devenv tasks run llama:server &
   sleep 5
   openspec-orch start <test-change>
   # wait for archive or question pause
   openspec-orch status
   openspec-orch stop
   ```
   Verify: (a) `graphos-out/orchestrator/*.log` contains artifact timestamps, (b) `metrics.jsonl` has events, (c) exit code is 0 on clean archive or 10 on question pause.

3. **Model isolation test**: Run `openspec-orch start <change>` with `OPENAI_API_KEY` set in the environment. Verify via network monitoring (`tcpdump` or `socat`) that zero requests leave `localhost:8080`.

4. **Verify gate test**: Create a change with an intentionally broken spec. Run `openspec-orch start <change>`. Verify the orchestrator does NOT archive — it should either remediate or fail after the retry budget.

5. **D devenv task test**: `devenv tasks run orchestrator:run` — verify the task runs the same Python script with `--all --no-health-check` args, confirming backward compatibility with the existing devenv task.

6. **Acceptance gates (cabal/build-test)**: Since the orchestrator is pure Python/dev-env tooling with no Haskell changes:
   - `cabal build` — passes (no Haskell changes)
   - `cabal test` — passes (no new test suites)
   - `devenv test` — validates devenv.nix syntax, script availability on PATH
   - `openspec validate --change add-qwen3-llamacpp-orchestrator` — validates spec conformance

## Iteration & Rollback

- **If artifact generation is too slow**: Increase `--timeout` or add per-artifact timeout based on artifact complexity (specs > design > tasks). Measure via `metrics.jsonl` — if median per-artifact time > 5 minutes, the prompt or model may need tuning.

- **If remediation rounds are high (>2 median)**: Feed the finding into the next PDCA cycle — refine the artifact prompt templates (in `openspec instructions`) and the verify-gate strictness. The `metrics.jsonl` format already captures remediation rounds per change.

- **If question-pause rate is too high**: Narrow the question-trigger heuristic in `orchestrate.py`'s `parse_opencode_stream()` (line 383) so only true decision points halt the loop, not routine clarifications.

- **If the orchestrator stalls on certain artifact types**: The `drive_change()` function (line 698) already handles the stuck case (line 716: "no ready artifact and change not complete"). Add logging for the specific artifact IDs that cause stalls and refine the opencode dispatch prompt accordingly.

- **Rollback**: Since the orchestrator is additive (no Haskell changes, no API changes), rollback is simply reverting the `devenv.nix` changes and `.envrc`/`.opencode/` additions. The Python `orchestrate.py` changes are already in the repo. No data migration or breaking changes.

## Migration Plan

1. **Add `.envrc`**: Create `.envrc` with default environment variables (`LLAMA_BASEURL`, `OPENCODE_MODEL`, `ORCHESTRATOR_LOG_DIR`, etc.) so the devenv shell provides sensible defaults via `direnv`.

2. **Create `.opencode/providers/llamacpp.json`**: Write the model provider configuration file that routes all requests to `http://localhost:8080/v1/chat/completions` with model `qwen3.6-35b-a3b`.

3. **Add `openspec-orch` scripts to `devenv.nix`**: Add `scripts.openspec-orch.exec` with `start`/`stop`/`status` subcommands. The `start` command runs the existing `orchestrate.py` in the background. Add `enterShell` hook to `mkdir -p graphos-out/orchestrator`.

4. **Add devenv task**: Add `"orchestrator:status"` task that runs `openspec-orch status` for devenv-based lifecycle management, consistent with `orchestrator:run`.

5. **Verify**: Run `devenv test` to validate scripts and PATH availability. Run `devenv tasks run orchestrator:run` to confirm the existing task still works. Manually test `openspec-orch start <change>` against a pilot change.

6. **No phased rollout needed**: The feature is opt-in (default-off via devenv flag). Existing `openspec` CLI commands and the Haskell library are untouched.

## Open Questions

1. **What happens when `opencode` changes its `--format json` event schema?** The `parse_opencode_stream()` function is resilient to unknown event types, but new event types could shift the text extraction logic. Consider versioning the event format or adding a compatibility layer.

2. **Should the `openspec-orch` wrapper support `--change` argument to the `start` subcommand, or should it use `ORCHESTRATOR_CHANGE` env var?** The Python script already accepts `<change>` as a positional argument. The wrapper could pass `"$@"` through for `start`, or use env vars for declarative configuration.

3. **How should multi-user dev environments handle PID file collisions?** If multiple developers share the same workspace directory, `orchestrator.pid` could collide. Consider using `<username>.pid` or a lock file pattern.

4. **Should the orchestrator support resume from a question pause across process restarts?** Currently, when stdin is not a TTY, the orchestrator exits with code 10 (PAUSED_QUESTIONS) and requires manual re-invocation. The question file persists answers across restarts, but the orchestrator does not automatically resume.

5. **What is the expected Qwen 3.6 model context window?** The `llama:server` task uses `--ctx-size 100768` (100K tokens). Artifact templates can be large (full spec + context + rules). If prompts exceed the context window, the orchestrator needs to truncate or split context.
