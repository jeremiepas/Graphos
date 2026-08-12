## Why

OpenSpec's PDCA workflow (proposal → specs → design → tasks → plan → do → check → act → archive) currently requires a human to invoke each `openspec` command manually, wait for the model, advance to the next artifact, answer questions, and finally verify/archive. This hand-driven loop is tedious, breaks focus, and leaves changes stalled mid-workflow. With a local Qwen 3.6 served by llama.cpp and opencode's automation hooks, we can run the entire PDCA loop as an autonomous dev-env service: advance artifacts unattended, surface only genuine decision points as questions, auto-verify, and archive when no remediation is needed — collapsing days of waiting into a single unattended run.

## What Changes

- **NEW** `openspec-orchestrator` dev-env service: a long-running loop that drives the OpenSpec PDCA artifact cycle for one (or many) changes using opencode driven by Qwen 3.6 served via llama.cpp.
- **NEW** opencode model provider wiring for `llama.cpp` (OpenAI-compatible `/v1/chat/completions` endpoint at `http://localhost:8080`) so the orchestrator agent uses the local Qwen 3.6 model — no cloud API, no token cost, fully offline.
- **NEW** artifact-stepping controller: parses `openspec status --change <name> --json`, finds the first `ready` artifact, runs `openspec instructions <id> --change <name>`, dispatches the artifact to opencode, writes the output, and repeats until all artifacts are `complete`.
- **NEW** question-pause semantics: when an artifact generation surfaces a question (opencode `question` tool or explicit `<QUESTION>` block in the artifact), the loop halts, emits the question to the dev-env console + a `questions/<change>.md` file, and waits for a user answer before resuming. No silent guessing on decisions.
- **NEW** auto-verify gate: after the last artifact, runs `openspec validate --change <name>` and `openspec verify --change <name>` (where available); if validation passes and no remediation items remain, auto-invokes `openspec archive --change <name>`.
- **NEW** remediation loop: if verify/check flags issues, the orchestrator feeds each finding back into the relevant artifact as a fix-up pass (bounded retry — default 3 rounds) before re-verifying. Only archives on a clean verify.
- **NEW** dev-env lifecycle hooks: `start` / `stop` / `status` commands so the service runs in the background of the dev shell (`shell.nix`), logs to `graphos-out/orchestrator/*.log`, and is restartable.
- **NEW** multi-change fan-out: orchestrator accepts a change name OR `--all` to drain every in-progress change in `openspec/changes/` that is not yet archived, one at a time (serial, to keep Qwen 3.6 context coherent).
- **BREAKING**: none. The orchestrator is additive; manual `openspec` commands still work. Default opt-in via a dev-env flag.

## Capabilities

### New Capabilities
- `openspec-orchestrator`: Autonomous PDCA loop driver — advances OpenSpec artifacts via opencode + Qwen 3.6/llama.cpp, pauses on questions, auto-verifies, archives on clean verify, retries remediation within a bounded budget.
- `llamacpp-model-provider`: opencode provider configuration for a local llama.cpp server running Qwen 3.6, exposing an OpenAI-compatible endpoint consumed by the orchestrator agent.

### Modified Capabilities
- `devenv-shell`: adds orchestrator service entry (start/stop/status) to the Nix dev shell so `nix-shell shell.nix` brings the loop up alongside existing tools.

## Impact

- **Code**: New `orchestrator/` directory (Infrastructure/dev-env tooling — not part of the Haskell Graphos library; a small driver script + opencode config). Touches `shell.nix` to register the service. No Domain/UseCase Haskell modules changed.
- **APIs**: Consumes `openspec` CLI (`status`, `instructions`, `validate`, `verify`, `archive`) and opencode's headless/automation interface. Consumes llama.cpp's OpenAI-compatible `/v1/chat/completions` endpoint.
- **Dependencies**: Requires a running `llama-server` (llama.cpp) with Qwen 3.6 GGUF loaded; no new Haskell dependencies. Requires `opencode` on PATH (already present in dev env).
- **Systems**: Writes logs and question files under `graphos-out/orchestrator/`. Reads/writes `openspec/changes/<name>/` artifacts. Invokes `openspec archive` on success — moves the change to `openspec/changes/archive/`.
- **Risk**: Autonomous archive is irreversible-by-design; mitigated by the verify gate and bounded remediation retries. Question-pause guarantees a human is in the loop for genuine decisions.

## PDCA Cycle

- **Plan**: Hypothesis — driving the full OpenSpec PDCA artifact cycle for a change with an autonomous Qwen 3.6/llama.cpp-backed opencode agent will reduce wall-clock time-to-archive from days (manual, human-gated) to hours (unattended, question-gated only), while keeping archive quality equal-or-better than manual runs. Success measured by: (a) ≥80% of simple changes archived without human intervention beyond answered questions; (b) zero archived changes that fail `openspec validate` post-hoc; (c) median artifacts-per-hour ≥ 2 across a 6-artifact PDCA chain; (d) remediation retry count median ≤ 1 per artifact.
- **Do**: Implement the `openspec-orchestrator` dev-env service + `llamacpp-model-provider` wiring (see specs/design/tasks). Run it against 3 seeded pilot changes (one trivial, one medium, one spec-heavy) end-to-end.
- **Check**: Collect per-change telemetry — artifact completion timestamps, question-pause count, verify pass/fail, remediation rounds, archive success — logged to `graphos-out/orchestrator/metrics.jsonl`. Compare against the Plan's four success criteria. Re-run `openspec validate --change <name>` on archived changes to confirm no regressions slipped through.
- **Act**: If ≥80% archival rate and zero post-archive validation failures hold, standardize: promote the orchestrator to default-on in `shell.nix` and document the workflow in `AGENTS.md`. If remediation rounds are high (>2 median), feed the finding into the next cycle: refine the artifact prompt templates and the verify-gate strictness, then re-run the pilot. If question-pause rate is too high, narrow the question-trigger heuristic so only true decision points halt the loop.