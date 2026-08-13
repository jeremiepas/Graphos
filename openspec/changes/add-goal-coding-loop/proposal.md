## Why

Graphos developers currently drive every code change by hand — writing specs, delegating to CoderAgent/TestEngineer one subtask at a time, and verifying results manually. This is the slow, high-touch path Andrew Ng's "Loop Engineering" post (and the `dataleadsfuture` OpenCode writeup) argues against: the agent should iterate against a measurable target on its own, not wait for the engineer to push every step. There is no `/goal` command in this workspace (`.opencode/commands/` does not exist yet) and no orchestrator/worker pair that turns a single user request into an autonomous requirements → plan → implement → verify loop. This change adds that loop so a developer can write one line (`/goal <task>`) and let the agent run, while still fitting Graphos's existing agent scaffolding conventions (`.opencode/agent/core/` + `.opencode/agent/subagents/`).

## What Changes

- **Add `/goal` command** — new `.opencode/commands/goal.md` that delegates to `goal-orch` via the `$ARGUMENTS` placeholder (PRD §13 agent-scaffolding extension; same Markdown-command mechanism already used by OpenCode skills).
- **Add `goal-orch` primary agent** — `.opencode/agent/core/goal-orch.md`. Orchestrator: clarifies requirements with the user (best-guess confirm, not open-ended), authors `reqs-manifest.md`, builds a dependency DAG over requirements, dispatches `goal-worker` instances in parallel for dep-free requirements, verifies completion against the manifest, runs acceptance + end-to-end checks, writes an implementation report. Pinned to `gemma/gemma4-moe` @ 64k context (per user decision 2026-08-12).
- **Add `goal-worker` subagent** — `.opencode/agent/subagents/code/goal-worker.md`. Standalone executor: plans before coding (tech stack, change surface, unit + edge-case tests), asks `goal-orch` on technology-choice ambiguity via session return (never the user), implements one requirement, returns an implementation report. Uses modern package managers (`uv`, `pnpm`) per the article. Does not depend on CoderAgent/TestEngineer — standalone by design (per scope decision). Pinned to `gemma/gemma4-moe` @ 64k context (per user decision 2026-08-12).
- **Add `reqs-manifest.md` contract** — a Markdown file authored by `goal-orch` in the workspace root (or `.tmp/goal/<run-id>/`) that tracks each requirement's status (pending / in_progress / completed / blocked) and serves as the acceptance record across the long-running loop.
- **Reuse OpenCode `task_id` resumption** — `goal-orch` answers `goal-worker` questions by re-invoking the `task` tool with the prior `task_id`, resuming the sub-session in context (per article's "foreman/worker reference number" pattern).
- **Optional end-to-end verification** — when the task touches a frontend, `goal-orch` SHALL call `@playwright-mcp` / `agent-browser` skill during final review and capture screenshots as proof.

No existing Graphos pipeline code, Domain/UseCase/Infrastructure modules, or Haskell sources change. This is a dev-env tooling addition (`.opencode/` only), consistent with `agent-scaffolding` (PRD §13) and the `openspec-orchestrator` precedent.

## Capabilities

### New Capabilities
- `goal-orchestrator`: The orchestrator agent — user clarification, `reqs-manifest.md` authoring, DAG dependency mapping, parallel worker dispatch via `task_id` resumption, acceptance verification, and end-to-end test orchestration.
- `goal-worker`: The worker subagent — plan-first implementation of a single requirement, ask-orchestrator-on-ambiguity via session return, implement + unit tests + edge cases, return an implementation report.
- `goal-command`: The `/goal` entry-point command — frontmatter `agent: goal-orch`, `$ARGUMENTS` placeholder, entry contract that bootstraps the loop.

### Modified Capabilities
<!-- None — no existing spec-level behavior changes. -->

## Impact

- **Code**: None. No Haskell sources, no Domain/UseCase/Infrastructure modules. All additions live under `.opencode/`.
- **Files added**: `.opencode/commands/goal.md`, `.opencode/agent/core/goal-orch.md`, `.opencode/agent/subagents/code/goal-worker.md`. The `.opencode/commands/` directory is created by this change.
- **Runtime artifacts**: `reqs-manifest.md` (workspace root or `.tmp/goal/<run-id>/`), per-requirement documentation notes, optional e2e screenshots — all git-ignored / ephemeral.
- **Dependencies**: No new Haskell or npm dependencies. Relies on existing OpenCode `task` tool semantics and (optional, frontend tasks only) the `@playwright-mcp` / `agent-browser` skill and `@observer` agent, none of which are installed by this change.
- **APIs**: None. The user-facing surface is the `/goal` command string contract.
- **Compatibility**: Additive. Existing agents (OpenAgent, CoderAgent, TestEngineer, ContextScout) are untouched and remain usable. No **BREAKING** changes.

## PDCA Cycle

- **Plan**: Add a `/goal`-driven Loop Engineering loop to this workspace. Target: a developer can run `/goal <task>` for a small, well-scoped task and have `goal-orch` autonomously clarify → manifest → plan → implement → verify → report without further human input, completing within ~30 minutes for a Fibonacci-script-sized task and within ~3 hours for a Tower-of-Hanoi-sized task (article's measured baselines). Success is measured by (a) `openspec validate --change add-goal-coding-loop` passing, (b) a manual `/goal` run on a trivial task producing a populated `reqs-manifest.md` and a completion report, and (c) no regression to existing agents (OpenAgent/CoderAgent still load). Aligns with PRD §16.3 reliability (graceful error handling — the loop MUST halt and surface questions rather than fail silently) and PRD §16.2 scalability (parallel worker dispatch keeps wall-clock time bounded as requirements grow).
- **Do**: Implement the three files listed in *What Changes* per the specs and design; follow Graphos agent-file conventions (frontmatter with `name`/`description`/`mode`/`temperature`/`permission`/`model` block, explicit tiered rules). Pin both agents to `gemma/gemma4-moe` @ 64k context (per user decision 2026-08-12). See `specs/` and `design.md`.
- **Check**: (1) `openspec validate --change add-goal-coding-loop` reports no violations. (2) Restart OpenCode, run `/goal Write a Fibonacci calculation script with the best possible performance.` and confirm: `goal-orch` opens a clarification conversation, authors `reqs-manifest.md`, dispatches `goal-worker`, and returns a completion report. (3) Run `/goal Build a playable Tower of Hanoi web game.` and confirm the loop completes with an implementation report (and, if `@playwright-mcp` is available, e2e screenshots). (4) Verify OpenAgent and CoderAgent still appear and function. All four checks MUST pass.
- **Act**: Standardize the loop's patterns (best-guess clarification, `reqs-manifest.md` as acceptance record, `task_id` resumption for worker questions) into a workspace convention note under `.opencode/skills/` if the manual runs succeed. Feed learnings (which model handles orchestration best, which tasks stall the loop) into the next iteration — a future `/loop` command for scheduled external-feedback ingestion (the article's third loop, out of scope here).