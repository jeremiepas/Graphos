## Context

Graphos developers today drive every change manually: they author an OpenSpec change, delegate subtasks to `CoderAgent`/`TestEngineer` one at a time, and verify each by hand. The `dataleadsfuture` article "No Plugins Needed, I Built a Fully Automated Coding Loop in OpenCode" demonstrates a different shape — a `/goal` command plus an orchestrator/worker agent pair that turns one user request into an autonomous requirements → plan → implement → verify loop. This change ports that loop into this workspace, adapted to Graphos's existing agent-scaffolding conventions (`.opencode/agent/core/` for primary agents, `.opencode/agent/subagents/` for subagents, frontmatter + tiered-rules body style established by `coder-agent.md`).

Current state:
- `.opencode/commands/` does not exist — no user-defined commands are registered.
- Primary agents live in `.opencode/agent/core/` (`openagent.md`, `graphos-navigator.md`).
- Subagents live in `.opencode/agent/subagents/<category>/` (`code/`, `core/`, `development/`, `system-builder/`).
- Model selection is configured per-workspace in `opencode.json` (currently `llama/qwen3.6-35b-a3b`); agents do not hardcode models.

Constraints:
- This is a dev-env tooling change. The HARD RULE of Graphos clean architecture (Domain has ZERO IO, UseCase has ZERO IO implementation, all side effects in Infrastructure) applies to the Haskell codebase — this change touches none of it. The new files are Markdown agent/command definitions under `.opencode/`; they have no Domain/UseCase/Infrastructure layering.
- The loop must reuse OpenCode's existing `task` tool semantics (the `task_id` resumption pattern) rather than inventing a new IPC mechanism.
- The loop is model-agnostic: no `model:` key in agent frontmatter, so the user's `opencode.json` selection governs.

Stakeholders: any Graphos developer who runs `/goal` in this workspace.

## Goals / Non-Goals

**Goals:**
- Add a `/goal` command that bootstraps an autonomous coding loop with one user line.
- Add `goal-orch` primary agent: clarify → manifest → DAG → parallel dispatch → verify → report.
- Add `goal-worker` subagent: plan → ask-on-ambiguity → implement + test → report.
- Fit Graphos agent-file conventions (frontmatter + tiered rules + conflict-resolution block).
- Keep the loop model-agnostic.
- Halt and surface on ambiguity (questions to orchestrator, never silent failures).

**Non-Goals:**
- The article's `/loop` command (scheduled external-feedback ingestion from GitHub/Jira). Out of scope; future change.
- Integrating `goal-worker` with `CoderAgent`/`TestEngineer`/`ContextScout`. Per the scope decision, `goal-worker` is standalone.
- Any change to Graphos Haskell sources, the pipeline, or `graphos-out/`.
- Auto-archiving or auto-PR creation. The loop produces a report; merging is human.
- Installing `@playwright-mcp`/`agent-browser`/`@observer`. E2E verification is conditional on those being present.

## Decisions

### Decision 1: File layout — primary in `core/`, worker in `subagents/code/`

| Choice | Rationale |
|--------|-----------|
| `goal-orch.md` in `.opencode/agent/core/` | Matches `openagent.md`, `graphos-navigator.md` — primary agents live in `core/`. |
| `goal-worker.md` in `.opencode/agent/subagents/code/` | Matches `coder-agent.md`, `build-agent.md`, `test-engineer.md` — code-executing subagents live in `subagents/code/`. |
| `/goal` command in `.opencode/commands/goal.md` | New directory; OpenCode's documented Markdown-command location. |
| `reqs-manifest.md` at workspace root (fallback `.tmp/goal/<run-id>/`) | Root mirrors the article; fallback avoids failure on read-only roots. |

**Alternatives considered:**
- *Worker in `subagents/core/`* — rejected; the worker writes code, so `code/` is the semantically correct category.
- *Manifest under `.opencode/state/`* — rejected; hidden from the user. The manifest is an acceptance record the user inspects, so it stays visible at the root.

### Decision 2: Orchestrator is a primary agent, not a subagent

`goal-orch` must dispatch `goal-worker` via the `task` tool. OpenCode primary agents can call subagents; subagents cannot call subagents. Therefore `goal-orch` MUST be `mode: primary`. Consequence: it appears in the OpenCode agent list (acceptable, matches the article).

**Alternatives considered:**
- *Make `goal-orch` a subagent and have `/goal` itself drive the loop* — rejected; commands are prompt text, not stateful controllers. The orchestrator logic must live in an agent.

### Decision 3: `task_id` resumption for worker questions

When `goal-worker` returns a question, `goal-orch` re-invokes `task` with the prior `task_id` and its answer. This resumes the worker's sub-session with full context intact, instead of starting a fresh session that would need the requirement re-explained.

**Alternatives considered:**
- *Have the worker ask the user directly* — rejected; violates the article's "worker asks orchestrator, not user" rule and would interrupt the user in autonomous mode.
- *Re-dispatch a new worker session with the answer prepended* — rejected; loses the worker's intermediate context (plan, partial code exploration), forcing re-work.

### Decision 4: DAG-based parallel dispatch

`goal-orch` computes a dependency DAG over confirmed requirements and dispatches dep-free requirements in parallel (multiple `task` calls in one message). Dependent requirements wait.

**Alternatives considered:**
- *Strict sequential dispatch* — rejected; the article shows parallelism cuts wall-clock time significantly on multi-requirement tasks (PRD §16.2 scalability).
- *Dispatch all at once, let workers block* — rejected; would start workers that cannot make progress and waste context.

### Decision 5: Model pinned to `gemma/gemma4-moe` @ 64k context

Both agents' frontmatter set `model: gemma/gemma4-moe`. `opencode.json` declares the `gemma` provider's `gemma4-moe` model with `limit.context: 65536` (the endpoint's native context is 256k; 64k is the configured working window per user decision 2026-08-12). This overrides the earlier "model-agnostic" choice — the user explicitly requested the gemma MoE model for this loop, and pinning it makes the loop reproducible across workspaces that have the `gemma` provider configured.

**Alternatives considered:**
- *Model-agnostic (no `model:` key)* — rejected by user decision 2026-08-12; the user wants gemma MoE for the loop.
- *Hardcode DeepSeek-V4-Pro/Flash per the article* — rejected; this workspace's `gemma` provider is the chosen runtime, not DeepSeek.
- *Pin `goal-orch` to a strong-reasoning model and `goal-worker` to a fast model* — rejected; the user asked for gemma MoE for all (the 26B A4B model handles both roles in this setup).

### Decision 6: E2E verification is conditional, not required

`goal-orch` SHALL attempt `@playwright-mcp`/`agent-browser` for frontend tasks, but if neither is installed it SHALL skip e2e, note the skip, and still complete. This keeps the loop from failing in workspaces without browser tooling.

**Alternatives considered:**
- *Require `@playwright-mcp` as a precondition for frontend `/goal`* — rejected; would block the loop on an unrelated installation. The loop's contract is code + unit tests; e2e is a bonus.

### Decision 7: Standalone worker (per scope decision)

`goal-worker` does not declare `CoderAgent`/`TestEngineer`/`ContextScout` as dependencies. It implements, tests, and reports on its own. This matches the article's design and the user's "standalone" scope choice.

**Alternatives considered:**
- *Reuse CoderAgent as the worker* — rejected by scope decision; would couple the loop to an existing agent whose prompt is tuned for Graphos-Haskell subtasks, not general `/goal` tasks.
- *Hybrid: worker calls ContextScout first* — rejected by scope decision; keep the worker self-contained for this iteration.

## Risks / Trade-offs

- [Loop runs long, context grows, orchestrator forgets steps] → Mitigation: `reqs-manifest.md` is the durable state; `goal-orch` re-reads it each cycle rather than relying on session memory. Per-requirement documentation notes (required by the goal-orchestrator spec) provide a recovery surface.
- [Worker makes a bad technology choice the orchestrator rubber-stamps] → Mitigation: the plan-first rule surfaces the choice before code is written; the orchestrator's sign-off is an explicit gate, not auto-approval.
- [Parallel workers edit overlapping files] → Mitigation: the DAG prevents dispatching requirements with dependencies before their predecessors complete. Independent requirements are assumed to touch disjoint surface area; if they collide, the acceptance pass catches it.
- [`task_id` resumption semantics change across OpenCode versions] → Mitigation: the loop degrades gracefully — a failed resume becomes a fresh dispatch with the answer prepended; the manifest still tracks progress.
- [Model-agnostic design means a weak model produces a poor loop] → Mitigation: `proposal.md` documents recommended model tiers (strong-reasoning for orch, fast for worker). The user is responsible for matching the model to the task.
- [`.opencode/commands/` is new; a future OpenCode update changes command loading] → Mitigation: the command is a thin entry point; all logic is in the agents, which use the more stable agent-file mechanism.
- [No e2e skill installed → silent skip hides frontend bugs] → Mitigation: the skip is recorded in the implementation report; the user sees it and can install the skill and re-run.

## Verification Strategy (Check)

This change adds Markdown files, not Haskell. There is no `cabal build`/`cabal test` surface. Verification is structural and manual:

1. **Structural validation**: `openspec validate --change add-goal-coding-loop` MUST report no violations.
2. **File presence and frontmatter**: each of the three files exists at its resolved path with the required frontmatter keys (`name`, `description`, `mode`, `temperature`, `permission`, and — for the command — `agent`). No `model:` key in either agent file. Verified by reading the files after implementation.
3. **OpenCode load**: restart OpenCode; `goal-orch` appears in the agent list; `/goal` is available as a command.
4. **Trivial `/goal` run**: run `/goal Write a Fibonacci calculation script with the best possible performance.`; confirm (a) `goal-orch` opens a best-guess clarification conversation, (b) a `reqs-manifest.md` is written on confirmation, (c) `goal-worker` is dispatched, plans, implements, tests, and reports, (d) `goal-orch` updates the manifest and emits an implementation report.
5. **Mid-sized `/goal` run**: run `/goal Build a playable Tower of Hanoi web game.`; confirm the full loop completes with an implementation report (and, if a browser skill is available, e2e screenshots).
6. **No regression**: `OpenAgent` and `CoderAgent` still load and function (the new files do not modify them).
7. **Graphos build sanity (unchanged)**: `cabal build` still succeeds in the dev shell, confirming the change touched no Haskell sources.

Acceptance gate: 1–6 MUST pass. 7 confirms non-regression.

## Iteration & Rollback (Act)

If Check fails:
- **Frontmatter/structural issue**: edit the offending agent file directly; re-run `openspec validate`.
- **`/goal` does not appear**: confirm `.opencode/commands/` is a directory (not a file) and `goal.md` is inside it; restart OpenCode.
- **Loop stalls on a worker question**: inspect `reqs-manifest.md` for the `blocked` status; the orchestrator should have surfaced the question. If it did not, strengthen the "halt and surface" rule in `goal-orch.md` and re-run.
- **Parallel workers collide**: tighten the DAG rule so requirements sharing a file prefix are serialized; re-run.

Rollback: delete `.opencode/commands/goal.md`, `.opencode/agent/core/goal-orch.md`, `.opencode/agent/subagents/code/goal-worker.md`, and any `reqs-manifest.md` / `.tmp/goal/`. No other files are touched, so rollback is complete and reversible.

Standardization for the next PDCA cycle:
- If manual runs succeed, extract the loop's conventions (best-guess clarification, manifest as acceptance record, `task_id` resumption, DAG parallelism) into a workspace note under `.opencode/skills/` so future agents inherit them.
- Feed learnings — which model handles orchestration best, which task shapes stall the loop — into the next iteration: a `/loop` command for scheduled external-feedback ingestion (the article's third loop, explicitly out of scope here).