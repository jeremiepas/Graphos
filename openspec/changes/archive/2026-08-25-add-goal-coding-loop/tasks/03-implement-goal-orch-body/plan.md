# 3.P Plan: Implement the `goal-orch` orchestrator body

## Scope
Replace the placeholder body of `.opencode/agent/core/goal-orch.md` with the full autonomous loop workflow, while keeping the same frontmatter.

## Affected area
- `.opencode/agent/core/goal-orch.md` (body only)

## Check criteria
(a) Body defines best-guess clarification phase (decompose → present list → confirm; no open-ended questions; offer `recommend` option).
(b) Body defines `reqs-manifest.md` authoring on confirmation and status updates on each worker report (fields: id, title, status `pending`/`in_progress`/`completed`/`blocked`, completion summary).
(c) Body defines DAG construction over requirements and parallel dispatch of dep-free requirements via multiple `task` calls in one message.
(d) Body defines `task_id` resumption for worker questions (re-invoke `task` with prior `task_id`, never the user).
(e) Body defines acceptance pass after all requirements `completed` and the implementation report.
(f) Body defines conditional e2e via `@playwright-mcp`/`agent-browser` (skip + note if absent).
(g) Tiered-rules and `<conflict_resolution>` blocks present, mirroring `coder-agent.md` structure.

## Risk
Orchestrator may forget manifest state over a long run — mitigate by requiring re-read of `reqs-manifest.md` each cycle.
