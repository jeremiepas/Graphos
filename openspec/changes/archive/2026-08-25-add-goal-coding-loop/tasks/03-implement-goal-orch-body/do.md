# 3.D Do: Implement the `goal-orch` orchestrator body

## Implementation plan
- Keep frontmatter unchanged.
- Replace the placeholder body with a structured agent prompt using the same tier structure as `coder-agent.md`:
  - `<tier level="1" ...>` Critical Operations
  - `<tier level="2" ...>` Core Workflow
  - `<tier level="3" ...>` Quality
  - `<conflict_resolution>` block
- Core workflow sections:
  1. **Clarify** — best-guess decompose, present list, confirm, offer `recommend`.
  2. **Manifest** — write `reqs-manifest.md` with id, title, status, completion summary; update after every worker report.
  3. **DAG + dispatch** — build dependency map, dispatch dep-free requirements in parallel via multiple `task` calls in one message.
  4. **Resume** — when worker asks a question, answer and re-invoke `task` with the prior `task_id`.
  5. **Acceptance** — after all requirements `completed`, re-check each, confirm test coverage, write implementation report.
  6. **Conditional e2e** — if task touches a frontend and `@playwright-mcp`/`agent-browser` is available, run e2e and capture screenshots; otherwise skip and note.
- Add explicit instructions to re-read `reqs-manifest.md` every cycle.

## Changes actually implemented
- Replaced the placeholder body of `.opencode/agent/core/goal-orch.md` with a full orchestrator workflow.
- Added sections: clarify, manifest, DAG, parallel dispatch, task_id resumption, acceptance, conditional e2e, final report.
- Added tiered-rules and conflict-resolution blocks mirroring `coder-agent.md` structure.
