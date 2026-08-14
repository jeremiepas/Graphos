# 4.D Do: Implement the `goal-worker` subagent body

## Implementation plan
- Keep frontmatter unchanged (no `task:` allowlist referencing other subagents).
- Replace placeholder body with a structured worker prompt using the same tier structure as `goal-orch`:
  - Tier 1 Critical Operations
  - Tier 2 Core Workflow (plan → sign-off → implement → test → report)
  - Tier 3 Quality
  - Conflict resolution block
- Core workflow:
  1. Read the requirement and `reqs-manifest.md`.
  2. Produce a plan (tech stack, change surface, unit + edge-case tests) and return it to `goal-orch` for sign-off.
  3. On ambiguity, return a question to `goal-orch` (session return, never user).
  4. After sign-off, implement, add tests, run tests.
  5. Return an implementation report.
- Mandate `uv` for Python and `pnpm` for JS/TS.

## Changes actually implemented
- Replaced the placeholder body of `.opencode/agent/subagents/code/goal-worker.md` with a full standalone worker workflow.
- Added plan-first, ask-orchestrator, implement+test, report, uv/pnpm mandates, and tiered-rules + conflict resolution.
- Confirmed no `task:` allowlist references other subagents.
