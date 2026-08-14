# 4.P Plan: Implement the `goal-worker` subagent body

## Scope
Replace the placeholder body of `.opencode/agent/subagents/code/goal-worker.md` with the full standalone worker workflow, keeping the existing frontmatter.

## Affected area
- `.opencode/agent/subagents/code/goal-worker.md` (body only)

## Check criteria
(a) Body defines plan-first: tech stack, change surface, unit + edge-case test approach, sent to `goal-orch` for sign-off before feature code.
(b) Body defines ask-orchestrator-on-ambiguity via session return (never the user).
(c) Body defines implement + unit tests + edge cases.
(d) Body defines returning an implementation report to `goal-orch`.
(e) Body mandates `uv` for Python and `pnpm` for JS/TS (not global `pip`/`npm`).
(f) Standalone — no `task:` allowlist entry referencing `CoderAgent`/`TestEngineer`/`ContextScout`.
(g) Tiered-rules and `<conflict_resolution>` blocks present, mirroring `coder-agent.md`.

## Risk
Worker may guess on technology choices — mitigate by plan-first + ask-orchestrator gates.
