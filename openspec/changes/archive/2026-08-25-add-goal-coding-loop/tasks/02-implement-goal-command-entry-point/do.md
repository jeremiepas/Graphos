# 2.D Do: Implement the `/goal` command entry point

## Implementation plan
- Keep the existing frontmatter `agent: goal-orch`.
- Replace the body with a short, thin instruction:
  1. Tell `goal-orch` to treat the user's text as the task for this run.
  2. Ask it to begin best-guess requirements clarification.
  3. End with the literal `$ARGUMENTS` placeholder so OpenCode substitutes the user's task text.
- No `model:` key, no `task(...)` calls, no plan/DAG/dispatch logic.

## Changes actually implemented
- Replaced the body of `.opencode/commands/goal.md` with a thin entry point.
- The body contains `$ARGUMENTS` exactly once and instructs `goal-orch` to begin clarification.
- No `task(...)`, DAG, or plan logic added.
