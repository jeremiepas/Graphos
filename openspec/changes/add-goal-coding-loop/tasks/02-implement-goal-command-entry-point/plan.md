# 2.P Plan: Implement the `/goal` command entry point

## Scope
Replace the placeholder body of `.opencode/commands/goal.md` with a thin command body that:
- contains the `$ARGUMENTS` placeholder exactly once,
- instructs `goal-orch` to treat the following as the user's task and begin requirements clarification,
- does no parsing, planning, or dispatch itself.

## Affected area
- `.opencode/commands/goal.md`

## Risk
Putting loop logic in the command couples it to the command loader and makes the orchestrator harder to reuse. Keep it thin.

## Check criteria
(a) Body contains `$ARGUMENTS` exactly once.
(b) Body mentions beginning clarification / requirements.
(c) Body contains no `task(...)` call, no DAG logic, no plan logic.
