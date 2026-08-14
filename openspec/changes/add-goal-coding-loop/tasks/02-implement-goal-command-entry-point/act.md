# 2.A Act: Implement the `/goal` command entry point

## Verdict
PASS

## Summary
`.opencode/commands/goal.md` now has a thin body that routes to `goal-orch`, contains `$ARGUMENTS` exactly once, instructs clarification, and includes no dispatch logic.

## Standardization note
Command files should be thin entry points; loop orchestration belongs in the agent.

## Next step
Proceed to Task 3: implement the `goal-orch` orchestrator body.
