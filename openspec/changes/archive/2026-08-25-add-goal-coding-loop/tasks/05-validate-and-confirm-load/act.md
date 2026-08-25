# 5.A Act: Validate the change and confirm load

## Verdict
PASS on structural validation and status; load confirmation requires an OpenCode restart.

## Summary
- `openspec validate --changes --json` reports `add-goal-coding-loop` as valid with zero issues.
- `openspec status --change add-goal-coding-loop` shows `proposal`, `specs`, `design`, `tasks` all `done`.
- The new files are in place; OpenCode must be restarted to load `goal-orch` and register `/goal`.

## Standardization note
Record: OpenSpec `validate` does not accept `--change`; use `--changes --json` and filter by id.

## User action required
1. **Restart OpenCode** in this workspace.
2. Select the primary agent `goal-orch`.
3. Type `/goal` and confirm the command appears in the command list.

## Next step
Task 6 (manual `/goal` smoke test) depends on the user confirming load. If the user confirms now, we can proceed; otherwise this is a natural pause point.
