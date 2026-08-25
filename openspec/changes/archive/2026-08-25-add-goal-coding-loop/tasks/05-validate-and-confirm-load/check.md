# 5.C Check: Validate the change and confirm load

## Verification plan
Run the two OpenSpec CLI commands and inspect outputs.

## Criteria / results
- [x] (a) `openspec validate` reports `add-goal-coding-loop` valid with no issues — PASS
- [x] (b) `openspec status` shows `proposal`, `specs`, `design`, `tasks` all `done` — PASS
- [ ] (c) `goal-orch` and `/goal` availability after OpenCode restart — PENDING user restart/confirmation

## Note
Validation had to use `openspec validate --changes --json` because the `validate` command does not accept a single `--change` option; the JSON summary confirms `add-goal-coding-loop` is valid with no violations.
