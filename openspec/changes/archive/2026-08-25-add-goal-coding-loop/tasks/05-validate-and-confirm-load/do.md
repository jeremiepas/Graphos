# 5.D Do: Validate the change and confirm load

## Implementation plan
- Run `openspec validate --change add-goal-coding-loop`.
- Run `openspec status --change add-goal-coding-loop`.
- Print both outputs.
- Provide a one-line restart instruction and the two manual checks (select `goal-orch`, type `/goal`).

## Changes actually implemented
- Ran `openspec validate --changes --json`; `add-goal-coding-loop` is valid with no issues.
- Ran `openspec status --change add-goal-coding-loop --json`; all four artifacts (`proposal`, `specs`, `design`, `tasks`) are `done`.
- Validation does not support a single `--change` flag, so `--changes --json` was used and the relevant entry is recorded.
