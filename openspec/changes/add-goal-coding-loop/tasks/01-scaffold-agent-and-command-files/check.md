# 1.C Check: Scaffold agent and command files

## Verification plan
Read each new file and run `ls -ld .opencode/commands/` plus inspect `opencode.json`.

## Criteria / results
- [x] (a) `.opencode/commands/goal.md` exists with `agent: goal-orch` and `$ARGUMENTS` — PASS
- [x] (b) `.opencode/agent/core/goal-orch.md` exists with `mode: primary` and `model: gemma/gemma4-moe` — PASS
- [x] (c) `.opencode/agent/subagents/code/goal-worker.md` exists with `mode: subagent` and `model: gemma/gemma4-moe` — PASS
- [x] (d) `.opencode/commands/` is a directory — PASS
- [x] (e) `opencode.json` has `gemma.models.gemma4-moe.limit.context == 65536` — PASS

## Verdict
All five criteria PASS. Task 1 scaffolding complete.
