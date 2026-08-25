# 1.D Do: Scaffold agent and command files

## Implementation plan
- Write `.opencode/commands/goal.md` with frontmatter `agent: goal-orch` and a minimal body ending with `$ARGUMENTS`.
- Write `.opencode/agent/core/goal-orch.md` with frontmatter keys `name`, `description`, `mode: primary`, `temperature: 0`, `model: gemma/gemma4-moe`, and a minimal `permission` block; add placeholder tiered-rules body.
- Write `.opencode/agent/subagents/code/goal-worker.md` with frontmatter keys `name`, `description`, `mode: subagent`, `temperature: 0`, `model: gemma/gemma4-moe`, and a minimal `permission` block; add placeholder tiered-rules body.
- Verify `opencode.json` already declares gemma4-moe @ 65536 — no edit needed.

## Changes actually implemented
- Created `.opencode/commands/goal.md` with `agent: goal-orch` and `$ARGUMENTS`.
- Created `.opencode/agent/core/goal-orch.md` with `mode: primary`, `model: gemma/gemma4-moe`, and tiered-rules placeholder body.
- Created `.opencode/agent/subagents/code/goal-worker.md` with `mode: subagent`, `model: gemma/gemma4-moe`, and tiered-rules placeholder body.
- Confirmed `.opencode/commands/` is a directory and `opencode.json` already declares `gemma4-moe` with `limit.context: 65536`; no edit needed.
