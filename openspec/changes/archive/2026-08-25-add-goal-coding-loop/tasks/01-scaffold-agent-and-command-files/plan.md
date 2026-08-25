# 1.P Plan: Scaffold agent and command files

## Scope
Create the three files required to bootstrap the `/goal` loop:
1. `.opencode/commands/goal.md` — command definition with `agent: goal-orch` and `$ARGUMENTS`
2. `.opencode/agent/core/goal-orch.md` — primary orchestrator agent with `mode: primary`, `model: gemma/gemma4-moe`
3. `.opencode/agent/subagents/code/goal-worker.md` — subagent worker with `mode: subagent`, `model: gemma/gemma4-moe`

Also ensure `.opencode/commands/` is a directory and `opencode.json` declares `gemma4-moe` with 64k context.

## Affected areas
- `.opencode/commands/goal.md` (new)
- `.opencode/agent/core/goal-orch.md` (new)
- `.opencode/agent/subagents/code/goal-worker.md` (new)
- `opencode.json` (existing, no edit needed — already declares gemma4-moe @ 65536)

## Risks
- Wrong frontmatter key/values prevent OpenCode from loading the agents.
- `model:` key format must match the provider model id (`gemma/gemma4-moe`).
- Command frontmatter uses `agent:` (not `model:`).

## Check criteria
(a) `.opencode/commands/goal.md` exists with `agent: goal-orch` and body containing `$ARGUMENTS`.
(b) `.opencode/agent/core/goal-orch.md` exists with `mode: primary`, `model: gemma/gemma4-moe`, and tiered-rules body.
(c) `.opencode/agent/subagents/code/goal-worker.md` exists with `mode: subagent`, `model: gemma/gemma4-moe`, and tiered-rules body.
(d) `.opencode/commands/` is a directory.
(e) `opencode.json` declares `gemma.models.gemma4-moe.limit.context` equal to `65536`.
