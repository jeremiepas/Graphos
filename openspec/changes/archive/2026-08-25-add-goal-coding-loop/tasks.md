## 1. Scaffold agent and command files

- [x] 1.P Plan: Create the three files at their resolved paths with correct frontmatter. Check criteria: (a) `.opencode/commands/goal.md` exists with frontmatter `agent: goal-orch` and body containing `$ARGUMENTS`; (b) `.opencode/agent/core/goal-orch.md` exists with `mode: primary`, `model: gemma/gemma4-moe`, and tiered-rules body; (c) `.opencode/agent/subagents/code/goal-worker.md` exists with `mode: subagent`, `model: gemma/gemma4-moe`, and tiered-rules body; (d) `.opencode/commands/` is a directory; (e) `opencode.json` declares `gemma4-moe` with `limit.context: 65536`. No code logic yet — just the scaffolding shells. Affected areas: `.opencode/` and `opencode.json`. Risk: wrong frontmatter key/values will prevent OpenCode from loading the agents.
- [x] 1.D Do: Create `.opencode/commands/` directory; write the three files with frontmatter only and a minimal body placeholder. Mirror the frontmatter key set of `.opencode/agent/subagents/code/coder-agent.md` (`name`, `description`, `mode`, `temperature`, `permission`) for the two agent files; for the command, `agent` plus a body with `$ARGUMENTS`.
- [x] 1.C Check: Read each file back. Verify (a) command file frontmatter has `agent: goal-orch` and body contains the literal `$ARGUMENTS`; (b) `goal-orch.md` frontmatter `mode` is `primary` and `model` is `gemma/gemma4-moe`; (c) `goal-worker.md` frontmatter `mode` is `subagent` and `model` is `gemma/gemma4-moe`; (d) `.opencode/commands/` is a directory (use `ls -ld`); (e) `opencode.json` has `gemma.models.gemma4-moe.limit.context` equal to `65536`. Record PASS/FAIL per criterion.
- [x] 1.A Act: If all four criteria PASS, this task PASSES. If any FAIL, fix the offending frontmatter/body and re-run Check. Standardize the frontmatter key set as the workspace convention for future agent files.

### Attempt history (1)

<!-- empty unless a retry is needed -->

## 2. Implement the `/goal` command entry point

- [x] 2.P Plan: Fill `.opencode/commands/goal.md` body so `/goal <task text>` sends a message to `goal-orch` with the substituted task. Check criteria: (a) body contains the `$ARGUMENTS` placeholder exactly once; (b) body instructs the orchestrator to begin its clarification phase; (c) body explicitly does no parsing/planning/dispatch itself (thin entry point, per goal-command spec). Affected area: `.opencode/commands/goal.md`. Risk: putting loop logic in the command couples it to the command loader.
- [x] 2.D Do: Write the command body: a short instruction telling `goal-orch` to treat the following as the user's task and begin requirements clarification; then `$ARGUMENTS`. No `model:` key. No dispatch code.
- [x] 2.C Check: Read the file. Verify (a) `$ARGUMENTS` appears once; (b) the body mentions beginning clarification / requirements; (c) the body contains no `task(...)` call, no DAG logic, no plan logic. Record PASS/FAIL per criterion.
- [x] 2.A Act: If PASS, task PASSES. If the body drifted into dispatch logic, strip it back to the thin entry point and re-run Check.

### Attempt history (2)

## 3. Implement the `goal-orch` orchestrator body

- [x] 3.P Plan: Fill `.opencode/agent/core/goal-orch.md` body with the orchestrator workflow. Check criteria: (a) body defines the best-guess clarification phase (decompose task → present list → confirm; no open-ended questions; offer a `recommend` option); (b) body defines `reqs-manifest.md` authoring on confirmation and status update on each worker report (fields: id, title, status `pending`/`in_progress`/`completed`/`blocked`, completion summary); (c) body defines DAG construction over requirements and parallel dispatch of dep-free requirements via multiple `task` calls in one message; (d) body defines `task_id` resumption for worker questions (re-invoke `task` with prior `task_id`, never the user); (e) body defines the acceptance pass after all requirements `completed` and the implementation report; (f) body defines conditional e2e via `@playwright-mcp`/`agent-browser` (skip + note if absent); (g) tiered-rules and `<conflict_resolution>` blocks present, mirroring `coder-agent.md` structure. Affected area: `.opencode/agent/core/goal-orch.md`. Risk: orchestrator that forgets the manifest over long runs — mitigate by requiring re-read of `reqs-manifest.md` each cycle.
- [x] 3.D Do: Write the orchestrator body covering all seven criteria. Reuse `coder-agent.md`'s tier structure (`<tier level="1" ...>` Critical Operations, `<tier level="2" ...>` Core Workflow, `<tier level="3" ...>` Quality). Add a `<conflict_resolution>` block. Set `model: gemma/gemma4-moe` in frontmatter. Reference the goal-orchestrator spec scenarios as the behavior contract.
- [x] 3.C Check: Read the file. For each criterion (a)–(g), verify the corresponding instruction is present in the body. Record PASS/FAIL per criterion. Do not invent new criteria.
- [x] 3.A Act: If all seven PASS, task PASSES. If any are missing, add the missing instruction and re-run Check. Standardize the tiered-rules body style for the worker task (Task 4) to match.

### Attempt history (3)

## 4. Implement the `goal-worker` subagent body

- [x] 4.P Plan: Fill `.opencode/agent/subagents/code/goal-worker.md` body with the worker workflow. Check criteria: (a) body defines plan-first: tech stack, change surface, unit + edge-case test approach, sent to `goal-orch` for sign-off before feature code; (b) body defines ask-orchestrator-on-ambiguity via session return (never the user); (c) body defines implement + unit tests + edge cases; (d) body defines returning an implementation report to `goal-orch`; (e) body mandates `uv` for Python and `pnpm` for JS/TS (not global `pip`/`npm`); (f) standalone — does not declare `CoderAgent`/`TestEngineer`/`ContextScout` as dependencies in frontmatter `task:` allowlist; (g) tiered-rules and `<conflict_resolution>` blocks present, mirroring `coder-agent.md`. Affected area: `.opencode/agent/subagents/code/goal-worker.md`. Risk: worker guesses on technology choices — mitigate by the plan-first + ask-orchestrator gates.
- [x] 4.D Do: Write the worker body covering all seven criteria. Use the same tier structure as Task 3's orchestrator for consistency. Set `model: gemma/gemma4-moe` in frontmatter. Do not add a `task:` allowlist referencing other subagents (standalone per scope decision). Reference the goal-worker spec scenarios as the behavior contract.
- [x] 4.C Check: Read the file. For each criterion (a)–(g), verify the corresponding instruction is present. Confirm frontmatter `model` is `gemma/gemma4-moe` and no `task:` allowlist entry references CoderAgent/TestEngineer/ContextScout. Record PASS/FAIL per criterion.
- [x] 4.A Act: If all seven PASS, task PASSES. If any missing, add and re-run Check.

### Attempt history (4)

## 5. Validate the change and confirm load

- [x] 5.P Plan: Validate the OpenSpec change and confirm OpenCode loads the new agents/command. Check criteria: (a) `openspec validate --change add-goal-coding-loop` reports no violations; (b) `openspec status --change add-goal-coding-loop` shows `proposal`, `specs`, `design`, `tasks` all `done`; (c) after restarting OpenCode (or instructing the user to), `goal-orch` is selectable as a primary agent and `/goal` is available as a command. Affected areas: none (read-only validation). Risk: OpenCode caches agent files — mitigate by requiring a restart.
- [x] 5.D Do: Run `openspec validate --change add-goal-coding-loop` and `openspec status --change add-goal-coding-loop`. Print the outputs. Provide the user a one-line restart instruction and the two manual checks (select `goal-orch`, type `/goal`).
- [x] 5.C Check: Record PASS/FAIL for (a) validate output has no violations; (b) status shows the four artifacts `done`; (c) user confirms (or the agent reports, if it can introspect) that `goal-orch` and `/goal` are available. Do not invent criteria beyond these three.
- [x] 5.A Act: If all three PASS, the change is ready for implementation handoff. If validate fails, fix the flagged artifact and re-run. If load fails, fix frontmatter/path and re-run. Standardize: record any frontmatter gotchas in a workspace note for future agent authors.

### Attempt history (5)

## 6. Manual `/goal` smoke test

- [ ] 6.P Plan: Run a trivial `/goal` task end-to-end to confirm the loop works. Check criteria: (a) `/goal Write a Fibonacci calculation script with the best possible performance.` produces a best-guess clarification conversation from `goal-orch`; (b) on confirmation, a `reqs-manifest.md` is written with all requirements `pending`; (c) `goal-worker` is dispatched, plans, gets sign-off, implements, adds unit tests, returns a report; (d) `goal-orch` updates `reqs-manifest.md` to `completed` for each requirement and emits a final implementation report. Affected areas: workspace root (transient `reqs-manifest.md` and generated Fibonacci script). Risk: model too weak to drive the loop — mitigate by documenting recommended model tiers in the report.
- [ ] 6.D Do: Instruct the user to run the Fibonacci `/goal` (or run it if acting as the agent). Observe the conversation. Inspect `reqs-manifest.md` and the generated script.
- [ ] 6.C Check: Record PASS/FAIL per criterion (a)–(d) by inspecting the conversation transcript, `reqs-manifest.md`, and the generated files. Do not invent criteria.
- [ ] 6.A Act: If all four PASS, the change is fully verified. If any FAIL, record the failure mode under Attempt history, strengthen the corresponding agent instruction, and re-run the smoke test as attempt 2. Standardize learnings (e.g. which model handled orchestration) into a workspace note and feed into the future `/loop` command change.

### Attempt history (6)