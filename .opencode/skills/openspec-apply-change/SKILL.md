---
name: openspec-apply-change
description: Implement tasks from an OpenSpec change. Use when the user wants to start implementing, continue implementation, or work through tasks.
allowed-tools: Bash(openspec:*)
license: MIT
compatibility: Requires openspec CLI.
metadata:
  author: openspec
  version: "1.0"
  generatedBy: "1.7.0"
---

Implement tasks from an OpenSpec change.

**Store selection:** If the user names a store (a store is a standalone OpenSpec repo registered on this machine) or the work lives in one, run `openspec store list --json` to discover registered store ids, then pass `--store <id>` on the commands that read or write specs and changes (`new change`, `status`, `instructions`, `list`, `show`, `validate`, `archive`, `doctor`, `context`, `view`). Other commands do not take the flag. Hints printed by commands already carry the flag; keep it on follow-ups. Without a store, commands act on the nearest local `openspec/` root.

**Input**: Optionally specify a change name. If omitted, check if it can be inferred from conversation context. If vague or ambiguous you MUST prompt for available changes.

**Steps**

1. **Select the change**

   If a name is provided, use it. Otherwise:
   - Infer from conversation context if the user mentioned a change
   - Auto-select if only one active change exists
   - If ambiguous, run `openspec list --json` to get available changes and ask the user to select one

   Always announce: "Using change: <name>" and how to override (e.g., `/opsx-apply <other>`).

2. **Check status to understand the schema**
   ```bash
   openspec status --change "<name>" --json
   ```
   Parse the JSON to understand:
   - `schemaName`: The workflow being used (e.g., "spec-driven")
   - `planningHome`, `changeRoot`, and `actionContext`: planning scope and edit constraints
   - Which artifact contains the tasks (typically "tasks" for spec-driven, check status for others)

3. **Get apply instructions**

   ```bash
   openspec instructions apply --change "<name>" --json
   ```

   This returns:
   - `contextFiles`: artifact ID -> array of concrete file paths (varies by schema - could be proposal/specs/design/tasks or spec/tests/implementation/docs)
   - Progress (total, complete, remaining)
   - Task list with status
   - Dynamic instruction based on current state
   - Optional `context`: current required project instruction input from the selected root
   - Optional `operationGuidance`: current advisory guidance for apply

   **Handle states:**
   - If `state: "blocked"` (missing artifacts): show message, suggest using openspec-continue-change (if it is not installed, run `openspec status --change "<name>" --json` to see the next artifact and `openspec instructions <artifact-id> --change "<name>" --json` for how to create it)
   - If `state: "all_done"`: congratulate, suggest archive
   - Otherwise: proceed to implementation

   Treat `context` as a required prompt-level input. Read and consider it, and
   apply relevant project facts, conventions, and constraints while implementing.
   Treat `operationGuidance` as optional additive advice. Read and consider every
   entry, and follow entries that are applicable and compatible with the built-in
   workflow.

   Keep both fields separate from CLI-returned state, missing artifacts, tasks,
   progress, `contextFiles`, and the built-in `instruction`. They are not
   evidence of task completion, do not replace the built-in instruction, and do
   not permit bypassing a blocked state. If context conflicts with the built-in
   instruction, an explicit user choice, or a CLI-controlled value, report the
   conflict and preserve the controlling value. If guidance is inapplicable or
   conflicts with those controlling inputs, do not follow it and explain why.
   These are prompt-level behavior contracts, not enforceable checks.

4. **Read minimal top-level context**

   Read ONLY the top-level planning files from `contextFiles`:
   - **proposal.md** (the "Why" and "What Changes")
   - **specs** (delta spec files, only if needed for understanding requirements)
   - **design.md** (architecture and key decisions, only if needed)
   - **tasks.md** (the tracked task index)

   Do NOT read per-task files (`tasks/**/plan.md`, `tasks/**/do.md`,
   `tasks/**/check.md`, `tasks/**/act.md`) at this stage. They will be read
   one-at-a-time in Step 6.

   **Respect schema-specific context budget:**
   The `instruction` field returned by `openspec instructions apply` may contain
   a context-budget directive (e.g. PDCA's "read one task at a time"). If present,
   follow it strictly. The goal is to keep the active context small enough for
   small models (64k context windows).

   Do not copy `context` or `operationGuidance` verbatim into implementation
   files or planning artifacts unless the user separately asks for that content.

5. **Show current progress**

   Display:
   - Schema being used
   - Progress: "N/M tasks complete"
   - Next pending task (do not show full details yet)
   - Dynamic instruction from CLI

6. **Implement tasks one at a time with context clearing**

   This skill processes **ONE task per cycle** to stay within small-model
   context limits. After each task, it asks before loading the next one.

   For the first/next pending task:
   a. **Identify the task** from `tasks.md` (first unchecked `- [ ] N.P` step).
   b. **Ask for confirmation**: "Next task: N. <task name>. Continue?"
      - If the user declines or wants to change task: pause and ask what to do.
      - If the user agrees: proceed.
   c. **Load ONLY this task's context**:
      - Read the task's entry in `tasks.md` (1-2 paragraphs max).
      - Read the task's per-task files from `contextFiles`:
        - `tasks/<task-slug>/plan.md` (scope, check criteria, affected modules)
        - `tasks/<task-slug>/do.md` (implementation plan)
        - `tasks/<task-slug>/check.md` (verification plan)
      - Read only the source files listed in that `plan.md`.
      - Do NOT read per-task files from other tasks.
      - Do NOT re-read the full proposal/design/specs unless the task explicitly
        references them.
   d. **Implement the code changes** required by `do.md`.
   e. **Run the checks** from `check.md` against the criteria in `plan.md`.
      Update `check.md` with actual results.
   f. **Update `do.md`** to reflect what was actually implemented
      (deviations from plan).
   g. **Generate `act.md`** as the final verdict trace for the whole PDCA cycle.
   h. **Mark the task complete** in `tasks.md`: change `- [ ]` to `- [x]` for
      `N.P`, `N.D`, `N.C`, and `N.A`.
   i. **Clear context**: write a 2-line summary of the completed task. Do not
      carry the detailed task files forward into the next cycle.

   **Loop / ask before next task:**
   After completing the task, ask:
   > "Task N complete. Continue to task N+1? (yes/no)"

   - If yes: identify the next pending task and repeat from step 6b.
   - If no: pause, show overall progress, and wait for direction.
   - If all tasks are complete: proceed to Step 7.

   **Pause if:**
   - Task is unclear → ask for clarification
   - Implementation reveals a design issue → suggest updating artifacts
   - Error or blocker encountered → report and wait for guidance
   - User interrupts or says no to "Continue?"

7. **On completion or pause, show status**

   Display:
   - Tasks completed this session
   - Overall progress: "N/M tasks complete"
   - If all done: suggest archive
   - If paused: explain why and wait for guidance

**Output During Task Implementation**

```
## Implementing: <change-name> (schema: <schema-name>)

Task N/M: <task name>

Loading only this task's context...

[...implementation happening...]

✓ Task N complete — act.md generated, tasks.md checkboxes updated.

Continue to task N+1? (yes/no)
```

**Output On Completion (all tasks done)**

```
## Implementation Complete

**Change:** <change-name>
**Schema:** <schema-name>
**Progress:** N/N tasks complete ✓

### Completed This Session
- [x] Task 1
- [x] Task 2
...

All tasks complete! Ready to archive this change.
```

**Output On Pause (Issue Encountered)**

```
## Implementation Paused

**Change:** <change-name>
**Schema:** <schema-name>
**Progress:** N/M tasks complete

### Issue Encountered
<description of the issue>

### Options
1. <option 1>
2. <option 2>
3. Other approach

What would you like to do?
```

**Output On "No" To Continue**

```
## Implementation Paused — Awaiting Next Task

**Change:** <change-name>
**Schema:** <schema-name>
**Progress:** N/M tasks complete

Task N is complete. To continue, ask me to proceed with the next task or tell me which task to work on.
```

**Guardrails**
- Process ONE task per cycle; ask before loading the next
- Always read context files before starting (from the apply instructions output)
- Respect any context-budget directive in the schema's `apply.instruction`
- Read top-level context once, then only the current task's per-task files + affected source files
- Do not accumulate context across tasks; clear it after each act.md
- Do not read per-task files for tasks that are not currently being worked on
- If task is ambiguous, pause and ask before implementing
- If implementation reveals issues, pause and suggest artifact updates
- Keep code changes minimal and scoped to each task
- Update task checkboxes immediately after completing each task
- Pause on errors, blockers, or unclear requirements - don't guess
- Use contextFiles from CLI output, don't assume specific file names
- Do not use context or operation guidance as proof that a task is complete
- Apply relevant project context; report conflicts with controlling workflow inputs
- Consider every guidance entry; explain any inapplicable or conflicting advice
- Do not copy runtime context or operation guidance into implementation files or planning artifacts
- Preserve CLI-controlled blocked/ready/all-done behavior and completion criteria

**Fluid Workflow Integration**

This skill supports the "actions on a change" model:

- **Can be invoked anytime**: Before all artifacts are done (if tasks exist), after partial implementation, interleaved with other actions
- **Allows artifact updates**: If implementation reveals design issues, suggest updating artifacts - not phase-locked, work fluidly
