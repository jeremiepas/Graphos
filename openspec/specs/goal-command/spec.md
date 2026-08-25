# goal-command Specification

## Purpose
TBD - created by archiving change add-goal-coding-loop. Update Purpose after archive.
## Requirements
### Requirement: Goal command file exists and delegates to goal-orch

The workspace SHALL contain `.opencode/commands/goal.md` defining a `/goal` command. The file's YAML frontmatter SHALL set `agent: goal-orch` so the command routes to the orchestrator agent, not the default agent. The command body SHALL use the `$ARGUMENTS` placeholder so the user's task text is substituted into the message sent to `goal-orch` (PRD §13 agent-scaffolding extension; OpenCode Markdown-command mechanism).

#### Scenario: Command file is present with correct frontmatter

- **WHEN** the change is implemented
- **THEN** `.opencode/commands/goal.md` exists, its frontmatter contains `agent: goal-orch`, and its body contains the `$ARGUMENTS` placeholder

#### Scenario: Commands directory is created if absent

- **WHEN** the change is implemented and `.opencode/commands/` did not previously exist
- **THEN** the directory is created by the implementation and `goal.md` is placed inside it

### Requirement: Goal command bootstraps the loop with the user's task

On `/goal <task text>`, OpenCode SHALL send a message to `goal-orch` containing the substituted task text. `goal-orch` SHALL then begin its clarification phase (per the goal-orchestrator spec) — the command itself does no parsing, planning, or dispatch. The command is a thin entry point; all loop logic lives in `goal-orch` and `goal-worker`.

#### Scenario: Task text reaches the orchestrator

- **WHEN** the user runs `/goal Write a Fibonacci calculation script with the best possible performance.`
- **THEN** `goal-orch` receives a message containing the Fibonacci task text and begins its best-guess requirements clarification

#### Scenario: Empty task is rejected by the orchestrator

- **WHEN** the user runs `/goal` with no arguments
- **THEN** `goal-orch` replies asking for a task description and does not enter the autonomous loop (PRD §16.3 — fail visibly, not silently)

