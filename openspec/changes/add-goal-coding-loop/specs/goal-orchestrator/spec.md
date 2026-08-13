## ADDED Requirements

### Requirement: Goal orchestrator agent file exists and is a primary agent

The workspace SHALL contain `.opencode/agent/core/goal-orch.md` defining a primary agent named `goal-orch` (frontmatter `mode: primary`). The file MUST follow the Graphos agent-file convention: YAML frontmatter with `name`, `description`, `mode`, `temperature`, `model`, and a `permission` block; the body MUST contain tiered rules (`<tier level="1" ...>`/`<tier level="2" ...>`/`<tier level="3" ...>`) and a `<conflict_resolution>` block, mirroring the structure of `.opencode/agent/subagents/code/coder-agent.md`. The agent's frontmatter `model` SHALL be pinned to `gemma/gemma4-moe` (PRD §13 agent-scaffolding extension; pinned per user decision 2026-08-12). `opencode.json` SHALL declare the `gemma` provider's `gemma4-moe` model with `limit.context: 65536`.

#### Scenario: Agent file is present with correct frontmatter

- **WHEN** the change is implemented and OpenCode is restarted
- **THEN** `.opencode/agent/core/goal-orch.md` exists, its frontmatter `mode` equals `primary`, its frontmatter `model` equals `gemma/gemma4-moe`, and `opencode.json` declares `gemma4-moe` with `limit.context: 65536`

#### Scenario: Agent appears in the OpenCode agent list

- **WHEN** OpenCode starts in this workspace after implementation
- **THEN** `goal-orch` is selectable as a primary agent in the OpenCode interface (consistent with the article's note that primary agents surface in the agent list)

### Requirement: Orchestrator clarifies requirements via best-guess confirmation

On receiving a task from `/goal`, `goal-orch` SHALL NOT begin coding immediately and SHALL NOT ask open-ended questions. It SHALL take its best guess at the user's intent, decompose the task into a list of atomic requirements, present that list to the user, and request confirmation before entering autonomous mode (PRD §16.3 reliability — surface ambiguity early rather than fail silently). The clarification MAY offer a `recommend` option for each item so a non-engineer user can accept defaults.

#### Scenario: Best-guess list is presented before autonomous mode

- **WHEN** the user runs `/goal Build a playable Tower of Hanoi web game.`
- **THEN** `goal-orch` replies with a decomposed requirements list (e.g. disk count, move rules, win condition, UI, reset) and a confirmation prompt, and does not call any worker until the user confirms

#### Scenario: Open-ended question is not asked

- **WHEN** `goal-orch` receives an underspecified task
- **THEN** it emits its best-guess requirements list with a confirm/edit prompt rather than asking "what do you want?" without options

### Requirement: Orchestrator authors and maintains a requirements manifest

After the user confirms the requirements list, `goal-orch` SHALL write a `reqs-manifest.md` file (at the workspace root, or under `.tmp/goal/<run-id>/` if the root is not writable) tracking each requirement with an id, title, status (`pending` / `in_progress` / `completed` / `blocked`), and (when completed) a one-line completion summary. The manifest is the acceptance record for the run; `goal-orch` SHALL update it as each requirement progresses (PRD §16.3 — durable, human-readable record reconstructable from state, mirroring the checkpoint principle used for `graphos-out/`).

#### Scenario: Manifest is created on confirmation

- **WHEN** the user confirms the requirements list for a `/goal` run
- **THEN** a `reqs-manifest.md` file is written listing every requirement with status `pending`

#### Scenario: Manifest status tracks worker progress

- **WHEN** a `goal-worker` reports a requirement complete
- **THEN** `goal-orch` updates that requirement's status to `completed` in `reqs-manifest.md` with a one-line summary before dispatching the next work

### Requirement: Orchestrator builds a dependency DAG and dispatches in parallel

Before entering the implementation loop, `goal-orch` SHALL map dependency relationships among requirements into a DAG. Requirements with no inter-dependencies SHALL be dispatched to separate `goal-worker` instances in parallel (multiple `task` tool calls in one message). Requirements with unsatisfied dependencies SHALL remain pending until their dependencies are `completed` (the "Graph Engineering" escalation described in the article; PRD §16.2 scalability — parallelism keeps wall-clock time bounded).

#### Scenario: Independent requirements run in parallel

- **WHEN** the confirmed requirements list contains two requirements A and B with no dependency between them
- **THEN** `goal-orch` issues two `task` calls to `goal-worker` in a single message and both execute concurrently

#### Scenario: Dependent requirements wait

- **WHEN** requirement B depends on requirement A and A is still `in_progress`
- **THEN** `goal-orch` does not dispatch B, and B remains `pending` in `reqs-manifest.md`

### Requirement: Orchestrator resumes worker sub-sessions by task_id

When a `goal-worker` returns a question (technology choice, ambiguity) instead of a completion report, `goal-orch` SHALL answer the question and re-invoke the `task` tool passing the prior `task_id` to resume the worker's sub-session in context — it SHALL NOT start a new worker session for the same requirement (article's "foreman/worker reference number" pattern). `goal-orch` SHALL NOT surface worker questions to the user; the orchestrator is the decision-maker.

#### Scenario: Worker question is answered by orchestrator

- **WHEN** `goal-worker` returns a question about a technology choice
- **THEN** `goal-orch` formulates an answer and re-invokes `task` with the previous `task_id` and the answer, resuming the same sub-session

#### Scenario: Worker question is not surfaced to the user

- **WHEN** `goal-worker` asks `goal-orch` a question mid-implementation
- **THEN** `goal-orch` does not emit a user-facing prompt; the user is not interrupted

### Requirement: Orchestrator runs acceptance and end-to-end verification

After all requirements report `completed`, `goal-orch` SHALL run an acceptance pass: re-check each requirement description against the worker's implementation report, confirm unit-test coverage exists, and — when the task involves a frontend — invoke the `@playwright-mcp` or `agent-browser` skill to run end-to-end tests and capture screenshots as proof. `goal-orch` SHALL then write an implementation report summarizing what was built, what passed, and any blockers needing human attention (PRD §16.3 reliability — the loop halts and surfaces rather than fails silently).

#### Scenario: Acceptance pass after all requirements complete

- **WHEN** every requirement in `reqs-manifest.md` is `completed`
- **THEN** `goal-orch` re-checks each requirement against its implementation report and produces an overall implementation report

#### Scenario: Frontend task triggers end-to-end test

- **WHEN** the completed task includes a web UI and `@playwright-mcp` or `agent-browser` is available
- **THEN** `goal-orch` invokes that skill to run end-to-end tests and captures screenshots referenced in the implementation report

#### Scenario: Missing e2e skill does not crash the loop

- **WHEN** the task includes a frontend but neither `@playwright-mcp` nor `agent-browser` is installed
- **THEN** `goal-orch` skips e2e verification, notes the skip in the implementation report, and still completes the run successfully