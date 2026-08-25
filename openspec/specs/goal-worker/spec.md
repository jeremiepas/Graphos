# goal-worker Specification

## Purpose
TBD - created by archiving change add-goal-coding-loop. Update Purpose after archive.
## Requirements
### Requirement: Goal worker agent file exists and is a subagent

The workspace SHALL contain `.opencode/agent/subagents/code/goal-worker.md` defining a subagent named `goal-worker` (frontmatter `mode: subagent`). The file MUST follow the Graphos agent-file convention: YAML frontmatter with `name`, `description`, `mode`, `temperature`, `model`, and a `permission` block; tiered rules (`<tier level="1" ...>`/`<tier level="2" ...>`/`<tier level="3" ...>`); and a `<conflict_resolution>` block, mirroring the structure of `.opencode/agent/subagents/code/coder-agent.md`. The agent's frontmatter `model` SHALL be pinned to `gemma/gemma4-moe` (PRD §13 agent-scaffolding extension; pinned per user decision 2026-08-12). `opencode.json` SHALL declare the `gemma` provider's `gemma4-moe` model with `limit.context: 65536`. The agent is standalone — it does not declare `CoderAgent` or `TestEngineer` as dependencies.

#### Scenario: Agent file is present with correct frontmatter

- **WHEN** the change is implemented and OpenCode is restarted
- **THEN** `.opencode/agent/subagents/code/goal-worker.md` exists, its frontmatter `mode` equals `subagent`, its frontmatter `model` equals `gemma/gemma4-moe`, and `opencode.json` declares `gemma4-moe` with `limit.context: 65536`

#### Scenario: Worker is not independently invocable by the user

- **WHEN** OpenCode starts in this workspace after implementation
- **THEN** `goal-worker` is available as a subagent for `goal-orch` to dispatch via the `task` tool but is not the default agent for direct user messages

### Requirement: Worker plans before coding

For each requirement received, `goal-worker` SHALL produce a plan before writing any feature code. The plan MUST cover: the technology stack to be used, the files/surface area to be changed, and how unit tests and edge cases will be handled. `goal-worker` SHALL send this plan to `goal-orch` for review and SHALL NOT begin feature implementation until `goal-orch` signs off (article's "plan first, then act" rule; PRD §16.3 reliability — fail early on a bad plan, not late on bad code).

#### Scenario: Plan is produced before any feature code

- **WHEN** `goal-worker` receives a requirement to implement
- **THEN** it emits a plan (tech stack, change surface, unit + edge-case test approach) and waits for `goal-orch` sign-off before writing feature code

#### Scenario: No feature code before sign-off

- **WHEN** `goal-orch` has not yet approved the plan
- **THEN** `goal-worker` has not created or modified any feature source files for that requirement

### Requirement: Worker asks orchestrator on technology-choice ambiguity

When `goal-worker` hits a technology choice or ambiguity (before or during implementation), it SHALL stop and return the question to `goal-orch` via a session return, then await `goal-orch`'s answer. `goal-worker` SHALL NOT ask the user directly and SHALL NOT guess on technology choices that materially affect the result (article's "ask before acting" rule for workers; PRD §16.3 — surface the decision, don't fail silently).

#### Scenario: Technology question is returned to orchestrator

- **WHEN** `goal-worker` must choose between two materially different libraries and the plan did not specify one
- **THEN** `goal-worker` stops, returns the question to `goal-orch`, and does not proceed until `goal-orch` answers

#### Scenario: Worker does not prompt the user

- **WHEN** `goal-worker` encounters a technology ambiguity
- **THEN** no user-facing prompt is emitted by `goal-worker`; only `goal-orch` is asked

### Requirement: Worker implements, tests, and reports

After plan sign-off, `goal-worker` SHALL implement the requirement (feature code, edge-case handling, and unit tests), then return an implementation report to `goal-orch` describing what was built, what tests were added, and their results. `goal-worker` SHALL use modern package managers (`uv` for Python, `pnpm` for JavaScript/TypeScript) when installing dependencies, not global `pip`/`npm` (article convention; keeps the dev environment clean).

#### Scenario: Implementation includes unit tests

- **WHEN** `goal-worker` completes a requirement
- **THEN** its implementation report lists unit tests added and their pass/fail status

#### Scenario: Modern package managers are used

- **WHEN** `goal-worker` installs Python dependencies for a requirement
- **THEN** it uses `uv` (not `pip install` into the global environment); for JS/TS it uses `pnpm` (not global `npm install`)

#### Scenario: Implementation report is returned

- **WHEN** `goal-worker` finishes a requirement
- **THEN** it returns a report to `goal-orch` summarizing built artifacts, tests, and any unresolved issues, and does not mark itself complete without that report

