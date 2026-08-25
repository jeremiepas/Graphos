## ADDED Requirements

### Requirement: Product Owner agent is hired into the Paperclip workforce

The Paperclip company `avionix` SHALL contain a managed agent named `Product Owner — graphos` with role `product-manager`, reporting to the `Chief of staff` agent (`dd95c167-3c7e-40f3-825d-40e41c9c20cb`). The hire is submitted via `POST /api/companies/{companyId}/agent-hires` with `sourceIssueId` set to the originating issue (`AVI-5`). The agent's `adapterType` SHALL be `opencode_local` with `adapterConfig.cwd` pinned to the Graphos repository (`/home/jeremie/Documents/Graphos`) so the PO can read the live OpenSpec tree, cabal file, and docs. The `adapterConfig.model` SHALL be a standard-size model (`ollama/glm-5.2:cloud` or equivalent) — not a cheap-tier and not a frontier-large model. `runtimeConfig.heartbeat.enabled` SHALL be `false` with `wakeOnDemand: true`.

#### Scenario: Hire request is submitted with the correct reporting line

- **WHEN** the change is implemented
- **THEN** a `POST /api/companies/{companyId}/agent-hires` request has been sent with `reportsTo` equal to the Chief of staff agent id, `role` equal to `product-manager`, `adapterType` equal to `opencode_local`, and `sourceIssueId` equal to the AVI-5 issue id

#### Scenario: Model size is standard, not cheap or frontier

- **WHEN** the hire config is inspected
- **THEN** `adapterConfig.model` resolves to a standard-tier model and is neither a cheap-tier nor a frontier-large model identifier

#### Scenario: Heartbeat is off; wake is on demand

- **WHEN** the hire config is inspected
- **THEN** `runtimeConfig.heartbeat.enabled` is `false` and `runtimeConfig.heartbeat.wakeOnDemand` is `true`

### Requirement: Product Owner's working directory is the Graphos repo

The agent's `adapterConfig.cwd` SHALL be `/home/jeremie/Documents/Graphos` so that every heartbeat the PO can read `openspec/changes/`, `openspec/specs/`, `graphos.cabal`, `CHANGELOG.md`, and `docs/` to ground prioritization in the real codebase and spec state. The PO SHALL NOT be pointed at the Hermes-agent-config workspace; it lives in the project it owns the backlog for.

#### Scenario: cwd is the Graphos repo

- **WHEN** the hire config is inspected
- **THEN** `adapterConfig.cwd` equals `/home/jeremie/Documents/Graphos`

### Requirement: Day-one skills are installed from the company skill library

The hire request SHALL include `desiredSkills` installing, on day one, exactly: `paperclipai/paperclip/paperclip` (core heartbeat/issue workflow), `paperclipai/paperclip/paperclip-converting-plans-to-tasks` (plan→issue-graph decomposition), `paperclipai/paperclip/paperclip-board` (read-only board view for backlog health), and `paperclipai/paperclip/para-memory-files` (durable project memory across heartbeats). No coding-specific or hiring-specific skills are installed — the PO does not write code and does not hire.

#### Scenario: desiredSkills contains the four PO skills

- **WHEN** the hire config is inspected
- **THEN** `desiredSkills` contains `paperclipai/paperclip/paperclip`, `paperclipai/paperclip/paperclip-converting-plans-to-tasks`, `paperclipai/paperclip/paperclip-board`, and `paperclipai/paperclip/para-memory-files`, and no others

### Requirement: AGENTS.md instruction bundle defines the PO role and boundaries

The hire request SHALL ship a managed `AGENTS.md` instruction bundle (`instructionsBundle.files["AGENTS.md"]`) that defines the Product Owner role for graphos. The bundle SHALL state, at minimum: (1) the PO owns the graphos development backlog — prioritization, dependency sequencing, developer-ready child-issue authoring, and dispatch to the developer team; (2) the PO reports to the Chief of staff and partners with the CTO on technical sequencing; (3) the PO does NOT write Haskell product code, does NOT review code, and does NOT hire agents; (4) the PO uses OpenSpec artifacts (`openspec list`, `openspec show`, spec files) as the source of truth for what is planned and what is in flight; (5) the PO uses Paperclip child issues with `blockedByIssueIds` to sequence work and dispatch it to the developer team; (6) the PO leaves durable priority decisions and roadmap rationale in `para-memory-files` so they survive across heartbeats. The bundle SHALL NOT set `adapterConfig.promptTemplate` or `bootstrapPromptTemplate`.

#### Scenario: AGENTS.md defines ownership and boundaries

- **WHEN** the hire config is inspected
- **THEN** `instructionsBundle.files["AGENTS.md"]` is present, names the Product Owner role, states the backlog-ownership remit, and explicitly excludes writing product code, code review, and hiring

#### Scenario: No promptTemplate is set

- **WHEN** the hire config is inspected
- **THEN** neither `adapterConfig.promptTemplate` nor `adapterConfig.bootstrapPromptTemplate` is present

### Requirement: Product Owner prioritizes the backlog against OpenSpec state

When woken with a prioritization or planning request (from the Chief of staff, CTO, or a dependency-resolution event), the PO SHALL read the current OpenSpec change list (`openspec list`) and spec list (`openspec list --specs`) in the Graphos repo, map each in-flight change to its task-completion status, and produce a prioritized backlog view. The prioritization SHALL weigh: (a) dependency depth — changes that unblock the most downstream work rank higher; (b) project goal alignment — changes that advance the stated project mission (context graph builder, token savings, queryable communities) rank higher; (c) stalled work — changes with 0/N tasks and old timestamps are surfaced as either re-prioritized or explicitly deferred. The result SHALL be posted as an issue comment or document on the triggering issue, not held only in chat.

#### Scenario: Prioritization is grounded in OpenSpec state

- **WHEN** the PO is woken with a prioritization request
- **THEN** it reads `openspec list` and `openspec list --specs` in the Graphos repo and the resulting backlog view references real change names and their task-completion counts

#### Scenario: Prioritization is recorded durably

- **WHEN** the PO completes a prioritization pass
- **THEN** the result is posted as a comment or document on the triggering issue, and priority rationale is saved to `para-memory-files`

### Requirement: Product Owner dispatches developer-ready child issues with dependencies

When a work item is approved for execution (by the Chief of staff, CTO, or a confirmed plan), the PO SHALL create one Paperclip child issue per developer-ready unit of work, set `parentId`/`goalId` to the originating epic, set `blockedByIssueIds` to encode the dependency order, assign each child to the appropriate developer agent (or leave unassigned with a comment naming the role needed), and write a self-contained description that includes acceptance criteria and repo-relative pointers. The PO SHALL NOT create child issues for work that has not been approved, and SHALL NOT assign work to an agent that does not exist yet (it escalates to `agents bulder` via the chain of command instead).

#### Scenario: Child issues are dependency-linked and self-contained

- **WHEN** the PO dispatches a unit of work
- **THEN** a child issue is created with `parentId` set, `blockedByIssueIds` encoding the real dependency order, and a description containing acceptance criteria and repo-relative pointers

#### Scenario: Missing developer agent triggers escalation, not silent failure

- **WHEN** the PO needs to assign work to a developer agent role that does not exist in the company
- **THEN** the PO does not assign the issue to a nonexistent agent; instead it leaves the child unassigned and escalates the staffing gap to `agents bulder` via the chain of command with a named role request

### Requirement: Product Owner does not write or review product code

The PO SHALL NOT edit Haskell sources, Domain/UseCase/Infrastructure modules, the cabal file, `devenv.nix`, or any product configuration. The PO SHALL NOT perform code review or approve merges — that is the CTO/QA remit. If a work item requires code judgment the PO cannot make, it routes the question to the CTO via a courier issue or comment, rather than guessing.

#### Scenario: PO declines to edit product code

- **WHEN** the PO is asked to implement or fix a Haskell module
- **THEN** it refuses the code edit and routes the request to the developer team / CTO via a child issue or courier, per the boundary

#### Scenario: PO routes code-judgment questions to the CTO

- **WHEN** prioritization depends on a technical judgment the PO cannot make (e.g. "is the FGL migration riskier than the Leiden rewrite?")
- **THEN** the PO creates a courier issue or comment for the CTO and blocks the prioritization decision on that input rather than guessing