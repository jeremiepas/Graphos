# Spec: openspec-artifact-view

## ADDED Requirements

### Requirement: Artifact view SHALL display all artifacts for a change

The artifact view SHALL list and display the following artifacts when they exist:
- `proposal.md` — the change proposal
- `specs/<capability>/spec.md` — one spec per capability listed in the proposal
- `design.md` — the design document (if present)
- `tasks.md` — the task list (if present)

#### Scenario: Full artifact display
WHEN a user requests artifacts for a change
THEN all existing artifacts SHALL be listed with their content or a summary

### Requirement: Artifact view SHALL show artifact status

Each artifact SHALL display its completion status:
- `done` — artifact exists and is complete
- `pending` — artifact is expected but not yet created
- `not-applicable` — artifact does not apply to this change

#### Scenario: Status display
WHEN artifacts are displayed
THEN each artifact SHALL show its status (done/pending/not-applicable)

### Requirement: Artifact view SHALL support depth levels

The artifact view SHALL support three depth levels:
- `summary` — artifact title and status only
- `full` — complete artifact content
- `sections` — artifact content grouped by top-level sections

#### Scenario: Depth level support
WHEN a depth level is specified
THEN the output SHALL match the requested depth level
