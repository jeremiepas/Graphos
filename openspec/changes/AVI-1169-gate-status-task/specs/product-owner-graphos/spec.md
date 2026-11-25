## ADDED Requirements

### Requirement: Product Owner gates task status on dependency resolution

The Product Owner SHALL gate each task's status against its dependencies on every
triage pass, reading both the task's child tasks and its first-class blockers
(`blockedByIssueIds`). A dependency SHALL count as resolved only when it has reached a
terminal state — `done` or `cancelled`; a dependency in `todo`, `in_progress`,
`in_review`, `backlog`, or `blocked` SHALL count as unresolved. When every child task
and every blocker of a `blocked` task is resolved, the Product Owner SHALL transition
the task out of `blocked` back to `backlog`, or to `todo` when it is immediately ready
to start. When any child task or any blocker is unresolved, the Product Owner SHALL keep
the task in `blocked` and SHALL NOT move it to `backlog`, even if most of its
dependencies have resolved. Blockers SHALL be expressed with `blockedByIssueIds`, never
as prose, and a comment on the issue SHALL NOT by itself be treated as evidence that a
blocker is resolved.

#### Scenario: fully-resolved dependencies return the task to backlog

- **WHEN** a blocked task has all of its child tasks and all of its
  `blockedByIssueIds` in a terminal state (`done` or `cancelled`)
- **THEN** the Product Owner transitions the task out of `blocked` back into `backlog`

#### Scenario: a cancelled blocker counts as resolved

- **WHEN** every child task of a blocked task is `done` and its remaining
  `blockedByIssueIds` blockers are `cancelled`
- **THEN** the Product Owner treats those blockers as resolved and moves the task out of
  `blocked` into `backlog` instead of leaving it blocked

#### Scenario: any pending dependency keeps the task blocked

- **WHEN** at least one child task or at least one `blockedByIssueIds` of a blocked
  task is in `todo`, `in_progress`, `in_review`, `backlog`, or `blocked`
- **THEN** the Product Owner leaves the task in `blocked` and does not move it to
  `backlog`

#### Scenario: partial resolution does not unblock

- **WHEN** some but not all child tasks or blockers of a blocked task are resolved
- **THEN** the task remains `blocked` until the final outstanding dependency resolves

### Requirement: Product Owner re-evaluates the gate each triage pass

The Product Owner SHALL re-run the status gate for every in-flight and blocked task
during each triage pass, confirming each blocker's state from the issue object rather
than from a comment, and SHALL record the resulting transition — or the explicit reason
a task stays blocked — as a comment or document on the affected issue so the decision is
durable rather than held only in chat.

#### Scenario: gate outcome is recorded durably

- **WHEN** the Product Owner evaluates a task's dependencies during a triage pass
- **THEN** the resulting status transition, or the explicit reason the task stays
  `blocked`, is posted as a comment or document on the issue
