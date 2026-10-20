## ADDED Requirements

### Requirement: Product Owner gates task status on dependency resolution

The Product Owner SHALL gate each task's status against its dependencies on every
triage pass. A task whose status is `blocked` SHALL transition back to `backlog`
when every child task and every task-blocker (`blockedByIssueIds`) associated with
that task has reached a completed state. If any child task or any task-blocker
remains incomplete, the task SHALL remain (or be placed) in `blocked`. A blocker is
treated as resolved only when the blocking issue reports completed; a partially
resolved dependency set does NOT unblock a task.

#### Scenario: fully-resolved blockers return the task to backlog
- **WHEN** a blocked task has all of its child tasks and all of its
  `blockedByIssueIds` marked completed
- **THEN** the Product Owner transitions the task out of `blocked` back into
  `backlog`

#### Scenario: any pending blocker keeps the task blocked
- **WHEN** at least one child task or at least one `blockedByIssueIds` of a blocked
  task is still incomplete
- **THEN** the Product Owner leaves the task in `blocked` and does not move it to
  `backlog`

#### Scenario: partial resolution does not unblock
- **WHEN** some but not all child tasks or blockers of a blocked task are completed
- **THEN** the task remains `blocked` until the final outstanding blocker resolves

### Requirement: Product Owner re-evaluates the gate each triage pass

The Product Owner SHALL re-run the status gate for every in-flight and blocked task
during each triage pass, and SHALL record the resulting transition — or the explicit
reason a task stays blocked — as a comment or document on the affected issue so the
decision is durable rather than held only in chat.

#### Scenario: gate outcome is recorded durably
- **WHEN** the Product Owner evaluates a task's blockers during a triage pass
- **THEN** the resulting status transition, or the explicit reason the task stays
  `blocked`, is posted as a comment or document on the issue
