## ADDED Requirements

### Requirement: Merge-gate lane gated on review-lane resolution

The merge-gatekeeper SHALL not process (merge) a merge-gate lane until its associated review lane has reached a `done` state. A merge-gate lane whose review lane is not `done` SHALL remain in its current state (typically `in_progress` or `blocked`) and SHALL NOT be merged through the merge gate. The `blockedBy` relationship between the merge-gate lane and the review lane is the gate key: as long as the review lane blocks the merge-gate lane, the merge-gatekeeper SHALL withhold merge approval.

#### Scenario: review lane done → merge-gate lane may proceed

- **WHEN** a merge-gate lane's associated review lane has reached a `done` state
- **THEN** the merge-gate lane is eligible for processing by the merge-gatekeeper

#### Scenario: review lane not done → merge-gate lane stays blocked

- **WHEN** a merge-gate lane's associated review lane has not reached a `done` state (i.e., it is `in_progress`, `blocked`, or any other non-`done` state)
- **THEN** the merge-gatekeeper SHALL not merge the merge-gate lane; it stays in its current state

#### Scenario: partial/mixed review-lane state does not unblock

- **WHEN** the review lane associated with a merge-gate lane is not in a `done` state, regardless of other partial progress
- **THEN** the merge-gate lane SHALL remain blocked; the merge-gatekeeper SHALL not proceed with the merge

### Requirement: Merge-gatekeeper re-evaluates the gate each merge-gate pass

The merge-gatekeeper SHALL re-run the status gate for every in-flight merge-gate lane during each daily merge-gate run (13:00 Europe/Paris), and SHALL record the resulting determination — or the explicit reason a merge-gate lane stays blocked — as a comment or document on the affected Paperclip issue so the decision is durable rather than held only in governance memory.

#### Scenario: gate outcome is recorded durably

- **WHEN** the merge-gatekeeper evaluates a merge-gate lane's review-lane blockers during a merge-gate run
- **THEN** the resulting determination (eligible or not-eligible, with reason), or the explicit reason the merge-gate lane stays blocked, is posted as a comment or document on the Paperclip issue