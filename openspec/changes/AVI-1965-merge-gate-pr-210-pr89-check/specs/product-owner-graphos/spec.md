## ADDED Requirements

### Requirement: Merge-gate lane `pr89-check` gated on review lane AVI-1901 resolution

The merge-gatekeeper SHALL not process (merge) merge-gate lane `pr89-check` (for Graphos PR #210) until its associated review lane AVI-1901 has reached a `done` state. A merge-gate lane `pr89-check` whose review lane AVI-1901 is not `done` SHALL remain in its current state (typically `in_progress` or `blocked`) and SHALL NOT be merged through the merge gate. The `blockedBy` relationship between the merge-gate lane `pr89-check` and the review lane AVI-1901 is the gate key: as long as the review lane AVI-1901 blocks the merge-gate lane `pr89-check`, the merge-gatekeeper SHALL withhold merge approval.

#### Scenario: review lane AVI-1901 done → merge-gate lane `pr89-check` may proceed

- **WHEN** a merge-gate lane `pr89-check`'s associated review lane AVI-1901 has reached a `done` state
- **THEN** the merge-gate lane `pr89-check` is eligible for processing by the merge-gatekeeper

#### Scenario: review lane AVI-1901 not done → merge-gate lane `pr89-check` stays blocked

- **WHEN** a merge-gate lane `pr89-check`'s associated review lane AVI-1901 has not reached a `done` state (i.e., it is `in_progress`, `blocked`, or any other non-`done` state)
- **THEN** the merge-gatekeeper SHALL not merge merge-gate lane `pr89-check`; it stays in its current state

#### Scenario: partial/mixed review-lane AVI-1901 state does not unblock

- **WHEN** the review lane AVI-1901 associated with merge-gate lane `pr89-check` is not in a `done` state, regardless of other partial progress
- **THEN** the merge-gate lane `pr89-check` SHALL remain blocked; the merge-gatekeeper SHALL not proceed with the merge

### Requirement: Merge-gatekeeper re-evaluates the gate each merge-gate pass

The merge-gatekeeper SHALL re-run the status gate for every in-flight merge-gate lane during each daily merge-gate run (13:00 Europe/Paris), and SHALL record the resulting determination — or the explicit reason a merge-gate lane stays blocked — as a comment or document on the affected Paperclip issue so the decision is durable rather than held only in governance memory.

#### Scenario: gate outcome is recorded durably

- **WHEN** the merge-gatekeeper evaluates merge-gate lane `pr89-check`'s review-lane AVI-1901 blockers during a merge-gate run
- **THEN** the resulting determination (eligible or not-eligible, with reason), or the explicit reason the merge-gate lane `pr89-check` stays blocked, is posted as a comment or document on the Paperclip issue