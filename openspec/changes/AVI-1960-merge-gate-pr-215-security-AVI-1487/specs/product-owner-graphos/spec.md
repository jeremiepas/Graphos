## ADDED Requirements

### Requirement: Merge-gate lane gated on review-lane AVI-1896 resolution for PR #215

The merge-gatekeeper SHALL not process (merge) merge-gate lane for Graphos PR #215 (security/AVI-1487-eliminate-curl-credentials) until its associated review lane AVI-1896 has reached a `done` state. A merge-gate lane for PR #215 whose review lane AVI-1896 is not `done` SHALL remain in its current state (typically `in_progress` or `blocked`) and SHALL NOT be merged through the merge gate. The `blockedBy` relationship between the merge-gate lane and review lane AVI-1896 is the gate key: as long as the review lane AVI-1896 blocks the merge-gate lane for PR #215, the merge-gatekeeper SHALL withhold merge approval.

#### Scenario: review lane AVI-1896 done → merge-gate lane may proceed for PR #215

- **WHEN** a merge-gate lane for PR #215's associated review lane AVI-1896 has reached a `done` state
- **THEN** the merge-gate lane for PR #215 is eligible for processing by the merge-gatekeeper

#### Scenario: review lane AVI-1896 not done → merge-gate lane stays blocked for PR #215

- **WHEN** a merge-gate lane for PR #215's associated review lane AVI-1896 has not reached a `done` state (i.e., it is `in_progress`, `blocked`, or any other non-`done` state)
- **THEN** the merge-gatekeeper SHALL not merge the merge-gate lane for PR #215; it stays in its current state

#### Scenario: partial/mixed review-lane AVI-1896 state does not unblock for PR #215

- **WHEN** the review lane AVI-1896 associated with a merge-gate lane for PR #215 is not in a `done` state, regardless of other partial progress
- **THEN** the merge-gate lane for PR #215 SHALL remain blocked; the merge-gatekeeper SHALL not proceed with the merge

### Requirement: Merge-gatekeeper re-evaluates the gate each merge-gate pass for PR #215

The merge-gatekeeper SHALL re-run the status gate for the merge-gate lane for PR #215 during each daily merge-gate run (13:00 Europe/Paris), and SHALL record the resulting determination — or the explicit reason the merge-gate lane for PR #215 stays blocked — as a comment or document on the affected Paperclip issue AVI-1960 so the decision is durable rather than held only in governance memory.

#### Scenario: gate outcome is recorded durably for PR #215

- **WHEN** the merge-gatekeeper evaluates the merge-gate lane for PR #215's review-lane blockers during a merge-gate run
- **THEN** the resulting determination (eligible or not-eligible, with reason), or the explicit reason the merge-gate lane for PR #215 stays blocked, is posted as a comment or document on Paperclip issue AVI-1960