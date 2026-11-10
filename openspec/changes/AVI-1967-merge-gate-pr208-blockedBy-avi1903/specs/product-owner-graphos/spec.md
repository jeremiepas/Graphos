## product-owner-graphos

## ADDED Requirements

### Requirement: Merge-gate lane gated on review-lane AVI-1903 resolution for PR #208

The merge-gatekeeper SHALL not process (merge) a merge-gate lane for PR #208 (pr104-head) until its associated review lane AVI-1903 has reached a `done` state. A merge-gate lane for PR #208 whose review lane AVI-1903 is not `done` SHALL remain in its current state (typically `in_progress` or `blocked`) and SHALL NOT be merged through the merge gate. The `blockedBy` relationship between the merge-gate lane and review lane AVI-1903 is the gate key: as long as the review lane AVI-1903 blocks the merge-gate lane for PR #208, the merge-gatekeeper SHALL withhold merge approval.

#### Scenario: review lane AVI-1903 done → merge-gate lane may proceed for PR #208

- **WHEN** a merge-gate lane for PR #208's associated review lane AVI-1903 has reached a `done` state
- **THEN** the merge-gate lane for PR #208 is eligible for processing by the merge-gatekeeper

#### Scenario: review lane AVI-1903 not done → merge-gate lane stays blocked for PR #208

- **WHEN** a merge-gate lane for PR #208's associated review lane AVI-1903 has not reached a `done` state (i.e., it is `in_progress`, `blocked`, or any other non-`done` state)
- **THEN** the merge-gatekeeper SHALL not merge the merge-gate lane for PR #208; it stays in its current state

#### Scenario: partial/mixed review-lane AVI-1903 state does not unblock for PR #208

- **WHEN** the review lane AVI-1903 associated with a merge-gate lane for PR #208 is not in a `done` state, regardless of other partial progress
- **THEN** the merge-gate lane for PR #208 SHALL remain blocked; the merge-gatekeeper SHALL not proceed with the merge

## Requirement: Merge-gatekeeper re-evaluates the gate each merge-gate pass for PR #208

The merge-gatekeeper SHALL re-run the status gate for the merge-gate lane for PR #208 during each daily merge-gate run (13:00 Europe/Paris), and SHALL record the resulting determination — or the explicit reason the merge-gate lane for PR #208 stays blocked — as a comment or document on the affected Paperclip issue AVI-1967 so the decision is durable rather than held only in governance memory.

#### Scenario: gate outcome is recorded durably for PR #208

- **WHEN** the merge-gatekeeper evaluates the merge-gate lane for PR #208's review-lane blockers during a merge-gate run
- **THEN** the resulting determination (eligible or not-eligible, with reason), or the explicit reason the merge-gate lane for PR #208 stays blocked, is posted as a comment or document on Paperclip issue AVI-1967