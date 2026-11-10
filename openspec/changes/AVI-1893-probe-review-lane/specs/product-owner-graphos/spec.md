## ADDED Requirements

### Requirement: Review lane gated on PROBE-check resolution

The review reviewer SHALL not mark a review lane as `done` until its associated PROBE check has reached a `done` state. A review lane whose PROBE check is not `done` SHALL remain in its current state (typically `in_progress` or `blocked`) and SHALL NOT be marked `done` through the review process. The `blockedBy` relationship between the review lane and the PROBE check is the gate key: as long as the PROBE check blocks the review lane, the reviewer SHALL withhold marking the review lane as done.

#### Scenario: PROBE done → review lane may be marked done

- **WHEN** a review lane's associated PROBE check has reached a `done` state
- **THEN** the review lane is eligible to be marked `done` by the reviewer

#### Scenario: PROBE not done → review lane stays blocked

- **WHEN** a review lane's associated PROBE check has not reached a `done` state (i.e., it is `in_progress`, `blocked`, or any other non-`done` state)
- **THEN** the reviewer SHALL not mark the review lane as `done`; it stays in its current state

#### Scenario: partial/mixed PROBE-check state does not unblock

- **WHEN** the PROBE check associated with a review lane is not in a `done` state, regardless of other partial progress
- **THEN** the review lane SHALL remain in its current state; the reviewer SHALL not mark it as `done` until the PROBE check reaches `done`

#### Scenario: durable recording of the gate outcome

- **WHEN** the gate outcome (PROBE done/ not done) is evaluated during a review wake
- **THEN** the outcome is recorded durably on the Paperclip issue via the `blockedBy` relationship, so the rationale is reconstructable across heartbeats