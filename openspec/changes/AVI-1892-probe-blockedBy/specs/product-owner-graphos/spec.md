## ADDED Requirements

### Requirement: Merge-gate lane gated on PROBE-check resolution

The merge-gatekeeper SHALL not process (merge) a merge-gate lane until its associated PROBE check has reached a `done` state. A merge-gate lane whose PROBE check is not `done` SHALL remain in its current state (typically `in_progress` or `blocked`) and SHALL NOT be merged through the merge gate. The `blockedBy` relationship between the merge-gate lane and the PROBE check is the gate key: as long as the PROBE check blocks the merge-gate lane, the merge-gatekeeper SHALL withhold merge approval.

#### Scenario: PROBE done → merge-gate lane may proceed

- **WHEN** a merge-gate lane's associated PROBE check has reached a `done` state
- **THEN** the merge-gate lane is eligible for processing by the merge-gatekeeper

#### Scenario: PROBE not done → merge-gate lane stays blocked

- **WHEN** a merge-gate lane's associated PROBE check has not reached a `done` state (i.e., it is `in_progress`, `blocked`, or any other non-`done` state)
- **THEN** the merge-gatekeeper SHALL not merge the merge-gate lane; it stays in its current state

#### Scenario: partial/mixed PROBE-check state does not unblock

- **WHEN** the PROBE check associated with a merge-gate lane is not in a `done` state, regardless of other partial progress
- **THEN** the merge-gate lane SHALL remain blocked; the merge-gatekeeper SHALL not proceed with the merge