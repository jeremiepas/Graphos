## ADDED Requirements

### Requirement: Cluster-only persists its result

When `--cluster-only` completes, the system SHALL write the clustered graph to
`graph.json` in the output directory (atomically, with
`metadata.graphos.communities` filled), SHALL write `community_graph.json`
when `--community-graph` is set, and SHALL keep `graph.checkpoint.json`.

#### Scenario: cluster-only writes graph.json
- **WHEN** `graphos ./src --cluster-only` runs with a valid checkpoint
- **THEN** `graph.json` exists and `metadata.graphos.communities` is non-empty

#### Scenario: cluster-only writes the community graph
- **WHEN** `graphos ./src --cluster-only --community-graph` runs
- **THEN** `community_graph.json` exists and the checkpoint is still present
