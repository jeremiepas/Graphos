# community-size-cap Specification

## Purpose
Bound the size of every community after clustering, so that no densely connected subgraph collapses into one
mega-community (seen: 32k–43k nodes, 24–33 % of a 132k-node graph) that dominates centrality and times out queries.
Oversized communities are split by a post-pass and reported, and noise nodes are excluded from centrality.

## Requirements

### Requirement: Community size cap

The system SHALL split any community whose member count exceeds
`maxCommunityFraction` of total nodes (default 0.05) into smaller sub-communities
after Leiden clustering completes. The cap SHALL apply to Leiden partitions only.
A partition produced by the Bunch algorithm SHALL be exported unsplit, because
Bunch optimises MQ, whose cluster factor already penalises modules that absorb
loosely coupled files (`bunch-clustering`).

#### Scenario: oversized community is split
- **WHEN** clustering yields a community of 40,000 nodes in a 130,000-node graph and the cap is 0.05
- **THEN** that community is split into sub-communities each at or below the cap
- **AND** no resulting community exceeds `maxCommunityFraction` of total nodes

#### Scenario: normal communities untouched
- **WHEN** all communities are within the cap
- **THEN** clustering output is unchanged

#### Scenario: Bunch partitions are not capped
- **WHEN** a build runs with `--cluster-algorithm bunch` on a 29-file graph and the cap is 0.05 (at most one file per community)
- **THEN** no Bunch module is split, and the exported partition has fewer communities than files

### Requirement: Oversize warning

The system SHALL emit a WARNING identifying any community that exceeds the cap
before it is split.

#### Scenario: warning emitted
- **WHEN** a community exceeds the cap
- **THEN** a WARNING log reports the community id and its node count

### Requirement: Centrality excludes noise nodes

The system SHALL exclude nodes flagged as generated or vendored from god-node and
bridge-node computation.

#### Scenario: god-nodes reflect real code
- **WHEN** generated binding nodes are present in the graph
- **THEN** god-node results contain no generated/vendored nodes

### Requirement: Cap configuration

The system SHALL expose the cap fraction and an enable/disable switch via config
and CLI.

#### Scenario: cap can be disabled
- **WHEN** the cap is disabled
- **THEN** no size-cap splitting occurs and clustering output is Leiden's raw result
