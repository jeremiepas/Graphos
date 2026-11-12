## MODIFIED Requirements

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
