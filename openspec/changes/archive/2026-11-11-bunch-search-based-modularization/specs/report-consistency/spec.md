## ADDED Requirements

### Requirement: Partition quality in the report

`GRAPH_REPORT.md` SHALL print a `Partition quality` line naming the clustering
algorithm and giving the MQ, modularity and intra-edge fraction of the
communities written to `graph.json`, computed from the same `CommunityMap`
the export uses. (PRD §5.4)

#### Scenario: report agrees with export
- **WHEN** a clustered build completes
- **THEN** the MQ printed in `GRAPH_REPORT.md` equals `modularizationQuality` recomputed from the communities in `graph.json`

#### Scenario: algorithm named
- **WHEN** a build runs with `--cluster-algorithm bunch`
- **THEN** the `Partition quality` line names `bunch`; without the flag it names `leiden`
