## MODIFIED Requirements

### Requirement: Partition quality in the report

`GRAPH_REPORT.md` SHALL print a `Partition quality` line naming the clustering
algorithm and giving the MQ, modularity and intra-edge fraction of the
communities written to `graph.json`, computed from the same `CommunityMap`
the export uses. The modularity SHALL be scored on the simple undirected
support graph of the build (one record per unordered pair of distinct nodes),
whether or not the build is `--directed`, and SHALL therefore lie in
`[-0.5, 1]`. (PRD §5.4)

#### Scenario: report agrees with export
- **WHEN** a clustered build completes
- **THEN** the MQ printed in `GRAPH_REPORT.md` equals `modularizationQuality` recomputed from the communities in `graph.json`

#### Scenario: algorithm named
- **WHEN** a build runs with `--cluster-algorithm bunch`
- **THEN** the `Partition quality` line names `bunch`; without the flag it names `leiden`

#### Scenario: directed build reports undirected modularity
- **WHEN** a build runs with `--directed`
- **THEN** the printed modularity equals the modularity of the same communities on the undirected projection of the graph, and lies in `[-0.5, 1]`

#### Scenario: solario-core regression
- **WHEN** solario-core `14d86be28` is built with `--granularity file --directed`
- **THEN** the printed modularity is at most `1` (it printed `1.2149` before this change)
