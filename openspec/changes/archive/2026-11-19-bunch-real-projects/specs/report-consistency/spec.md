## MODIFIED Requirements

### Requirement: Partition quality in the report

`GRAPH_REPORT.md` SHALL print a `Partition quality` line naming the clustering
algorithm and giving the MQ, modularity and intra-edge fraction of the
communities written to `graph.json`, computed from the same `CommunityMap`
the export uses. For a Leiden partition the three figures SHALL be scored over
every edge of the exported graph. For a Bunch partition they SHALL be scored
over the edges whose relation is in the Bunch MDG relations
(`clustering.bunch.relations` minus `contains`, `documents`, `depends_on`,
`inferred`) and whose two endpoints lie in MDG files (the system's code files,
see `bunch-clustering`). These are the edges Bunch clustered on: edges added
by edge inference after clustering, including the code↔doc `references` edges
of semantic and bounded edge inference, are not counted. The line SHALL name
those relations. Modularity SHALL be scored on the simple undirected support
graph of the scored edges (one record per unordered pair of distinct nodes),
whether or not the build is `--directed`, and SHALL therefore lie in
`[-0.5, 1]` (`modularity-undirected-view`). (PRD §5.4)

#### Scenario: report agrees with export
- **WHEN** a clustered build completes
- **THEN** the MQ printed in `GRAPH_REPORT.md` equals `modularizationQuality` recomputed from the communities in `graph.json` over the edges the line says it scored (every edge for Leiden, the Bunch MDG relations for Bunch)

#### Scenario: algorithm named
- **WHEN** a build runs with `--cluster-algorithm bunch`
- **THEN** the `Partition quality` line names `bunch`; without the flag it names `leiden`

#### Scenario: Bunch quality ignores inferred edges
- **WHEN** a Bunch build's exported graph holds `depends_on` / `inferred` edges added by edge inference
- **THEN** the printed MQ equals the MQ of the same communities over the import, call, reference, extends and implements edges only, and the line reads `Partition quality (bunch, scored over imports, calls, references, extends, implements)`

#### Scenario: Bunch quality ignores inferred code↔doc references
- **WHEN** edge inference adds `references` edges from code nodes to Markdown header nodes after a Bunch clustering
- **THEN** the printed MQ, modularity and intra-edge fraction equal those of the Step-4 partition before inference

#### Scenario: directed build reports undirected modularity
- **WHEN** a build runs with `--directed`
- **THEN** the printed modularity equals the modularity of the same communities on the undirected projection of the graph, and lies in `[-0.5, 1]`

#### Scenario: solario-core regression
- **WHEN** solario-core `14d86be28` is built with `--granularity file --directed`
- **THEN** the printed modularity is at most `1` (it printed `1.2149` before `modularity-undirected-view`)
