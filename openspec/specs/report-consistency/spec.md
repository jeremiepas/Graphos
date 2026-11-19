# report-consistency Specification

## Purpose
TBD - created by archiving change fix-graph-quality-and-tracing. Update Purpose after archive.

## Requirements

### Requirement: Report and export derive from identical graph data

`GRAPH_REPORT.md` and `graph.json` (PRD §12 export formats) MUST be generated from the same graph state: the enriched graph (post edge-inference) and the final community map (post re-clustering). Node, edge, and community totals stated in the report SHALL equal the counts of the corresponding arrays/objects in `graph.json`.

#### Scenario: Totals match between report and export

- **WHEN** the full pipeline completes on any input
- **THEN** the node count, edge count, and community count in `GRAPH_REPORT.md` equal `length(nodes)`, `length(edges)`, and the number of communities in `graph.json`

#### Scenario: Connectivity claims reflect exported graph

- **WHEN** the exported graph contains more than one connected component
- **THEN** the report does not claim the graph is well-connected, and articulation/biconnectivity statistics are computed on the same exported graph

### Requirement: Deduplicated surprising connections

The report's "Surprising Connections" section MUST NOT contain duplicate entries. Entries SHALL be unique by (source, target, reason).

#### Scenario: Repeated inferred edges collapse to one entry

- **WHEN** edge inference produces multiple identical surprising connections between the same source and target with the same reason
- **THEN** the rendered report lists that connection exactly once, verifiable by `cabal test` on the report renderer

### Requirement: Edge-collapse sanity warning

The pipeline MUST log a prominent warning after the build stage when a code-dominant input produces an edge-to-node ratio below a named threshold constant (default 0.05), so silent edge-extraction regressions (PRD §16.3 reliability) are surfaced.

#### Scenario: Implausibly sparse code graph triggers warning

- **WHEN** a build over predominantly code files yields 8,000 nodes and 1 edge
- **THEN** a warning identifying the low edge/node ratio is logged before export

#### Scenario: Sparse non-code inputs stay quiet

- **WHEN** an input dominated by images or documents yields few edges
- **THEN** no edge-collapse warning is logged

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

### Requirement: Bounded bridge-node table

The "Bridge Nodes (Articulation Points)" section of `GRAPH_REPORT.md` SHALL list at most a named constant number of articulation points (default 50), ordered by degree descending, excluding nodes of degree at most 1 (PRD §12 export formats). Each row SHALL render label, kind, source file and degree rather than the raw node id, and the section SHALL state the total number of articulation points and how many were omitted.

#### Scenario: Large graph report stays readable
- **WHEN** the report is rendered for a graph with 18,894 articulation points
- **THEN** the bridge table has at most 50 rows, a footer states `18,894 articulation points, 18,844 omitted`, and the report file is under 2 MB, verifiable by `cabal test` on the report renderer with a synthetic analysis

#### Scenario: Leaves are not bridges
- **WHEN** an articulation point has degree 1
- **THEN** it does not appear in the bridge table

### Requirement: Edge-drop accounting at build

The build stage SHALL log, at INFO, the number of extracted edges it discarded because an
endpoint was unknown and the number collapsed because they shared a (source, target) key, so the
difference between the extraction's edge count and the exported edge count is explained (PRD
§3.3 build stage, §16.3 reliability).

#### Scenario: Dropped edges are counted
- **WHEN** extraction yields 151,689 edges and the built graph exports 136,340
- **THEN** the log contains one line reporting the unknown-endpoint count and the collapsed-duplicate count whose sum is 15,349, verifiable by `cabal test` on a synthetic extraction with one dangling and one duplicate edge
