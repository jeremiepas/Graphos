# Report Consistency Capability

## ADDED Requirements

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
