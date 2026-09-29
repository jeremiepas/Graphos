## MODIFIED Requirements

### Requirement: Report and export derive from identical graph data

`GRAPH_REPORT.md` and `graph.json` (PRD §12 export formats) MUST be generated
from the same graph state: the enriched graph (post edge-inference) and the
final community map (post re-clustering). Node, edge, and community totals
stated in the report SHALL equal the counts of the corresponding
arrays/objects in `graph.json`. Connectivity statistics (articulation points,
biconnected-component count) and cohesion values stated in the report SHALL
be the single set of values computed during the clustering/analysis stage on
that same graph — the report and the export paths SHALL NOT independently
recompute them, so they cannot diverge.

#### Scenario: Totals match between report and export

- **WHEN** the full pipeline completes on any input
- **THEN** the node count, edge count, and community count in
  `GRAPH_REPORT.md` equal `length(nodes)`, `length(edges)`, and the number of
  communities in `graph.json`

#### Scenario: Connectivity claims reflect exported graph

- **WHEN** the exported graph contains more than one connected component
- **THEN** the report does not claim the graph is well-connected, and
  articulation/biconnectivity statistics are the shared analysis values
  computed on the same exported graph

#### Scenario: Cohesion column equals the clustering cohesion map

- **WHEN** the report's Communities table is rendered
- **THEN** each community's cohesion figure equals the value in the cohesion
  map produced by clustering (no per-section recomputation)
