# research-view

Multi-query induced-subgraph research view — runs N scored queries, takes the union of
matched nodes, induces the subgraph, and renders a self-contained interactive HTML (D3 /
vis-network force-directed) + JSON (`ResearchView`) artifact with per-term discovery legend,
community coloring, and hover-to-inspect node detail (PRD §7.2 compact navigable context,
§13.1 query family + HTML export, §16.1 query < 500ms).

## ADDED Requirements

### Requirement: graphos research subcommand

The system SHALL provide `graphos research <term>... [--subgraph <term>...] [--terms-file
<path>] [--label <text>] [--html] [--json] [--graph <path>] [--budget <n>] [--label-width
<n>] [--edges semantic|all]` which runs the scored query path (`queryGraphWithIndexScored`)
for each supplied term, takes the union of matched `QueryResponse.nodes[*].id`, induces the
subgraph (all edges where both endpoints are in the union), and renders the result as a
self-contained interactive HTML file (D3 / vis-network force-directed graph) and/or a single
`ResearchView` JSON document. The command SHALL honor the uniform flag surface (`--graph`,
`--budget`, `--label-width`, `--edges`) and SHALL NOT emit interleaved log lines on stdout
in JSON mode.

#### Scenario: research returns union of multiple queries
- **WHEN** `graphos research phase work block --json` is run on a graph where `graphos query
  "phase" --json` returns nodes `[A, B, C]`, `graphos query "work" --json` returns `[B, C, D]`,
  and `graphos query "block" --json` returns `[C, E]`
- **THEN** stdout is a single JSON document with `rvNodes` containing nodes `[A, B, C, D, E]`
  (the set union), `rvEdges` containing only edges where both endpoints are in `{A, B, C, D, E}`,
  and `rvTerms = ["phase", "work", "block"]`

#### Scenario: single-term equivalence with query
- **WHEN** `graphos research "auth" --json` is run and `graphos query "auth" --json` returns
  node ids `{X, Y, Z}`
- **THEN** the `research` JSON `rvNodes` set equals `{X, Y, Z}` exactly

#### Scenario: research on terms with no matches
- **WHEN** `graphos research zzzz nonexistent --json` is run and both terms return verdict
  `none` with empty `nodes`
- **THEN** stdout is a valid `ResearchView` JSON with empty `rvNodes`, empty `rvEdges`,
  non-empty `rvMetadata` (`node_count: 0, edge_count: 0`), and `rvTerms = ["zzzz",
  "nonexistent"]`

#### Scenario: research produces self-contained HTML
- **WHEN** `graphos research phase work --html --label test` is run
- **THEN** a file `graphos-out/research-test.html` is written that, when opened via
  `file://` in a browser, renders an interactive force-directed graph with no server
  dependency, shows a legend listing "phase" and "work" with distinct colors, and populates
  a detail panel on node hover showing the node's `discovered_by`, `best_score`,
  `source_file`, and `community`

#### Scenario: terms-file appends terms
- **WHEN** `graphos research phase --terms-file terms.txt --json` is run and `terms.txt`
  contains `work\nblock\n` (two lines)
- **THEN** the research view is built from terms `["phase", "work", "block"]` (positional
  terms first, file terms appended, duplicates removed, order preserved)

### Requirement: ResearchView JSON shape

The `ResearchView` JSON document SHALL contain: `terms` (array of input query terms in
order), `nodes` (array of `ResearchNode`), `edges` (array of induced edges), `communities`
(object keyed by community id, each with `label`, `composition`, `member_count`), and
`metadata` (`generated_at`, `graph_hash`, `node_count`, `edge_count`). Each `ResearchNode`
SHALL contain `id`, `label`, `source_file`, `community`, `discovered_by` (array of terms
that matched this node, in input order), `best_score` (highest score across discovering
terms), and `scores` (array of `{term, score}` pairs, one per input term, with `score: 0`
for terms that did not match this node). `composition` SHALL be `null` when `gCompositions`
is absent (legacy graph).

#### Scenario: node discovered by multiple terms
- **WHEN** `graphos research phase work --json` is run and node `B` is matched by both
  "phase" (score 0.8) and "work" (score 0.5)
- **THEN** the `ResearchNode` for `B` has `discovered_by: ["phase", "work"]`,
  `best_score: 0.8`, and `scores: [{"term": "phase", "score": 0.8}, {"term": "work",
  "score": 0.5}]`

#### Scenario: node discovered by one term
- **WHEN** `graphos research phase work --json` is run and node `A` is matched only by
  "phase" (score 0.9)
- **THEN** the `ResearchNode` for `A` has `discovered_by: ["phase"]`, `best_score: 0.9`,
  and `scores: [{"term": "phase", "score": 0.9}, {"term": "work", "score: 0}]`

#### Scenario: legacy graph composition is null
- **WHEN** `graphos research auth --json` is run on a `graph.json` without `compositions`
- **THEN** every entry in `communities` has `composition: null` while `label` and
  `member_count` are still populated

#### Scenario: metadata records graph hash and counts
- **WHEN** `graphos research phase work --json` is run against a `graph.json` with hash
  `a3f29c01` and the union has 42 nodes and 17 induced edges
- **THEN** `metadata.graph_hash` equals `"a3f29c01"`, `metadata.node_count` equals `42`,
  `metadata.edge_count` equals `17`, and `metadata.generated_at` is an ISO 8601 timestamp

### Requirement: Induced subgraph contains only union-internal edges

The `rvEdges` array SHALL contain exactly those edges from `gEdges` where both `edgeSource`
and `edgeTarget` are members of the union node set, after applying the `--edges
semantic|all` refinement (dropping trivia-target `contains` edges and self-edges in
`semantic` mode, per spec `query-noise-control`). No edge with an endpoint outside the
union SHALL appear in `rvEdges`.

#### Scenario: induced edges respect union
- **WHEN** `graphos research phase --json` is run and the union is `{A, B, C}` and the
  graph has edges `(A,B)`, `(B,C)`, `(C,D)`, `(D,E)`
- **THEN** `rvEdges` contains `(A,B)` and `(B,C)` only; `(C,D)` and `(D,E)` are excluded
  because `D` and `E` are not in the union

#### Scenario: semantic edge filtering applies to induced edges
- **WHEN** `graphos research phase --edges semantic --json` is run and the induced subgraph
  contains a `contains` edge targeting a node labeled `undefined`
- **THEN** that edge is absent from `rvEdges`

#### Scenario: all-edges mode preserves trivia edges
- **WHEN** the same research is run with `--edges all`
- **THEN** the trivia-targeting `contains` edge is present in `rvEdges`

### Requirement: --subgraph seed expansion

The `--subgraph <term>...` flag SHALL run the scored query path for each subgraph term, add
the matched nodes to the union, then expand the union by one BFS hop (all immediate
neighbors of every union node are added). The induced subgraph SHALL then be recomputed on
the expanded union. `--subgraph` SHALL only add nodes — it SHALL NEVER remove nodes that
were in the original union.

#### Scenario: subgraph expands union by one hop
- **WHEN** `graphos research phase --subgraph work --json` is run, "phase" matches `{A, B}`,
  "work" matches `{C}`, and `A` has a neighbor `D` (not matched by any term)
- **THEN** the union is `{A, B, C, D}` (D added by 1-hop BFS from A), and `rvEdges` includes
  edges among all four nodes

#### Scenario: subgraph never removes original matches
- **WHEN** `graphos research phase --subgraph work --json` is run and "phase" matches `{A, B}`
- **THEN** `A` and `B` are always present in `rvNodes` regardless of `--subgraph` results

### Requirement: HTML discovery legend and detail panel

The research HTML SHALL include a legend listing each input term with a distinct color
(D3 schemeCategory10 or equivalent deterministic palette), and SHALL color each node by the
color of the first term that discovered it (in input order). The HTML SHALL include a detail
panel (`<div id="research-detail">`) that, on node hover or click, displays the node's
`discovered_by` list, `best_score`, per-term `scores`, `source_file`, and `community`. The
HTML SHALL be self-contained (openable via `file://` with no server dependency for static
rendering, matching the `navigator-query-view` offline-fallback contract).

#### Scenario: legend lists all terms
- **WHEN** `graphos research phase work block --html` is run
- **THEN** the HTML contains a legend with three entries ("phase", "work", "block"), each
  with a distinct color swatch

#### Scenario: node colored by first discoverer
- **WHEN** node `B` is discovered by "phase" (input order 1) and "work" (input order 2)
- **THEN** the HTML renders `B` with the color assigned to "phase" (the first discoverer)

#### Scenario: detail panel populates on hover
- **WHEN** the user hovers over node `B` in the rendered HTML
- **THEN** the detail panel shows `discovered_by: phase, work`, `best_score: 0.8`,
  `scores: phase=0.8, work=0.5`, `source_file: src/Auth.hs`, `community: 483`

#### Scenario: HTML opens offline
- **WHEN** the generated `research-test.html` is opened via `file://` in a browser with no
  server running
- **THEN** the force-directed graph renders (vis-network loads from CDN on first open), the
  legend and detail panel are present, and node hover populates the detail panel with no
  network calls beyond the CDN script tag

### Requirement: HTTP port endpoint for research (deferred)

Once the `query-http-port` HTTP port lands, the system SHALL expose
`GET /api/research?terms=a,b,c&subgraph=d,e&edges=semantic` returning the same `ResearchView`
JSON as the CLI `--json` path, with `terms` parsed as a comma-separated list and `subgraph`
as a comma-separated list of seed terms.

#### Scenario: HTTP research matches CLI
- **WHEN** `curl '/api/research?terms=phase,work&edges=semantic'` is called
- **THEN** the response JSON is byte-for-byte equal to
  `graphos research phase work --edges semantic --json` for the same `graph.json`

#### Scenario: empty terms returns 400
- **WHEN** `curl '/api/research?terms='` is called
- **THEN** the response has HTTP status 400 with a clear error message