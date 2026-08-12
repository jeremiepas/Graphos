# query-noise-control

## Purpose

Signal-per-token controls for query-family output (`query`, `path`, `explain`, `neighbors`; PRD §13.1, §7.3 compact context format). Removes structural trivia, duplicate declarations, and oversized labels that drown semantic content.

Signal-per-token controls for query-family output (`query`, `path`, `explain`,
`neighbors`; PRD §13.1, §7.3 compact context format). Removes structural trivia,
duplicate declarations, and oversized labels that drown semantic content.
## Requirements
### Requirement: Semantic edge filtering
Query-family commands — including the `select_context` and `query_graph` MCP tools —
SHALL accept `--edges semantic|all` with default `semantic`. In `semantic` mode the
output MUST exclude `contains` edges whose target label is a trivia token (`undefined`,
`unknown`, `null`, bare primitive or wrapper types such as `Promise` and `Result`, and
single-token type parameters) AND any edge whose `Confidence` is below the `INFERRED`
threshold (i.e. `AMBIGUOUS`-confidence edges are dropped by default). `--edges all` SHALL
restore the unfiltered edge set. Edges within a rendered payload SHALL be ordered by
query-relevance of their endpoints (descending), not by map-iteration order.

#### Scenario: Trivia edges dropped by default
- **WHEN** a query result subgraph contains a `contains` edge targeting a node labeled `undefined`
- **THEN** that edge is absent from default `select_context` and `query_graph` output

#### Scenario: Ambiguous edges dropped by default
- **WHEN** the selected subgraph contains an edge with `Confidence < 0.7` (labelled `AMBIGUOUS`)
- **THEN** that edge is absent from default `select_context` and `query_graph` output

#### Scenario: All-edges mode preserves everything
- **WHEN** the same query is run with `--edges all` on either MCP tool
- **THEN** the trivia-targeting and `AMBIGUOUS` edges are present in the output

#### Scenario: Edges are relevance-ranked
- **WHEN** a `select_context` response contains multiple edges
- **THEN** edges are ordered so those with higher-relevance endpoints appear before lower-relevance ones

### Requirement: Self-referential edge collapse
Query-family output MUST NOT contain edges whose source and target resolve to the same
node id.

#### Scenario: Self-edge suppressed
- **WHEN** the selected subgraph contains an edge from a node to itself
- **THEN** the rendered output contains no `X --rel--> X` line for it

### Requirement: Duplicate declaration deduplication
Query-family output SHALL merge nodes that represent the same declaration — labels
differing only by declaration prefix (such as `export const X`, `const X`, `X`) with an
identical source file and identical start line — into a single rendered node using the
shortest label, with edges from all merged variants attached to it.

#### Scenario: Declaration triple collapses to one node
- **WHEN** the result set contains `export const foo = …`, `const foo = …`, and `foo` all pointing at the same file and line
- **THEN** exactly one node for `foo` is rendered, carrying the union of their edges

#### Scenario: Same-name symbols at different locations stay separate
- **WHEN** two nodes share a label but have different source files or lines
- **THEN** both nodes are rendered separately

### Requirement: Label elision
Query-family commands SHALL accept `--label-width N` (default 120). Labels longer than N
characters MUST be elided at a word boundary with an ellipsis, and the full node id MUST
still be printed so the complete node remains retrievable via `explain`.

#### Scenario: Long label elided
- **WHEN** a matched node's label is 400 characters and `--label-width` is left at default
- **THEN** the rendered label is at most 120 characters plus an ellipsis, and the node id is printed unelided

#### Scenario: Custom width honored
- **WHEN** `--label-width 40` is passed
- **THEN** no rendered label exceeds 40 characters plus the ellipsis marker

### Requirement: query_graph MCP response carries verdict, hash, and ranked nodes
The `query_graph` MCP tool SHALL return a JSON object with fields `verdict`
(`strong|weak|none`), `best_score`, `hash` (result-set hash over ordered result node ids),
`nodes` (scored, descending), `edges`, and `omitted` (counts of nodes/edges dropped by
budget truncation). The handler MUST compute this from a single call to
`queryGraphWithIndexScored` and MUST NOT invoke the query path more than once per
request. When `verdict` is `none`, the response MUST contain zero nodes and zero edges.

#### Scenario: Strong match returns ranked nodes and verdict
- **WHEN** `query_graph` is called with terms that match node labels at or above the strong threshold
- **THEN** the response JSON contains `verdict: "strong"`, a numeric `best_score`, a `hash`, and a `nodes` array ordered by descending score

#### Scenario: No-match returns empty node set with verdict
- **WHEN** `query_graph` is called with terms absent from the graph vocabulary
- **THEN** the response JSON contains `verdict: "none"`, an empty `nodes` array, an empty `edges` array, and `omitted: {nodes: 0, edges: 0}`

#### Scenario: Query path invoked exactly once
- **WHEN** `query_graph` is called
- **THEN** the underlying `queryGraphWithIndexScored` is invoked exactly one time for that request (no triple invocation)

