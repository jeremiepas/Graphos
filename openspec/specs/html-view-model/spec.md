# html-view-model Specification

## Purpose
Define the payload embedded in `graph.html` as an explicit, interned, style-free view model.
Today the payload is 99.8% of a 101.2 MB artifact (42.6 MB nodes at 409 B/node, 53.9 MB edges at
441 B/edge) and most of those bytes are constants repeated per item, endpoint ids repeated as
long strings, and fields duplicating other fields (`HTML.hs:824–856`).
## Requirements
### Requirement: Payload string tables intern repeated values

The embedded payload SHALL intern node ids, `source_file` values, `kind` values and edge
`relation` values into string tables, with nodes and edges referencing entries by integer index.
Each distinct string SHALL appear exactly once in the document.

- **Plan**: A 104K-node corpus has ~3,862 distinct source files and a handful of kinds and
  relations, yet each is written out per item; endpoint ids are written twice per edge and are
  themselves long text.
- **Do**: Build the tables during projection; emit `strings`, `files`, `kinds`, `relations`
  arrays plus index-bearing node and edge records.
- **Check**: Scenarios below plus the budget requirement in `html-lod-viewer`.
- **Act**: If interning shifts the cost to the id table, evaluate emitting ids only for nodes
  reachable by deep-link and referencing the rest positionally.

#### Scenario: Source files appear once

- **WHEN** a graph with 104,101 nodes spanning 3,862 distinct source files is exported
- **THEN** each source-file path appears exactly once in the document, and every node references
  it by integer index

#### Scenario: Edge endpoints are indices

- **WHEN** any edge is serialized
- **THEN** its source and target are integer indices into the node array, and no node id string
  appears inside the edge payload

#### Scenario: Round-trip fidelity

- **WHEN** the interned payload is expanded back to `(id, label, source_file, kind, relation)`
  tuples
- **THEN** the result equals the same tuples derived directly from the in-memory graph, for every
  node and every edge

### Requirement: Payload carries no presentation constants

The payload SHALL NOT contain any value that is constant across items or derivable from another
field. Specifically it SHALL NOT contain per-node `color`, `group` or `title`, and SHALL NOT
contain per-edge `color`, `arrows`, `dashes`, `width`, `title` or `label`. Presentation SHALL be
expressed once, as renderer group definitions and CSS derived from the community palette and the
relation set.

- **Plan**: `color` is `colorForCommunity community_id` (`HTML.hs:900`), `group` equals
  `community_id` (`HTML.hs:903–904`), `title` is `source_file <> " [" <> cid <> "]"`
  (`HTML.hs:898`), and edge `title` equals edge `label` (`HTML.hs:919–920`).
- **Do**: Move styling into a single groups/CSS block; compose tooltips in the viewer from the
  fields already present.
- **Check**: Scenarios below, asserted by a key-set test over the emitted payload.
- **Act**: If a renderer feature genuinely requires a per-item value, add it to the view model
  explicitly with a recorded reason rather than reintroducing blanket duplication.

#### Scenario: Forbidden keys are absent

- **WHEN** the emitted payload is parsed
- **THEN** no node record contains `color`, `group` or `title`, and no edge record contains
  `color`, `arrows`, `dashes`, `width`, `title` or `label`

#### Scenario: Tooltips are still available

- **WHEN** a user hovers a node in the viewer
- **THEN** the tooltip shows the source file and community, composed client-side from the node's
  file index and community id

#### Scenario: Community color is applied once

- **WHEN** a graph with 17,651 communities is exported
- **THEN** the color for each community is defined once in a group/CSS block and referenced by
  its members, and no color literal is repeated per node

### Requirement: Node view model carries only varying fields

Each node record SHALL carry exactly: label, file index, line start, community id, degree, bridge
flag, kind index and file type. It SHALL NOT carry the node signature. Signatures SHALL be
retrieved on demand from the query API (`/api/explain`) when the document is served, and the
detail panel SHALL degrade to omitting the signature when opened from `file://`.

#### Scenario: Signature is not embedded

- **WHEN** a graph whose nodes carry signatures is exported
- **THEN** no signature text appears in the embedded payload

#### Scenario: Detail panel fetches the signature when served

- **WHEN** the document is served by `graphos serve` and a user opens a node's details
- **THEN** the signature is fetched from the query API and displayed

#### Scenario: Detail panel degrades offline

- **WHEN** the document is opened from `file://` and a user opens a node's details
- **THEN** the panel renders kind, location, community, degree and neighbours with no signature
  section and no error

### Requirement: Community aggregates have a single source of computation

Community aggregates embedded in the viewer SHALL be the values computed by
`Graphos.UseCase.Cluster.computeCommunityAggregates`, passed into the exporter. The exporter
SHALL NOT recompute aggregates, articulation points, or cohesion, and SHALL NOT re-parse graph
data it already holds in typed form.

- **Plan**: `HTML.hs:935–983` duplicates `UseCase/Cluster.hs:96–160`, recomputes
  `articulationPoints` (`HTML.hs:44` and `:939`) and `cohesionScore` (`HTML.hs:953`), and
  round-trips `gCompositions` through `encode`/`eitherDecode` (`HTML.hs:945–949`). The duplicate
  emits `inter_community_edges = 0` (`HTML.hs:960`), violating `html-lod-viewer`.
- **Do**: Thread the already-computed aggregates into the export port; delete the duplicate.
- **Check**: Scenarios below.
- **Act**: Any future aggregate field is added in `Cluster.hs` only; the exporter projects it.

#### Scenario: Inter-community edges are real

- **WHEN** a clustered graph is exported and a community has edges to three other communities
- **THEN** that community's `inter_community_edges` in the viewer payload lists those three
  targets with their counts, and is not `0` or empty

#### Scenario: Viewer and graph.json agree

- **WHEN** the same run writes `graph.json` and `graph.html`
- **THEN** for every community, `member_count`, `cohesion`, `bridge_count` and
  `inter_community_edges` are identical in both artifacts

#### Scenario: Articulation points computed once

- **WHEN** the export stage runs
- **THEN** articulation points are computed once and passed to the exporter, not recomputed
  inside it

### Requirement: Community identity has one type across the payload

Community identifiers SHALL be emitted with the same JSON type in every section of the payload —
node records, aggregate records and any index — so that strict equality comparisons in the viewer
succeed.

- **Plan**: `HTML.hs:951` emits aggregate ids as strings (`T.pack (show cid)`) while
  `HTML.hs:832` emits node `community_id` as a number, so `find(c => c.id === cid)`
  (`HTML.hs:513–515`) never matches and the node-detail panel always shows a bare
  `Community <n>` — the unmet part of `fix-community-labels-in-html`.
- **Do**: Emit numeric community ids everywhere.
- **Check**: Scenario below plus a viewer test that the detail panel shows the label.
- **Act**: Add a type-agreement assertion to the golden test so the divergence cannot return.

#### Scenario: Node detail shows the community label

- **WHEN** community 4 is labeled `Authentication Module` and a user opens the details of one of
  its member nodes
- **THEN** the detail panel shows `Authentication Module`, not `Community 4`

### Requirement: Payload emission is deterministic

Two exports of an unchanged graph SHALL produce byte-identical payload sections, with stable
ordering of nodes, edges, aggregates and string-table entries.

#### Scenario: Repeated export is byte-identical

- **WHEN** the exporter runs twice over the same in-memory graph
- **THEN** the emitted node, edge, string-table and aggregate sections are byte-identical between
  the two runs

