# html-viewer-interaction Specification

## Purpose
Give the viewer the interaction surface it lacks: faceted filtering over the metadata already in
the graph, a details panel that answers "what is this and what touches it", a legend that explains
the colors, and edge styling that distinguishes relations. The reference implementation is the
617-node subgraph viewer, whose controls (facet toggles, text filter,
click-through details with in/out neighbours, aggregate-driven legend, relation-keyed edge
styling) are the model for the requirements below. This capability is additive to the existing
search surface specified by `navigator-query-view`, which remains unchanged.
## Requirements
### Requirement: Facet filters over node and edge metadata

The viewer SHALL provide client-side facet filters over file type, node kind, community, edge
relation and bridge status, plus a free-text filter matching node label and source path. Filters
SHALL compose conjunctively across facets and SHALL re-render without reloading the document or
re-fetching data. Each facet SHALL display the count of items it matches.

- **Plan**: `file_type`, `kind`, `community_id`, `is_bridge` and edge `relation` are first-class
  fields already present in the graph, but the viewer exposes none of them as controls — a user
  cannot say "show only docs", "hide contains edges" or "only bridges".
- **Do**: Build the facet index from the interned payload at load; drive rendering from a single
  filter state object.
- **Check**: Scenarios below.
- **Act**: If facet evaluation costs more than the drill-down latency budget on the reference
  corpus, precompute per-facet index sets at export time and record the size impact.

#### Scenario: Filtering by file type

- **WHEN** a user enables the `doc` file-type facet on a mixed corpus
- **THEN** only document nodes and the edges between visible nodes remain rendered, and the facet
  shows the matching node count

#### Scenario: Hiding a relation

- **WHEN** a user disables the `contains` relation facet
- **THEN** no `contains` edge is rendered, and nodes that become isolated remain visible

#### Scenario: Facets compose

- **WHEN** a user enables the `code` file-type facet, the `Function` kind facet and types `config`
  in the text filter
- **THEN** the rendered set is the intersection of the three conditions

#### Scenario: Filtering does not refetch

- **WHEN** any facet is toggled on a document opened from `file://`
- **THEN** the view updates with no network request and no page reload

### Requirement: Node detail panel with location and neighbours

The viewer SHALL show, on node selection, a detail panel containing the node label, kind,
`source_file:line`, community label, degree, bridge status, and its incoming and outgoing
neighbours grouped by relation. Neighbour entries SHALL be clickable and SHALL select the
corresponding node.

- **Plan**: The current `showNodeDetail` (`HTML.hs:492–524`) cannot even resolve the community
  label because of the id type mismatch, and shows no neighbour lists.
- **Do**: Render the panel from the interned payload; resolve the community label from the
  aggregates.
- **Check**: Scenarios below.
- **Act**: If neighbour lists are unbounded on hub nodes, cap them with an explicit "and N more"
  affordance rather than truncating silently.

#### Scenario: Panel content

- **WHEN** a user selects a node defined at line 119 of `src/services/logging/resolve-logging-config.ts`
  in community `Logging`
- **THEN** the panel shows the label, its kind, `src/services/logging/resolve-logging-config.ts:119`,
  `Logging`, its degree, and its neighbour lists

#### Scenario: Neighbour navigation

- **WHEN** a user clicks a neighbour entry in the panel
- **THEN** that neighbour becomes the selected node and the panel updates to describe it

#### Scenario: Hub nodes are capped

- **WHEN** a selected node has 697 neighbours
- **THEN** the panel lists a bounded number and indicates how many more exist

### Requirement: Legend derived from community aggregates

The viewer SHALL render a legend listing communities with their color, label and member count,
ordered by member count descending, derived from the embedded aggregates. Selecting a legend entry
SHALL filter the view to that community. The legend SHALL NOT mutate the aggregate array it reads.

- **Plan**: The sidebar list sorts the shared `communityAggregates` array in place before slicing
  (`HTML.hs:245`), and the overview dot tooltip still shows `Community <id>` instead of the label
  (`HTML.hs:276`) — a residual scenario of `fix-community-labels-in-html`.
- **Do**: Render from a copy; use labels everywhere a community is named.
- **Check**: Scenarios below.
- **Act**: Keep a test asserting the aggregate array is unmodified after rendering.

#### Scenario: Legend uses labels

- **WHEN** community 4 is labeled `Authentication Module` with 17 members
- **THEN** the legend entry reads `Authentication Module` with its color swatch and `17`

#### Scenario: Overview tooltip uses the label

- **WHEN** a user hovers the overview dot for community 4
- **THEN** the tooltip names `Authentication Module`, not `Community 4`

#### Scenario: Depth hint uses the label

- **WHEN** a user expands community 4
- **THEN** the depth hint reads `Exploring Authentication Module — 17 nodes`

#### Scenario: Legend rendering is non-mutating

- **WHEN** the legend renders
- **THEN** the order of the underlying aggregate array is unchanged

### Requirement: Edge styling is keyed by relation

Edges SHALL be styled by relation through shared style definitions — distinct color and stroke
per relation, with structural relations rendered less prominently than dependency relations — and
the mapping SHALL be shown in the legend. No styling attribute SHALL be serialized per edge.

#### Scenario: Relations are visually distinct

- **WHEN** a graph containing `contains`, `imports` and `depends_on` edges is rendered
- **THEN** each relation is drawn with its own color and stroke style, and the legend documents
  the mapping

#### Scenario: Styling comes from definitions, not data

- **WHEN** the embedded payload is inspected
- **THEN** edge records carry only endpoints and a relation index

### Requirement: Existing search behaviour is preserved

The API-backed search surface specified by `navigator-query-view` SHALL continue to function
unchanged, including its client-side substring fallback when the query API is unavailable, and its
results SHALL respect the active facet filters.

#### Scenario: API search still works

- **WHEN** the document is served and a user searches a term
- **THEN** results are fetched from the query API and rendered with verdict, score and suggestions
  as before

#### Scenario: Offline fallback still works

- **WHEN** the document is opened from `file://` and a user searches
- **THEN** the client-side substring fallback returns matches

#### Scenario: Search respects facets

- **WHEN** a facet filter is active and a search returns nodes excluded by it
- **THEN** the excluded results are marked as filtered rather than silently rendered

