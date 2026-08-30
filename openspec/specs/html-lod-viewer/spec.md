# html-lod-viewer Specification

## Purpose
Define the behavior of the interactive HTML graph viewer (`graph.html`) so that large graphs (78K+ nodes) render fluidly and are explorable, replacing the flat single-render vis-network model with a two-phase community-based level-of-detail (LOD) viewer. Served over HTTP via `graphos serve` (PRD §13.1) as the primary delivery path.
## Requirements
### Requirement: Two-phase level-of-detail rendering

The HTML viewer SHALL render the graph at a user-selected depth level — `Overview` (one dot per
community, sized by member count, colored by community), `Community` (one community expanded into
its members with internal and bridge edges), `Full` (all individual nodes, no aggregation), or
`Custom` (N-hop neighbourhood around a selected node) — defaulting to `Overview` on initial load.
The viewer SHALL NOT render all individual nodes simultaneously unless the user explicitly selects
`Full` or `Custom`. The former two-phase `currentPhase` state machine is replaced by a
multi-level depth state (see `html-depth-selector`).

#### Scenario: Overview depth renders community dots only

- **WHEN** the viewer loads a graph with 104,101 nodes across its detected communities
- **THEN** the overview renders exactly one dot per community and zero individual node dots

#### Scenario: Community depth expands a single community

- **WHEN** a user selects a community dot at `Overview` depth
- **THEN** that community's members render as individual nodes with internal edges and dashed
  bridge edges to the remaining collapsed community dots

#### Scenario: Full depth requires explicit selection

- **WHEN** the viewer is at `Overview` or `Community` depth
- **THEN** the number of rendered node-level dots does not exceed the member count of the single
  expanded community, and is strictly less than the total node count for graphs with more than one
  community

#### Scenario: Overview phase renders community dots only

- **WHEN** the HTML viewer loads a graph with 78,529 nodes across 8,519 communities
- **THEN** the overview phase renders exactly one dot per community (8,519 dots) and zero individual node dots

#### Scenario: Drill-down expands a single community

- **WHEN** a user clicks a community dot in the overview phase
- **THEN** that community's members are rendered as individual node dots with their internal edges and any bridge edges to other communities, and the remaining communities stay collapsed as dots

#### Scenario: No simultaneous full-graph render

- **WHEN** the viewer is in any phase (overview or drill-down)
- **THEN** the number of rendered node-level dots SHALL NOT exceed the member count of the single expanded community plus the community dots, and SHALL be strictly less than the total node count for graphs with more than one community

### Requirement: Community aggregate dataset in graph.json

The JSON export SHALL include a top-level `community_aggregates` key containing one entry per detected community with the fields: `id`, `member_count`, `cohesion`, `bridge_count`, `color`, `label`, `representative_labels` (up to 3 member labels), and `inter_community_edges` (list of target community IDs with edge counts). This dataset SHALL be the data source for the overview phase. The `label` field SHALL be the LLM-generated label when `community_labels` is provided for that community id, and SHALL fall back to `"Community <id>"` when no label is available (absent `community_labels` map, or community id not in the map). The same labeled dataset SHALL be embedded inline in `graph.html` so the HTML viewer displays the labels without reading `graph.json`.

- Plan: ensure the HTML viewer shows the labels the user paid LLM tokens to produce, instead of the placeholder.
- Do: thread `Maybe (Map CommunityId Text)` from `exportAll` → `epExportHTML` → `communityAggregatesToJSON`; use the label when present, fall back otherwise.
- Check: the scenarios below verify the label appears in the embedded HTML data and the sidebar.

#### Scenario: Community aggregates present in export

- **WHEN** the pipeline runs on a graph with 8,519 communities
- **THEN** `graph.json` contains a `community_aggregates` array with exactly 8,519 entries

#### Scenario: Aggregate fields populated

- **WHEN** a community with 17 members and 2 bridge nodes is exported
- **THEN** its aggregate entry has `member_count = 17`, `bridge_count = 2`, `cohesion` equal to the Leiden cohesion score, `color` from the community palette, `label` from community labeling (or `Community <id>` fallback), and `representative_labels` containing up to 3 member node labels

#### Scenario: Inter-community edges listed

- **WHEN** community A has 5 edges to community B and 2 edges to community C
- **THEN** community A's `inter_community_edges` contains entries for B (count 5) and C (count 2)

#### Scenario: Aggregate fields populated with LLM label

- **WHEN** a community with id 4 and 17 members is exported with `community_labels = {4: "Authentication Module"}`
- **THEN** its aggregate entry has `label = "Authentication Module"` (not `"Community 4"`), and the same `label` appears in the `_communityAggregatesData` embedded in `graph.html`

#### Scenario: Fallback when no labels provided

- **WHEN** a community with id 7 is exported with `community_labels = Nothing` (or a map that does not contain key 7)
- **THEN** its aggregate entry has `label = "Community 7"` (the fallback), both in `graph.json` and in `graph.html`'s embedded data

#### Scenario: HTML viewer shows label in sidebar

- **WHEN** `graph.html` is opened in a browser and the embedded `_communityAggregatesData` contains an entry with `label = "Authentication Module"`
- **THEN** the sidebar community list renders that entry with the text `"Authentication Module"` (from `c.label`) rather than `"Community 4"`, and the overview dot's tooltip includes the label

### Requirement: Overview physics uses forceAtlas2Based

The overview phase SHALL use the `forceAtlas2Based` physics solver with `hideEdgesOnDrag: true` for the community-dot graph, and SHALL disable physics after stabilization completes. This matches the graphify HTML approach that renders ~7K nodes acceptably and is required for the 8K-community-dot overview to stay interactive.

#### Scenario: Overview solver is forceAtlas2Based

- **WHEN** the overview phase initializes the vis-network instance
- **THEN** the physics solver is `forceAtlas2Based` (not `barnesHut`) and `hideEdgesOnDrag` is `true`

#### Scenario: Physics disables after stabilization

- **WHEN** the overview phase completes stabilization
- **THEN** physics is disabled so the community-dot layout is static for interaction

### Requirement: Drill-down reuses member nodes with community color

The drill-down phase SHALL render member nodes using their community color (from the joined `nodeCommunityId`) and SHALL include edges internal to the community plus bridge edges (edges where one endpoint is in the expanded community and the other is in a different community, shown as dashed lines to the collapsed community dots). The `phaseHint` text shown when drilling into a community SHALL use the community's label (LLM label when available, `"Community <id>"` fallback) instead of only the numeric id.

#### Scenario: Member nodes colored by community

- **WHEN** a user drills into community 4 (color `#7dd3fc`)
- **THEN** all 17 member nodes are rendered with background color `#7dd3fc`

#### Scenario: Bridge edges shown to collapsed communities

- **WHEN** a member node of the expanded community has an edge to a node in community 8
- **THEN** a dashed edge is drawn from the member node to community 8's dot (the target node is not individually rendered)

#### Scenario: Drill-down phaseHint uses label

- **WHEN** a user drills into community 4 which has the LLM label `"Authentication Module"`
- **THEN** the `phaseHint` reads `"Exploring Authentication Module — 17 nodes"` (using the label), not `"Exploring Community 4 — 17 nodes"`

### Requirement: Self-contained HTML with inline data

The `graph.html` SHALL be self-contained: all graph data (nodes, edges, community aggregates)
SHALL be embedded inline as JSON, **and the rendering library, viewer JavaScript and stylesheet
SHALL be embedded in the document as well**, so the file works from `file://` with no network
access whatsoever. The document SHALL reference no external origin. The streaming-to-handle write
approach SHALL be preserved to avoid building the full HTML in memory.

#### Scenario: HTML works without a server

- **WHEN** `graph.html` is opened directly via `file://` protocol
- **THEN** the viewer loads and renders the overview phase without any network fetch for graph data

#### Scenario: Works from file:// without a server

- **WHEN** `graph.html` is opened directly from the filesystem with networking disabled
- **THEN** the graph renders, all data is available, and zero network requests are issued

#### Scenario: No external origins referenced

- **WHEN** the emitted document is searched for external URLs in `src` or `href` attributes
- **THEN** none are found

#### Scenario: Streaming write preserves low memory

- **WHEN** the export runs on a 100K-node graph
- **THEN** the export writes header, embedded assets, inline payload and footer in separate handle
  writes, so peak memory does not hold the full HTML in memory

### Requirement: Served via graphos serve

The existing `graphos serve --dir <d> --port <p>` command (PRD §13.1) SHALL serve the LOD viewer over HTTP without modification to the static server. The HTML file is the same self-contained artifact whether served or opened via `file://`.

#### Scenario: Serve delivers the LOD HTML

- **WHEN** `graphos serve --dir graphos-out --port 8080` runs and a browser navigates to `http://localhost:8080/graph.html`
- **THEN** the two-phase LOD viewer loads and renders the overview phase

### Requirement: Interaction latency targets

The LOD viewer SHALL meet interaction latency targets for graphs up to 100K nodes: initial
overview load < 3 seconds, drill-down into a community < 500 ms, pan/zoom frame rate > 30 fps with
`hideEdgesOnDrag` enabled — **and these targets SHALL be measured and recorded on a reference
corpus rather than asserted**. The prior citation of "PRD §16.1" is removed: `PRD.md:797–805`
contains no HTML or browser row, so this requirement is the authoritative source until the PRD
gains one.

#### Scenario: Targets are measured, not assumed

- **WHEN** a change to the viewer or its payload is proposed
- **THEN** load, drill-down and frame-rate figures are recorded for the reference corpus, with the
  measurement method stated

#### Scenario: Drag optimisation is effective

- **WHEN** the user pans or zooms at any depth
- **THEN** edge rendering is suppressed during the gesture through the renderer's interaction
  settings

#### Scenario: Initial load under 3 seconds

- **WHEN** a 78K-node / 8.5K-community graph.html is loaded in a browser
- **THEN** the overview phase is interactive (pannable, zoomable) within 3 seconds of page load

#### Scenario: Drill-down under 500ms

- **WHEN** a user clicks a community dot in the overview
- **THEN** the drill-down expansion completes and member nodes are interactive within 500ms

#### Scenario: Pan/zoom stays above 30fps

- **WHEN** a user pans or zooms the overview phase with 8,519 community dots rendered
- **THEN** the frame rate remains above 30fps (edges hidden during drag via `hideEdgesOnDrag`)

### Requirement: Embedded payload size budget

The embedded payload SHALL respect a size budget: at most 200 bytes per node, at most 24 bytes per
edge, and at most 30 MB total for a graph of 104,101 nodes and 122,347 edges. The export SHALL
report the emitted payload size and the per-item averages, and the budget SHALL be asserted by an
automated test.

- **Plan**: The measured baseline for that exact corpus is 101.2 MB — 42.6 MB of nodes (409
  B/node) and 53.9 MB of edges (441 B/edge), against 176 KB of actual document. The archived
  large-graph analysis concluded the browser wall is JSON parse plus heap, so bytes are the
  primary lever available without changing renderer or storage architecture.
- **Do**: Enforce the budget with the interned, style-free view model (`html-view-model`).
- **Check**: Scenarios below, computed from the emitted bytes rather than estimated.
- **Act**: If the budget is met and the browser still stalls at 158K nodes, that measurement is
  the trigger for the deferred sidecar/WebGL architecture — with evidence attached.

#### Scenario: Reference corpus fits the budget

- **WHEN** the reference corpus of 104,101 nodes and 122,347 edges is exported
- **THEN** the emitted `graph.html` is at most 30 MB, with at most 200 bytes per node and at most
  24 bytes per edge of payload

#### Scenario: Budget is enforced automatically

- **WHEN** a change causes the per-node or per-edge payload to exceed its budget
- **THEN** the test suite fails, naming the measured and permitted values

#### Scenario: Export reports payload size

- **WHEN** the export stage completes
- **THEN** the run output states the emitted payload size and the per-node and per-edge averages

