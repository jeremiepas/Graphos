# html-lod-viewer Capability — Delta

## Purpose

Add the first enforceable size budget for `graph.html`, strengthen the self-containment
requirement to include the renderer, and generalize the two-phase state machine to the four depth
levels absorbed from the superseded `add-profondeur-view-selector`. The requirements describing
the overview render, the aggregate dataset, overview physics and the `graphos serve` contract are
unchanged.

## MODIFIED Requirements

### Requirement: Self-contained HTML with inline data

The `graph.html` SHALL be self-contained: all graph data (nodes, edges, community aggregates)
SHALL be embedded inline as JSON, **and the rendering library, viewer JavaScript and stylesheet
SHALL be embedded in the document as well**, so the file works from `file://` with no network
access whatsoever. The document SHALL reference no external origin. The streaming-to-handle write
approach SHALL be preserved to avoid building the full HTML in memory.

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

## ADDED Requirements

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
