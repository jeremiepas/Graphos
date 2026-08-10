# HTML Depth Selector Capability

## Purpose

Define the behavior of the depth-level selector control in the `graph.html` viewer (PRD §12) that lets users switch between abstraction levels: Overview (community dots), Community (one community expanded), Full (all individual nodes), and Custom (N-hop neighborhood around a selected node). The selector replaces the implicit two-phase state machine with an explicit user-driven depth control, while preserving the LOD default for large graphs.

## ADDED Requirements

### Requirement: Depth selector control in the header

The HTML viewer SHALL render a depth selector control in the header (next to the search box) with four discrete levels: `Overview`, `Community`, `Full`, and `Custom`. The selector SHALL default to `Overview` on initial load. Selecting a level SHALL switch the rendered graph to that depth's dataset and destroy the previous `vis.Network` instance before creating the new one (no overlapping canvases).

- Plan: give users explicit control over abstraction level so the viewer serves both architectural overviews and node-level navigation.
- Do: add a `<select>` element to `htmlHeader`; wire its `change` event in `htmlBody` to a `switchDepth(level)` dispatcher.
- Check: the scenarios below verify each level renders the correct dataset.
- Act: if a level proves unusable above a graph-size threshold, document the threshold in the legend tooltip and cap the selector.

#### Scenario: Selector present on load and defaults to Overview

- **WHEN** `graph.html` is loaded in a browser
- **THEN** the header contains a `<select id="depthSelector">` with options `Overview`, `Community`, `Full`, `Custom`, and the selected option is `Overview`

#### Scenario: Switching to Full re-renders all nodes

- **WHEN** a user selects `Full` from the depth selector on a graph with 942 nodes and 3 communities
- **THEN** the viewer destroys any existing `vis.Network` instance and renders one dot per individual node (942 dots), colored by community, with all inter-node edges

#### Scenario: No overlapping canvases on depth switch

- **WHEN** a user switches from `Community` depth to `Full` depth
- **THEN** at most one `<canvas>` element exists inside `#graph` after the switch completes (the previous canvas is removed before the new network is created)

### Requirement: Community depth requires a selected community

The `Community` depth SHALL expand a single community into its member nodes. When the user selects `Community` depth without having previously chosen a community, the viewer SHALL keep the last selected community (or community 0 if none) expanded and SHALL update the `phaseHint` text to indicate which community is shown. Selecting a community dot while in `Community` depth SHALL swap the expanded community without returning to `Overview`.

- Plan: make the existing drill-down one of several explicit depth levels rather than a separate phase.
- Do: keep `expandedCommunity` as a module-level variable reused by the `Community` depth path.
- Check: switching community within `Community` depth updates the rendered members without an overview round-trip.

#### Scenario: Community depth expands last selected community

- **WHEN** a user drills into community 4 from Overview, then switches to `Full`, then selects `Community` from the depth selector
- **THEN** the viewer renders community 4's member nodes (the last expanded community)

#### Scenario: No community selected defaults to community 0

- **WHEN** the viewer loads and the user selects `Community` depth without ever clicking a community dot
- **THEN** the viewer renders community 0's members (or the first community id present in `communityAggregates`) and the `phaseHint` reads `Exploring Community <id> — <N> nodes`

### Requirement: Custom neighborhood depth with N-hop BFS

The `Custom` depth SHALL render an N-hop neighborhood around a user-selected node using BFS expansion over the embedded `allEdges` adjacency. The viewer SHALL show a numeric input (range 1–6, default 2) next to the depth selector, visible only when `Custom` is selected. Selecting a node while in `Custom` depth SHALL re-run the BFS from that node with the current hop count and re-render. The BFS SHALL be computed client-side over the already-shipped `allEdges` data (no backend round-trip, no new `graph.json` field).

- Plan: let users explore around a focal node without leaving the HTML viewer, matching the CLI `graphos neighbors <id> --depth N` ergonomics (PRD §13, neighbor-expansion spec).
- Do: add `buildNeighborhoodData(nodeId, hops)` in `htmlBody` that performs BFS over `allEdges` and returns `{nodes, edges}` induced subgraph.
- Check: the N=2 neighborhood of a known node matches the CLI `graphos neighbors <id> --depth 2` node set.
- Act: if JS BFS is slow above N=4 on large graphs, cap the input at 4 and add a tooltip warning.

#### Scenario: Custom depth shows neighborhood input

- **WHEN** a user selects `Custom` from the depth selector
- **THEN** a numeric input `id="neighborhoodHops"` becomes visible next to the selector with `min=1`, `max=6`, `value=2`

#### Scenario: N=2 neighborhood matches CLI neighbors

- **WHEN** a user in `Custom` depth clicks a node `X` with the neighborhood input set to `2`
- **THEN** the rendered node set equals the set of nodes reachable from `X` within 2 hops over `allEdges`, which is identical to the node set reported by `graphos neighbors X --depth 2`

#### Scenario: Changing hop count re-runs BFS

- **WHEN** a user in `Custom` depth with node `X` selected changes the neighborhood input from `2` to `3`
- **THEN** the viewer re-runs BFS from `X` with depth 3 and re-renders the induced subgraph

### Requirement: Depth persists across reload via sessionStorage

The selected depth level (and, for `Custom` depth, the last focused node id and hop count) SHALL be persisted in `sessionStorage` under keys `graphos_depth`, `graphos_neighborhood_node`, `graphos_neighborhood_hops`. On page load, the viewer SHALL read these keys and restore the depth (defaulting to `Overview` if absent or invalid). `sessionStorage` (not `localStorage`) is used so the preference is scoped to the browsing session and does not pollute future visits.

- Plan: avoid losing the user's view context on accidental reload or navigation.
- Do: write to `sessionStorage` in `switchDepth` and the neighborhood input `change` handler; read in the `DOMContentLoaded` init.
- Check: reload preserves the selected depth and Custom parameters.

#### Scenario: Reload preserves selected depth

- **WHEN** a user selects `Full` depth, then reloads the page (same tab/session)
- **THEN** the viewer loads directly into `Full` depth without the user re-selecting it

#### Scenario: Reload preserves Custom neighborhood parameters

- **WHEN** a user in `Custom` depth focuses node `X` with hops `3`, then reloads
- **THEN** the viewer loads into `Custom` depth with neighborhood input `3` and, if `X` is still present in the data, renders the N=3 neighborhood of `X`

#### Scenario: Absent keys default to Overview

- **WHEN** the page loads with no `graphos_depth` key in `sessionStorage` (fresh session)
- **THEN** the viewer initializes at `Overview` depth

### Requirement: Selector replaces the back button affordance

The `btnBack` "← Back" button SHALL be removed from the header. The back-to-overview affordance becomes selecting `Overview` in the depth selector. The `backToOverview` and `expandCommunity` JS functions SHALL remain as internal helpers called by the depth dispatcher, but SHALL NOT be the only user-facing entry points. No dead UI element (a button with no handler) SHALL remain in the DOM.

- Plan: collapse the two competing navigation affordances (back button + phase state) into one explicit selector.
- Do: remove the `<button id="btnBack">` markup; route all depth transitions through `switchDepth`.
- Check: no `btnBack` element exists in the rendered HTML; selecting Overview from any depth returns to the overview render.

#### Scenario: No back button in the DOM

- **WHEN** `graph.html` is rendered
- **THEN** no element with id `btnBack` exists in the header, and the depth selector is the only depth-navigation control

#### Scenario: Overview selectable from any depth

- **WHEN** the viewer is in `Community` depth and the user selects `Overview` from the depth selector
- **THEN** the viewer destroys the community network and renders the overview community dots, equivalent to the former `backToOverview()` behavior