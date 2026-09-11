# studio-data-sources

How the studio gets a graph: open a local `graph.json`, or connect to a `graphos serve`
origin. One data-source interface, two implementations — every other studio capability is
source-agnostic.

## ADDED Requirements

### Requirement: Local file mode

The studio SHALL open a local `graph.json` (file picker and drag-and-drop) entirely
client-side — no server, no network — parsing the `graph-json-contract` shape including
`community_aggregates` when present, and synthesizing aggregates client-side when absent. A
size guard SHALL refuse files above a documented threshold with a message recommending
connected mode, rather than freezing the tab.

- **Plan**: The offline path for small graphs and quick previews; the 4.4 GB case is exactly
  what the guard redirects to connected mode.
- **Do**: File API through a port; one decoder for the contract shape; aggregate synthesis
  reuses the community fields on nodes.
- **Check**: Scenarios below.
- **Act**: Measure the practical parse ceiling on reference hardware and set the threshold
  from data; record it in the design doc.

#### Scenario: Drag and drop renders offline

- **WHEN** a user drags a 40 MB `graph.json` onto the studio with networking disabled
- **THEN** the overview renders from the parsed file and every navigation feature works

#### Scenario: Oversized file is refused clearly

- **WHEN** a user opens a `graph.json` above the size threshold
- **THEN** the studio refuses with a message naming the threshold and recommending
  connected mode, and the tab remains responsive

### Requirement: Connected mode with capability detection

The studio SHALL connect to a user-supplied `graphos serve` origin and detect its capability
surface: slice and group-evaluation endpoints (per `progressive-graph-interface`) when
present; otherwise fall back to fetching `graph.json` from the static origin, subject to the
same size guard as file mode. Connection state (origin, reachability, detected capabilities,
graph hash) SHALL be visible in the shell, and mutation features SHALL be offered only when
the server's write surface is available.

- **Plan**: The studio must work against today's servers (static + `/api/query` +
  `/api/cypher/mutate`) and get better against servers that ship the slice API — without two
  code paths in every feature.
- **Do**: Capability probe on connect; the data-source interface routes per detected surface.
- **Check**: Scenarios below.
- **Act**: If probe latency is noticeable, cache detection per origin+hash and re-probe on
  hash change.

#### Scenario: Full-surface server uses slices

- **WHEN** the studio connects to a server exposing the slice endpoints on a large graph
- **THEN** the studio boots from aggregates and fetches community slices on drill-down,
  never requesting the full `graph.json`

#### Scenario: Legacy server falls back

- **WHEN** the studio connects to a server without slice endpoints serving a small graph
- **THEN** the studio fetches `graph.json` once, renders, and marks slice-dependent
  affordances with their unavailability reason

#### Scenario: Unreachable origin is explicit

- **WHEN** the configured origin stops responding mid-session
- **THEN** the shell shows a disconnected state with retry, and unsaved local work (edits,
  groups) is preserved

### Requirement: Graph identity and staleness

The studio SHALL track the identity of the loaded graph (server graph hash in connected
mode; content fingerprint in file mode), key all persisted per-graph state (groups, tuning,
edit log) by it, and on identity change SHALL never silently mix data: resident graph data is
dropped and re-fetched, while persisted per-graph state for the previous identity remains
stored under its own key.

- **Plan**: Same staleness discipline as `progressive-graph-viewer`, extended to studio
  artifacts — an edit log applied to the wrong graph version is corruption.
- **Do**: One identity value threaded through the data-source interface and the persistence
  keys.
- **Check**: Scenarios below.
- **Act**: If users need to migrate artifacts across graph versions, design an explicit
  migration flow — never implicit reuse.

#### Scenario: Server graph replaced mid-session

- **WHEN** the connected server's graph hash changes
- **THEN** the studio informs the user, drops resident graph data, and does not apply any
  pending edit intended for the previous hash

#### Scenario: Per-graph state does not leak

- **WHEN** a user loads corpus A (with groups defined), then corpus B
- **THEN** corpus B shows no groups from A, and reloading A restores A's groups
