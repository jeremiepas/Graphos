# html-lod-viewer (delta)

The unconditional self-contained rule cannot hold at 4.4 GB of graph data. It becomes
size-conditional: below the threshold, today's behavior byte-for-byte; above it, a
remote-mode document (assets still inline, data fetched from `graph-slice-api`). All other
requirements of this capability (LOD phases, aggregates dataset, physics, drill-down
semantics, latency targets) are unchanged.

## MODIFIED Requirements

### Requirement: Self-contained HTML with inline data

The emitted `graph.html` SHALL be self-contained below a configurable payload-size threshold
(overridable by CLI flag in either direction): all graph data (nodes, edges, community aggregates)
SHALL be embedded inline as JSON, and the rendering library, viewer JavaScript and stylesheet
SHALL be embedded in the document as well, so the file works from `file://` with no network
access whatsoever. Above the threshold, the emitted document SHALL embed the rendering
library, viewer JavaScript and stylesheet — but no graph payload — and SHALL load data
exclusively from the slice endpoints under `graphos serve` (see `progressive-graph-viewer`),
showing an explicit message naming `graphos serve` when opened without a reachable server. In
both modes the document SHALL reference no external origin, and the streaming-to-handle write
approach SHALL be preserved to avoid building the full HTML in memory.

#### Scenario: HTML works without a server

- **WHEN** a below-threshold `graph.html` is opened directly via `file://` protocol
- **THEN** the viewer loads and renders the overview phase without any network fetch for
  graph data

#### Scenario: Works from file:// without a server

- **WHEN** a below-threshold `graph.html` is opened directly from the filesystem with
  networking disabled
- **THEN** the graph renders, all data is available, and zero network requests are issued

#### Scenario: No external origins referenced

- **WHEN** the emitted document (either mode) is searched for external URLs in `src` or
  `href` attributes
- **THEN** none are found

#### Scenario: Above-threshold export omits the payload

- **WHEN** the pipeline exports a graph whose payload exceeds the threshold
- **THEN** the emitted document contains no embedded graph data, its assets remain inline,
  and opened under `graphos serve` it boots from the aggregates endpoint

#### Scenario: Remote document names its requirement

- **WHEN** an above-threshold document is opened from `file://` with no server reachable
- **THEN** the page shows an explicit message that this graph requires `graphos serve`,
  rather than an empty canvas or a script error
