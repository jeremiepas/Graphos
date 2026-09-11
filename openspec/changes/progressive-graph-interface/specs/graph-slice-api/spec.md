# graph-slice-api

Bounded, cursor-paged slice endpoints on `graphos serve`, so a client can explore a graph of
any size (4.4 GB `graph.json` targets) without any single response — or the sum of a
navigation session — approaching the full payload. All endpoints are read-only, served from
the shared in-memory graph + `GraphIndex`, and share `query-http-port`'s routing, CORS and
405 behavior.

## ADDED Requirements

### Requirement: Overview aggregates endpoint

The server SHALL expose an endpoint returning the `community_aggregates` dataset (id,
member_count, cohesion, bridge_count, color, label, representative_labels,
inter_community_edges) plus graph totals (node count, edge count, community count) and the
graph content hash — and SHALL NOT include any individual nodes or edges in this response.
This is the only data a fresh remote-mode viewer load requires.

- **Plan**: The aggregates already exist as the LOD overview data source
  (`html-lod-viewer`); this endpoint serves them instead of relying on inline embedding.
- **Do**: Serve from the shared load; include the hash so clients can invalidate caches.
- **Check**: Scenarios below.
- **Act**: If aggregate payloads grow past interactive size on extreme community counts,
  page this endpoint like the others and record the threshold.

#### Scenario: Aggregates only, with totals

- **WHEN** a client requests the overview endpoint on a graph with 1.2M nodes and 8K
  communities
- **THEN** the response contains 8K aggregate entries, the totals (1.2M nodes), and the graph
  hash, and contains zero node or edge objects

#### Scenario: Hash tracks the loaded graph

- **WHEN** the in-memory graph is replaced (e.g. by a cypher mutation) and the overview is
  re-requested
- **THEN** the returned hash differs from the previous response's hash

### Requirement: Community slice endpoint

The server SHALL expose an endpoint returning one community's member nodes and internal
edges, cursor-paged, ordered by degree descending (hubs first), with per-page size bounded by
a server-side hard cap. Each page SHALL include: the page of nodes/edges, the community's
total member and internal-edge counts, the next cursor (absent on the last page), and the
bridge edges from already-returned members to other communities (target community id only —
not the foreign nodes).

- **Plan**: Drill-down today renders from the embedded payload; this endpoint feeds the same
  drill-down phase remotely, hubs-first so the first page is the most informative.
- **Do**: Serve from the community reverse index and precomputed adjacency in `GraphIndex`.
- **Check**: Scenarios below.
- **Act**: If degree ordering makes pathological pages on hub-heavy communities, fall back to
  stable id order past the first page and note the cutoff.

#### Scenario: First page of a large community

- **WHEN** a client requests community 483 (12,000 members) with page size 500
- **THEN** the response has the 500 highest-degree members, their internal edges, totals
  saying 12,000, and a cursor for the next page

#### Scenario: Cursor walks to completion

- **WHEN** a client follows cursors until absent on a community of 1,200 members with page
  size 500
- **THEN** exactly 3 pages are returned and the union of pages has 1,200 distinct nodes

#### Scenario: Page size is capped

- **WHEN** a client requests a page size above the server hard cap
- **THEN** the response uses the hard cap, and the response body states the effective page
  size

### Requirement: Neighborhood slice endpoint

The server SHALL expose an endpoint returning the neighborhood of one node — BFS up to a
requested depth, node- and depth-capped by server hard limits — including each returned
node's community id, the connecting edges, and a truncation flag per direction stating
whether more neighbours exist beyond the caps. Unknown node ids SHALL return a clear
not-found error, not an empty slice.

- **Plan**: This feeds neighbour expansion and the viewer-navigation path/detail surfaces
  without loading whole communities.
- **Do**: BFS over the precomputed adjacency; annotate truncation explicitly (no silent
  caps).
- **Check**: Scenarios below.
- **Act**: If hub nodes make depth-2 responses routinely hit the cap, add a per-relation
  filter parameter in a follow-up rather than raising caps.

#### Scenario: Depth-limited expansion

- **WHEN** a client requests the neighborhood of `mod_Auth` at depth 2 with node cap 200
- **THEN** the response contains at most 200 nodes, each with its community id, only edges
  among returned nodes, and truncation flags indicating whether the cap cut either depth

#### Scenario: Unknown node

- **WHEN** a client requests the neighborhood of a node id absent from the graph
- **THEN** the server responds with a not-found error naming the id, and no slice payload

### Requirement: Bounded responses with explicit truncation

Every slice endpoint SHALL enforce a server-side hard cap on nodes and edges per response,
SHALL state totals so clients can display "N of M loaded", and SHALL mark every truncation
explicitly in the response body. No endpoint SHALL stream or return the full node or edge
set of a graph exceeding the cap, regardless of requested parameters.

- **Plan**: The interface contract that makes 4.4 GB graphs navigable: the wire format is
  bounded by design, so client memory (see `progressive-graph-viewer`) — not graph size —
  is the browser's constraint.
- **Do**: One shared cap-and-annotate layer for all three endpoints.
- **Check**: Scenarios below.
- **Act**: Log cap hits server-side; if a deployment routinely saturates caps, that is the
  trigger for the follow-up on-disk-storage change, not for raising caps.

#### Scenario: No full-graph escape hatch

- **WHEN** a client requests a community slice with page size 10^9 on a graph larger than
  the hard cap
- **THEN** the response is capped, marked truncated, and the server returns it without
  materializing the full graph in the response buffer

#### Scenario: Totals always present

- **WHEN** any slice endpoint returns any page
- **THEN** the response states the totals of the sliced scope (community member count,
  neighborhood size within caps, or graph totals) alongside the page
