# query-serving

Performance and correctness requirements for the MCP server query path (`Infrastructure/Server/MCP.hs`) and the FGL-backed graph algorithms it invokes. Ensures heavy index/structure construction happens once at load time, not per request, and that FGL node indexing is collision-free.

## ADDED Requirements

### Requirement: Load-time index sharing across requests

The MCP server SHALL build `GraphIndex` and `CachedFGL` exactly once at server startup (in `loadGraphFromFile` / `startMCPServerFromFile`) and thread them through every request handler. Tool handlers SHALL NOT rebuild `GraphIndex` or `CachedFGL` per tool call. Handlers SHALL call the `*WithIndex` / `*WithCached` query variants, not the legacy `queryGraph` / `pathQuery` / `explainNode` fallbacks.

#### Scenario: query_graph does not rebuild the index

- **WHEN** the MCP server receives two consecutive `query_graph` tool calls with the same graph loaded
- **THEN** the `GraphIndex` is constructed exactly once (at startup), not twice; the second call's latency is bounded by the term-matching cost O(k × log N + hits), not by O(N) index construction

#### Scenario: shortest_path does not rebuild the FGL graph

- **WHEN** the MCP server receives two consecutive `shortest_path` tool calls with the same graph loaded
- **THEN** the `CachedFGL` is constructed exactly once (at startup); the second call's latency is bounded by the FGL algorithm cost (e.g., O(V + E) for BFS shortest path), not by O(N + E) FGL conversion

#### Scenario: Legacy query functions remain available but are not used by the server

- **WHEN** `queryGraph` / `pathQuery` / `explainNode` (the index-rebuilding fallbacks) are called directly outside the MCP server (e.g., from tests or ad-hoc code)
- **THEN** they still work, building a temporary index per call as before; but the MCP server import list SHALL NOT reference them

### Requirement: Single query invocation per request

`handleQueryGraph` SHALL invoke the query function exactly once per `query_graph` tool call and derive all response fields (`nodes`, `edges`, `verdict`, `best_score`, `hash`, `suggestions`) from the single result. It SHALL NOT call the query function multiple times to extract different fields.

#### Scenario: query_graph response is derived from one call

- **WHEN** the MCP server receives a `query_graph` tool call
- **THEN** the underlying `queryGraphWithIndexScored` (or equivalent) is invoked exactly once, and the response object's `nodes`, `edges`, `verdict`, `best_score`, `hash`, and `suggestions` fields all originate from that single invocation's `QueryResponse`

### Requirement: LoadResult carries a prebuilt CachedFGL

`LoadResult` SHALL include a `lrCachedFGL :: CachedFGL` field, built once in `loadGraphFromFile` alongside `lrIndex`. Consumers that need FGL-backed algorithms (`shortestPath`, `articulationPoints`, `biconnectedComponents`, `dominators`) SHALL use `lrCachedFGL` rather than rebuilding the cache.

#### Scenario: LoadResult exposes the cached FGL

- **WHEN** `loadGraphFromFile` succeeds
- **THEN** the returned `LoadResult` has a fully-evaluated `lrCachedFGL` whose `cfgGraph` contains every node and edge from the loaded graph, and whose index maps are bijective (see `fgl-adapter` capability)

#### Scenario: Path query uses the cached FGL

- **WHEN** `pathQueryWithIndex` (or the MCP `shortest_path` handler) computes a shortest path
- **THEN** it reads from the `lrCachedFGL` built at load time and does NOT call `toCachedFGL` again
