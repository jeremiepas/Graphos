# query-serving

Performance and correctness requirements for the MCP server query path (`Infrastructure/Server/MCP.hs`) and the FGL-backed graph algorithms it invokes. Ensures heavy index/structure construction happens once at load time, not per request, and that FGL node indexing is collision-free.

## MODIFIED Requirements

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

# fgl-adapter

Correctness requirements for the adapter between Graphos domain types and the `fgl` graph library (`Domain/Graph/FGL.hs`, `Domain/Graph/Analysis.hs` `CachedFGL`). Ensures node-index mapping is bijective so no `NodeId` is silently lost to hash collisions.

## MODIFIED Requirements

### Requirement: Bijective node-index mapping

The FGL conversion (`toCachedFGL` and/or `toFGL`) SHALL assign each `NodeId` a distinct `Int` index in the range `0 .. N-1` where N is the number of nodes. The mapping SHALL be bijective: two distinct `NodeId`s MUST NOT share an FGL Int index. The reverse mapping (`cfgNidMap :: Map Int NodeId`) SHALL cover every index. `cachedFindIdx` SHALL be O(log N) via a `Map NodeId Int`, not O(N) association-list lookup.

#### Scenario: No node is lost to hash collision

- **WHEN** a graph contains two `NodeId`s that would collide under any hash-based `NodeId → Int` scheme (constructed by picking two strings with equal hash mod `maxBound`)
- **THEN** both nodes appear in `cfgNidMap`, both have distinct indices in `cfgIdxMap`, and `cachedFindIdx` returns `Just` for both

#### Scenario: shortestPath finds paths through collision-prone node pairs

- **WHEN** `shortestPath` (or `shortestPathWithCached`) is called with a source or target `NodeId` that would have collided under the previous `nidToInt` hash
- **AND** a path exists between them in the graph
- **THEN** the function returns `Just path` (not `Nothing`); the path contains both endpoints

#### Scenario: cachedFindIdx is O(log N)

- **WHEN** `cachedFindIdx` is called on a graph with N nodes
- **THEN** the lookup is a `Map` lookup (O(log N)), not an association-list `lookup` (O(N)); the `CachedFGL` record exposes `cfgIdxMap :: Map NodeId Int` (or equivalent O(log N) structure), not `cfgIdxList :: [(NodeId, Int)]`

### Requirement: FGL-backed algorithms preserve semantics under sequential indexing

Switching `nidToInt` (hash) to sequential `0..N-1` indices changes the internal order of FGL nodes. Algorithms that return lists (`articulationPoints`, `biconnectedComponents`, `dominators`) SHALL produce results equivalent to the pre-change implementation up to element order (i.e., comparing as `Set`s or sorted lists), so that existing tests and downstream consumers are unaffected.

#### Scenario: articulation points are unchanged as a set

- **WHEN** `articulationPoints` is called on the same graph before and after the sequential-index change
- **THEN** the returned list contains the same `NodeId`s (order may differ); `Set.fromList (old) == Set.fromList (new)`

#### Scenario: Existing test suite passes

- **WHEN** `cabal test` is run after the change
- **THEN** all existing tests pass without modification (any order-sensitive assertions on FGL-backed algorithm output are relaxed to set/sorted comparison)

### Requirement: buildLabelIndex and buildPathIndex use O(N) list construction

`buildLabelIndex` and `buildPathIndex` (`Domain/Graph/Index.hs`) SHALL use `Map.fromListWith (:)` followed by a `map reverse` (or equivalent O(1)-per-insert construction), not `Map.fromListWith (++)`. This keeps one-time index construction O(N × avg_tokens) rather than O(N × avg_tokens × avg_hits_per_term).

#### Scenario: Label index content is preserved

- **WHEN** `buildLabelIndex` is built with `(++)` vs `(:)`+`reverse` on the same node map
- **THEN** both produce the same `Map Text [NodeId]` when compared order-insensitively (e.g., `Map.map Set.fromList` on both sides); `findMatchingNodes` returns the same `(NodeId, score)` pairs either way