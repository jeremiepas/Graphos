## Why

MCP query tool calls (`query_graph`, `shortest_path`) are slow enough to appear blocked/hung on large graphs. The root cause is not slow algorithms — it is repeated O(N) work on every request that should happen once at load time. Three independent problems compound, and a fourth (silent correctness bug) hides behind them:

1. **MCP server rebuilds `GraphIndex` on every tool call.** `Infrastructure/Server/MCP.hs` imports and calls the legacy `queryGraph` / `pathQuery` (lines 30, 130–132, 201), not the `*WithIndex` variants. The legacy functions internally call `buildIndex g Map.empty` per invocation — O(N) over all nodes, thrown away after the call. `loadGraphFromFile` already builds a proper `GraphIndex` at startup (line 54: `buildIndexWithLabels`), but `startMCPServerFromFile` discards `lrIndex` and never threads it to `startMCPServer`.

2. **`handleQueryGraph` calls `queryGraph` three times per request** (lines 130/131/132) to extract three fields from the same result. That's 3× the O(N) index rebuild for a single `query_graph` tool call. Even after fixing #1, this triple call is obviously wrong.

3. **`shortestPath` rebuilds the FGL graph on every call.** `Domain/Graph/Query.hs` line 69: `toCachedFGL g` runs `toFGL (gNodes g) (gEdges g)` → `mkGraph` — O(N + E) allocation per invocation. Even the "fast" `pathQueryWithIndex` variant bottoms out in `shortestPath g`, so the index optimization only fixed node *lookup*; path *computation* still pays O(N + E) per request. The `CachedFGL` pattern in `Analysis.hs` was designed to share FGL across algorithms in one call, but it is not shared across requests.

4. **`nidToInt` is a hash with no collision handling (correctness).** `Domain/Graph/FGL.hs` line 55 maps `NodeId → Int` via a polynomial hash mod `maxBound`. On 100K-node graphs, birthday-paradox collisions are near-certain. `mkGraph` with duplicate Int keys silently overwrites one node; the colliding `NodeId` vanishes from `labNodes`, so `cachedFindIdx` returns `Nothing` and `shortestPath` returns `Nothing` even when a path exists. Same silent data loss affects `articulationPoints`, `biconnectedComponents`, `dominators`, `edgeBetweenness`, `breadthFirstSearch`, `depthFirstSearch`. This is not a perf issue — it is wrong results.

A fifth, minor issue: the legacy `queryGraph` / `pathQuery` / `explainNode` build the index with `Map.empty` communities, so `communityOfNode` via that index returns `Nothing` for every node. Any community-aware logic on the legacy path is silently broken.

## What Changes

- **Thread `GraphIndex` through the MCP server.** `startMCPServerFromFile` passes `lrIndex` to `startMCPServer` → `requestLoop` → `handleRequest` → `handleToolCall`. Handlers switch from `queryGraph` / `pathQuery` / `explainNode` to `queryGraphWithIndexScored` / `pathQueryWithIndex` / `explainNodeWithIndex`. The index built once at load is reused for every request — O(N) once, O(k) per query.
- **Fix `handleQueryGraph` triple call.** Bind `let resp = queryGraphWithIndexScored g idx question mode budget` once; derive `nodes`/`edges`/`traverse`/`verdict`/`score`/`hash`/`suggestions` from the single result. Also fix `handleShortestPath` to use `pathQueryWithIndex`.
- **Thread `CachedFGL` through load + queries.** `loadGraphFromFile` builds `lrCachedFGL` once (alongside `lrIndex`). `shortestPath`, `breadthFirstSearch`, `depthFirstSearch` take `CachedFGL` instead of `Graph` so callers share the O(N + E) FGL conversion across requests. `articulationPoints` / `biconnectedComponents` / `dominators` already accept `Graph` and build `CachedFGL` internally — add `*WithCached` variants that accept the prebuilt cache, keep the old ones as thin wrappers for backward compat.
- **Replace `nidToInt` hash with bijective sequential indices.** `toCachedFGL` assigns `0..N-1` via `Map.elems` position (or a `Map NodeId Int` built with `Map.fromList (zip nids [0..])`). `toFGL` uses these indices instead of hashing. Eliminates collision-induced silent node loss. `cfgIdxList` becomes a `Map NodeId Int` for O(log N) lookup instead of the current O(N) `lookup` on an association list.
- **Switch `buildLabelIndex` from `(++)` to `(:)`.** Same fix pattern as `optimise-community-detection-large-graph` applies to Leiden: `Map.fromListWith (:)` + final `map reverse` is O(N) vs O(N × avg_hits_per_term) for `(++)`. Speeds up the one-time index build that Thread 1 makes hot again on the legacy path.

## Capabilities

### Modified Capabilities

- `query-serving`: The MCP server (`Infrastructure/Server/MCP.hs`) SHALL thread the `GraphIndex` and `CachedFGL` built at load time through every request, and SHALL NOT rebuild them per tool call. Tool handlers SHALL use the `*WithIndex` / `*WithCached` query variants. `handleQueryGraph` SHALL call the query function exactly once per request and derive all response fields from the single result.
- `fgl-adapter`: The FGL conversion (`Domain/Graph/FGL.hs` + `Domain/Graph/Analysis.hs` `CachedFGL`) SHALL use bijective sequential node indices (0..N-1), not a hash. Two distinct `NodeId`s MUST NOT map to the same FGL Int. `cachedFindIdx` SHALL be O(log N), not O(N) list lookup.

## Impact

**Code**:
- `src/Graphos/Infrastructure/Server/MCP.hs` — the bulk of the MCP work: thread `GraphIndex` (+ `CachedFGL`) through `startMCPServerFromFile` / `startMCPServer` / `requestLoop` / `handleRequest` / `handleToolCall`; rewrite `handleQueryGraph` (single call), `handleShortestPath` (`pathQueryWithIndex`); update import list.
- `src/Graphos/UseCase/Load.hs` — add `lrCachedFGL :: CachedFGL` to `LoadResult`; build it once in `loadGraphFromFile`.
- `src/Graphos/Domain/Graph/Query.hs` — add `shortestPathWithCached` / `breadthFirstSearchWithCached` / `depthFirstSearchWithCached` taking `CachedFGL`; keep existing functions as thin wrappers that build the cache (backward compat for any direct caller).
- `src/Graphos/Domain/Graph/Analysis.hs` — `toCachedFGL` uses sequential indices; `cachedFindIdx` uses `Map NodeId Int` not association list; add `articulationPointsWithCached` / `biconnectedComponentsWithCached` / `dominatorsWithCached`.
- `src/Graphos/Domain/Graph/FGL.hs` — `toFGL` accepts an explicit `Map NodeId Int` (or the index list) instead of hashing via `nidToInt`; `nidToInt` removed or kept only for non-colliding legacy callers (audit: none exist outside this module).
- `src/Graphos/Domain/Graph/Index.hs` — `buildLabelIndex` / `buildPathIndex` switch `(++)` → `(:)` + `map reverse`.
- `src/Graphos/UseCase/Query.hs` — `pathQueryWithIndex` / `explainNodeWithIndex` updated to thread `CachedFGL` if `shortestPath` signature changes; or no change if wrappers preserve the old signature (preferred — keep the blast radius small).

**APIs/Dependencies**: No new Haskell dependencies. All changes use existing `containers` / `fgl` APIs. `LoadResult` gains a field (additive — existing consumers ignore it). `CachedFGL` internals change (not part of a stable public API).

**Systems**: No runtime/IO change. All work is in pure Domain + UseCase + the MCP stdio handler. Peak memory rises by the size of one `CachedFGL` kept resident (~the FGL graph size, already paid once per analysis call today — now kept instead of rebuilt). Net memory is flat or down because we stop rebuilding `GraphIndex` per request.

**Tests**: Existing `cabal test` suite must pass unchanged (algorithm semantics preserved). Add Hspec properties:
- `nidToInt`-replacement: `toCachedFGL` is bijective — for every `NodeId` in the graph, `cachedFindIdx` returns a distinct `Just idx`.
- `shortestPathWithCached` returns the same path as `shortestPath` on random graphs (semantic equivalence of the wrapper).
- `buildLabelIndex` with `(:)` produces the same `Map Text [NodeId]` set as the `(++)` version (order-insensitive comparison).
- Regression: a synthetic graph with two `NodeId`s that collide under the old `nidToInt` hash now finds the correct shortest path through both (the bug-finding test).

## PDCA Cycle

- **Plan**: Hypothesis — threading `GraphIndex` + `CachedFGL` through the MCP server removes O(N) and O(N + E) per-request work, dropping `query_graph` latency from seconds to milliseconds on 100K-node graphs, and `shortest_path` from O(N + E) per call to O(V_path + E_path) plus a one-time O(N + E) at load. The `nidToInt` fix eliminates a class of silent missing-path / missing-bridge bugs. Success measured by a before/after timed comparison on the largest available `graph.json` and by the new collision regression test.
- **Do**: Thread `lrIndex` and `lrCachedFGL` through the MCP server; rewrite `handleQueryGraph` (single call) and `handleShortestPath` (`pathQueryWithIndex`); switch `toCachedFGL` to sequential indices and `cachedFindIdx` to `Map` lookup; add `*WithCached` query variants; switch `buildLabelIndex` to `(:)`.
- **Check**: Run `cabal test` (no regression). Run `cabal run graphos -- mcp <graph.json>` and issue `query_graph` + `shortest_path` tool calls against the largest available graph — confirm latency drops by the expected order of magnitude and that `shortest_path` now returns paths between node pairs that previously returned `Nothing` due to collisions. Confirm the collision regression test fails on the old code and passes on the new.
- **Act**: If latency does not drop, profile the handler — the remaining cost is likely in `findMatchingNodes` (already O(k × log N)) or in JSON serialization of large result sets (separate concern). If the collision regression test passes on the old code, the synthetic collision case is not representative — construct a real-world collision from the largest graph's `NodeId`s. If `cabal test` regresses, the sequential-index mapping or the `(:)` label index has an order-sensitivity bug — bisect by reverting one change at a time.

## Relationship to `optimise-community-detection-large-graph`

Independent. That change optimizes the *extraction/clustering* pipeline (`Domain/Community.hs`, `Pipeline.hs` Step 5). This change optimizes the *query serving* path (`Server/MCP.hs`, `Domain/Graph/Query.hs`, `Domain/Graph/FGL.hs`). No file overlap except `Domain/Graph/Index.hs` (the `buildLabelIndex` `(++)`→`(:)` fix), which is a one-line change with no interaction with Leiden. Merge order: either first; the two are commutative.