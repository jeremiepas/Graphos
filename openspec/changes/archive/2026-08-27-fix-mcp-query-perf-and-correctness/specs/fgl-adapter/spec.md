# fgl-adapter

Correctness requirements for the adapter between Graphos domain types and the `fgl` graph library (`Domain/Graph/FGL.hs`, `Domain/Graph/Analysis.hs` `CachedFGL`). Ensures node-index mapping is bijective so no `NodeId` is silently lost to hash collisions.

## ADDED Requirements

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

### Requirement: buildLabelIndex and buildPathIndex use O(1)-per-insert list construction

`buildLabelIndex` and `buildPathIndex` (`Domain/Graph/Index.hs`) SHALL use O(1)-per-insert list construction. The current implementation uses `Map.fromListWith (++)` with singleton lists `[nid]` followed by `Map.map reverse`, which is O(1) per insert (`[nid] ++ existing = nid : existing` — one cons, no traversal). This keeps one-time index construction O(N × avg_tokens).

#### Scenario: Label index content is preserved

- **WHEN** `buildLabelIndex` is built with `(++)` + singleton lists `[nid]` on the same node map
- **THEN** it produces a `Map Text [NodeId]` where each term maps to the list of `NodeId`s whose label contains that term; `findMatchingNodes` returns the correct `(NodeId, score)` pairs
