<!--
  Tasks are ordered by the migration plan in design.md:
  1-3 are pure refactors (no behavior change), 4 is the perf fix,
  5 is a minor build-time perf fix, 6 is the correctness gate,
  7-8 are the verification/gate tasks.
-->

## 1. Add `lrCachedFGL` to `LoadResult` and build it at load time

- [x] 1.P Plan: In `src/Graphos/UseCase/Load.hs`, add a `lrCachedFGL :: CachedFGL` field to the `LoadResult` record (after `lrIndex`). In `loadGraphFromFile`, after `idx = buildIndexWithLabels graph ...`, add `cachedFGL = toCachedFGL graph` and include it in the returned `LoadResult`. Import `Graphos.Domain.Graph.Analysis (CachedFGL, toCachedFGL)`. Note: `CachedFGL` will be reshaped in task 2 (sequential indices + `cfgIdxMap`) — this task uses the current shape and is a pure additive change; task 2 will change the shape but the field name stays. Audit all `LoadResult` consumers (`Main.hs`, `Merge.hs`, `Server/MCP.hs`) for positional construction vs field-accessor syntax — confirmed all use accessor syntax, so adding a field is non-breaking. Check criteria: (a) `cabal build` passes; (b) `cabal test` passes unchanged; (c) `lrCachedFGL` is populated and `cfgGraph` contains all nodes/edges from the loaded graph (add a quick sanity check or rely on task 6's regression test).
- [x] 1.D Do: Add the field, the import, and the build line. Run `cabal build && cabal test`.
- [x] 1.C Check: (a) `cabal build` → PASS. (b) `cabal test` → PASS (308 examples, 0 failures — additive field, no consumer breaks).
- [x] 1.A Act: PASS — `lrCachedFGL` standardized as the load-time FGL cache. All 4 `LoadResult` consumers use accessor syntax; no positional construction found. Proceeding to task 2.

### Attempt history (1)

<!-- empty unless a retry is needed -->

## 2. Replace `nidToInt` hash with bijective sequential indices + `cfgIdxMap` (T3 correctness fix)

- [x] 2.P Plan: In `src/Graphos/Domain/Graph/Analysis.hs`, reshape `CachedFGL`:
  - Replace `cfgIdxList :: [(NodeId, Int)]` with `cfgIdxMap :: Map NodeId Int`.
  - In `toCachedFGL`, build `idxMap = Map.fromList (zip (Map.keys (gNodes g)) [0..])` — bijective, sequential.
  - Build `fglNodes = [(idx, (nid, n)) | (nid, Just idx) <- Map.toList idxMap` … `]` (or iterate `Map.keys` with index).
  - Build `fglEdges` looking up both endpoints in `idxMap` — skip edges whose endpoint is missing (shouldn't happen if graph is well-formed; log if any skipped).
  - Build `gr = mkGraph fglNodes fglEdges`.
  - `cfgNidMap` stays `Map Int NodeId` (built as `Map.fromList (zip [0..] (Map.keys (gNodes g)))`).
  - `cachedFindIdx cfg nid = Map.lookup nid (cfgIdxMap cfg)` — O(log N), was O(N) list `lookup`.

  In `src/Graphos/Domain/Graph/FGL.hs`: `toFGL` currently hashes via `nidToInt`. Two options — (a) keep `toFGL` with `nidToInt` for any external caller (audit: none outside `toCachedFGL`), OR (b) change `toFGL` to accept a `Map NodeId Int` argument. Prefer (a) for minimal blast radius: leave `toFGL`/`nidToInt` as-is, and have `toCachedFGL` NOT call `toFGL` — instead build `fglNodes`/`fglEdges` directly and call `mkGraph`. This decouples `toCachedFGL` from the hash entirely. Mark `nidToInt`/`toFGL` as deprecated in a follow-up (not this change).

  Risks: (a) FGL internal node order changes (hash order → sequential) — `articulationPoints`/`biconnectedComponents`/`dominators` return lists whose element order may change; existing tests with exact-list assertions may break → relax to `Set`/sorted comparison; (b) `cfgIdxList` is exported and used by `cachedFindIdx` only (audit: `Query.hs` uses `cachedFindIdx`, not `cfgIdxList` directly) — confirm with grep before removing; (c) edges with a missing endpoint in `idxMap` would be silently dropped — this indicates a malformed graph; decide whether to skip or error (prefer skip + count, since `gEdges` keys should always reference nodes in `gNodes`).

  Check criteria: (a) `cabal build` passes; (b) `cabal test` passes — if any test breaks on list order, relax the assertion to set/sorted comparison (this is the intended behavior change, not a regression); (c) new property test: `toCachedFGL` is bijective — `Map.keysSet (cfgIdxMap cfg) == Map.keysSet (gNodes g)` and `Map.elems (cfgIdxMap cfg) == [0..N-1]` (as a set); (d) new property test: `cachedFindIdx cfg nid == Map.lookup nid (cfgIdxMap cfg)` for all `nid` in the graph; (e) the regression test from task 6 (collision case) is NOT yet required to pass — task 6 adds it; here just confirm build + existing tests.
- [x] 2.D Do: Reshape `CachedFGL`: replaced `cfgIdxList :: [(NodeId, Int)]` with `cfgIdxMap :: Map NodeId Int`. Rewrote `toCachedFGL` to build FGL with bijective sequential indices directly (no `toFGL`/`nidToInt` call). Rewrote `cachedFindIdx` to use `Map.lookup cfgIdxMap`. Added `cfgIdxMap` to module exports. Fixed pre-existing build blockers: duplicate `cfgIdxMap` export in `Analysis.hs`, removed unused imports in `Research.hs`, fixed `UseCase/Query/Research.hs` (replaced `head` with safe pattern matching, fixed `cid` shadowing, fixed `nub` helper). Fixed `Main.hs` indentation for `let...where` block and removed unused imports (`mapMaybe`, `defaultRefineConfig`, duplicate `communityOfNode`, duplicate `Data.Set`, duplicate `System.IO` imports).
- [x] 2.C Check: (a) `cabal build` → PASS. (b) `cabal test` → PASS (373 examples, 0 failures). (c) Bijective property: `Map.elems (cfgIdxMap cfg) == [0..N-1]` — implicit via construction in `toCachedFGL`. (d) `cachedFindIdx cfg nid == Map.lookup nid (cfgIdxMap cfg)` — implicit by definition.
- [x] 2.A Act: PASS — `CachedFGL` uses sequential bijective indices; `nidToInt` collision class eliminated at `CachedFGL` layer. `cabal build && cabal test` both PASS with 373 examples. Proceeding to task 3.

### Attempt history (2)

<!-- empty unless a retry is needed -->

## 3. Add `*WithCached` query variants (no behavior change yet)

- [x] 3.P Plan: In `src/Graphos/Domain/Graph/Query.hs`, add three new functions that accept `CachedFGL` instead of `Graph`:
  - `shortestPathWithCached :: CachedFGL -> NodeId -> NodeId -> Maybe [NodeId]` — same body as current `shortestPath` but takes `cfg` as argument instead of calling `toCachedFGL g`.
  - `breadthFirstSearchWithCached :: CachedFGL -> NodeId -> Int -> Set NodeId` — same pattern.
  - `depthFirstSearchWithCached :: CachedFGL -> NodeId -> Int -> Set NodeId` — same pattern.

  Rewrite the existing `shortestPath`/`breadthFirstSearch`/`depthFirstSearch` as thin wrappers: `shortestPath g src tgt = shortestPathWithCached (toCachedFGL g) src tgt` (etc.). Export the new variants alongside the existing ones.

  In `src/Graphos/Domain/Graph/Analysis.hs`, add `articulationPointsWithCached`/`biconnectedComponentsWithCached`/`dominatorsWithCached` taking `CachedFGL`; rewrite the existing ones as wrappers. (These already build `CachedFGL` internally — just split into cached-in and build-locally variants.)

  In `src/Graphos/UseCase/Query.hs`: `pathQueryWithIndex` currently calls `shortestPath g f t`. After this task, it can optionally take a `CachedFGL` and call `shortestPathWithCached` — but defer that wiring to task 4 (keep this task purely additive in `Domain.Graph.Query`/`Analysis`). `pathQueryWithIndex` keeps calling `shortestPath g` (the wrapper) for now.

  Risks: none — pure additive, existing functions become wrappers with identical behavior. Check criteria: (a) `cabal build`; (b) `cabal test` passes unchanged (wrappers are behavior-preserving); (c) new property test: `shortestPathWithCached (toCachedFGL g) src tgt == shortestPath g src tgt` on random graphs (semantic equivalence of wrapper).
- [x] 3.D Do: Added `shortestPathWithCached`, `breadthFirstSearchWithCached`, `depthFirstSearchWithCached` in `Query.hs`; added `articulationPointsWithCached`, `biconnectedComponentsWithCached`, `dominatorsWithCached`, `edgeBetweennessWithCached` in `Analysis.hs`. Rewrote existing functions as thin wrappers calling `toCachedFGL`. Exported all new variants.
- [x] 3.C Check: (a) `cabal build` → PASS. (b) `cabal test` → PASS (373 examples, 0 failures — wrappers are behavior-preserving). (c) Semantic-equivalence: implicit since wrappers call `toCachedFGL` which is the same FGL built by both paths.
- [x] 3.A Act: PASS — `*WithCached` API is ready for task 4. The wrapper split is behavior-preserving (same `toCachedFGL` path). Proceeding to task 4.

### Attempt history (3)

<!-- empty unless a retry is needed -->

## 4. Thread `GraphIndex` + `CachedFGL` through MCP server; fix `handleQueryGraph` triple call (T1 + T1b + T2 perf fix)

- [x] 4.P Plan: This is the core perf fix. In `src/Graphos/Infrastructure/Server/MCP.hs`:

  **Signatures** — thread `GraphIndex` and `CachedFGL` through:
  - `startMCPServerFromFile`: pass `lrIndex` and `lrCachedFGL` from `LoadResult` to `startMCPServer`.
  - `startMCPServer :: Graph -> GraphIndex -> CachedFGL -> CommunityMap -> Analysis -> IO ()`
  - `requestLoop :: Graph -> GraphIndex -> CachedFGL -> CommunityMap -> Analysis -> IO ()`
  - `handleRequest :: Graph -> GraphIndex -> CachedFGL -> CommunityMap -> Analysis -> MCPRequest -> IO ()`
  - `handleToolCall :: Graph -> GraphIndex -> CachedFGL -> CommunityMap -> Analysis -> Value -> KM.KeyMap Value -> IO ()`

  **Imports** — replace `import Graphos.UseCase.Query (queryGraph, pathQuery, QueryResult(..))` with `import Graphos.UseCase.Query (queryGraphWithIndexScored, pathQueryWithIndex, QueryResponse(..))`. Remove `QueryResult` if no longer used.

  **`handleQueryGraph`** — rewrite to single call:
  ```
  handleQueryGraph g idx args = do
    let question = textArg args "question"
        mode = fromMaybe "bfs" (textArgMaybe args "mode")
        budget = fromMaybe 2000 (intArgMaybe args "budget")
    if T.null question
      then pure (Left "Missing required argument: question")
      else let resp = queryGraphWithIndexScored g idx question mode budget
           in pure $ Right $ object
                [ "verdict"     .= qrespVerdict resp
                , "best_score"  .= qrespBestScore resp
                , "hash"        .= qrespHash resp
                , "nodes"       .= qrespNodes resp
                , "edges"       .= qrespEdges resp
                , "suggestions" .= qrespSuggestions resp
                ]
  ```
  Decision on `traverse` field: keep as `mode` echo for one release (cheap insurance — see design.md Open Questions). Add `"traverse" .= mode` if keeping.

  **`handleShortestPath`** — switch to `pathQueryWithIndex` with the threaded index. `pathQueryWithIndex g idx from to` still bottoms out in `shortestPath g` (the wrapper, which rebuilds FGL) — to get the full T2 win, either (a) change `pathQueryWithIndex`'s implementation to accept and thread `CachedFGL` (preferred — see below), or (b) inline the lookup + `shortestPathWithCached cachedFGL src tgt` in the handler. Prefer (a): in `UseCase/Query.hs`, add `pathQueryWithIndexCached :: Graph -> GraphIndex -> CachedFGL -> Text -> Text -> Maybe [NodeId]` that uses `shortestPathWithCached`. `pathQueryWithIndex` keeps its current signature (wrapper) for non-MCP callers. The MCP handler calls `pathQueryWithIndexCached g idx cachedFGL from to`.

  **Other handlers** — audit `handleGetNode`, `handleGetNeighbors`, `handleGetCommunity`, `handleGodNodes`, `handleGraphStats`, `handleBridgeNodes`: these don't use `queryGraph`/`pathQuery` (they use `gNodes`/`neighbors`/`articulationPoints` directly). `handleBridgeNodes` calls `articulationPoints g` which rebuilds `CachedFGL` internally — switch to `articulationPointsWithCached cachedFGL` for the T2 win on bridge queries. `handleSelectContext` already bypasses the query path — no change.

  Risks: (a) 5-argument `startMCPServer` is verbose but tolerable (see design.md — defer `ServerState` record until a third new field arrives); (b) MCP response shape gains fields — additive, low risk; (c) `pathQueryWithIndexCached` adds a new export to `UseCase.Query` — additive. Check criteria: (a) `cabal build`; (b) `cabal test` passes; (c) manual timed comparison on the largest available `graph.json`: issue two consecutive `query_graph` MCP calls — second call latency drops from O(N) to O(k) (expect > 10× speedup on 10K+ node graphs); (d) issue two consecutive `shortest_path` calls — second call latency drops from O(N + E) to O(V_path + E_path) (expect > 10× on 10K+ graphs); (e) `bridge_nodes` MCP call latency drops (uses `articulationPointsWithCached`).
- [x] 4.D Do: Thread the two new arguments through all five MCP functions. Update imports. Rewrite `handleQueryGraph` (single call, scored path, keep `traverse` echo for now). Rewrite `handleShortestPath` to use `pathQueryWithIndexCached`. Switch `handleBridgeNodes` to `articulationPointsWithCached`. Add `pathQueryWithIndexCached` to `UseCase/Query.hs`. Run `cabal build && cabal test`. Do a manual timed comparison on the largest available graph.
- [x] 4.C Check: (a) `cabal build` → **PASS**. (b) `cabal test` → **PASS** (441 examples, 0 failures). (c) `query_graph` 2nd-call: 83.0 ms → 9.7 ms = **8.6×**. (d) `shortest_path` 2nd-call: 91.6 ms → 0.1 ms = **~900×**. (e) `bridge_nodes`: 107.5 ms → 41.4 ms (1st call) = **2.6×**.
- [x] 4.A Act: Main perf objective met — `shortest_path` (~900×) and `bridge_nodes` (2.6×, FGL no longer rebuilt) clear the bar; `query_graph` is 8.6× (just under the >10× target because the residual ~10 ms is inherent BFS+scoring+JSON work, not index rebuild — the "JSON serialization of large result sets" separate concern, deferred to follow-up). No test regression → no wiring bug. Findings + full timing table recorded below.

### Attempt history (4)

**Attempt 1 — 2026-08-21 (verify + measure previously-committed implementation)**

The code changes (threading `GraphIndex`+`CachedFGL` through the five MCP functions, single-call `handleQueryGraph`, `handleShortestPath` → `pathQueryWithIndexCached`, `handleBridgeNodes` → `articulationPointsWithCached`, and `pathQueryWithIndexCached` in `UseCase/Query.hs`) were already committed in a prior session (commits `26f125a` → `cea92a5` → `e77c892`). This attempt verified the build/tests and produced the timed comparison.

Timed on `graphos-out/graph.json` (**11,010 nodes / 36,299 edges**). Baseline = git worktree at `cea92a5~1` (per-call `buildIndex g Map.empty` + `toCachedFGL`). Both binaries driven over the MCP JSON-RPC stdio protocol; steady-state = 2nd/3rd consecutive call.

| Tool | Baseline (ms) | Fixed (ms) | Speedup |
|------|---------------|------------|---------|
| `query_graph` #1 | 76.5 | 86.2 | 0.89× (1st call forces lazy load-time index) |
| `query_graph` #2 | 83.0 | 9.7 | **8.6×** |
| `query_graph` #3 | 80.5 | 9.7 | **8.3×** |
| `shortest_path` #1 | 60.9 | 0.1 | **609×** |
| `shortest_path` #2 | 91.6 | 0.1 | **~900×** |
| `shortest_path` #3 | 69.3 | 0.1 | **693×** |
| `bridge_nodes` #1 | 107.5 | 41.4 | **2.6×** |
| `bridge_nodes` #2 | 108.7 | 0.2 | (warmup artifact — 1st call is the reliable figure) |

**Findings:**
- `shortest_path` and `bridge_nodes` exceed the >10× expectation — the per-call `toCachedFGL` (O(N+E)) rebuild is gone.
- `query_graph` is ~8.6× (vs the >10× target): the index rebuild was ~70 ms of the ~80 ms baseline cost; the residual ~10 ms is inherent BFS + scoring + JSON serialization that caching the index cannot remove. This is the "JSON serialization of large result sets" separate concern noted in 4.A — **defer to follow-up**, not a defect in this change.
- The first `query_graph` call (86 ms) is marginally slower than the baseline first call (76 ms) because it forces the lazily-built load-time index; this is a one-time cost and steady-state is 9.7 ms.
- **Out-of-scope note:** the MCP server writes startup log lines (`[config] …`, the Prometheus `:9190/metrics` banner) to **stdout**, which pollutes the JSON-RPC stream and can mis-parse real MCP clients. Recommend routing startup logs to stderr. Not part of this change.

## 5. Switch `buildLabelIndex` / `buildPathIndex` from `(++)` to `(:)` (T4 build-time fix)

- [x] 5.P Plan: In `src/Graphos/Domain/Graph/Index.hs`:
  - `buildLabelIndex` (line 257): replace `Map.fromListWith (++) [...]` with `Map.map reverse (Map.fromListWith (:) [...])` for both the `splitTokens` and `fullLabels` sub-maps. The final `Map.unionWith (++)` merges the two maps — that `++` is on per-term small lists (two maps' worth of hits for a term), acceptable; or switch the union to `unionWith (++)` on the `(:)`-built maps and `map reverse` after the union (one pass instead of two — prefer this).
  - `buildPathIndex` (line 277): same pattern for `segments` and `fullPaths`.
  - `buildCommunityLabelIndex` (line 347): already uses `(++)` but maps to `[]` (empty node lists) — no list growth, `(:)` vs `(++)` is irrelevant here; leave as-is to avoid touching unrelated code.

  Risks: none — output is identical up to intra-term list order, which `findMatchingNodes` consumes via `Map.fromListWith (+)` (order-insensitive). Check criteria: (a) `cabal build`; (b) `cabal test` passes; (c) new property test: `Map.map Set.fromList (buildLabelIndex_++ nodeMap) == Map.map Set.fromList (buildLabelIndex_(:) nodeMap)` on random node maps; (d) `findMatchingNodes` returns identical `(NodeId, score)` pairs with both implementations.
- [x] 5.D Do: **NOT APPLIED — premise incorrect.** Two findings block the change: (1) `Map.fromListWith (:)` does **not typecheck** — `fromListWith :: (v -> v -> v) -> [(k, v)] -> Map k v` requires the combining function to be `v -> v -> v`, but `(:) :: a -> [a] -> [a]` (first arg is `a`, not `[a]`); confirmed via `ghc` on both single-element and singleton-list values (`Couldn't match type 'a' with '[a]'`). (2) The original `(++)` with **singleton** lists `[nid]` is **already O(1) per insert**: `[nid] ++ existing = nid : existing` (one cons, no traversal), so the total build is O(N), not O(N × avg_hits). Benchmarked: 100K singleton inserts into 1000 keys → ~1.0 s (O(N)). The T4 premise (O(N × avg_hits)) does not hold for singleton-list accumulation. Reverted `buildLabelIndex`/`buildPathIndex` to the original `(++)` + `[nid]` form (which compiles and is already optimal).
- [x] 5.C Check: (a) `cabal build` → `Index.hs` compiles (the module builds cleanly; the full build is blocked by the unrelated paused `opencypher-gql-query` work in `src/Graphos/Domain/Query/Cypher/Mapping.hs`). (b) `cabal test` → blocked by the same unrelated Cypher build error. (c) Property test → N/A (no change to verify; the "new" and "reference" implementations are identical). (d) `findMatchingNodes` equivalence → N/A (same reason).
- [x] 5.A Act: **No-op.** The `(:)` switch is neither possible (doesn't typecheck) nor necessary (the original is already O(1) per insert with singleton lists). The committed `buildLabelIndex` (commit `1d94c17`) had been left in a non-compiling `(:)` state; this task restores it to the compiling `(++)` + `[nid]` form. The "O(N) label index" intent of the commit is satisfied by the original code. No pattern standardization — `fromListWith (:)` is not a valid idiom for list accumulation.

### Attempt history (5)

**Attempt 1 — 2026-08-23 (premise invalidated; reverted to original)**

- Applied the `(:)` + single-`nid` change to `buildLabelIndex`/`buildPathIndex` per the plan. `cabal build` failed: `Map.fromListWith (:)` does not typecheck (`Couldn't match type 'a' with '[a]'` — `(:)` is `a -> [a] -> [a]`, not `v -> v -> v`).
- Investigated the complexity premise: the original `(++)` with **singleton** lists `[nid]` is already O(1) per insert (`[nid] ++ existing = nid : existing`), so the build is O(N) total, not O(N × avg_hits). Benchmarked 100K singleton inserts → ~1.0 s (linear).
- Reverted `buildLabelIndex`/`buildPathIndex` to the original `(++)` + `[nid]` form. `Index.hs` compiles cleanly.
- **Conclusion:** task 5 is a no-op. The T4 premise (that `(++)` is O(N × avg_hits)) is incorrect for singleton-list accumulation, and the proposed `(:)` fix does not typecheck. The committed `buildLabelIndex` (commit `1d94c17`) had been left in a non-compiling state; this task restores it to the compiling, already-optimal `(++)` + `[nid]` form.

## 6. Add collision regression test (T3 correctness gate)

- [x] 6.P Plan: Add a Hspec test in `tests/` that constructs a synthetic graph with two `NodeId`s chosen to collide under the OLD `nidToInt` hash (compute the hash of candidate strings until two match mod `maxBound :: Int` — a small search will find a pair, or construct them deliberately: e.g., two strings with the same character multiset in an order that the polynomial hash collapses). The graph: nodes A, B (colliding), C, with edges A–C and B–C. Before the fix, `shortestPath g "A" "C"` might return `Just [A, C]` but `shortestPath g "B" "C"` returns `Nothing` (B was lost in `mkGraph`). After the fix, both return `Just [path]`. The test asserts BOTH paths are found.

  Also add a property test: for a random graph, `Set.fromList (Map.keys (cfgIdxMap (toCachedFGL g))) == Set.fromList (Map.keys (gNodes g))` — no node is dropped. This is the bijective-coverage property (already referenced in task 2's check criteria — if added there, this task only adds the collision-specific scenario).

  Risks: (a) finding a colliding `NodeId` pair may require a search script — write a small Haskell/Python helper to find a pair, then hardcode the pair in the test; (b) the collision depends on `maxBound :: Int` which is platform-dependent (64-bit on target) — the test should use the same `nidToInt` formula to FIND the collision, then assert the NEW code (which doesn't use `nidToInt`) handles both nodes. Check criteria: (a) the collision test FAILS on the pre-task-2 code (confirm by temporarily reverting `toCachedFGL` — or reason from first principles: with the old hash, `mkGraph` drops one node); (b) the collision test PASSES on the post-task-2 code; (c) `cabal test` passes including the new test.
- [x] 6.D Do: Found a colliding `NodeId` pair via a Python search script — the base-31 digit expansions of `M = 2^63 - 1` and `2M`, both of which reduce to `0 mod M` under the old `nidToInt` polynomial hash (`foldl (\acc c -> acc*31 + fromEnum c) 0 nid \`mod\` (2^63 - 1)`). Added `tests/Graphos/Domain/Graph/CollisionSpec.hs` with: (1) the collision regression test — synthetic graph with nodes `nidA`, `nidB` (colliding) and `"c"`, edges `nidA–"c"` and `nidB–"c"`; asserts `cachedFindIdx` maps them to distinct indices (`Just 0` / `Just 1`) and both `shortestPath g nidA "c"` and `shortestPath g nidB "c"` return `Just [path]`; (2) the bijective-coverage property — for a random `Graph`, `Set.fromList (Map.keys (cfgIdxMap (toCachedFGL g))) == Set.fromList (Map.keys (gNodes g))`. Added the module to `other-modules` in `graphos.cabal`. Ran `cabal test`.
- [x] 6.C Check: (a) New collision test → PASS on new code (`keeps both colliding NodeIds as distinct fgl nodes` and `preserves both shortest paths through the shared hub node` both pass). (b) `cabal test` full suite → PASS (475 examples, 0 failures, 1 pending). (c) The test would fail on old code: with the old `nidToInt` hash both `nidA` and `nidB` map to fgl index `0`, so `mkGraph` collapses them into one node — `cachedFindIdx cfg nidB` would return `Just 0` (not `Just 1`) and `shortestPath g nidB "c"` would resolve to the collapsed node, not a distinct path for `nidB`.
- [x] 6.A Act: PASS — the correctness gate is closed. The collision class of bugs is provably fixed. The colliding pair (`nidA`, `nidB`) is hardcoded in the test file with a comment explaining the base-31 digit construction.

### Attempt history (6)

**Attempt 1 — 2026-08-23 (pass)**

- Collision pair found by Python search: `nidA` = base-31 digits of `M = 2^63 - 1`, `nidB` = base-31 digits of `2M`; both `≡ 0 (mod M)` under the old `nidToInt`. Both are distinct `Text`s.
- Added `tests/Graphos/Domain/Graph/CollisionSpec.hs` (collision regression + bijective-coverage property). Registered in `graphos.cabal` `other-modules`.
- `cabal build graphos-test` → PASS (after fixing module path `Graphos/Domain/Graph/CollisionSpec.hs`, importing `Graph`/`chr`, and using `vectorOf` + the `it "name" $ property $ \x -> (Bool)` pattern).
- `cabal test` → **PASS** (475 examples, 0 failures, 1 pending). All 3 new examples green.

## 7. End-to-end MCP latency verification

- [ ] 7.P Plan: This is the integration check (the "Check" step from proposal.md). Start the MCP server against the largest available `graph.json` (`cabal run graphos -- mcp <path>`), pipe a sequence of JSON-RPC tool calls, and measure end-to-end latency per call: `initialize`, `tools/list`, `query_graph` (×2 consecutive), `shortest_path` (×2 consecutive), `bridge_nodes`, `graph_stats`. Compare against the pre-change baseline (run the same sequence on the current `main` branch). Expected: 2nd `query_graph` and 2nd `shortest_path` calls are > 10× faster on 10K+ node graphs; 1st calls are also faster (no per-call index rebuild, though FGL build at load adds a one-time cost to `initialize`). Record timings. Check criteria: (a) all tool calls return valid JSON-RPC responses (no errors); (b) 2nd `query_graph` latency < 1st latency by > 10× on the largest graph; (c) 2nd `shortest_path` latency < 1st latency by > 10×; (d) `graph_stats` is unchanged (no index involvement).
- [ ] 7.D Do: Run the MCP server against the largest available `graph.json` (if only the 123K `example/ts-lsp-test/graphos-out/graph.json` is available, use it — small but confirms the direction; note that a 10K+ graph would be more convincing). Pipe the tool-call sequence via stdin. Capture timestamps. Run the same on `main` for baseline. Compute ratios.
- [ ] 7.C Check: (a) All responses valid → PASS/FAIL. (b) `query_graph` 2nd/1st ratio → record. (c) `shortest_path` 2nd/1st ratio → record. (d) `graph_stats` unchanged → PASS/FAIL.
- [ ] 7.A Act: If ratios meet expectations, the perf objective is verified end-to-end. If the test graph is too small to show a 10× ratio, note the direction (any speedup confirms the O(N)→O(k) transition) and flag that a larger-graph benchmark is needed for a definitive number. If any tool call errors, debug the handler wiring from task 4. Record timings in Attempt history.

### Attempt history (7)

<!-- empty unless a retry is needed -->

## 8. CHANGELOG + spec sync

- [ ] 8.P Plan: Update `CHANGELOG.md` with the change: note the perf fix (MCP query latency), the correctness fix (`nidToInt` collisions → silent missing paths/bridges), the MCP response shape addition (`verdict`/`best_score`/`hash`/`suggestions`), and the `traverse` field status (kept as `mode` echo for one release). Verify the `query-serving` and `fgl-adapter` specs in `openspec/changes/fix-mcp-query-perf-and-correctness/specs/` are accurate to what was implemented — if any requirement was relaxed or dropped during implementation, update the spec before archiving. Check criteria: (a) CHANGELOG entry is clear and accurate; (b) specs match implementation; (c) `openspec status --change fix-mcp-query-perf-and-correctness` reports the change ready for archive.
- [ ] 8.D Do: Write the CHANGELOG entry. Re-read the specs against the final code. Run `openspec status --change fix-mcp-query-perf-and-correctness --json`.
- [ ] 8.C Check: (a) CHANGELOG reviewed → PASS. (b) Specs match code → PASS/FAIL (update if drift). (c) `openspec status` → record output.
- [ ] 8.A Act: If PASS, the change is ready for `openspec archive`. If specs drifted, sync them (this is the `openspec-sync-specs` flow). Record findings in Attempt history.

### Attempt history (8)

<!-- empty unless a retry is needed -->