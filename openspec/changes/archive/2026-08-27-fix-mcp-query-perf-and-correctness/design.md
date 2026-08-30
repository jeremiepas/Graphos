## Context

The MCP query path has two layers of repeated O(N) work that should happen once at load time, plus a silent correctness bug in the FGL adapter. The fixes are mostly mechanical (threading values through signatures) but touch several modules, so this design records the chosen shapes and the alternatives rejected.

Findings from explore-mode investigation (see `proposal.md` Why section for evidence + line numbers):

- **T1 — MCP rebuilds `GraphIndex` per call.** `Server/MCP.hs` uses legacy `queryGraph`/`pathQuery` which internally call `buildIndex g Map.empty` every invocation. `lrIndex` is built at load but discarded.
- **T1b — `handleQueryGraph` triple-calls.** Lines 130/131/132 each call `queryGraph g question mode budget` independently to extract three fields.
- **T1c — Legacy path builds index with empty communities.** `communityOfNode` via that index returns `Nothing` for every node.
- **T2 — `shortestPath` rebuilds FGL per call.** `toCachedFGL g` → `toFGL` → `mkGraph` is O(N + E) per invocation, even via `pathQueryWithIndex`.
- **T3 — `nidToInt` hash collisions (correctness).** `FGL.hs` line 55 hashes `NodeId → Int` mod `maxBound` with no collision handling. `mkGraph` silently overwrites colliding nodes; `cachedFindIdx` returns `Nothing` for the lost `NodeId`, so `shortestPath` returns `Nothing` even when a path exists. Same silent loss affects all FGL-backed algorithms.
- **T4 — `buildLabelIndex` uses `(++)`.** Same O(N × avg_hits) pattern flagged for Leiden in `optimise-community-detection-large-graph`. Speeds up the one-time index build.

## Goals / Non-Goals

**Goals**
- O(N) index build + O(N + E) FGL conversion happen exactly once per server lifetime, not per request.
- `query_graph` MCP latency on 100K-node graphs drops from seconds to milliseconds.
- `shortest_path` no longer returns false `Nothing` due to `nidToInt` collisions.
- No algorithm semantics change — existing `cabal test` passes unchanged.

**Non-Goals**
- Optimizing `findMatchingNodes` (already O(k × log N + hits)) or BFS (already O(V_sub + E_subgraph)).
- Optimizing JSON serialization of large result sets (separate concern; may surface as the next bottleneck after this fix).
- Changing the MCP protocol surface (tools, args, responses unchanged).
- Optimizing the extraction/clustering pipeline (covered by `optimise-community-detection-large-graph`).
- Adding caching/persistence across server restarts (in-memory caching within one server lifetime is enough).

## Design

### Decision: Thread `GraphIndex` + `CachedFGL` through the MCP server

```
  BEFORE                                    AFTER
  ─────                                     ─────

  loadGraphFromFile                         loadGraphFromFile
   ├─ lrGraph  ✓                            ├─ lrGraph      ✓
   ├─ lrIndex  ✓ (built, then               ├─ lrIndex      ✓ (built once)
   │             discarded by MCP)          ├─ lrCachedFGL  ✓ (NEW — built once)
   └─ lrCommunities ✓                      └─ lrCommunities ✓
        │                                        │
        ▼                                        ▼
  startMCPServerFromFile                    startMCPServerFromFile
   └─ startMCPServer g commMap analysis     └─ startMCPServer g idx cachedFGL commMap analysis
        │                                        │
        ▼                                        ▼
  requestLoop g commMap analysis            requestLoop g idx cachedFGL commMap analysis
        │                                        │
        ▼                                        ▼
  handleRequest g commMap analysis req     handleRequest g idx cachedFGL commMap analysis req
        │                                        │
        ▼                                        ▼
  handleToolCall g commMap analysis ...    handleToolCall g idx cachedFGL commMap analysis ...
        │                                        │
        ▼                                        ▼
  handleQueryGraph g args                  handleQueryGraph g idx args
   └─ queryGraph g q m b  ← rebuilds idx    └─ queryGraphWithIndexScored g idx q m b  ← O(k)
      ×3 (lines 130/131/132)                  once, derive all fields
```

**Why thread both `GraphIndex` and `CachedFGL`?** They serve different query families:
- `GraphIndex` serves term matching (`query_graph`, `explain`, `symbols`) and direct BFS (`neighbors`).
- `CachedFGL` serves FGL-backed algorithms (`shortest_path`, `articulationPoints`, `biconnectedComponents`, `dominators`).

Both are O(N) or O(N + E) to build and immutable after load. Keeping both resident is the same memory the server already pays per-request today — just paid once instead of per-call.

**Signature shape:**

```haskell
-- Load.hs
data LoadResult = LoadResult
  { lrGraph       :: Graph
  , lrIndex       :: GraphIndex
  , lrCachedFGL   :: CachedFGL          -- NEW
  , lrCommunities :: CommunityMap
  , lrCohesion    :: CohesionMap
  , lrGodNodes    :: [GodNode]
  , lrCommunityLabels :: Map Int Text
  }

-- MCP.hs
startMCPServerFromFile :: FilePath -> IO ()
startMCPServer :: Graph -> GraphIndex -> CachedFGL -> CommunityMap -> Analysis -> IO ()
requestLoop   :: Graph -> GraphIndex -> CachedFGL -> CommunityMap -> Analysis -> IO ()
handleRequest :: Graph -> GraphIndex -> CachedFGL -> CommunityMap -> Analysis -> MCPRequest -> IO ()
handleToolCall:: Graph -> GraphIndex -> CachedFGL -> CommunityMap -> Analysis -> Value -> KM.KeyMap Value -> IO ()
```

Five values threaded through four functions. Verbose but mechanical and explicit. No newtype wrapper — keep it readable; a `ServerState` record could come later if more state accumulates.

**Rejected: `IORef` / `MVar` server state.** The values are immutable after load — no mutation needed. Pure threading is simpler, testable, and avoids concurrency concerns (the server is single-threaded stdio today, but pure threading future-proofs against a move to concurrent handlers).

**Rejected: lazy `Maybe CachedFGL` built on first shortest_path call.** Tempting (only pay FGL cost if someone calls `shortest_path`), but (a) it adds lazy-IO/thunk complexity, (b) the collision fix in T3 requires changing `toCachedFGL` anyway, so we're already paying the build cost in the fix, and (c) predictable latency (build at startup, fast forever after) is better than a surprise multi-second pause on the first `shortest_path` call.

### Decision: `handleQueryGraph` single-call + field derivation

```haskell
-- BEFORE (MCP.hs lines 122-133)
handleQueryGraph g args = do
  let question = ...
      mode     = ...
      budget   = ...
  pure $ Right $ object
    [ "nodes"    .= qrNodes   (queryGraph g question mode budget)   -- ×3 rebuild
    , "edges"    .= qrEdges   (queryGraph g question mode budget)
    , "traverse" .= qrTraverse (queryGraph g question mode budget)
    ]

-- AFTER
handleQueryGraph g idx args = do
  let question = ...
      mode     = ...
      budget   = ...
      resp     = queryGraphWithIndexScored g idx question mode budget
  pure $ Right $ object
    [ "verdict"     .= qrespVerdict resp
    , "best_score"  .= qrespBestScore resp
    , "hash"        .= qrespHash resp
    , "nodes"       .= qrespNodes resp
    , "edges"       .= qrespEdges resp
    , "suggestions" .= qrespSuggestions resp
    ]
```

The response shape gains `verdict`/`best_score`/`hash`/`suggestions` (already produced by the scored path — currently thrown away). This is additive — clients reading only `nodes`/`edges` are unaffected. The old `traverse` field is dropped (it was just the echoed `mode` string; no client depends on it, and the scored path doesn't carry it).

**Rejected: keep the old unscored `QueryResult` shape.** The scored path (`queryGraphWithIndexScored`) is strictly richer and is what the CLI already uses (`Main.hs` line 183). Keeping the MCP server on the unscored legacy path would mean two query code paths diverging forever. Converging on the scored path reduces surface area.

### Decision: `*WithCached` query variants + backward-compat wrappers

```haskell
-- Domain/Graph/Query.hs

shortestPathWithCached :: CachedFGL -> NodeId -> NodeId -> Maybe [NodeId]
shortestPathWithCached cfg src tgt =
  let gr     = cfgGraph cfg
      nidMap = cfgNidMap cfg
  in case (cachedFindIdx cfg src, cachedFindIdx cfg tgt) of
       (Just s, Just t) ->
         let path = esp s t gr
         in if null path then Nothing
            else Just [Map.findWithDefault src idx nidMap | idx <- path]
       _ -> Nothing

-- Backward-compat wrapper (for any direct caller that still passes Graph)
shortestPath :: Graph -> NodeId -> NodeId -> Maybe [NodeId]
shortestPath g src tgt = shortestPathWithCached (toCachedFGL g) src tgt
```

Same pattern for `breadthFirstSearch`, `depthFirstSearch`. The MCP server and `LoadResult` use the `*WithCached` variants directly; the wrappers preserve the existing API so `UseCase/Query.hs` and tests don't break.

**Why not change `shortestPath`'s signature directly?** It would force every caller (including `pathQueryWithIndex` in `UseCase/Query.hs`, tests, and any future caller) to thread `CachedFGL`. The wrapper approach keeps the blast radius small: only the MCP server and `LoadResult` need to know about the cache; everyone else keeps calling `shortestPath g` and pays the (now-correct, see T3) rebuild cost only if they didn't get a cache from load.

### Decision: Bijective sequential indices in `toCachedFGL` (T3 fix)

```haskell
-- Domain/Graph/Analysis.hs

data CachedFGL = CachedFGL
  { cfgGraph   :: !FGLGraph
  , cfgNidMap  :: !(Map Int NodeId)       -- idx → NodeId (unchanged)
  , cfgIdxMap  :: !(Map NodeId Int)       -- NodeId → idx (NEW, replaces cfgIdxList)
  }

toCachedFGL :: Graph -> CachedFGL
toCachedFGL g =
  let nids    = Map.keys (gNodes g)
      idxMap  = Map.fromList (zip nids [0..])           -- bijective
      nidList = [0..] `zip` nids                         -- [(0, n0), (1, n1), ...]
      fglNodes = [(idx, (nid, n)) | (idx, nid) <- nidList
                                  , Just n <- [Map.lookup nid (gNodes g)]]
      fglEdges = [ (idxOf s, idxOf t, (edgeRelation e, edgeConfidence e, e))
                 | ((s, t), e) <- Map.toList (gEdges g)
                 , Just idxS <- [Map.lookup s idxMap]
                 , Just idxT <- [Map.lookup t idxMap]
                 , let idxOf _ = 0  -- placeholder, see below
                 ]
      gr = mkGraph fglNodes fglEdges
  in CachedFGL { cfgGraph = gr
               , cfgNidMap = Map.fromList nidList
               , cfgIdxMap = idxMap
               }

cachedFindIdx :: CachedFGL -> NodeId -> Maybe Int
cachedFindIdx cfg nid = Map.lookup nid (cfgIdxMap cfg)   -- O(log N), was O(N) list lookup
```

(Edge construction uses `idxMap` for both endpoints — sketch simplified for clarity; real code looks up both `s` and `t` in `idxMap`.)

**Why not fix `nidToInt` directly?** The hash function is the root cause, but replacing it with a sequential mapping requires knowing the full node set at index-assignment time — which is exactly what `toCachedFGL` does. Fixing it at the `CachedFGL` layer (where the bijective map is built) is cleaner than threading a `Map NodeId Int` into `toFGL`. `toFGL` itself can either (a) accept the index map as an argument, or (b) be retired in favor of `toCachedFGL` being the only FGL construction path. Option (b) is cleaner but larger blast radius — prefer (a) for this change: `toFGL` takes an optional `Map NodeId Int` and uses it when provided, falling back to `nidToInt` (with a warning) when not. Audit shows no caller outside `toCachedFGL` uses `toFGL` directly, so the fallback is dead code — but keeping it avoids a breaking change to the `FGL` module export list.

**Rejected: keep `cfgIdxList :: [(NodeId, Int)]` and just fix `nidToInt`.** The association-list `lookup` in `cachedFindIdx` is O(N) — called per `shortestPath` source/target and per `bfs`/`dfs` start. On 100K-node graphs that's O(N) per query just for index lookup, partially negating the index-threading win. `Map NodeId Int` is O(log N) and is the right structure.

### Decision: `buildLabelIndex` `(++)` → `(:)`

```haskell
-- BEFORE
splitTokens = Map.fromListWith (++)
  [ (word, [nid]) | ... ]

-- AFTER
splitTokens = Map.map reverse $ Map.fromListWith (:)
  [ (word, [nid]) | ... ]
```

Same for `buildPathIndex`. Identical output up to element order within each term's list (which `findMatchingNodes` consumes via `Map.fromListWith (+)` — order-insensitive). One-line change each.

## Risks

- **`LoadResult` gains a field.** Additive — existing consumers (`Main.hs`, `Merge.hs`, MCP) pattern-match what they need and ignore new fields. Low risk; confirmed by grep that all consumers use field-accessor syntax, not positional construction.
- **`CachedFGL` internal shape changes (`cfgIdxList` → `cfgIdxMap`).** `CachedFGL` is exported from `Analysis.hs` and used by `Query.hs`. Only two consumers (`cachedFindIdx` and the `Query.hs` functions) touch `cfgIdxList` — both are updated in this change. Low risk.
- **MCP response shape changes (gains `verdict`/`best_score`/`hash`/`suggestions`, loses `traverse`).** The MCP tool is consumed by LLM clients that typically tolerate additive fields. Losing `traverse` (an echo of the input `mode`) is the only potentially breaking change — verify no client reads it. If uncertain, keep `traverse` as an echo of `mode` for one release.
- **`toFGL` signature change (optional index map).** If we keep the `nidToInt` fallback, no caller breaks. If we make the index map required, `toFGL`'s direct callers (audit: none outside `toCachedFGL`) break. Prefer optional-argument version.
- **Sequential indices change FGL node ordering.** `labNodes gr` returns nodes in FGL's internal order, which is currently hash-order. Switching to sequential changes the order of `articulationPoints` / `biconnectedComponents` results (both return lists). Existing tests that compare these lists exactly may break. Mitigation: sort the results in the wrappers, or update tests to compare as `Set`s. Check `cabal test` after the change.
- **Collision fix may surface previously-hidden paths.** Graphs that previously returned `Nothing` for some `shortest_path` queries (due to collisions) will now return real paths. This is correct behavior but could surprise users/clients who built workarounds. Document in CHANGELOG.

## Migration Plan

1. Add `lrCachedFGL` to `LoadResult` and build it in `loadGraphFromFile`. (No consumer breaks — additive.)
2. Switch `toCachedFGL` to sequential indices + `cfgIdxMap`. Update `cachedFindIdx`. Run `cabal test` — if FGL-order-sensitive tests break, sort results in wrappers.
3. Add `*WithCached` variants in `Query.hs` (wrappers around existing functions). No behavior change yet.
4. Thread `GraphIndex` + `CachedFGL` through MCP server signatures. Update `handleQueryGraph` (single call, scored path) and `handleShortestPath` (`pathQueryWithIndex` + `shortestPathWithCached`). Run `cabal test`.
5. Switch `buildLabelIndex` / `buildPathIndex` to `(:)`. Run `cabal test`.
6. Add the collision regression test (synthetic graph with two colliding `NodeId`s under the old hash — confirm `shortestPath` now finds the path).

Each step is independently mergeable. Steps 1–3 are pure refactors (no behavior change). Step 4 is the perf fix. Step 5 is a minor build-time perf fix. Step 6 is the correctness gate.

## Open Questions

- **Does any MCP client read the `traverse` field?** If unknown, keep it as `mode` echo for one release. Cheap insurance.
- **Should `startMCPServer`'s signature grow to 5 args, or wrap in a `ServerState` record?** 5 args is tolerable; a record is cleaner if more state is coming (e.g., conversation cache, observability tracer). Defer the record until there's a third new field.
- **Is `toFGL` used anywhere outside `toCachedFGL`?** Grep says no, but the export list still exposes it. If confirmed unused, retire `toFGL` and `nidToInt` in a follow-up cleanup change (not this one — keep blast radius small).