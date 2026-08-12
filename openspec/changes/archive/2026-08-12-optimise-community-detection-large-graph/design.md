## Context

`Pipeline.hs` Step 5 (lines 263-264, 296) is stubbed:

```haskell
let (finalComm, finalCohes) = (Map.empty, Map.empty)  -- TODO: clusterGraphWithResolution enrichedGraph' res
    anal = Analysis Map.empty Map.empty [] [] []      -- TODO: analyzeGraph enrichedGraph' Map.empty Map.empty
-- ...
let aggregates = []  -- TODO: compute aggregates for large graphs
```

Step 4 computes a real `CommunityMap` (8,519 communities for the 78K-node solario run) and Step 5 discards it. `joinCommunitiesToNodes` joins against an empty map, so `graph.json` ships `community_id: null` and `community_aggregates: []`. The HTML LOD viewer from `refactor-html-large-graph-lod` has nothing to render — its task 7 benchmark gate cannot pass until Step 5 is unstubbed.

Unstubbing Step 5 means running `clusterGraphWithResolution` on the *enriched* graph (with inferred edges) for the first time. On 78K nodes this stresses the Leiden core in `Domain/Community.hs`, which has three algorithmic hotspots that make the unstubbed path slower than the PRD §16.1 target (under 30s at 100K nodes).

This change depends on `refactor-html-large-graph-lod` for the HTML viewer (tasks 1-6) and is its prerequisite for task 7 (the benchmark). The two changes touch disjoint files except for `Pipeline.hs` Step 5, where this change provides the real data the HTML viewer's write calls already expect.

## Goals / Non-Goals

**Goals:**
- Unstub `Pipeline.hs` Step 5: re-cluster the enriched graph, run `analyzeGraph`, compute real `community_aggregates`. Produce `graph.json` with non-null `community_id` and non-empty `community_aggregates`.
- Fix the three algorithmic hotspots in `Domain/Community.hs`:
  1. Quadratic community grouping (`fromListWith (++)` → `fromListWith (:)`)
  2. Repeated full-vector scans per node in `bestCommunityFor` and the move accounting (one-pass fold)
  3. Vector-of-vectors adjacency → CSR
- Fix `scoreAllCohesion`'s per-node `neighbors g nid` allocation.
- Cluster the 78K-node solario graph in < 30s (the `graphos_cluster_duration_seconds` histogram). Peak memory during clustering < 1.5GB.
- `cabal test` passes unchanged (algorithm semantics preserved across all four optimizations).

**Non-Goals:**
- Changing Leiden semantics — modularity objective, resolution parameter, merge strategy, phase order are all unchanged. Only data structures and scan patterns change.
- Changing the HTML viewer (`Infrastructure/Export/HTML.hs`) — that is `refactor-html-large-graph-lod`.
- Switching to a different community detection algorithm (Louvain proper, Label Propagation, etc.). Leiden is retained; this change makes it faster, not different.
- Parallelizing the local-moving pass. The pass is sequential by Leiden semantics (earlier moves in a pass must be observed). Parallelization is a future cycle.
- GPU/WebGL acceleration. Out of scope for the Haskell core.

## Decisions

### Decision 1: Unstub Step 5 with the existing `clusterGraphWithResolution` + `analyzeGraph`

Step 5 SHALL call `clusterGraphWithResolution enrichedGraph' res` (the same function Step 4 uses, on the enriched graph) and `analyzeGraph enrichedGraph' finalComm finalCohes`. `computeCommunityAggregates` SHALL be called with the joined graph, `finalComm`, `finalCohes`, `analysisArticulationPoints anal`, and the LLM labels. The hardcoded `aggregates = []` is removed.

**Why not a separate "re-export" path**: The design considered adding a minimal re-export mode that reads a pre-existing `graph.json` and only recomputes communities/aggregates, to avoid re-running extraction. Rejected for this change — it adds a second pipeline path that must be kept in sync, and the extraction step is not the bottleneck (clustering is). Re-running the full pipeline is simpler and exercises the real code path.

**Why re-cluster instead of reusing Step 4's `commMap`**: Inferred edges change the topology. Re-clustering on the enriched graph gives communities that reflect inferred bridges, which is the whole point of `inferEdges`. Reusing Step 4's map would ignore the inferred-edge topology change and produce aggregates that don't match the enriched graph's edges.

**Alternatives considered:**
- *Reuse Step 4's commMap*: Rejected — ignores inferred-edge topology change; `computeCommunityAggregates` would count inter-community edges that don't exist in the pre-inference graph.
- *Skip re-cluster, join Step 4's map to the enriched graph*: Same problem — community boundaries don't reflect inferred edges.

### Decision 2: `fromListWith (:)` for community grouping (both call sites)

`leidenStateToCommunityMap` (line 292) and `refineCommunitiesOpt` (line 238) both use `IntMap.fromListWith (++) [(assign VU.! i, [i]) | i <- [0..n-1]]`. Each `(++)` is `O(|list|)`, making the largest community's list O(size²). Replaced with `IntMap.fromListWith (:)` (prepend, O(1) per insert) then optionally `Map.map reverse` for a stable head-tail order. Total: O(N).

**Why `(:)` not a builder/accumulator pattern**: `fromListWith (:)` is the idiomatic, minimal change. The member-list order is not semantically significant — Leiden reads members as a set (the `wellConnected` filter and the `members` list in `CommunityMap` are both order-independent). A `reverse` pass is included only if a downstream consumer (e.g. `representativeLabels` sorting by `nodeLabel`) benefits from stable order; otherwise skipped.

```
  BEFORE (O(N²/C) for the largest community)     AFTER (O(N) total)
  ══════════════════════════════════════════     ═════════════════════════════
  IntMap.fromListWith (++) [                      IntMap.fromListWith (:) [
    (cid, [i]) | i <- [0..n-1]                      (cid, i) | i <- [0..n-1]
  ]                                                ]
  -- largest community: K appends of avg K/2       -- every insert is O(1)
  -- ⇒ O(K²/2) for that community                  -- ⇒ O(N) total
```

**Alternatives considered:**
- *Mutable `IntMap`-of-`Vector` builders*: Rejected — over-engineering for a one-shot pass; `(:)` is enough.
- *Group via `VU.filter` over the assignment vector per community*: Rejected — O(N × C), worse than the current O(N²/C) when C is small.

### Decision 3: One-pass modularity-gain scoring via a count map

`bestCommunityFor` currently scans `commOfNb` once per candidate community with `VU.filter (== c)` to compute `sigmaIn` (edges to community `c`), then `localMovingLoop` scans it twice more for `edgesToOld`/`edgesToNew`. Replaced with a single fold over `commOfNb` building `Map.map CommunityId Int` (or `IntMap Int` — community IDs are `Int`), then all reads come from the map. `sigmaIn[c] = countMap ! c`, `edgesToOld = countMap ! currentComm`, `edgesToNew = countMap ! bestComm`.

```
  BEFORE (O(degree × |unique comms| + 2×degree) per moved node)
  ═══════════════════════════════════════════════════════════════
  nbs            = lsNeighbors[i]                          O(1)
  commOfNb       = VU.mapM read assign[nbs]                 O(degree)
  neighborComms  = nubInt commOfNb                          O(degree)
  bestCommunityFor:
    for c in neighborComms:
      sigmaIn = VU.length (VU.filter (==c) commOfNb)  ◄── O(degree) × |comms|
  if moved:
    edgesToOld = VU.filter (== old) commOfNb           ◄── O(degree) re-scan
    edgesToNew = VU.filter (== new) commOfNb           ◄── O(degree) re-scan

  AFTER (O(degree) per node)
  ══════════════════════════════════════════
  nbs            = CSR slice for i                        O(1)
  commOfNb       = VU.mapM read assign[nbs]                O(degree)
  countMap       = foldl' (\m c -> Map.insertWith (+) c 1 m) 
                         Map.empty (VU.toList commOfNb)    O(degree)
  neighborComms  = Map.keys countMap                       O(|comms|)
  bestCommunityFor:
    for c in neighborComms:
      sigmaIn = countMap ! c                         ◄── O(1)
  if moved:
    edgesToOld = Map.findWithDefault 0 old countMap         O(1)
    edgesToNew = Map.findWithDefault 0 new countMap         O(1)
```

**Why `IntMap Int` not a mutable array**: Community IDs are sparse and unbounded (Leiden can create new community IDs during refinement). An `IntMap` is the right structure. The per-node `IntMap` is small (|unique neighbor comms| entries, typically ≤ 10) and allocated once per node visit — the allocation cost is far less than the re-scan cost it eliminates.

**`cohesionToCommunityIdx` gets the same treatment**: it currently does `VU.filter (== cid) commOfNb` per node. Replaced with a count-map lookup (or, since it only needs the count for one community, a single `VU.foldl'` count — even cheaper than building a full map).

**Alternatives considered:**
- *Mutable `STRef (IntMap Int)` updated in place across the pass*: Rejected — the count map is per-node, not per-pass. Rebuilding per node is correct and cheap.
- *Precompute a global community-pair edge count*: Rejected — that's `interEdgeCounts` in `computeCommunityAggregates`, a different concern (global, not per-node-neighborhood).

### Decision 4: CSR adjacency in `LeidenState`

`lsNeighbors :: V.Vector (VU.Vector Int)` is replaced with:

```haskell
data LeidenState = LeidenState
  { lsNodeIds   :: !(V.Vector NodeId)
  , lsAdj       :: !(VU.Vector Int)    -- contiguous neighbors (length 2E)
  , lsOffset    :: !(VU.Vector Int)    -- start index per node (length N+1)
  , lsDegrees   :: !(VU.Vector Double)
  , lsAssignment :: !(VU.Vector Int)
  , lsSigmaTot   :: !(IntMap Double)
  , lsM          :: !Double
  , lsGamma      :: !Double
  , lsN          :: !Int
  }
```

`buildLeidenState` computes the CSR by first collecting per-node neighbor lists (as today), then concatenating into `lsAdj` and accumulating offsets into `lsOffset`. The local-moving pass reads `let (start, end) = (offset VU.! i, offset VU.! (i+1)); nbs = VU.slice start (end - start) adj`.

```
  BEFORE (vector-of-vectors)              AFTER (CSR)
  ════════════════════════════════════    ════════════════════════════════════
  lsNeighbors :: V.Vector (VU.Vector Int) lsAdj    :: VU.Vector Int  (one block)
                  ↑                              lsOffset :: VU.Vector Int  (N+1)
            78K heap objects
            78K pointer indirections              slice i = VU.slice (off!i) 
            cache-miss per node lookup                            (off!(i+1)-off!i) adj
                                            contiguous, prefetcher-friendly
```

**Why CSR not adjacency as `Map NodeId [NodeId]`**: The `Graph` already stores adjacency as `Map NodeId (Set NodeId)` (`gAdjFwd`/`gAdjBack`). CSR is the compact form for the Leiden inner loop, which needs indexed, contiguous access — exactly what `Map`/`Set` don't provide. The build cost (one pass over the graph's adjacency maps) is paid once; every pass benefits.

**Dangling-edge handling preserved**: the current self-loop fallback (`if Map.lookup nb nidToIdx == Nothing then i else idx`) is preserved in the CSR build — dangling neighbors are stored as a self-loop index.

**Build cost**: O(N + E). The per-node neighbor `Set.toList` + `Map.lookup` per neighbor is the same work as today; the difference is we concatenate into one vector instead of 78K small vectors. Build is one-shot, not per-iteration.

**Memory**: CSR is `2E + (N+1)` ints vs `N` vector-pointers + `2E` ints (vector-of-vectors). CSR is strictly smaller (no per-node vector header overhead). On solario (78K nodes, ~235K edges): CSR ≈ 470K + 78K ints ≈ 2.2MB; vector-of-vectors ≈ 470K ints + 78K × (24-byte vector header) ≈ 2.8MB. The win is cache locality, not raw size.

**Alternatives considered:**
- *Keep vector-of-vectors, only fix the scans*: Rejected — the per-node cache miss is the dominant cost on the local-moving pass, which runs up to 50×. CSR is the standard representation for this workload.
- *Flat `Array Int Int` (boxed) instead of `VU.Vector Int`*: Rejected — unboxed is strictly better for `Int` payload.
- *Adjacency as `V.Vector (V.Vector Int)` (boxed inner)*: Rejected — boxed inner vectors defeat the purpose (indirection + boxing overhead).

### Decision 5: `scoreAllCohesion` reads `gAdjFwd`/`gAdjBack` directly

Current `cohesionScore g members` calls `neighbors g nid` per node, which does `Map.findWithDefault Set.empty` ×2 + `Set.union`, allocating a fresh `Set` per call. On solario this is ~470K allocations (sum of degrees). Replaced with direct `gAdjFwd`/`gAdjBack` reads and a membership check against the pre-built `memberSet`, no intermediate `Set` allocation.

```haskell
-- BEFORE: allocates a Set per neighbors call
internalEdges = length [1 | nid <- members
                          , n <- Set.toList (neighbors g nid)  -- allocates Set
                          , n `Set.member` memberSet
                          , nid < n]

-- AFTER: reads adj maps directly, no per-node Set
adjFwd = gAdjFwd g
adjBwd = gAdjBack g
directed = gDirected g
internalEdges = sum [1 | nid <- members
                       , let fwdNbs = Map.findWithDefault Set.empty nid adjFwd
                       , let nbs = if directed then fwdNbs
                                    else Set.union fwdNbs 
                                         (Map.findWithDefault Set.empty nid adjBwd)
                       , n <- Set.toList nbs
                       , n `Set.member` memberSet
                       , nid < n]
```

(Or, equivalently, derive internal-edge counts from the `interEdgeCounts` map that `computeCommunityAggregates` already builds — internal edges = total edges touching community − inter-community edges. This avoids the per-node neighbor iteration entirely. Preferred if the aggregate computation is already running on the same graph.)

**Why fix this here**: it runs right after clustering, before export, and the allocation spike contributes to GC pressure during the export step. It's one-shot (not per-iteration) so the win is modest, but it's a 5-line change with no semantic risk.

**Alternatives considered:**
- *Leave it, it's one-shot*: Rejected — the allocation spike is ~470K `Set`s on solario, measurable in GC time. The fix is trivial.
- *Compute cohesion during clustering (reuse Leiden's adjacency)*: Rejected — `scoreAllCohesion` runs on the enriched graph, not the Leiden `LeidenState`. Coupling them would violate layering (Domain.Analysis would need Domain.Community internals).

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| One-pass counting or CSR introduces a semantics bug (wrong `bestComm`, wrong `sigmaTot`) | The tasks require a deterministic-fixture regression test: cluster the same fixtures with the old and new implementations and assert identical `CommunityMap`s. Bisect by reverting one optimization at a time. |
| CSR build adds latency to `buildLeidenState` (one-shot, but on 78K nodes) | O(N+E), measured in the benchmark. If build is > 1s, profile — likely the `Set.toList` per node is the cost; can be replaced with a direct adjacency iteration. |
| Unstubbing Step 5 makes the enriched-graph re-cluster the new critical path — if it's still > 30s after all three optimizations, the change fails its own benchmark | The PDCA Act step defines the fallback: profile to find the next hotspot. Candidates: `refineCommunitiesOpt`'s `VU.unsafeUpd` per split community (one vector copy per split), `mergeSmallCommunities`'s `bestNeighborCommunity` rebuild. Both are fixable in a follow-up cycle. |
| `fromListWith (:)` reverses member-list order, breaking a downstream consumer that depends on order | `representativeLabels` sorts by `nodeLabel` before `take 3`, so order doesn't matter. `selectRepresentatives` sorts by degree. No consumer depends on member-list order. Verified by `cabal test`. |
| Re-clustering the enriched graph produces different community IDs than Step 4, breaking a consumer that cached Step 4 IDs | No consumer caches Step 4's `commMap` — it's local to `Pipeline.hs`. `finalComm` (Step 5) is the only one written. |
| Peak memory rises because Step 5 holds `enrichedGraph'` + `finalComm` + `anal` simultaneously | Already the case in the stubbed path (the stub holds `enrichedGraph'` and returns it). The real `finalComm`/`anal` add ~10–20MB on 78K nodes. The existing `performGC` after Step 5 (line 311) reclaims clustering intermediates. |

## Verification Strategy (Check)

1. **Unit/property tests (`cabal test`)**:
   - Deterministic fixture: cluster with old and new `Domain/Community.hs` (via a feature flag or a before/after commit comparison) → identical `CommunityMap`. One test per optimization (grouping, one-pass counting, CSR, cohesion) to bisect regressions.
   - QuickCheck: for any graph, `fromListWith (:)` grouping and `fromListWith (++)` grouping produce the same community-member sets.
   - QuickCheck: one-pass `bestCommunityFor` picks the same `bestComm` as multi-scan on random neighbor-community vectors.
   - New test: `scoreAllCohesion` with direct reads equals the old `neighbors`-based version on a fixture.

2. **Build gate (`cabal build`)**: `-Wall -Wcompat -Werror` per PRD §15.2.

3. **Pipeline integration (`cabal run graphos -- <small-fixture>`)**: produces `graph.json` with non-null `community_id` on community members and a non-empty `community_aggregates` array. This is the test that Step 5 is truly unstubbed.

4. **78K-node benchmark (`cabal run graphos -- <solario-path>`)**:
   - `graph.json` `nodes[].community_id` non-null for all 78,529 nodes.
   - `community_aggregates` length ≈ 8,519 (within ±5% of Step 4's count — inferred edges may shift boundaries).
   - `graphos_cluster_duration_seconds` histogram: Step 4 + Step 5 combined < 30s.
   - Peak memory during clustering < 1.5GB (RTS `-s` or heap profile).
   - `cabal test` passes.

5. **No-regression**: `graph.json` node count, edge count unchanged. Community count within ±5% (enriched-graph re-cluster is expected to differ slightly from Step 4).

## Iteration & Rollback (Act)

- **If the benchmark fails on latency**: profile (RTS `-p -s` or `eventlog`). The three optimizations target the known hotspots; if latency is still > 30s, the next hotspot is likely `refineCommunitiesOpt`'s `VU.unsafeUpd` (one vector copy per split community — could be batched) or `mergeSmallCommunities`'s `bestNeighborCommunity` (rebuilds edge counts per small community). Open a follow-up cycle.
- **If `cabal test` regresses**: bisect by reverting optimizations one at a time (grouping → one-pass counting → CSR → cohesion). The deterministic-fixture tests pin down which optimization broke semantics.
- **If memory regresses**: the CSR build or the enriched-graph re-cluster is retaining thunks. Check `deepseq` boundaries (the existing `_ <- evaluate (Map.size commMap + ...)` at line 239 is the model).
- **Rollback**: revert `Pipeline.hs` lines 263-264, 296 to the stub (communities empty again — breaks the HTML viewer, which is the expected pre-change state). The `Domain/Community.hs` optimizations are independent of the stub and can stay (they speed up Step 4 too, which already runs).

## Migration Plan

1. Unstub `Pipeline.hs` Step 5 (lines 263-264, 296) — re-cluster, analyze, compute aggregates. Run `cabal test` to confirm no regression (the test suite should already expect non-empty communities from the existing `leiden-scalability` spec).
2. Apply the `(:)` grouping fix in `leidenStateToCommunityMap` and `refineCommunitiesOpt`. Add the deterministic-fixture regression test. Run `cabal test`.
3. Apply the one-pass counting fix in `bestCommunityFor` and `localMovingLoop`. Add the one-pass-vs-multi-scan QuickCheck property. Run `cabal test`.
4. Apply the CSR adjacency change in `LeidenState`, `buildLeidenState`, `localMovingLoop`, `cohesionToCommunityIdx`. Add the CSR-vs-vector-of-vectors regression test. Run `cabal test`.
5. Apply the `scoreAllCohesion` direct-read fix. Run `cabal test`.
6. Run the 78K-node solario benchmark. Record `graphos_cluster_duration_seconds`, peak memory, and `community_aggregates` count.
7. No data migration — the next pipeline run populates the new fields. Old `graph.json` files load fine (consumers tolerate the missing/empty `community_aggregates` and null `community_id`).

## Open Questions

- Should the CSR build be reused for `scoreAllCohesion` and `computeCommunityAggregates` (which currently iterate `gEdges`/`gAdjFwd`)? They run on the `Graph` (not `LeidenState`), so coupling would mean either building a second CSR on the `Graph` or exposing `LeidenState`'s CSR to the UseCase layer. Lean: no — build CSR only in `LeidenState`; the other passes are one-shot and not the bottleneck.
- Should the deterministic-fixture regression test be a golden-file test (commit the expected `CommunityMap` as JSON) or a before/after comparison in-process? Lean: golden-file — it survives across commits and documents the expected output. The before/after comparison is the migration aid, removed once the golden file is committed.
- Should we add a microbenchmark suite (criterion) for the Leiden core, or rely on the 78K-node end-to-end benchmark? Lean: add a small criterion benchmark on a 10K-node synthetic graph — it runs in CI in seconds and catches per-pass regressions without needing the full solario corpus.