## Why

The community detection and clustering pipeline does not run end-to-end on large graphs today. `Pipeline.hs` Step 5 (line 263-264) is stubbed:

```haskell
let (finalComm, finalCohes) = (Map.empty, Map.empty)  -- TODO: clusterGraphWithResolution enrichedGraph' res
    anal = Analysis Map.empty Map.empty [] [] []      -- TODO: analyzeGraph enrichedGraph' Map.empty Map.empty
```

Step 4 computes a real `CommunityMap` (8,519 communities for the 78K-node solario run) and then Step 5 throws it away. `joinCommunitiesToNodes` joins against an empty map, `graph.json` ships `community_id: null` everywhere, `community_aggregates` is hardcoded to `[]` (line 296), and the HTML LOD viewer from `refactor-html-large-graph-lod` has nothing to render. Task 7 of that change (the 78K benchmark gate) cannot pass until this is fixed.

Re-enabling Step 5 means running `clusterGraphWithResolution` on the *enriched* graph (with inferred edges) for the first time. On 78K nodes this is the moment the Leiden core is stressed, and three algorithmic hotspots in `Domain/Community.hs` make it slower than it needs to be:

1. **Quadratic community grouping** — `leidenStateToCommunityMap` (line 292) and `refineCommunitiesOpt` (line 238) both use `IntMap.fromListWith (++)` to build community→member lists. `(++)` is O(|list|) per insert, so the largest community (which can be 500–2000 members around god nodes) costs O(size²). Across all communities: O(N × avg_community_size) = O(N²/C).

2. **Repeated full-vector scans per node** — `bestCommunityFor` (line 212) scans `commOfNb` once per candidate community with `VU.filter (== c)`, then `localMovingLoop` scans it twice more for `edgesToOld`/`edgesToNew` (lines 193-194). A single one-pass fold building a `Map CommunityId Count` replaces all scans: O(degree × comms) → O(degree) per node. Hub nodes (degree 100–500) see 3–20× speedup.

3. **Vector-of-vectors adjacency** — `lsNeighbors :: V.Vector (VU.Vector Int)` (line 138) allocates 78K separate heap objects with pointer indirection per node lookup. The local-moving pass reads `lsNeighbors V.! i` for every node — a cache miss per node. A CSR (compressed sparse row) representation packs all neighbors contiguously in two arrays, eliminating the indirection and making the inner loop prefetcher-friendly.

## What Changes

- **BREAKING (internal)**: `Pipeline.hs` Step 5 is unstubbed — `clusterGraphWithResolution enrichedGraph' res` runs for real, `analyzeGraph` runs for real, `computeCommunityAggregates` runs for real. `graph.json` will now carry non-null `community_id`, populated `communities`, and a non-empty `community_aggregates` key. This is the behavior the `node-schema` spec already requires but the pipeline never delivered.
- Fix `leidenStateToCommunityMap` and `refineCommunitiesOpt`: replace `IntMap.fromListWith (++)` with `IntMap.fromListWith (:)` (+ optional `map reverse` for stable order). O(N²/C) → O(N). Pure refactor, identical output up to member-list order (which Leiden does not depend on).
- Rewrite `bestCommunityFor` and the move accounting in `localMovingLoop`: one fold over `commOfNb` building `Map CommunityId Int` (edges-to-community counts), read all scores from it. Removes 2 full re-scans per moved node. `cohesionToCommunityIdx` gets the same treatment.
- Introduce a CSR adjacency representation in `LeidenState`: `lsAdj :: VU.Vector Int` (contiguous neighbors) + `lsOffset :: VU.Vector Int` (start index per node). `lsNeighbors` is removed. `buildLeidenState` builds CSR once; `localMovingLoop` and `cohesionToCommunityIdx` read contiguous slices via `VU.slice`. No change to Leiden semantics.
- Replace `scoreAllCohesion`'s per-node `neighbors g nid` allocation (which builds a fresh `Set` per call, ~470K allocations on solario) with a direct `gAdjFwd`/`gAdjBack` read, or derive cohesion from the inter-edge counts that `computeCommunityAggregates` already computes.

## Capabilities

### Modified Capabilities

- `pipeline-clustering`: The `Pipeline.hs` Step 5 re-cluster + analyze path SHALL be unstubbed. The enriched graph (post `inferEdges`) is re-clustered, analyzed, and joined to nodes. `community_aggregates` is computed from the real `CommunityMap`, not hardcoded to `[]`. This fulfills the existing `node-schema` requirement that `nodeCommunityId` is populated.
- `leiden-algorithm`: The Leiden core in `Domain/Community.hs` SHALL use O(N) community grouping (`fromListWith (:)`), one-pass modularity-gain scoring, and a CSR adjacency representation. Algorithm semantics (Leiden phases, modularity objective, merge strategy) are unchanged; only the data structures and scan patterns change.

## Impact

**Code**:
- `src/Graphos/UseCase/Pipeline.hs` — unstub Step 5 (lines 263-264, 296). Wire `computeCommunityAggregates` for real.
- `src/Graphos/Domain/Community.hs` — the bulk of the work: `LeidenState` gains CSR fields, `buildLeidenState` builds CSR, `localMovingLoop`/`bestCommunityFor`/`cohesionToCommunityIdx` rewritten for one-pass counting, `leidenStateToCommunityMap`/`refineCommunitiesOpt` use `(:)`, `scoreAllCohesion` avoids `neighbors` allocation.
- `src/Graphos/UseCase/Cluster.hs` — no change (already pure, uses `Domain.Community`).
- `src/Graphos/Infrastructure/Export/HTML.hs` — no change (consumes `community_aggregates` which is now populated).

**APIs/Dependencies**: No new Haskell dependencies. `vector-unboxed` already in use. `containers` already in use.

**Systems**: No runtime/IO change. The Leiden core is pure Domain code. Peak memory drops (CSR packs neighbors more densely than vector-of-vectors; one-pass counting avoids intermediate `nubInt` IntMaps per node).

**Tests**: Existing `cabal test` suite must pass unchanged (algorithm semantics preserved). Add Hspec/QuickCheck properties asserting `fromListWith (:)` grouping produces the same community sets as the old `(++)` version, and that one-pass scoring picks the same `bestComm` as the multi-scan version on random fixtures. Add a microbenchmark or timed regression check on a 10K-node synthetic graph to confirm the speedup direction.

## PDCA Cycle

- **Plan**: Hypothesis — unstubbed Step 5 + the three optimizations lets `graphos` cluster a 78K-node enriched graph in < 30s (current unstubbed baseline estimated > 2 min based on per-pass cost extrapolation) and produces a `graph.json` with non-null `community_id` and 8K+ `community_aggregates`. Success measured against PRD §16.1 (100K-node scale target).
- **Do**: Unstub Step 5, apply the `(:)` grouping fix, rewrite `bestCommunityFor` for one-pass counting, introduce CSR adjacency, fix `scoreAllCohesion` allocation.
- **Check**: Run `cabal run graphos -- <solario-path>`. Verify: (1) `graph.json` `nodes[].community_id` non-null for all 78,529 nodes; (2) `community_aggregates` has ~8,519 entries; (3) cluster step duration < 30s (opRecordHistogram `graphos_cluster_duration_seconds`); (4) `cabal test` passes (no Leiden regression); (5) peak memory during clustering < 1.5GB (vs vector-of-vectors baseline).
- **Act**: If the cluster step is still > 30s, profile to find the next hotspot (likely `refineCommunitiesOpt`'s `VU.unsafeUpd` per split community, or `mergeSmallCommunities`'s `bestNeighborCommunity` rebuild). If `cabal test` regresses, the one-pass counting or CSR slicing has a semantics bug — bisect by reverting one optimization at a time. If memory is fine and latency passes, standardize CSR as the Leiden adjacency representation going forward.

## Relationship to `refactor-html-large-graph-lod`

This change is a hard prerequisite for `refactor-html-large-graph-lod` task 7 (the 78K benchmark). Without unstubbed Step 5, the HTML LOD viewer renders an empty overview. The two changes are otherwise independent: this change touches `Domain/Community.hs` and `Pipeline.hs` Step 5; the HTML change touches `Infrastructure/Export/HTML.hs` and `Pipeline.hs` Step 5's write calls (which are already wired but receive empty data). Merge order: this change first, then re-run the HTML benchmark.