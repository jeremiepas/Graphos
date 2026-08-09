# Proposal: fix-edge-inference-blowup

## Why

A 3,136-file corpus (1,857 code + 1,271 docs, function granularity → 75,285 nodes / 80,687 edges, extraction in 4 s) consumed **47 GB of RAM and hung in "Step 4: Detecting communities..."** — after the Leiden fixes landed. Clustering itself is fast (100k-node benchmark: 10.5 s); the blowup is in the edge-inference stage (PRD §3 infer stage) that runs inside the same un-logged span. Three quadratic constructions in `Graphos.UseCase.Infer` (plus one in `Graphos.Domain.Analysis`) are responsible:

1. **`inferCommunityBridges` materializes O(C²) edges**: it emits an inferred edge for *every pair* of community centroids. Measured on this repo: 314 communities → 48,112 inferred edges (~every pair). At the corpus's expected 10–15k communities that is **50–110 million Edge allocations** — matching the observed 47 GB.
2. **`nubBy` deduplication is O(k²)**: `inferTransitiveDeps` and `inferCodeDocEdges` dedupe edge lists with linked-list `nubBy` (pairwise comparisons), which is quadratic in list length and retains the entire input.
3. **`inferCodeDocEdges` has unbounded label fan-out**: every doc node whose label equals a code label links to *all* matching code nodes. With 1,271 doc files and generic labels ("Config", "Usage") matching hundreds of code nodes across 1,857 files, this feeds millions of edges into the quadratic `nubBy`.
4. `crossCommunitySurprises` (Domain.Analysis) uses the same `nubBy` pattern over all cross-community edges.

Note: even `--edge-density sparse` is affected (it still runs `inferCodeDocEdges`). This violates PRD §16.1/§16.2 (100k-node pipelines within memory bounds) and blocks the primary use case.

## What Changes

- **`inferCommunityBridges` bridges only adjacent communities** — pairs that share at least one real inter-community edge — bounded by a named cap constant. Complexity drops from O(C²) to O(E).
- **All edge/candidate deduplication becomes O(k log k)** via an order-preserving, first-wins `dedupOn` helper (Set-based), replacing `nubBy` in `inferTransitiveDeps`, `inferCodeDocEdges`, and `crossCommunitySurprises`.
- **Label fan-out in `inferCodeDocEdges` is capped**: labels matching more than a named-constant number of code nodes are skipped (ambiguous names carry no linking signal).

Out of scope: the remaining `fix-runtime-ram-crash` items (LSP concurrency, accumulators, node representation), edge-density semantics, new inference strategies.

## Capabilities

### New Capabilities
- `bounded-edge-inference`: edge inference scales linearly with real graph size — adjacency-based community bridging, log-linear deduplication, capped doc-code label fan-out (workflows: 01-full-pipeline, 02-incremental-pipeline).

### Modified Capabilities
<!-- none — inference output on small graphs changes only where the old behavior was pathological (full centroid mesh); node/edge schema unchanged -->

## Impact

- **Code**: `src/Graphos/UseCase/Infer.hs`, `src/Graphos/Domain/Analysis.hs` (dedup helper + use). Pure Domain/UseCase changes, no API/CLI/config changes, no new dependencies.
- **Behavior**: inferred bridge edges connect only communities with real adjacency (previously: all pairs); dedup keeps the same first-wins semantics; doc-code links for hyper-ambiguous labels are dropped.
- **Tests**: new `InferSpec` (bridges adjacency + cap, dedup semantics, fan-out cap); existing suite green.

## PDCA Cycle

- **Plan**: Hypothesis — the three constructions above are the entire Step 4 memory blowup. Success criteria (PRD §16.1/§16.2): full pipeline at default (`Normal`) density on this repo completes with inferred-edge count proportional to real inter-community adjacency (thousands, not ~C²/2); unit specs pass; peak process memory on a 75k-node-scale synthetic inference stays bounded (seconds, not GB-minutes).
- **Do**: Implement the three fixes + helper (design.md, tasks.md).
- **Check**: `cabal test` (new InferSpec + suite); full pipeline run on this repo comparing inferred-edge count against the 48,112 baseline; audit script passes.
- **Act**: If the corpus still exceeds memory bounds at Step 4, profile with heap profiling (fix-runtime-ram-crash item 7) and iterate; otherwise archive and note the "no unbounded pair enumeration; no nubBy on unbounded lists" convention.
