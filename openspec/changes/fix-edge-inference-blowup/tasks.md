# Tasks: fix-edge-inference-blowup

<!-- PDCA-per-task. Task 1 (dedup helper) is foundational. Tasks 2-3 use it.
     Task 4 is the integration gate. -->

## 1. Order-preserving dedup helper (Domain) + replace nubBy everywhere

- [x] 1.P Plan: Add `dedupOn :: Ord k => (a -> k) -> [a] -> [a]` (Set-based, first-wins, order-preserving) to `src/Graphos/Domain/Analysis.hs` (exported); replace `nubBy` in `crossCommunitySurprises` (Analysis.hs:98), `inferTransitiveDeps` (Infer.hs:77), `inferCodeDocEdges` (Infer.hs:145). Check criteria: (a) QuickCheck property — `dedupOn key xs == nubBy (\a b -> key a == key b) xs` for small lists; (b) unit cases for order and first-wins; (c) `cabal build -Werror` + suite green.
- [x] 1.D Do: `dedupOn` added to Domain.Analysis (exported, haddock'd, BangPatterns); all three `nubBy` call sites replaced; `tests/Graphos/UseCase/InferSpec.hs` with QuickCheck equivalence property; registered in cabal.
- [x] 1.C Check: `cabal test` PASS — property + unit cases green.
- [x] 1.A Act: "No nubBy on graph-scale lists" convention recorded in the `dedupOn` haddock.

## 2. Adjacency-based community bridges with cap (UseCase)

- [x] 2.P Plan: Rewrite `inferCommunityBridges`: candidate pairs = normalized community pairs that have ≥1 real inter-community edge (map edge endpoints through `nodeCommunityMap`, Set-dedup) — O(E log C); emit centroid bridges for candidates only; truncate at `maxCommunityBridges` (named constant, 10000, haddock'd). Check criteria: (a) Hspec — adjacent pair bridged exactly once; disconnected pair not bridged; output ≤ cap; `notEdgeAlready` still respected; (b) `cabal build -Werror` + suite green.
- [x] 2.D Do: Implemented adjacency-derived candidates from `Map.keys (gEdges g)` + `nodeCommunityMap`; `take maxCommunityBridges`; star-community InferSpec fixtures (leaf-connected so centroids have no pre-existing edge).
- [x] 2.C Check: `cabal test` PASS. Note: first fixture attempt used triangle communities whose connecting-edge endpoints became centroids — `notEdgeAlready` correctly suppressed the bridge; fixture redesigned with star communities.
- [x] 2.A Act: Semantics change documented (no fabricated bridges between unrelated communities); disconnected content now legitimately forms separate components.

## 3. Doc-code label fan-out cap (UseCase)

- [x] 3.P Plan: In `inferCodeDocEdges`, filter `codeLabelIdx` and `codeBaseIdx` entries whose match lists exceed `maxLabelFanOut` (named constant, 20, haddock'd). Check criteria: (a) Hspec — ambiguous label (matches > cap) yields no edges; specific label yields edges; no duplicate (source,target) pairs; (b) `cabal build -Werror` + suite green.
- [x] 3.D Do: `boundedIdx` filter applied to both indexes; InferSpec cases for ambiguous (30 matches → 0 edges) and specific (1 match → 1 References edge) labels.
- [x] 3.C Check: `cabal test` PASS (178 examples, 0 failures).
- [x] 3.A Act: Done.

## 4. Integration verification

- [x] 4.P Plan: Full pipeline at default (Normal) density on this repo. Check criteria: (a) "Inferred N additional edges" ≪ 48,112 baseline (order of real inter-community adjacency); (b) pipeline completes without memory growth anomalies; (c) `scripts/audit_graph.py` passes; (d) full `cabal test` + `cabal build -Werror` green.
- [x] 4.D Do: Ran pipeline; compared against baseline; updated the audit script's component heuristic (per-file islanding bound instead of per-node — honest doc islands are now expected since fabricated mesh connectivity is gone).
- [x] 4.C Check:
  - (a) Inferred edges: **48,112 → 9,974** (4.8×); total edges 53,908 → 15,180 ✓
  - (b) Pipeline completed normally, no memory anomalies ✓
  - (c) `scripts/audit_graph.py` PASS (193 components < 320 source files — honest structure) ✓
  - (d) `cabal build` + `cabal test` PASS (178 examples, 0 failures) ✓
- [x] 4.A Act: Ready to archive. **User action**: re-run the 3,136-file corpus at default density — Step 4 should now complete in seconds with bounded RAM (adjacent-pair bridges scale with E≈80k, not C²≈10⁸). If it still misbehaves, profile with `+RTS -s` and open a follow-up.
