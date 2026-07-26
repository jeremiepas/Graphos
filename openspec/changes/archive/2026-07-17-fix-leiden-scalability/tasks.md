# Tasks: fix-leiden-scalability

<!-- PDCA-per-task. Task 1 captures goldens BEFORE any behavior-adjacent edit.
     Tasks 2–4 are the fixes. Task 5 is the integration/perf gate. -->

## 1. Golden result-equivalence specs (tests first)

- [x] 1.P Plan: Capture current clustering outputs as golden Hspec assertions before touching the implementation: deterministic reference graphs (path, star, two cliques + bridge, small-community merge case) → exact `CommunityMap`s from `clusterGraphWithResolution`. Check criteria: (a) new specs pass against the CURRENT implementation; (b) `cabal build -Werror` clean; (c) existing suite green.
- [x] 1.D Do: Added 3 golden cases (two-cliques+bridge, path-of-6, triangle+chained-pairs) to `CommunitySpec.hs` asserting exact membership sets via `communityMembershipSets`.
- [x] 1.C Check: `cabal test` PASS (145 examples, 0 failures) — goldens verified against current implementation.
- [x] 1.A Act: Goldens locked. **Bug discovered while capturing**: `mergeSmallCommunities` LOSES NODES — on triangle{a,b,c}+pair{d,e} with raw comms {b},{c,a},{e,d}, node `b` vanishes because `mergeOne` inserts the stale member snapshot when a previously-merged-into community is itself merged. Node-preservation spec deferred to task 3 (would fail on current impl).

## 2. ST-based local moving pass (Domain)

- [x] 2.P Plan: Rewrite `localMovingPass` in `src/Graphos/Domain/Community.hs` to thaw the assignment once (`VU.thaw` in `runST`), mutate per move, freeze at end; same node order and ΔQ logic; `lsSigmaTot` updates unchanged. Check criteria: (a) golden specs from task 1 pass unchanged; (b) full suite green; (c) `cabal build -Werror` clean; (d) module remains IO-free (ST only, nothing escapes).
- [x] 2.D Do: Implemented `localMovingPass` + `localMovingLoop` (forall-scoped ST); scoring extracted to pure `bestCommunityFor`; removed `findBestCommunity`/`moveNode`. Safe `VU.thaw`, `unsafeFreeze` at end.
- [x] 2.C Check: `cabal test` PASS (145 examples incl. goldens unchanged). Build clean.
- [x] 2.A Act: ST pattern documented in haddocks.

## 3. Batched refinement + incremental merge index (Domain)

- [x] 3.P Plan: (a) `refineCommunitiesOpt`: accumulate reassignment decisions and apply via a single `VU.unsafeUpd` per pass (escalate to ST if goldens shift); (b) `mergeSmallCommunities`: build reverse index once, thread through the fold, update moved members per merge; `bestNeighborCommunity` takes the index as an argument. Check criteria: (a) golden + merge specs pass; (b) merge-target spec (vs freshly recomputed index) passes; (c) suite green, `cabal build -Werror` clean.
- [x] 3.D Do: Batched `unsafeUpd` per split community in refinement; `mergeSmallCommunities` rewritten: folds over small CIDs, looks up CURRENT members, threads incremental index, skips communities grown ≥ minSize; `bestNeighborCommunity` takes the index. Added node-preservation + growth-skip specs.
- [x] 3.C Check: `cabal test` PASS (147 examples). Goldens unchanged. Node-loss bug from task 1 fixed: triangle+pair graph now yields all 5 nodes in one community.
- [x] 3.A Act: Fixed latent stale-snapshot bug (nodes silently dropped when a merged-into community was itself merged) — documented in `mergeSmallCommunities` haddock.

## 4. Honest NFData for LeidenState (Domain)

- [x] 4.P Plan: Replace `rnf LeidenState{} = ()` with a field-forcing instance (vectors seq'd, boxed vector and IntMap forced structurally). Check criteria: (a) rnf smoke spec passes; (b) suite green; (c) `cabal build -Werror` clean.
- [x] 4.D Do: Field-forcing instance implemented; deepseq smoke spec added (deepseq added to test-suite build-depends).
- [x] 4.C Check: `cabal test` PASS (148 examples).
- [x] 4.A Act: Done.

## 5. Integration + performance verification

- [x] 5.P Plan: Traced pipeline run on this repo; synthetic scale measurement. Check criteria: (a) `span_cluster` ≤ 232 ms baseline on this repo; (b) synthetic sparse graph ≥ 50k nodes clusters in seconds (record actual number vs PRD §16.1 30 s @ 100k); (c) full `cabal test` + `cabal build -Werror` pass; (d) `scripts/audit_graph.py` passes on the run output.
- [x] 5.D Do: Traced pipeline run; compiled `-O2` benchmark harness (100k nodes / 120k edges, ring+chords) run against BOTH implementations via git stash.
- [x] 5.C Check:
  - (a) `span_cluster` on this repo: **140 ms** vs 232 ms baseline (1.65×, no regression) ✓
  - (b) 100k nodes compiled -O2: **OLD 169.2 s → NEW 10.5 s (16×)**; PRD §16.1 target (< 30 s @ 100k) now met ✓ (old violated it 5.6×)
  - (c) `cabal test` PASS (148 examples, 0 failures); build clean ✓
  - (d) `scripts/audit_graph.py` PASS ✓
  - Note: old impl produced 19,591 communities vs new 19,641 on the synthetic graph — expected drift from the merge node-loss bugfix; deterministic goldens unchanged.
- [x] 5.A Act: Ready to archive. Standard recorded: mutable-in-ST for hot per-element loops, safe thaw, honest NFData. Aggregation phase NOT needed to meet the PRD target — `leiden-aggregation-phase` follow-up deferred unless 1M-scale demands it.
