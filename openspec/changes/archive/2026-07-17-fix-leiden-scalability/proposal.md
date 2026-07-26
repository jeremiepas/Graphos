# Proposal: fix-leiden-scalability

## Why

Community detection on a 117,161-node / 138,472-edge graph (982 code files) ran for minutes, violating the PRD §16.1 target of **Leiden @ 100k nodes < 30 seconds**. Code inspection of `Graphos.Domain.Community` (PRD §5 community detection) found three defects that make the implementation quadratic in node count:

1. **O(n) vector copy per node move.** `moveNode` updates the assignment via `VU.unsafeUpd assign [(i, newComm)]`, which copies the entire 117k-element unboxed vector to change one slot. `localMovingPass` visits every node; in early passes ~half of them move — ≈55 GB of memory traffic *per pass*, up to 50 passes, twice per pipeline run (initial cluster + re-cluster after inference). This alone explains the observed scaling: 232 ms at 4.3k nodes → minutes at 117k (cost grows ~n²).
2. **Per-community reverse-index rebuild.** `bestNeighborCommunity` calls `buildReverseIndex commMap` — O(n) — once for *every* small community merged. At fine extraction granularity with thousands of small communities this is another O(n·C) hotspot.
3. **No-op NFData instance.** `instance NFData LeidenState where rnf LeidenState{} = ()` forces nothing, so the `deepseq` in `leidenLoop` does not prevent thunk buildup across iterations.

The same pattern (`VU.unsafeUpd` inside a per-element fold) also appears in `refineCommunitiesOpt`.

## What Changes

- **Local moving pass mutates in place**: the assignment vector is thawed once per pass, mutated per move in `ST`, and frozen at the end — each move becomes O(1) instead of O(n). Clustering results are unchanged (same visit order, same ΔQ decisions).
- **Refinement batches its updates**: reassignments are accumulated and applied in a single `unsafeUpd` call (one copy per pass) instead of one copy per node.
- **Merge phase keeps an incremental reverse index**: built once, updated per merge for only the moved members, replacing the per-community O(n) rebuild.
- **Honest `NFData LeidenState`**: forces all fields so `deepseq` between iterations actually clears thunks.

Out of scope: the Leiden aggregation phase (true 3-phase Leiden — a larger algorithmic change, deferred), granularity of extraction (handled by `configurable-extraction-granularity`), Resolution parameter semantics.

## Capabilities

### New Capabilities
- `leiden-scalability`: community detection scales to 100k+ node graphs within PRD performance targets via in-place assignment updates, batched refinement, and incremental merge indexing (workflows: 01-full-pipeline, 02-incremental-pipeline, 09-merge).

### Modified Capabilities
<!-- none — clustering behavior (community assignments) is preserved; only complexity changes -->

## Impact

- **Code**: `src/Graphos/Domain/Community.hs` only (Domain layer, stays pure — `ST` is internal and escapes nothing).
- **Behavior**: identical community maps for identical inputs (deterministic same-order passes); wall-clock drops from quadratic to near-linear per pass.
- **No API/CLI/config changes**; no new dependencies (`vector` already provides `Data.Vector.Unboxed.Mutable`).
- **Tests**: existing `CommunitySpec` guards correctness; new specs assert result-equivalence on reference graphs and exercise the merge index.

## PDCA Cycle

- **Plan**: Hypothesis — the three defects above dominate clustering time. Success criteria (PRD §16.1): existing community tests pass unchanged; `span_cluster` on this repo does not regress; on a synthetic large graph (≥50k nodes) clustering completes in seconds, not minutes; identical `CommunityMap` output before/after on the test suite's reference graphs.
- **Do**: Implement the four fixes (see design.md, tasks.md).
- **Check**: `cabal test` (correctness + new specs); traced pipeline run on this repo comparing `span_cluster` against the 232 ms baseline; timing measurement on a large synthetic graph.
- **Act**: If targets are met, archive and record the mutation pattern as the standard for hot loops; if 100k-scale is still above 30 s, open the follow-up change for the aggregation phase.
