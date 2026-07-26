# Design: fix-leiden-scalability

## Context

`Graphos.Domain.Community` (Domain layer, pure) implements Leiden local moving + refinement + small-community merging over immutable structures:

| Hotspot | Location | Complexity today | Complexity target |
|---|---|---|---|
| Assignment update per move | `moveNode` (`VU.unsafeUpd`, full copy) | O(n) per move → O(n²) per pass | O(1) per move |
| Refinement reassignment | `refineCommunitiesOpt` (fold of single-element `unsafeUpd`) | O(n) per reassigned node | O(n) per pass total |
| Merge target lookup | `bestNeighborCommunity` → `buildReverseIndex` per small community | O(n) per small community | O(n) once + O(k) per merge |
| Iteration forcing | `NFData LeidenState` is `rnf _ = ()` | thunks accumulate | full force |

Observed: 232 ms at 4.3k nodes; minutes at 117k nodes — consistent with quadratic cost. PRD §16.1 requires < 30 s at 100k nodes.

## Goals / Non-Goals

**Goals:**
- Each node move costs O(1); each local-moving pass costs O(n + e).
- Merge phase costs O(n) index build + O(moved members) per merge.
- Identical `CommunityMap` output for identical inputs (pure refactor, no algorithmic change).
- `Domain` stays IO-free; mutation confined to `runST` with no escaping references.

**Non-Goals:**
- Aggregation phase (true 3-phase Leiden) — separate, larger change.
- Parallel clustering, resolution semantics, or merge-strategy changes.
- Changes outside `Domain/Community.hs`.

## Decisions

### D1 — In-place assignment via `ST` inside the local moving pass (Domain)

`localMovingPass` thaws `lsAssignment` into an `MVector` once (`runST`), performs all reads/writes on the mutable vector in the same node order as today, and freezes once at the end. `lsSigmaTot` (IntMap) continues to update functionally per move — its per-move cost is O(log C), not the bottleneck.

- **Alternatives considered:**
  - *Keep immutable vector, batch updates per pass* — rejected for local moving: `findBestCommunity` must observe moves made earlier in the same pass (Leiden semantics); batching would change results.
  - *`IORef`/IO-based mutation* — rejected: violates the Domain-has-zero-IO rule; `runST` keeps the function observably pure.
  - *IntMap assignment instead of vector* — rejected: O(log n) per read on the hottest read path, and loses unboxed density.

### D2 — Batched reassignment in refinement (Domain)

`refineCommunitiesOpt` decides reassignments per community against the pass-start assignment snapshot; updates are accumulated as a list and applied with a single `VU.unsafeUpd` (one copy per pass). Reads during the decision phase use the snapshot — matching the current code's semantics where `cohesionToCommunityIdx` is evaluated against the accumulator as it evolves per community; the batch preserves per-community decision order by folding communities in the same `IntMap` order and threading only the *decisions*, not per-node copies.

- **Alternatives considered:** full `ST` here as well — acceptable, but a single batched `unsafeUpd` is simpler and refinement touches far fewer nodes than local moving; chosen for lower diff risk. If result-equivalence tests reveal order sensitivity, escalate to the D1 pattern.

### D3 — Incremental reverse index in the merge phase (Domain)

`mergeSmallCommunities` builds `NodeId → CommunityId` once, passes it through the fold, and each `mergeOne` updates entries only for the merged community's members. `bestNeighborCommunity` receives the current index instead of rebuilding it; stale-target lookups (neighbor pointing at an already-merged community) resolve through the index update, fixing a latent staleness bug in addition to the cost.

- **Alternatives considered:** leave rebuild but memoize per iteration — rejected: still O(n) whenever any merge happens, and the staleness bug remains.

### D4 — Honest `NFData LeidenState` (Domain)

`rnf` forces every field: unboxed vectors are NF at WHNF (cheap seq), the boxed vector-of-vectors and `IntMap` are forced structurally. `leidenLoop`'s existing `deepseq` then genuinely clears thunks between iterations.

- **Alternatives considered:** derive via `Generic` — the record contains `V.Vector (VU.Vector Int)` needing its instances anyway; a hand-written instance is 5 lines and explicit. Chosen hand-written.

## Risks / Trade-offs

- [Result drift from refactor] → Result-equivalence specs: reference graphs from `CommunitySpec` must produce identical `CommunityMap`s before/after (golden assertions on the existing deterministic cases).
- [`unsafeThaw`/aliasing bugs] → Use safe `VU.thaw` (copies once per pass — still O(n) per pass, amortized fine) rather than `unsafeThaw`, eliminating aliasing risk for a negligible cost.
- [D2 order sensitivity changes refinement output] → covered by equivalence specs; fallback documented in D2 (escalate to ST).
- [Merge-index staleness fix changes merge targets on graphs that hit the latent bug] → acceptable: new behavior is the *correct* one; note in Check if any existing test output shifts.

## Verification Strategy (Check)

- **Unit (Hspec, `cabal test`):**
  - All existing `Graphos.Domain.CommunitySpec` cases pass unchanged (correctness + determinism guards, incl. `resMaxIterations` cases).
  - Result-equivalence: deterministic reference graphs (path, star, two-cliques-with-bridge) produce the same `CommunityMap` as golden values captured from the current implementation.
  - Merge index: a graph with several sub-min-size communities merges to the same targets as a naive recomputed index.
  - `NFData`: `rnf` on a constructed `LeidenState` completes (smoke) — guards against partial instances.
- **Performance (integration):**
  - Traced pipeline on this repo: `span_cluster` ≤ current 232 ms baseline (no regression).
  - Synthetic scale test (script or temporary bench, not CI-gating): random sparse graph ≥ 50k nodes / ~1.2 edges-per-node clusters in seconds; record numbers in Check notes against the PRD §16.1 30 s @ 100k target.
- **Build gate:** `cabal build` clean with dev `-Wall -Werror` flags.

## Iteration & Rollback (Act)

- **If 100k-scale remains > 30 s:** the remaining cost is pass count × O(n+e) without aggregation — open the follow-up `leiden-aggregation-phase` change; this change still stands on its own (removes the quadratic floor).
- **Rollback:** single-module revert; no data or schema impact.
- **Standardize:** record "mutable-in-ST for hot per-element update loops; safe thaw; honest NFData" as the Domain performance convention (compilation-optimisation context store).
