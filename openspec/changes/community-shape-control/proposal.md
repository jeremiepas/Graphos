## Why

Graphos' clustering defaults are tuned for small graphs (`gamma=1.0, minSize=3, maxIter=50`) but must handle 500k+ node graphs cold. At that scale the pipeline runs Leiden twice from scratch, produces tens of thousands of fragment communities, and churns thousands of small-community merges — making generation slow, ingest (LLM labeling: 10k communities / batch 20 = 500+ calls) expensive, and query/context selection painful. The `community-detection` spec's size-based auto-tune table (PRD §5.2, §5.3) was written but never wired into code. The same stage also fails at the opposite extreme: a mega-community swallowing 24–33% of the graph (the `cap-oversized-communities` problem). Both extremes are one missing capability: deterministic control of community *shape* after clustering.

## What Changes

- **Wire the spec'd auto-tune table** (PRD §5.2, §5.3) into resolution selection so 500k+ graphs get large-graph parameters (`gamma≈0.4, minSize=15, maxIter≈12`) by default, with the 100k+ row pinned to concrete values.
- **Add a community-shape post-pass** after Leiden clustering (and after the re-cluster): first split any community exceeding a cap fraction of total nodes (default 5%) via recursive Leiden on the induced subgraph with connected-component and degree-chunk fallbacks; then merge smallest-first into best-neighbor communities until the count reaches a target `K(N)` (e.g. `K = N/1000` at 100k+), never merging into a community at or over the cap. Absorbs and supersedes the parked `cap-oversized-communities` change (its centrality-exclusion item stays with the detection change, per its own design note).
- **Performance enablers on the same stage**: seed the Step 5 re-cluster from the Step 4 partition via the existing `buildLeidenStateSeeded`; seed ingest/incremental re-clustering from the on-disk partition; drop the forbidden `lsNeighbors` vector-of-vectors duplicate of the CSR; remove remaining `fromListWith (++)` member-list accumulations.
- **Lean 4 model of the post-pass** (`lean/` in this change), following the `lean-proof-methodology` rules (Lean 4.29.1 core only, goal-guided `rw`, no `decide` over un-kernel-reducible functions, zero errors/warnings, `VERIFICATION.md`): partition soundness, cap invariant, count bound under feasible parameters, best-effort count disjunct, termination, and determinism of the canonical tie-breaking.

## Capabilities

### New Capabilities
- `community-shape-control`: the split-cap + merge-to-target post-pass, its parameter feasibility rules (auto-tuned parameters must be simultaneously satisfiable: `K·minSize ≤ N`, `N/K ≤ cap·N`), the count/cap invariants it guarantees, and the Lean model proving them. Supersedes the `community-size-cap` capability proposed by the parked `cap-oversized-communities` change.

### Modified Capabilities
- `community-detection`: the auto-tune requirement (PRD §5.2, §5.3) moves from unwired aspiration to mandatory behavior with pinned 100k+ values, and gains the community-count target parameter `K(N)` alongside `resGamma`/`resMinSize`/`resMaxIterations`.

## Impact

- **Domain/Community**: new pure shape post-pass (split + merge) operating on cluster output; seeded clustering entry points already exist. Auto-tune is a pure function of graph size.
- **Domain/Config + Config/Ingest + CLI**: new config keys and flags (cap fraction, count target, disable switches); defaults must make a 500k+ graph work with zero manual tuning.
- **UseCase/Cluster + Pipeline/Core + Pipeline/Incremental**: wire the post-pass after both clustering steps and after merge/incremental re-cluster; seed re-clusters from existing partitions. Query/selection code is untouched — count reduction is expected to relieve query pain.
- **Supersession**: `cap-oversized-communities` (planning complete, 0/14 tasks) is marked superseded by this change; its tasks are absorbed here.
- **Formal**: new `lean/` verification artifact mirroring the Domain post-pass, CI-checkable via `lake build` with no Mathlib, per the `lean-proof-methodology` change's rules.
- **Observability**: emit community count, cap splits, and merges as metrics (existing `graphos_communities` gauge) plus a WARNING log when a pre-split community exceeds the cap.