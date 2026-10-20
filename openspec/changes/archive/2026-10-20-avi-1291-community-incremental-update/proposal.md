## Why

[AVI-512](/AVI/issues/AVI-512) rev-3 closed the requirements layer for element **G6 — incremental community update** (`docs/math/incremental-edge-updates.md`, AVI-519: theorem register REQ-EQV/REQ-PARTITION/REQ-REMOVAL-EFFICIENT/REQ-INFER-BOUNDED/REQ-MODULARITY/REQ-DETERMINISTIC/REQ-CHECKPOINT), but **no community-level incrementality spec exists**. The existing capabilities `incremental-cache-eviction` and the cache requirements of `02-incremental-pipeline` cover the **extraction/embedding content-addressed cache — not G6**; they are explicitly not conflated here. Worse, the feasibility finding of AVI-519 §7 records that `runIncrementalPipeline` currently builds the changed-files' subgraph and never reconciles it with the retained global graph, so `Update(G, Δ) = G_{t+1}` is **not** satisfied (PROP-EQV). This change adopts the bounded-delta community-update register as SHALL requirements, spec-first and marked **new-feature**; implementation is separate `head-of-dev` work.

## What Changes

- **New capability `community-incremental-update`** adopting the AVI-519 register as SHALL requirements, each with checkable acceptance and a **new-feature** classification (no community-level incremental update exists; this spec mandates its behavior):
  - Incremental-equivalence boundary (PROP-EQV / REQ-EQV): `Update(G, Δ)` SHALL equal a full rebuild on `G_{t+1}` (Theorem 1 edge/node-set equality) — the central correctness claim, currently violated (§7 gap recorded honestly).
  - Bounded-delta community update (REQ-PARTITION + Lemma 1): after churn `Δ = (R, A)`, the partition SHALL remain valid, every added node SHALL be assigned, the churn scope is `N(Δ)` / `C(Δ)`, and untouched communities SHALL be preserved verbatim.
  - Affected-scope inference + fan-out caps (REQ-INFER-BOUNDED, INV-FANOUT/INV-DEDUP).
  - Sub-linear complexity (Theorem 3, REQ-REMOVAL-EFFICIENT): removal via reverse adjacency `O(deg)` — never a full `O(E)` edge scan; one-pass modularity delta.
  - Deterministic churn (REQ-DETERMINISTIC, INV-HASH-DET) with the four §6 decision points pinned, plus checkpoint-resume equivalence (REQ-CHECKPOINT, INV-CHECKPOINT).
- **Explicit non-conflation**: `incremental-cache-eviction` (extraction/embedding cache LRU) and the `02-incremental-pipeline` cache-key requirements are a different surface; no requirement of this capability touches cache policy.

## Capabilities

### New Capabilities
- `community-incremental-update`: Under node/edge churn `Δ = (R, A)` between pipeline runs, the graph and community partition are updated by a bounded delta — edge/node sets equal a full rebuild (Theorem 1), the partition remains valid with untouched communities preserved, only `C(Δ)` re-clusters, inference touches only affected scopes under named fan-out caps, and the same churn always produces the same `gHash`.

### Modified Capabilities
- none — `02-incremental-pipeline` ("Workflow 02 — incremental pipeline with --update", "Workflow 03 — watch mode", "checkpoint resume") names the workflow this capability constrains; its cache requirements are a distinct surface. `community-detection` (Leiden three phases, `cohesionScore`) and `bounded-edge-inference` (log-linear dedup, fan-out caps) are consumed unchanged. A behavioral delta on `02-incremental-pipeline` belongs to the implementation change that lands PROP-EQV, not to this requirements adoption.

## Impact

- **Specs**: new `openspec/specs/community-incremental-update/spec.md` (after archive). Sources: `docs/math/incremental-edge-updates.md` (§1 objects, §3 invariants, §4 theorems, §5 bounds, §7 feasibility finding, §8 REQ register), consolidated GT-R7/CT-R3 (GraphDiff pushout) for vocabulary.
- **Domain (constrained surface, no edits in this change)**: `Graphos.Domain.Graph.Core` (`Graph`, `buildGraph`, `mergeGraphs`, `deleteNode`, `deleteEdgesTouching`), `Graphos.Domain.Graph.Mutation` (`deleteEdgesTouching`, `rebuildAdjacency`, future `deleteNodeIncident`), `Graphos.Domain.Graph.Diff`/`Types.Graph` (`graphDiff`, `GraphDiff`), `Graphos.Domain.Community` (`detectCommunitiesWithResolution`, `cohesionScore`, `CommunityMap`), `Graphos.UseCase.Pipeline.Incremental` (`runIncrementalPipeline`/`runSingleFilePipeline`).
- **Haskell**: none in this change (spec-first). The AVI-519 §7 required fix (merge old+new via `mergeGraphs`, detach edges of removed nodes via `deleteNodeIncident`, re-cluster only `C(Δ)`) routes to `head-of-dev` after the feasibility gate.

## Feasibility gate

`head-of-dev` SHALL confirm the Haskell impact (expected: `Pipeline.Incremental` reconciliation rework + one `Domain.Graph.Mutation` helper + property tests) via a comment on [AVI-1291](/AVI/issues/AVI-1291) before this change is archived.