## Why

[AVI-512](/AVI/issues/AVI-512) rev-3 closed the requirements layer for all 12 plan elements. The joint element **J1 — colimit–modularity consistency** (`docs/math-requirements/AVI-529-colimit-modularity-consistency.md`, register UQI-1…UQI-8, proven claims P-1…P-6) binds the two readings of the merged graph — the structural colimit reading `π_struct` and the modularity reading `π_mod` → `G_s` — but **no OpenSpec capability binds them today**. `merge-confluence` (M1–M5) pins the colimit side and `community-detection`/`leiden-scalability` pin the Leiden side; nothing states that one single value `M` carries both readings, which projections read which fields, or where monotonicity breaks. This change adopts the UQI register as SHALL requirements, spec-first: no Haskell is written here; implementation is separate `head-of-dev` work after the spec lands.

## What Changes

- **New capability `colimit-modularity-consistency`** adopting the UQI register as SHALL requirements, one requirement per UQI cluster, each with a checkable acceptance (property test / invariant / boundary counterexample / complexity assertion):
  - UQI-1/UQI-2: single-object determinacy + projection independence (`π_struct` and `π_mod` read disjoint field-sets) — classification **requirement-creation**.
  - UQI-3: combinatorial grounding of `G_s` on the colimit (`|V(G_s)| = |gNodes M|`, `m = (Σ_v degree M v)/2`) — classification **requirement-creation**.
  - UQI-4: community-query freshness (detection runs on the colimit apex, never reuses view community ids) — classification **requirement-creation**; explicitly preserves the CT-R10 non-commutativity boundary.
  - UQI-5: monotonicity boundary (`Q_γ` non-decreasing within one detection on fixed `M`; no cross-merge monotonicity) — classification **requirement-creation**.
  - UQI-6: cohesion cross-reading consistency (`cohesion_M(c) = cohesion_viewUnion(c)`, `cohesion ∈ [0,1]`) — classification **requirement-creation**.
  - UQI-7: query cost model / scale guard (detection `O(maxIter·(E_s+N))`, neighbor/degree `O(deg(v))`, cohesion `O(E_s)`, memory `O(N+C+E_s)`) — classification **requirement-creation**.
  - UQI-8: order-independence on consistent diagrams, old-wins order-dependence at conflicts — classification **requirement-creation**.
- **No behavior change to existing code or specs**: `merge-confluence` M1–M5, `community-detection`, `leiden-scalability` are consumed as-is. The new capability *binds* them; it does not weaken them. In particular the CT-R10 non-commutativity boundary (merge × cluster not commutative under conflicts, `merge-confluence` CTO note 3) is preserved and made operational by UQI-4/UQI-5.

## Capabilities

### New Capabilities
- `colimit-modularity-consistency`: The merged context graph is one object read two ways — structurally the colimit of extraction views (`π_struct`) and combinatorially the support multigraph on which Leiden runs (`π_mod` → `G_s`); the two projections are disjoint-field pure functions, modularity queries recompute fresh on the apex, and every query cost is bounded by the Leiden budget.

### Modified Capabilities
- none — `merge-confluence` and `community-detection` are cross-referenced from the design, not modified.

## Impact

- **Specs**: new `openspec/specs/colimit-modularity-consistency/spec.md` (after archive). Grounding source: `docs/math-requirements/AVI-529-colimit-modularity-consistency.md` (register + P-1…P-6) and `docs/math-requirements/consolidated-requirements.md` (JOINT-1…JOINT-3, GT-R1…GT-R11, CT-R5/R7/R9/R10).
- **Domain (constrained surface, no edits in this change)**: `Graphos.Domain.Graph.Core` (`Graph` record, `mergeGraphs`), `Graphos.Domain.Graph.Query` (`neighbors`, `degree`), `Graphos.Domain.Community` (`detectCommunitiesWithResolution`, `computeCommunityStats`, `cohesionScore`, `scoreAllCohesion`, `buildLeidenState`, `localMovingPass`), `Graphos.UseCase.Merge` (`mergeGraphsAndAnalyze`).
- **Haskell**: none in this change (spec-first gate). Acceptance property tests for UQI-1…UQI-8 route to `head-of-dev` after the feasibility gate on the adoption issue.

## Feasibility gate

`head-of-dev` SHALL confirm the Haskell impact (expected: none for the spec adoption; property tests add `Tests/` coverage only) via a comment on [AVI-1291](/AVI/issues/AVI-1291) before this change is archived.