# Tasks: avi-578-weighted-modularity-cohesion

<!-- Backfilled change record for AVI-578 (AVI-535 G4). The implementation and
     tests shipped in commits ea699e1 (implement confidence-weighted modularity
     and cohesion) and 7d053c8 (O(N+E) weighted aggregates + cohesion
     determinism + characterization test) on
     `AVI-512-add-math-in-graphos-project`, before this change directory was
     created during AVI-847 verification remediation. Every task below is
     checked because the work exists in the tree and is covered by the shipped
     test suite. -->

## 1. Confidence-weighted aggregates (Domain)

- [x] 1.P Plan: Define the confidence-weighted surface (weighted degree, weighted
  edge mass, internal weight, weighted aggregates) per AVI-535 §1.1–§1.3,
  weighting by `edgeConfidence` (AF-1). Check criteria: (a) reduces to the
  unweighted aggregates under uniform confidence; (b) complexity O(N + E) per
  partition; (c) `cabal build -Werror` + suite green.
- [x] 1.D Do: `nodeWeightedDegrees`, `weightedEdgeMass`, `buildWeightedAdj`
  (one-pass per-source/per-target adjacency + weighted degree),
  `internalWeight` in `src/Graphos/Domain/Community.hs` (ea699e1, 7d053c8).
- [x] 1.C Check: `cabal test` PASS — reduction + range scenarios in
  `tests/Graphos/Domain/CommunitySpec.hs`.
- [x] 1.A Act: Requirements recorded in `specs/weighted-community-metrics/spec.md`.

## 2. weightedModularity (γ=1, configuration null model)

- [x] 2.P Plan: Implement auditable `weightedModularity :: Graph ->
  CommunityMap -> Double` per AVI-535 (*) with `P_uv = d_u·d_v/(2m_w)`; `m_w = 0`
  ⇒ 0; reduce to AVI-533 `modularity` under uniform confidence (INV-CONSTR,
  simple-graph scope per AV-ARB-1). Check criteria: (a) INV-Qw0/Qw1/Qw2/Qw3/Qw4
  scenarios; (b) reduction for `w₀ ∈ {0.25, 1, 4}`; (c) suite green.
- [x] 2.D Do: `communityQuality` + `weightedModularity` implemented with fixed
  `Map.toList` fold order; exact all-singletons value pinned (`−6/16` path
  fixture); bidirectional-pair characterization test (order-independent, well
  defined) per the AV-ARB-1 scope.
- [x] 2.C Check: `cabal test` PASS — INV-CONSTR (incl. bidirectional pair),
  INV-Qw0/Qw1/Qw4 exact value, determinism/order-independence scenarios.
- [x] 2.A Act: Requirement + scenarios recorded in
  `specs/weighted-community-metrics/spec.md`.

## 3. cohesionWeighted

- [x] 3.P Plan: Implement `cohesionWeighted :: Graph -> [NodeId] -> Double`
  (`C(S) = (1/|S|)·Σ in_w(i)/d_w(i)`) with `deg_w = 0 ⇒ 0`, singleton/empty ⇒ 0
  (AF-3-b: 0-by-formula), folded in sorted member-id order (determinism,
  AVI-805 remediation). Check criteria: (a) INV-C1/C2/C3/C4/C5; (b) reduction to
  spec cohesion under uniform confidence; (c) suite green.
- [x] 3.D Do: `cohesionWeighted` implemented (ea699e1; sorted-order fold +
  order-independence test added in 7d053c8).
- [x] 3.C Check: `cabal test` PASS — singleton 0, disconnected 0, INV-C2
  reduction (`8/9` fixture), order-independence scenario.
- [x] 3.A Act: Requirement + scenarios recorded in
  `specs/weighted-community-metrics/spec.md`.

## 4. O(N + E) weighted aggregates (reviewer remediation, AVI-805)

- [x] 4.P Plan: Precompute weighted adjacency once (`buildWeightedAdj`) and share
  it across `internalWeight`, `cohesionWeighted`, `communityQuality`,
  `weightedModularity`, `candidateScore`, `computeCommunityStatsWeighted` so a
  partition is O(N + E) (INV-CONSTR MINOR + determinism MAJOR findings). Check
  criteria: (a) no per-community `gEdges` rescan; (b) suite green.
- [x] 4.D Do: Shared `WeightedAdj` view wired through the whole family
  (`Community.hs`, 7d053c8).
- [x] 4.C Check: `cabal test` PASS — existing scenarios re-verified on the
  refactored implementation.
- [x] 4.A Act: Complexity requirement recorded in
  `specs/weighted-community-metrics/spec.md`.

## 5. Null-model export (`shall-null-model`, AF-5)

- [x] 5.P Plan: Record the chosen null model (`P_uv = d_u·d_v/(2m_w)`) in graph
  metadata + export: `NullModel` ADT in `Types/Graph.hs`, `analysisNullModel`
  field defaulting to `DefaultNullModel` in `Analysis.analyze`, serialized as
  `null_model` in graph.json. Check criteria: (a) `null_model` key present in
  export; (b) ToJSON/FromJSON round-trip; (c) suite green.
- [x] 5.D Do: `NullModel(..)` + instances (ea699e1), `FromJSON` added in
  7d053c8, `null_model` key in `Infrastructure/Export/JSON.hs`.
- [x] 5.C Check: `cabal test` PASS — `null_model` presence in
  `tests/Graphos/UseCase/ExportSpec.hs`.
- [x] 5.A Act: `null_model` requirement recorded in
  `specs/graph-json-contract/spec.md` delta.

## 6. Verification & reconciliation

- [x] 6.P Plan: Verify the shipped implementation against the grounding doc and
  backfill the change record so AVI-847 verification can complete. Check
  criteria: (a) `openspec validate avi-578-weighted-modularity-cohesion` passes;
  (b) every shipped behavior mapped to a delta requirement/scenario; (c) no
  code change beyond this change dir in the backfill commit.
- [x] 6.D Do: Change dir created under `openspec/changes/` with proposal, delta
  specs, and this tasks.md; archive performed by openspec-verifier
  (`--skip-specs` not needed — canonical spec sync via archive).
- [x] 6.C Check: `openspec validate` green; all tasks above checked; the delta
  requirements cross-checked against `weightedModularity`/`cohesionWeighted`/
  `NullModel` in the shipped `Community.hs`/`Graph.hs`/`JSON.hs`.
- [x] 6.A Act: Archived change record + synced canonical specs close the
  AVI-847 checklist #1–#4.