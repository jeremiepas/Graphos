# Proposal: avi-578-weighted-modularity-cohesion

## Why

AVI-578 (implementing AVI-535 "G4" — confidence-weighted modularity & cohesion)
shipped in `ea699e1` and `7d053c8` but **without an openspec change directory**,
which the [AVI-847](/AVI/issues/AVI-847) openspec-verifier correctly flagged as a
verification blocker: no `tasks.md` to confirm, no delta specs to cross-check,
no archiveable change record.

This directory is the **backfilled change record** for the shipped
implementation. It documents the behavior that already exists on
`AVI-512-add-math-in-graphos-project` and is verified by the test suite — it does
not propose new code. Grounding doc:
`docs/math-requirements/AVI-535-weighted-modularity-cohesion.md`.

Naming note: the change id carries the parent issue id (AVI-578); the
grounding math doc and the R&D deliverable use AVI-535 ("G4" of the
AVI-533..536 family). They refer to the same deliverable.

## What Changes (shipped behavior being recorded)

- **Confidence-weighted aggregates (O(N + E))** in `Graphos.Domain.Community`:
  `buildWeightedAdj` (one-pass weighted adjacency: per-source/per-target
  neighbour lists + weighted degree), consumed by `internalWeight`,
  `communityQuality`, `cohesionWeighted`, `weightedModularity`,
  `candidateScore`, and `computeCommunityStatsWeighted` so a whole partition
  costs O(N + E) instead of an O(E) rescan of `gEdges` per community/member.
- **`weightedModularity :: Graph -> CommunityMap -> Double`** — auditable
  weighted modularity Q_w at resolution γ = 1 over the configuration null model
  `P_uv = d_u·d_v/(2m_w)` (m_w = total confidence mass): `Σ_c [σ_in^w/(2m_w) −
  σ_tot^w²/(2m_w)²]`. `m_w = 0` (or empty partition) ⇒ 0. Reduces to the
  AVI-533 auditable `modularity` under any uniform confidence (INV-CONSTR,
  scoped to simple graphs per AV-ARB-1).
- **`cohesionWeighted :: Graph -> [NodeId] -> Double`** — weighted cohesion
  `C(S) = (1/|S|)·Σ_i in_w(i)/d_w(i)`; `deg_w(i) = 0 ⇒ 0`, singleton/empty ⇒ 0,
  folded in sorted member-id order so the value is order-independent.
- **Determinism + O(N + E) remediation (from `7d053c8`)**: one shared
  `buildWeightedAdj` view per entry point; `cohesionWeighted` folded in sorted
  member-id order (reviewer AVI-805 determinism finding).
- **Null-model export**: `NullModel` ADT (`DefaultNullModel` /
  `DirectedNullModel`, JSON tags `degree_config_undirected` /
  `degree_config_directed`) in `Graphos.Domain.Types.Graph` with `ToJSON` +
  `FromJSON`; serialized into graph.json as `null_model` by
  `Graphos.Infrastructure.Export.JSON` via `analysisNullModel` (default
  `DefaultNullModel` in `Graphos.Domain.Analysis.analyze`).
- **Characterization tests**: `tests/Graphos/Domain/CommunitySpec.hs` —
  INV-CONSTR reduction for `w₀ ∈ {0.25, 1, 4}`, bidirectional-pair
  Q_w well-defined/order-independent (AVI-822 scenario-9 analogue), INV-Qw0/Qw1/
  Qw2/Qw3/Qw4, INV-C1/C2, cohesion order-independence, exact all-singletons
  value, signal-strength sensitivity, `null_model` key present in export
  (`tests/Graphos/UseCase/ExportSpec.hs`).

## Capabilities

### New Capabilities
- `weighted-community-metrics`: auditable confidence-weighted modularity Q_w and
  weighted cohesion, their null model, complexity bound, determinism, and
  null-model export.

### Modified Capabilities
- `graph-json-contract`: adds the `null_model` top-level key to the writer key
  set (additive export field recording which configuration null model was used).

## Out of Scope
- Reweighting Leiden's internal search (AF-2: auditable measure only; INV-Qw5
  optional future work).
- Legacy `modularity` multigraph convention fix (AVI-533 defect, tracked
  separately per AV-ARB-1).
- `NullModel` loader wiring beyond the export key (the field defaults at
  `analyze`; loader-side symmetric read is part of the normal graph-json
  round-trip contract, not this change).