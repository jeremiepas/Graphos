## 1. Spec adoption (AVI-1291 element G6)

- [x] 1.1 Adopt the AVI-519 register (REQ-EQV/PARTITION/REMOVAL-EFFICIENT/INFER-BOUNDED/MODULARITY/DETERMINISTIC/CHECKPOINT) as SHALL requirements in `specs/community-incremental-update/spec.md`, each classified new-feature with a checkable acceptance.
- [x] 1.2 Record the AVI-519 §7 feasibility finding honestly inside the PROP-EQV requirement (current pipeline violates it; compliance comes from the implementation change).
- [x] 1.3 Verify non-conflation with `incremental-cache-eviction` / `02-incremental-pipeline` cache requirements (extraction/embedding cache ≠ G6) — stated in the delta Purpose; neither capability is modified.
- [x] 1.4 Verify notation (churn `Δ = (R,A)`, `k`, `N(Δ)`, `C(Δ)`, `E_Δ`) and no contradiction with the consolidated layer (CT-R3 GraphDiff pushout, CT-R7 pinned precedence, CT-R10 fresh re-detection; G4 reduces to G2 via `mergeGraphs`/`buildGraph` primitives).
- [x] 1.5 Validate the change with `openspec validate avi-1291-community-incremental-update --strict`.

## 2. Handoff

- [x] 2.1 `head-of-dev` feasibility gate: confirm Haskell impact (expected: `Pipeline.Incremental` reconciliation rework, `Domain.Graph.Mutation.deleteNodeIncident`, property tests) via a comment on [AVI-1291](/AVI/issues/AVI-1291) — required before archiving.
- [x] 2.2 After the gate: archive this change (syncs to `openspec/specs/community-incremental-update/spec.md`), then route the PROP-EQV implementation (AVI-519 §7 required fix) to `head-of-dev` as a separate change. (Not in this spec-only issue.)