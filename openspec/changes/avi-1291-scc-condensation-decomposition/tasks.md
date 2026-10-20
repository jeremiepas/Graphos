## 1. Spec adoption (AVI-1291 element G5)

- [x] 1.1 Adopt the SCC/condensation register (`docs/math-requirements/AVI-518-shortest-path-scc.md` §1.3, Thm 2.2–2.3, INV-7/8/9/10; `AVI-523-cycle-detection-connectivity.md` INV-C1/C3/C5/C6, golden fixtures) as SHALL requirements in `specs/scc-decomposition/spec.md`, each classified new-feature.
- [x] 1.2 Verify notation matches the consolidated glossary §0 plus AVI-518 §0 extensions (`E`, `adj⁺`, `⤳`, `↔`, `cond(G)`, `ℓ`, `C_c`/`C_w`); no symbol without glossary backing.
- [x] 1.3 Verify no contradiction with the consolidated layer: condensation-vs-colimit duality recorded (G4 reduces to G2 check), GT-W4 all-pairs scale guard preserved, `merge-confluence` M4/M5 untouched (INV-10 derives from Map semantics).
- [x] 1.4 Validate the change with `openspec validate avi-1291-scc-condensation-decomposition --strict`.

## 2. Handoff

- [ ] 2.1 `head-of-dev` feasibility gate: confirm Haskell impact (expected: pure `Domain.Graph` additions `stronglyConnectedComponents`, `connectedComponents`, `weaklyConnectedComponents`, `isolatedNodes`, `isAcyclicDirected`, `shortestPathReachable` + `*WithCached` variants, property + golden tests) via a comment on [AVI-1291](/AVI/issues/AVI-1291) — required before archiving (AVI-518 §7 GAP-1 lists the exact surface).
- [ ] 2.2 After the gate: archive this change (syncs to `openspec/specs/scc-decomposition/spec.md`), then route the GAP-1 implementation to `head-of-dev` as a separate change. (Not in this spec-only issue.)