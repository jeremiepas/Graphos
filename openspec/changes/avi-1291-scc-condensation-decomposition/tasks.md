## 1. Spec adoption (AVI-1291 element G5)

- [x] 1.1 Adopt the SCC/condensation register (`docs/math-requirements/AVI-518-shortest-path-scc.md` §1.3, Thm 2.2–2.3, INV-7/8/9/10; `AVI-523-cycle-detection-connectivity.md` INV-C1/C3/C5/C6, golden fixtures) as SHALL requirements in `specs/scc-decomposition/spec.md`, each with an explicit classification — `requirement-creation` where the surface exists (`stronglyConnectedComponents(WithCached)`, `shortestPathReachable`, the §1.4 disambiguation over `Domain.Graph.Analysis`, the O(N+E) guard) and `new-feature` where code is still missing (condensation, `isAcyclicDirected`, connectivity decompositions).
- [x] 1.2 Verify notation matches the consolidated glossary §0 plus AVI-518 §0 extensions (`E`, `adj⁺`, `⤳`, `↔`, `cond(G)`, `ℓ`, `C_c`/`C_w`); no symbol without glossary backing.
- [x] 1.3 Verify no contradiction with the consolidated layer: condensation-vs-colimit duality recorded (G4 reduces to G2 check), GT-W4 all-pairs scale guard preserved, `merge-confluence` M4/M5 untouched (INV-10 derives from Map semantics).
- [x] 1.4 Validate the change with `openspec validate avi-1291-scc-condensation-decomposition --strict`.

## 2. Handoff

- [ ] 2.1 `head-of-dev` feasibility gate: confirm Haskell impact (expected: implementation restricted to the missing surface — condensation build, `isAcyclicDirected`, `connectedComponents`, `weaklyConnectedComponents`, `isolatedNodes`, `hasCycleUndirected` + `*WithCached` variants, property + golden tests; `stronglyConnectedComponents(WithCached)` and `shortestPathReachable` already exist per AVI-530/PR #30) via a comment on [AVI-1291](/AVI/issues/AVI-1291) — required before archiving (AVI-518 §7 GAP-1 lists the surface).
- [ ] 2.2 After the gate: archive this change (syncs to `openspec/specs/scc-decomposition/spec.md`), then route the GAP-1 implementation to `head-of-dev` as a separate change. (Not in this spec-only issue.)