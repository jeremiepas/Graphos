## 1. C3 delta adoption (AVI-1291)

- [x] 1.1 Add the LG-C1…LG-C5 delta requirements to `merge-confluence` in `specs/merge-confluence/spec.md` (ADDED-only; M1–M5 untouched), each classified requirement-creation with a checkable acceptance.
- [x] 1.2 Record the subsumption decision (M1 subsumes LG-COLIM; per-row split table) in `design.md` §2, including the M4-divergence alignment for LG-C4 and the policy-agnostic scoping of LG-C2 to identical payloads.
- [x] 1.3 Verify G4-reduces-to-G2: every LG row derives from the AVI-511 colimit register + `Data.Map.union` semantics (AVI-647); no contradiction with `consolidated-requirements.md` (CT-R8/R9) and no community/CT-R10 claim introduced.
- [x] 1.4 Validate the change with `openspec validate avi-1291-c3-local-global-delta --strict`.

## 2. Handoff

- [ ] 2.1 `head-of-dev` feasibility gate: confirm Haskell impact (expected: none — behavior already implemented per AVI-647; optional LG-C3 fiber-product property test only) via a comment on [AVI-1291](/AVI/issues/AVI-1291) — required before archiving.
- [ ] 2.2 After the gate: archive this change (syncs the delta into `openspec/specs/merge-confluence/spec.md`) and record the subsumption decision in the adoption-issue summary comment. (Not in this spec-only issue.)