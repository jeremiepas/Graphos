## 1. Spec adoption (AVI-1291 element J1)

- [x] 1.1 Adopt register UQI-1…UQI-8 of `docs/math-requirements/AVI-529-colimit-modularity-consistency.md` as SHALL requirements in `specs/colimit-modularity-consistency/spec.md`, each with a checkable acceptance (property/invariant/boundary/complexity) and a classification + surface anchor.
- [x] 1.2 Verify notation is verbatim from `docs/math-requirements/consolidated-requirements.md` §0 (`π_struct`, `π_mod`, `G_s`, `m`, `Q_γ`, `cohesion`); no symbol introduced without a glossary backing.
- [x] 1.3 Verify no weakening of the CT-R10 non-commutativity boundary: UQI-4 (fresh re-detection), UQI-5 (no cross-merge monotonicity), UQI-8 (divergence bounded by `merge-confluence` `Δ`); `merge-confluence` M1–M5 untouched.
- [x] 1.4 Validate the change with `openspec validate avi-1291-uqi-colimit-modularity-consistency --strict`.

## 2. Handoff

- [ ] 2.1 `head-of-dev` feasibility gate: confirm Haskell impact (expected: none for the spec adoption; property tests only) via a comment on [AVI-1291](/AVI/issues/AVI-1291) — required before archiving.
- [ ] 2.2 After the gate: archive this change (syncs to `openspec/specs/colimit-modularity-consistency/spec.md`), then route property-test implementation (P-1…P-6 acceptance) to `head-of-dev` as a separate change. (Not in this spec-only issue.)