## Why

[AVI-512](/AVI/issues/AVI-512) rev-3 closed the requirements layer for element **C3 — local-to-global consistency** (`docs/math-requirements/AVI-537-local-to-global-consistency.md`, register LG-C1…LG-C5, all Proven; the LG-C3 dev loop closed in [AVI-647](/AVI/issues/AVI-647)). The OpenSpec capability `merge-confluence` **partially** covers C3 today: its M1/M3/M5 pin the colimit universal property, order-independence up to iso, and idempotence for consistent views, and M4 pins conflict handling — but no requirement states the *label-level* local-to-global agreement: shared keys carrying identical labels map identically under both cocone legs (LG-C2, the issue's primary SHALL), the merge square is a pullback on labels exactly on the agreement region (LG-C3), the pushout-but-not-pullback honesty boundary under conflict (LG-C4), and the naturality of the structural projection (LG-C5). Per the adoption instructions, this delta adds the missing requirements **and records the decision** of what M1 already subsumes, spec-first, no Haskell.

## What Changes

- **Delta on `merge-confluence`** (MODIFIED capability, additions only — no existing requirement M1–M5 is altered or weakened):
  - **Recorded decision (subsumption).** M1 + M3 + M5 already entail: the colimit universal property and uniqueness up to unique iso (LG-COLIM), order-independence for consistent views, and idempotence. The LG register adds label-level constraints that M1 does not pin: identical-label shared keys resolve to **identical** node/edge values (LG-C2 — ADD), the pullback-on-labels fiber-product identity on the agreement region (LG-C3 — ADD), the conflict honesty boundary (LG-C4 — ADD, consistent with M4's divergence set `Δ`), and naturality of `Struct` (LG-C5 — ADD as an explicit requirement; M1's "up to the NodeId+weight+community iso" phrasing does not state the structural projection is a natural iso on keys). LG-C1 (cocone legs are structure-preserving morphisms) partially overlaps M1's cocone-leg premise and is ADDED as the explicit per-leg morphism property.
  - **New requirement: cocone legs are structure-preserving graph morphisms (LG-C1).** Node injection, edge incidence `(src,tgt)`, `edgeRelation`, `edgeWeight`, `edgeConfidence` preserved per leg; `gDirected M = gDirected A || gDirected B` (canonical, order-independent — AVI-658 M1 T1).
  - **New requirement: shared nodes/edges map identically under both legs on the agreement region (LG-C2, SHALL).** On keys where both views carry identical `Node`/`Edge` payloads, `ι_A(k) = ι_B(k)` in `M` — independent of the conflict policy (old-wins and last-write-wins coincide on identical values).
  - **New requirement: pushout is a pullback on labels where views agree (LG-C3).** `Agree(A,B) ≅ A ×_M B` on identical-label keys; dedup identity `|keys(gNodes M)| = |keys(gNodes A) ∪ keys(gNodes B)|`.
  - **New requirement: honesty boundary — conflict over-identification (LG-C4).** Where views disagree on a shared key, the square remains a pushout but is **not** a pullback; the non-pullback is the recorded expected behavior (policy artifact), bounded by M4's `Δ`.
  - **New requirement: structural projection is natural under merge (LG-C5).** `Struct(M) = (⋃_i keys, ⋃_i edges, gDirected-or)` — the local-to-global structural agreement.
- All requirements classified **requirement-creation**; acceptance criteria are the AVI-537 property tests (existing test surface: `tests/Graphos/Domain/Graph/MergeConfluenceSpec.hs`, `ColimitPropertySpec.hs`, `ConflictSpec.hs` already witness parts of these properties).

## Capabilities

### New Capabilities
- none.

### Modified Capabilities
- `merge-confluence`: Adds the LG-C1…LG-C5 label-level local-to-global consistency requirements on the cocone legs and agreement region. M1–M5 are untouched; the additions strengthen (never weaken) the capability — in particular the CT-R10-related conflict statements of M4 keep their full divergence semantics, and LG-C4 *records* the over-identification boundary instead of claiming pullback-on-conflict.

## Impact

- **Specs**: `openspec/specs/merge-confluence/spec.md` gains five added requirement blocks after archive. Source: `docs/math-requirements/AVI-537-local-to-global-consistency.md` (§0 notation, §2 LG-C2 SHALL, §2.3 LG-PULLBACK, §3 register, §4 acceptance tests, §6 arbitration).
- **Domain (constrained surface, no edits in this change)**: `Graphos.Domain.Graph.Core` (`mergeGraphs:140` left-biased `Data.Map.union`, `mergeExtractions:129`, `buildGraph:96`), `Graphos.Domain.Types.Graph` (`Extraction` views, `extNodes/extEdges`), `Graphos.Domain.Types.Node/Edge` (labels), `Graphos.UseCase.Merge` (`mergeGraphsAndAnalyze:37`).
- **Haskell**: none in this change (spec-first). Behavior is already implemented and property-tested (`MergeConfluenceSpec` etc.); this delta pins the spec wording. Any additional property test for the fiber-product identity routes to `head-of-dev` after the gate.

## Feasibility gate

`head-of-dev` SHALL confirm the Haskell impact (expected: none — existing `mergeGraphs` already satisfies LG-C1…LG-C5 per AVI-647; optional extra property test only) via a comment on [AVI-1291](/AVI/issues/AVI-1291) before this change is archived.