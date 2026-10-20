# Design: C3 local-to-global consistency delta (AVI-1291, element C3)

Adopts `docs/math-requirements/AVI-537-local-to-global-consistency.md` (LG-C1…LG-C5) as a delta on `merge-confluence`, with the recorded subsumption decision required by the adoption instructions.

## 1. Math approach (one notation, AVI-511/consolidated §0 glossary)

**Cocone over views.** Views are `Extraction` values (`extNodes/extEdges`); `F_d(V) = buildGraph d V`. The apex is `M = mergeGraphs A B` (`Core.hs:140`: `gNodes old <> gNodes new` — left-biased, first operand wins; edges keyed `(NodeId,NodeId)` unioned then dangling-filtered; adjacency `Map.unionWith Set.union`; `gDirected old || gDirected new`). Cocone legs are the canonical key-wise inclusions `ι_A, ι_B` on `gNodes`/`gEdges`/`gAdjFwd`/`gAdjBack`.

**Agreement region.** For views `A, B` sharing target shape:
```
Agree(A,B) = { k | k ∈ keys(extNodes A) ∩ keys(extNodes B) ∧ nodeValue_A(k) = nodeValue_B(k) }
           ∪ { (s,t,r) | shared edge key ∧ edgeValue_A = edgeValue_B }
```
— precisely the domain on which a value-preserving embedding into both views exists.

**The pullback claim (LG-C3).** On `Agree(A,B)` the merge square is a pushout **and** a pullback restricted to labels:
```
Agree(A,B) ≅ A ×_M B = { (a,b) ∈ keys(A) × keys(B) : ι_A(a) = ι_B(b) }   (identical-label keys)
```
Forward: agreement ⇒ both legs map `k` to the same merged node (LG-C2). Backward: `ι_A(a) = ι_B(b)` forces `a = b = k` with identical label (legs are value-preserving inclusions; left-biased union introduces no hidden cross-key collision — AVI-647). Pins: no identification beyond shared identical labels; no duplicate copy of an agreed label.

**Honesty boundary (LG-C4).** Where a shared key carries differing labels, no value-preserving embedding exists: the square is still a pushout but **not** a pullback (the fiber product over-identifies the conflicting pair). This is exactly the conflict over-identification already quantified by `merge-confluence` M4's divergence set `Δ = Δ_nodes ∪ Δ_edges`; LG-C4 records it as the expected non-pullback, not a correctness violation.

**Naturality (LG-C5).** `Struct : Graph → (NodeMap, EdgeMap, Bool)`, `Struct g = (gNodes g, gEdges g, gDirected g)`. The structural square commutes as an iso on keys: `keys(Struct M) = ⋃_i keys(Struct(D(V_i)))` and `gDirected M = gDirected A || gDirected B` (canonical, order-independent — AVI-658 M1 T1).

## 2. Recorded decision — what M1 subsumes vs what this delta adds

The adoption instructions allow either a delta requirement or a recorded decision that M1 subsumes the register. The verified split (checked against `openspec/specs/merge-confluence/spec.md` wording):

| LG row | M1/M3/M4/M5 already pin it? | Action |
|---|---|---|
| LG-COLIM (terminal cocone; uniqueness up to unique iso) | Yes — M1's universal property + "unique up to the NodeId+weight+community iso" | **Subsumed — no delta text** (decision recorded here) |
| LG-C1 (legs are structure-preserving morphisms) | Partially — M1 names cocone legs but does not state the per-leg incidence/payload/directed-flag preservation | **ADD requirement** (explicit per-leg property) |
| LG-C2 (shared identical-label keys map identically under both legs — the SHALL) | No | **ADD requirement** |
| LG-C3 (pullback on labels on the agreement region + dedup identity) | No (M5 idempotence ≠ fiber-product identity) | **ADD requirement** |
| LG-C4 (conflict ⇒ pushout, not pullback; over-identification) | Partially — M4 quantifies divergence `Δ` but does not state the non-pullback boundary | **ADD requirement** (aligned with M4's `Δ`, no policy change) |
| LG-C5 (`Struct` natural iso on keys + canonical `gDirected`) | Partially — M3 pins canonical form/hash; the naturality statement is absent | **ADD requirement** |

Nothing in this delta contradicts or weakens M1–M5: LG-C2 is scoped to the agreement region of identical payloads, where any deterministic winner selection returns the same value (policy-agnostic by identical-payload scoping — the feasibility-gate wording note, AVI-1310 2026-10-20), and LG-C4 restates the conflict boundary in pullback language without touching M4's divergence semantics.

## 3. Requirements register → spec mapping (classification: requirement-creation throughout)

| Req | Acceptance (AVI-537 §4) | Scenarios |
|---|---|---|
| LG-C1 | property: per-leg payload/adjacency preservation on random consistent graphs | node/edge payload preserved; adjacency superset; directed flag or-semantics |
| LG-C2 | property: `Map.lookup k (gNodes M) == Map.lookup k (gNodes A) == Map.lookup k (gNodes B)` for agreeing `k` | agreeing shared node + edge |
| LG-C3 | property: fiber-product identity + `|keys(gNodes M)| = |A ∪ B|` (no duplicate) | fiber-product identity |
| LG-C4 | counterexample: conflicting shared key recorded as expected non-pullback | conflict over-identification |
| LG-C5 | property: `Struct(M)` square commutes, `gDirected M = gDirected A ∥ gDirected B` | structural projection naturality |

## 4. Surface grounding

- **Capabilities**: `merge-confluence` (modified — additions only). Consumed unchanged: `09-merge` (Workflow 09 union semantics), `lsp-extraction` (view level where `EdgeId` keeps relation-differing edges distinct — the LG-C3 edge clause holds at the Extraction/`EdgeId` level; the Graph-level `(source,target)` key collapse of relation-differing edges is pre-existing documented design orthogonal to this delta, AVI-537 §6 AQ-LG2), `domain-types` (`Node`/`Edge` labels).
- **Modules constrained**: `src/Graphos/Domain/Graph/Core.hs` (`mergeGraphs:140`, `mergeExtractions:129`, `buildGraph:96`), `src/Graphos/Domain/Types/Graph.hs` (`Extraction:47`, `extNodes/extEdges`), `src/Graphos/Domain/Types/Node.hs` + `Edge.hs` (label records), `src/Graphos/UseCase/Merge.hs` (`mergeGraphsAndAnalyze:37`).
- **Test surface**: `tests/Graphos/Domain/Graph/MergeConfluenceSpec.hs` (M1/M4 properties), `ColimitPropertySpec.hs`, `ConflictSpec.hs`, `MergeIdempotencySpec.hs` already witness LG-C1/C2/C5-class properties on the current implementation.

## 5. Consistency with the consolidated layer

- **G4 reduces to G2 (explicit check):** every LG row is derived from the G2/AVI-511 colimit register (`CT-R5/R7/R8/R9`) plus the `containers` semantics (`Data.Map.union` left-bias) confirmed in AVI-647. No new math; no contradiction with `consolidated-requirements.md` §1 (CT-R9 structural naturality ≡ LG-C5/`merge-confluence` M3; CT-R8 dedup ≡ LG-C3 dedup identity).
- **CT-R10 untouched:** this delta constrains structure/labels only; community re-detection freshness is J1's concern (`colimit-modularity-consistency` UQI-4) and is not claimed pullback-able here.
- **Status:** all five rows are **Proven** (LG-C3 locked by AVI-647); the delta's scenarios are directly checkable against the existing implementation.

## 6. Feasibility gate

Archival gated on `head-of-dev` confirmation of Haskell impact (expected verdict per AVI-647: none — behavior already implemented; optional extra property test for the LG-C3 fiber-product identity) via a comment on [AVI-1291](/AVI/issues/AVI-1291).