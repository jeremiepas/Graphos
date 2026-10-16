# AVI-616 Consolidated Requirements Layer — Lean 4 Formalization of the Graphos Context Graph (Phase 1 + Phase 2)

**Author:** Head of R&D (consolidation of the Graph Theory Expert's Phase 1 and Category Theory Expert's Phase 2 deliverables).
**Status:** consolidated requirements — one notation, no contradictions, every requirement mapped to a Graphos surface. Requirements-only: no Haskell written, no Lean written, no OpenSpec changes. Lean-body authorship is the PO's decomposition; Haskell-constraining pins are gated on the Graphos Dev feasibility gate.
**Board scope (interaction `a48fae1a`, answered):** full **Phase 1+2+3** · CI posture **report-only** (`lake build` job non-blocking) · maintenance owner **R&D cell** · timebox **gated on ROI** (Phase 3 deferred).
**Plan (accepted, interaction `de2a4182`):** Lean 4 as a **parallel verification companion** — Lean proves the spec; Haskell implements and property-tests it. No FFI, no runtime bridge; `formalization/lean/` is a separate lake package outside the cabal build.

> **Reconstruction note (2026-09-30).** The original consolidated deliverable and the Phase 2 file were lost when branch `AVI-616-r-and-d-proof-math` was deleted and its worktree detached; only Phase 1 had been committed to `main` (commit `5b23458`). This layer was reconstructed from the durable artifacts — the Phase 1 file (restored via `main`) and the CTE's Phase 2 deliverable published in the AVI-620 thread. During reconstruction the merge-precedence pin was **source-verified and corrected** (§1 M-5): the CTE draft said `Map.union` is second-wins; the code is left-biased **first/old-wins**. The Phase 2 file on this branch carries the correction; this consolidation records it.

---

## 0. Why Lean + Haskell (the direct answer, restated)

**Yes — adding Lean (https://lean-lang.org/) alongside Haskell is a good fit for Graphos, scoped as a parallel verification companion.** What Lean buys us: machine-checked proofs of the two hardest mathematical claims behind the context graph — the modularity-Q range bound (−½ ≤ Q ≤ 1 for undirected simple support graphs) and the merged-context-graph-as-colimit universal property (existence, uniqueness, order-independence on consistent views). Both are currently only property-tested; Lean turns them into theorems. What Lean does *not* do: replace Haskell at runtime, FFI into the executable, or gate the build. Phase 3 (formalizing additional Domain core functions) remains deferred and gated on Phase 1+2 ROI.

---

## 1. One pipeline, one notation

### 1.1 The verified pipeline (every stage grounded in source)

```
LSP extraction views {Eᵢ} : Extraction        (Domain/Graph/Core.hs:100)
  → colimit merge  M = foldl' ⊕ emptyExtraction   ⊕ = mergeExtractions, FIRST/OLD-WINS
      fold sites: UseCase/Build.hs:12 (foldr), UseCase/Extract/Core.hs:124/466/506 (strict foldl′)
      streaming lane: mergeIntoRunning = running ⊕ ext (accumulator-first → older running view wins)
  → merged context graph = buildGraph gDirected M   (Domain/Graph/Core.hs:96)
  → Graph (LabeledGraph, Types/Graph.hs:83)
  → simple support graph G_s = (V, A)               (neighbors collapses multi-edges; Query.hs:37)
  → partition π : V → CommunityId                   (CommunityMap)
  → auditable modularity Q(π) = Σ_c [σ_in(c)/(2m) − (σ_tot(c)/(2m))²]   (Community.hs:92,151)
```

Phase 2 output (merged `M`, the colimit) feeds Phase 1 input (`G_s`, the support graph `Q` is defined over). Weighted claims are out of scope everywhere: both the Leiden pipeline and `computeCommunityStats` act on the unweighted `G_s`.

### 1.2 Unified notation glossary (no conflicts — verified symbol-by-symbol)

Domain objects inherited verbatim from the Phase 1 glossary (AVI-618 §0), used identically in Phase 2 (AVI-620 §0.1–0.2): `NodeId = Text` (`Node.hs:46`), `CommunityId = Int` (`Types/Graph.hs:41`), `CommunityMap = Map CommunityId [NodeId]` (`Types/Graph.hs:43`), `Graph` (`Types/Graph.hs:83`, `Core.hs:47`), `gDirected : Bool` (`Core.hs:52`), `neighbors`/`degree` (`Graph/Query.hs:37/44`), `csSigmaIn/csSigmaTot/csDegrees/csM` (`Community.hs:85-90`), `computeCommunityStats` (`Community.hs:92`), `buildReverseIndex` (`Community.hs:78`), `Extraction` + `extNodes/extEdges` (`Core.hs:100,131-132`), `emptyExtraction` (`Build.hs:12`), `EdgeId = EdgeId Text` (`Convert.hs:165`), `Relation` + `edgeSource/edgeTarget/edgeRelation` (`Types/Graph.hs:73`).

Graph quantities (`G_s`, `N`, `E_s`, `k_v`, `m`, `σ_in`, `σ_tot`, `a_c`, `d_c`, `γ`, `Q`) — Phase 1 only; extraction-view/merge symbols (`Extraction`, `EdgeId`, `⊕`, `foldl' ⊕ emptyExtraction`, `foldr ⊕ emptyExtraction`, `mergeIntoRunning`) — Phase 2 only. **The only shared-merge symbol is `⊕`, pinned once below.** No symbol is redefined in either doc.

| Symbol | Pin | Source |
|---|---|---|
| `⊕ ≡ mergeExtractions :: Extraction → Extraction → Extraction` | key-wise `Map.union` on node & edge maps; **first (old) operand wins** on a shared key; `O(n₁+n₂)` | `Core.hs:129-136` |

### 1.3 The one correction (head-rnd arbitration M-5) — recorded, not hidden

The CTE Phase 2 draft pinned `mergeExtractions` as **second-wins / new-wins** and named the lax-colimit artifact `mergedExtraction_laxColimit_newWins`. Source verification (2026-09-30) shows `Map.union` is **left-biased** (`Core.hs:131-132`), and the call sites confirm a first-biased **operator**: `mergeIntoRunning` merges `running ⊕ ext` (older running view wins) and `Build.hs:12`'s `foldr` keeps the earliest view's value on conflicts. Resolution: **operator precedence is first/old-wins**; the artifact is renamed `mergedExtraction_laxColimit_oldWins`; the Phase 2 file carries the full correction banner. This matches the corpus precedent (commits `2ee8bca`, `a7e6eb2`), which had already corrected the same mistake in the mergeGraphs docs. Note the list-level consequence, which is a genuine (recorded) divergence between lanes: under a first-wins operator, `foldr` (batch) makes the **earliest** view win conflicts while `foldl'` with accumulator-first (streaming) makes the **last** view win. On consistent inputs no divergence exists.

---

## 2. The consolidated named-result map (10 Lean results)

### Phase 1 — auditable modularity Q (AVI-618 §2–§3; six stubs + range lemma)

| # | Lean artifact | Statement (shared notation) | Checkable acceptance | Surface |
|---|---|---|---|---|
| 1 | `modularity_edgesAbsent_zero` | `E_s = 0 ⇒ Q(π) = 0` for every `π` | fixture: 4 isolated nodes, one community ⇒ `Q == 0.0` | `Domain/Community.modularity` (`Community.hs:151`), `m <= 0` guard (`Community.hs:232`) |
| 2 | `modularity_range_bound` (lemma) | undirected, `m ≥ 1`: `−1/2 ≤ Q(π) ≤ 1` (tight; K_{n,n} attains −½; 1 is sup) | property + fixture 5; directed reports labelled "bound not guaranteed" | `modularity` |
| 3 | `modularity_partitionOnly` | same blocks ⇔ same `Q` (member- and key-order independence) | `permuteMembers cm` preserves `Q` | `modularity` |
| 4 | `modularity_localMoveNonDecreasing` | one full Leiden local pass ⇒ `Q` non-decreasing; strict if ≥1 accepted move (`deltaQ` ≅ `2ΔQ`) | `Q (localMovesFrom g cm0) ≥ Q cm0` | `localMovingPass` (`Community.hs:178`), `bestCommunityFor` (`Community.hs:229,236,244`) |
| 5 | `modularity_deterministic` | deterministic pure function; fold over `Map.elems` ascending key order → bit-equal doubles | two calls bit-equal | `modularity` fold order |
| 6 | `modularity_fixturesExact` | fixture 1 (single community) `Q = 0`; fixture 2 (all singletons) `Q = −(1/(2m))²·Σ k_v²` | exact rational fixture values | `modularity` on AVI-618 §5 fixtures |
| 7 | cross-check contract | `∀ pinned fixture f. |Q_lean(f) − Q_hs(f)| ≤ eps` over the 7-fixture table (AVI-618 §6, incl. triangle `Q = 0` / `−1/3`, dual-computation fallback, bit-equality call) | script asserts equality; mismatch = spec violation | `tests/Graphos/Domain/CommunitySpec.hs` conventions |

### Phase 2 — merged context graph as colimit (AVI-620 §2–§4; four results)

| # | Lean artifact | Statement | Checkable acceptance | Surface |
|---|---|---|---|---|
| 8 | `mergedExtraction_isColimit` | consistent family ⇒ `M = ⋃Eᵢ` with canonical inclusions is the colimit: unique mediating view-embedding `u` for every compatible cocone | Lean `#check` under pinned hypotheses | `mergeExtractions` (`Core.hs:129`), fold sites |
| 9 | `mergedExtraction_strictColimit_orderIndependent` | consistent `A, B` ⇒ strict colimit and `⊕` commutes; n-way fold invariant under permutation/re-association; `foldr σ = foldl' σ` | property test; AC-3 timing evidence | both fold sites |
| 10 | `mergedExtraction_laxColimit_oldWins` (renamed per M-5) | conflicting key ⇒ no matching cocone (strict UP fails); `Map.union` is a lax colimit with non-invertible **first-wins** α: `value_{A⊕B}(k) = value_A(k)`, `A⊕B ≠ B⊕A` | AC-4: conflicting `EdgeId` test asserts the **first** operand's value survives; both duals differ | `Core.hs:131-132`; encoding `Convert.hs:165` |
| 11 | `mergedExtraction_nwayFold_linear` | `cost (foldl' ⊕ emptyExtraction) = O(Σᵢ |keys(Eᵢ)|)` in key operations | criterion run: disjoint views size 1000, n=100/200/400/800 → ×2 per doubling (recorded: 3052/6126/12162/24506 µs) | `Map.union` cost model (`Core.hs:126-128`) |

(Numbering: 7 contracts for Phase 1 counting the cross-check, 4 for Phase 2 — "10 named results" = 6 stubs + range lemma + 4 colimit results, with the cross-check script and harness as authorship deliverables.)

### 2.1 Conditional pins (until the feasibility gate resolves)

- If dev confirms **consistency is guaranteed** on shared keys ⇒ strict colimit (8–9) holds as-is; the lax lemma (10) becomes a conditional refinement.
- If **conflicts are silently resolved** ⇒ the lax **old-wins** lemma (10) is the primary spec.
- If the gate requires a **different merge** (e.g. edge-attribute merging) ⇒ `⊕` re-pins at AVI-620 §0.3/§2/§3 **before** Lean authorship lands.

---

## 3. Surface map (every requirement → Graphos surface)

| Surface | Role in the layer |
|---|---|
| `src/Graphos/Domain/Community.hs` (`modularity`, `localMovingPass`, `bestCommunityFor`, `computeCommunityStats`, fixtures in `tests/Graphos/Domain/CommunitySpec.hs`) | Phase 1 Lean results cross-check against it (results 1–7) |
| `src/Graphos/Domain/Graph/Core.hs` (`mergeExtractions`, `buildGraph`) | Phase 2 colimit pins (results 8–11); M→Graph seam |
| `src/Graphos/UseCase/Build.hs` / `src/Graphos/UseCase/Extract/Core.hs` (fold sites + `mergeIntoRunning`) | fold-order feasibility question; AC-3/AC-4 test sites |
| `docs/math-requirements/AVI-618-…md` / `AVI-620-…md` | authoritative per-phase requirement docs consolidated here |
| `formalization/lean/` (new, separate from cabal) | Lean bodies; PO-decomposed; report-only CI |
| corpus `docs/math-requirements/` (community-detection-modularity, colimit corpus functoriality-context-graph, consolidated-requirements) | inherited notation and prior claims |

---

## 4. Sequencing (what happens next, with owners)

| # | Item | Owner | Issue |
|---|---|---|---|
| 1 | **Feasibility gate** — four colimit-merge-semantics questions answered against source (consistency guarantee, fold-order, edge-conflict intent, streaming parity) | Graphos Dev | **AVI-949** (replacement for cancelled AVI-640) |
| 2 | **Lean authorship decomposition** — `formalization/lean/` lake package, 10 Lean bodies, AVI-618 §6 cross-check script, Phase-2 timing harness, M→Graph seam pin; report-only CI maintenance | Product Owner | **AVI-948** |
| 3 | DevOps provisioning of the Lean toolchain (lake/elan) | DevOps | via PO decomposition (AVI-948) |
| 4 | Phase 3 (optional core formalization) | deferred | gated on Phase 1+2 ROI review (board decision) |

This issue's own disposition: the R&D cell's requirements program for AVI-616 is **complete** — consolidated layer (this file), Phase 1 file (committed in `main`), Phase 2 corrected file (this branch) — with follow-up work delegated to AVI-948/AVI-949.

---

## 5. Report to Head of Graphos — proven / assumed / open

- **Proven (math-side, grounded):** modularity range bound −½ ≤ Q ≤ 1 for undirected simple support graphs (AVI-618 §3, numerically confirmed exhaustive/random); local-move monotonicity of Leiden's gain rule (`deltaQ = 2ΔQ` algebraic identity, AVI-618 INV-Q3); n-way merge fold linear in total key operations (AVI-620 §4, criterion-evidenced); colimit universal property + order-independence for consistent view families (AVI-620 §2–§3.1).
- **Source-verified and corrected:** merge precedence is **first/old-wins** (`Map.union` left-biased; three call sites checked) — the CTE draft's second-wins pin was wrong and is superseded (M-5).
- **Assumed:** Lean 4 stays a parallel verification companion (no FFI, no runtime coupling, separate lake package); board CI posture (report-only) and maintenance ownership (R&D) are honored in authorship tasks; Phase 2's timing ratios carry over from the recorded runs and get re-run at authorship.
- **Open (delegated, tracked):** (a) AVI-949 feasibility gate — four merge-semantics answers gate the strict-vs-lax split and any `⊕` re-pin; (b) AVI-948 PO authorship decomposition for the 10 Lean results; (c) Phase 3 deferred pending ROI; (d) the M → Graph → G_s seam is described here and must be pinned at Lean authorship — the only spec seam left open by Phase 2.

---

*(End of consolidated layer. The R&D cell's next action is consumption of AVI-949's verdicts — the only work item that can re-open a pin in this layer.)*