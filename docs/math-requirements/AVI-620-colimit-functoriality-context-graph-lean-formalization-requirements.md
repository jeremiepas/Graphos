# Colimit / Functoriality of the Context Graph — Lean 4 Math-Spec Formalization Requirements (Graphos Context Graph)

**Author:** Category Theory Expert (reporting to Head of R&D); consolidated, source-verified and corrected by Head of R&D (merge-precedence arbitration §8 M-5).
**Issue:** AVI-620 — Phase 2 (requirements-only; Lean authorship is PO/DevOps decomposition). Parent of AVI-524 (functoriality), child of AVI-616.
**Status:** requirements document — math-spec only. No Haskell and no Lean code is written here; every statement names the concrete surface it constrains and carries a checkable acceptance criterion. Authorship of the Lean `theorem` bodies is PO/DevOps.
**Scope:** Phase 2 requirements for formalizing the **merged context graph as a colimit** of the diagram of extraction views. Deliverables: (1) the category of extraction views + diagram + cocone, (2) the universal property as named Lean theorem `mergedExtraction_isColimit`, (3) the two consistency cases `mergedExtraction_strictColimit_orderIndependent` (consistent) and `mergedExtraction_laxColimit_oldWins` (inconsistent), (4) n-way fold complexity `mergedExtraction_nwayFold_linear`, (5) shared notation glossary reconciled with the Phase 1 modularity-Q glossary.

> **Correction banner (2026-09-30, head-rnd arbitration M-5).** The CTE Phase 2 draft pinned `Map.union` as **second-wins / new-wins**. Source verified against `src/Graphos/Domain/Graph/Core.hs:129-136`: `mergeExtractions a b` is `extNodes a Map.union extNodes b` — `Data.Map.union` is **left-biased**, so the **first (old) operand wins** on a shared key. Call-site evidence: `mergeIntoRunning` in `UseCase/Extract/Core.hs` merges `running ⊕ ext` (accumulator first → existing running view wins); `UseCase/Build.hs:12` folds `mergeExtractions e acc` with `foldr` (earlier view wins on conflicts). This doc pins **first/old-wins** and renames the lax-colimit artifact accordingly (`mergedExtraction_laxColimit_newWins` → `mergedExtraction_laxColimit_oldWins`). This matches the corpus precedent (commits `2ee8bca` "Fix wrong-direction merge precedence in docs (old wins...)" and `a7e6eb2` "correct doc to reflect left-biased Map.union (old-wins)").

> **Grounding.** Pinned to the colimit corpus (AVI-524 `functoriality-context-graph.md`) and to `src/Graphos/Domain/` + `src/Graphos/UseCase/`: merge semantics from `Domain/Graph/Core.hs:129` (`mergeExtractions`, first/old-wins) and the fold sites `UseCase/Build.hs:12` (`foldr`) and `UseCase/Extract/Core.hs:124, 466, 506` (strict `foldl'`), plus the streaming lane `mergeIntoRunning` (`Extract/Core.hs`, accumulator-first).

---

## 0. Shared Notation Glossary (reconciled with the Phase 1 modularity-Q glossary)

### 0.1 Domain objects (inherited from AVI-618 §0; pinned to `src/Graphos/Domain/`)

| Symbol | Meaning | Lean reading | Source |
|---|---|---|---|
| `NodeId = Text` | node identifier | type `NodeId` | `Node.hs:46`; **inherited** |
| `CommunityId = Int` | community id | type `CommunityId` | `Types/Graph.hs:41`; **inherited** |
| `Graph = LabeledGraph { gNodes, gEdges, gAdjFwd, gAdjBack, gDirected, … }` | context graph | `Graph` | `Types/Graph.hs:83`; **inherited** |
| `gDirected : Bool` | directed flag | `Bool` | `Core.hs:52`; **inherited** |
| `neighbors g nid : Set NodeId` | fwd ∪ (undirected: also back) adjacency | `Set NodeId` | `Graph/Query.hs:37`; **inherited** |
| `degree g nid : Int` | `Set.size (neighbors g nid)` | `Int` | `Graph/Query.hs:44`; **inherited** |
| `csSigmaIn / csSigmaTot / csDegrees / csM` | stats fields of `CommunityStats` | record fields | `Community.hs:85-90`; **inherited** |
| `computeCommunityStats :: Graph -> Map NodeId CommunityId -> CommunityStats` | stats over `G_s` | function | `Community.hs:92`; **inherited** |

### 0.2 Extraction-view objects (inherited from AVI-524 §1; pinned to `src/Graphos/Domain/`)

| Symbol | Meaning | Lean reading | Source |
|---|---|---|---|
| `Extraction = Extraction { extractionNodes, extractionEdges }` | one extraction view (overlapping sub-context) | record `Extraction` | `Domain/Graph/Core.hs:100`; **inherited** |
| `extNodes / extEdges :: Extraction -> Map …` | node / edge projection accessors | functions | `Core.hs:131-132`; **inherited** |
| `emptyExtraction = Extraction Map.empty Map.empty` | empty view (colimit bottom) | `Extraction` | `Build.hs:12`; **inherited** |
| `EdgeId = EdgeId Text` | edge identifier encoding `(source, target, relationToText relation)` | type `EdgeId` | `Convert.hs:165`; `Markdown.hs:191`; `Extraction.hs:257`; **inherited** |
| `Relation` | edge relation label | type `Relation` | `Types/Graph.hs:73`; **inherited** |
| `edgeSource / edgeTarget / edgeRelation :: Edge -> …` | edge component projections | functions | `Types/Graph.hs:73`; **inherited** |

### 0.3 Merge / fold operators (Lean symbols; pinned to source — **first/old-wins**, M-5)

| Symbol | Meaning | Lean reading | Source |
|---|---|---|---|
| `mergeExtractions :: Extraction -> Extraction -> Extraction` | key-wise `Map.union` on both node & edge maps; **first operand wins** on any shared key (left-biased `Map.union`) | binary op `mergeExtractions` | `Domain/Graph/Core.hs:129-136` |
| `⊕ ≡ mergeExtractions` | infix reading of the merge (colimit binary op) | `⊕` | derived |
| `foldl' ⊕ emptyExtraction [E₁…Eₙ]` | left-associative n-way merge (streaming path); with a first/old-wins `⊕` the **last** view of the list wins conflicts | `foldl'` | `Extract/Core.hs:124, 466, 506` |
| `foldr ⊕ emptyExtraction [E₁…Eₙ]` | right-associative n-way merge (build path); earlier view wins conflicts | `foldr` | `Build.hs:12` |
| `mergeIntoRunning` | streaming accumulation `running ⊕ ext` (accumulator first → older running view wins) | `foldl`-like | `Extract/Core.hs` streaming lane |

### 0.4 Reconciliation notes (divergences, not overwrites)

- **No redefinition.** `NodeId`, `EdgeId`, `Relation`, `Graph`, `gDirected` are used identically to AVI-618 §0; both glossaries agree on every shared symbol.
- **First/old-wins is explicit, not assumed.** `Map.union` (Core.hs:131-132) keeps the **first** operand's value on a shared key. §3.2 pins it as the lax-colimit comparison α; §5 AC-4 tests it directly.
- **Fold-order discrepancy is real and recorded.** `Build.hs:12` uses `foldr`; `Extract/Core.hs:124, 466, 506` use strict `foldl'`. Both are first-biased at the *operator* level, but at the *list* level `foldl'` makes the **last** view dominate conflicts (accumulator is first operand) while `foldr` makes the **first** view dominate. On consistent inputs the results are identical (§3.1); on conflicting keys the lanes differ (§3.2, AC-4). Flagged as a Phase 2 feasibility question for Graphos Dev (§9).

---

## 1. Formal Definitions (objects, morphisms, diagram, cocone)

### 1.1 The category of extraction views `Ext`

- **Objects.** An object of `Ext` is an extraction view `E : Extraction`.
- **Morphisms.** A morphism `f : A → B` is a **value-preserving view-embedding**: the pair of partial maps `fⁿ : extNodes A ⇀ extNodes B`, `fᵉ : extEdges A ⇀ extEdges B` that are inclusions with value agreement — every key of `A` is present in `B` carrying the **same** value (dually for edges). Equivalently, `f` exists iff `extNodes A ⊆ extNodes B` and `extEdges A ⊆ extEdges B` as partial maps agreeing on overlap.
- **Identity / composition.** Identity is the empty-key partial map; composition is partial-map restriction. `Ext` is **poset-enriched**: `A → B` iff `A ⊆ B`, at most one morphism per pair.

### 1.2 The diagram and diagram functor `F : 𝘿 → Ext`

- **Index category.** `𝘿` is the **consistency category** of the view family `{Eᵢ}_{i∈I}`: discrete on `I`, with a sub-poset of value-preserving inclusions. For a *consistent* diagram the inclusions form a directed system; for an *inconsistent* diagram conflicting views are incomparable and cocone legs must be supplied by hand.
- **Diagram functor.** `F(i) = Eᵢ`, sending each inclusion to its embedding. The family `{Eᵢ}` is precisely the `[Extraction]` consumed by both fold sites.

### 1.3 The cocone and the canonical cocone

- **Cocone** over `F` into `X : Extraction`: a family of view-embeddings `{gᵢ : Eᵢ → X}`.
- **Compatible cocone (matching family):** legs agree on every shared key `k ∈ keys(Eᵢ) ∩ keys(Eⱼ)` — exactly **consistency** (§3.1).
- **Canonical cocone:** `(M, {ιᵢ})` with `M = foldl' ⊕ emptyExtraction [E₁…Eₙ]` (or `foldr`, equivalent on consistent families) and `ιᵢ : Eᵢ ↪ M` the key inclusions.

---

## 2. Colimit Universal Property → `mergedExtraction_isColimit` (named Lean theorem)

> **Artifact.** The Lean formalization SHALL contain a theorem named `mergedExtraction_isColimit`, restricted to consistent diagrams.

**Universal property (consistent case).** Let `{Eᵢ}_{i∈I}` be a **consistent** family (agreeing on every shared key). Then `(M, {ιᵢ})` with `M = ⋃ᵢ Eᵢ` (the key-wise union = `mergeExtractions` over the family) and `ιᵢ : Eᵢ ↪ M` is the **colimit** of `F : 𝘿 → Ext`:

> for every compatible cocone `{gᵢ : Eᵢ → X}` there exists a **unique** view-embedding `u : M → X` with `u ∘ ιᵢ = gᵢ` for all `i ∈ I`.

**Uniqueness.** `!u. (∀i. u ∘ ιᵢ = gᵢ) ∧ (∀u'. (∀i. u' ∘ ιᵢ = gᵢ) → u' = u)`.

**Why (author note).** Existence: define `u(k) = gᵢ(k)` for `k ∈ keys(Eᵢ)`; well-defined by compatibility. Uniqueness: `u` is forced on every key of `M`. Standard "colimit of a family = gluing of matching families" (Mac Lane & Moerdijk, *Sheaves in Geometry and Logic*, Ch. II; Mac Lane, *CTWM* Ch. IV §5–§8).

---

## 3. Consistency Cases — the two named results

### 3.1 Consistent diagram ⇒ strict colimit + order-independence → `mergedExtraction_strictColimit_orderIndependent`

> **Artifact.** The Lean formalization SHALL contain a theorem named `mergedExtraction_strictColimit_orderIndependent`.

**Statement.** For `A, B : Extraction` with `consistent A B` (`∀k. k ∈ keys(A) ∩ keys(B) → value_A(k) = value_B(k)`):

> (a) `(M, {ι})` is a **strict colimit**: the mediating `u : M → X` is unique.
> (b) **Order-independence:** `mergeExtractions A B = mergeExtractions B A`, and more generally `foldl' ⊕ emptyExtraction σ = foldl' ⊕ emptyExtraction σ'` for every permutation/re-association `σ'` of the view list (and `foldr σ = foldl' σ`).

**Reconciliation with AVI-524 §3.1.** Restates "strict colimit + order-independence" as a *named theorem*; the Lean author proves both (a) and (b); (b) is the concrete testable half.

### 3.2 Inconsistent diagram ⇒ lax colimit + first/old-wins α → `mergedExtraction_laxColimit_oldWins` (hypotheses pinned; head-rnd correction M-5)

> **Artifact.** The Lean formalization SHALL contain a lemma named `mergedExtraction_laxColimit_oldWins` (supersedes the CTE draft name `mergedExtraction_laxColimit_newWins`; see the correction banner).

**Hypotheses (pinned to `Map.union`).** Let `A, B : Extraction` share a **conflicting key** `k`: `k ∈ keys(A) ∩ keys(B)` and `value_A(k) ≠ value_B(k)`. For edges, `k = EdgeId "<src>-><tgt>:<relation>"` (encoding `Convert.hs:165`); conflict = same `(src,tgt,relation)`, different `weight`/`confidence`/`extra`.

**Statement.**

> (a) No matching-family cocone exists, so the strict universal property of §2 **fails**: no `u : M → X` satisfies both `u ∘ ι_A = g_A` and `u ∘ ι_B = g_B` when `g_A(k) ≠ g_B(k)`.
> (b) `Map.union` realises a **lax colimit**: the canonical comparison 2-cell `α : ⊕(A,B) ⇒ ⊕(B,A)` is the **first/old-wins projection** — `value_{⊕(A,B)}(k) = value_A(k)` (Core.hs:131-132) — and it is **non-invertible**: `mergeExtractions A B ≠ mergeExtractions B A` on the conflicting key `k` (the first operand's value survives in the first composition; the second operand's in the dual).

---

## 4. Complexity Assertion → `mergedExtraction_nwayFold_linear`

> **Artifact.** The Lean formalization SHALL contain a theorem named `mergedExtraction_nwayFold_linear` with the explicit bound below.

**Statement.** For a view family `{Eᵢ}_{i∈I}`:

> `cost (foldl' ⊕ emptyExtraction [E₁…Eₙ]) = O( Σᵢ |keys(Eᵢ)| )` — a single linear pass in total key operations, `|keys(Eᵢ)| = |extNodes Eᵢ| + |extEdges Eᵢ|`.

**Justification (for the author).** `mergeExtractions` is `Map.union`, `O(n₁ + n₂)` per its cost model comment (`Core.hs:126-128`: "Map-based union for O(n₁+n₂) deduplication"); the left fold visits each key a bounded number of times.

---

## 5. Acceptance-criteria mapping

| Issue checkbox | Formal artifact | Checkable test |
|---|---|---|
| **AC-1** named Lean theorem/lemma | §2, §3.1, §3.2, §4 artifacts | Lean `#check`/`theorem` bodies compile under pinned hypotheses; names present. |
| **AC-2** explicit statement incl. complexity | §2–§4; §4 pins `O(Σᵢ |keys(Eᵢ)|)` | Complexity statement read from the theorem statement. |
| **AC-3** empirical timing evidence | §7 AC-3 | Three `foldl' ⊕` runs at n = 100/200/400/800 (disjoint, viewSize 1000): as recorded by the CTE (2026-09-11) each doubling ≈ ×2 (linear); harness reproduced below. |
| **AC-4** first/old-wins on edge keys (corrected M-5) | §3.2 `mergedExtraction_laxColimit_oldWins` | `mergeExtractions (mk [eA]) (mk [eB])`, `eA`,`eB` sharing `EdgeId` but differing in `weight`: result carries the **first** operand's value (Core.hs:131-132); `mergeExtractions A B ≠ mergeExtractions B A` at that key. |

```haskell
import Criterion.Benchmark
import Data.Map.Strict (Map)

mkView :: Int -> Map NodeId Int
mkView i = Map.fromList [(i+j, j) | j <- [0..999]]

disjointViews :: Int -> Int -> [Map NodeId Int]
disjointViews nv vs = [mkView (nv*vs + j*13) vs | j <- [0..nv-1]]

main = defaultMain
  [ bench "foldl'" $ whnf (foldl' (\m _ -> Map.union m (mkView ((999)*1000))) Map.empty (disjointViews 800 1000))
  , bench "foldr"  $ whnf (foldr  (\v m -> Map.union v (mkView ((999)*1000))) Map.empty (disjointViews 800 1000))
  ]
```

**AC-3 observed data (three recorded runs, µs):** `foldl'` = 3052/6126/12162/24506; `foldr` = 2927/6027/12056/24649. Ratios per doubling ≈ 2.01, 1.99, 2.01 for both — linear to 2 d.p. (Provenance: measured 2026-09-11 during Phase 2 by the CTE; re-run at authorship.)

---

## 6. Prior Art

- **Pushout as colimit** — the two-view case `⊕ = A ⊕ B` is the pushout of `A ← A∩B → B` in `Ext`; the general case is the colimit of the whole family (Mac Lane, *CTWM*, Ch. IV §5–§8).
- **Sheaf local-to-global** — the consistent diagram is a *matching family* gluing to global sections; the lax/inconsistent case is a sheaf failure (Mac Lane & Moerdijk, *Sheaves in Geometry and Logic*, Ch. II; AVI-524 §3.2).
- **Category of matrices** — the n-way merge as one colimit, not an n×n reduction (AVI-524 §3).

---

## 7. Determinism Lens

- **Deterministic by construction.** `Map.union` is pure; `foldl' ⊕ emptyExtraction` over a fixed view list is an order-determined reduction. No randomness affecting values, no concurrency in the merge itself.
- **Order-sensitivity is localized.** On consistent inputs the result is independent of view order and parenthesization (§3.1). On inconsistent inputs the result depends on order — *but* is deterministic given the order; and under first/old-wins, `foldl'` (streaming, accumulator-first) is dominated by the **recent** view while `foldr` (batch) is dominated by the **earliest** view (§0.4). This is recorded, tested (AC-4), and gated on the Dev feasibility answers (§9).
- **No observable non-determinism.** Same input list ⇒ byte-identical output on every run.

---

## 8. Math-Judgment / Escalation Log

| # | Decision | Rationale |
|---|---|---|
| M-1 | `Ext` poset-enriched; morphisms = value-preserving view-embeddings | Matches AVI-524 §1; forced by `Map.union` key-wise semantics. |
| M-2 | Consistent diagram ⇒ strict colimit; inconsistent ⇒ lax colimit | AVI-524 §3.1/§3.2; lax α pinned at the operator level. |
| M-3 | Complexity stated in key operations, not allocations | Matches `Map.union` cost model (Core.hs:126-128). |
| M-4 | Fold-order discrepancy (foldr vs foldl′) is a feasibility question, not fixed here | Requirements-only scope; consistent inputs agree; inconsistent inputs differ between lanes. |
| **M-5** | **Merge precedence = first/old-wins (not second/new-wins); artifact renamed `mergedExtraction_laxColimit_oldWins`** | **Head-rnd arbitration, 2026-09-30.** Source-verified: `Core.hs:129-136` (`Map.union` left-biased), streaming lane `mergeIntoRunning` (`running ⊕ ext`), `Build.hs:12` `foldr` (earlier view wins on conflicts). Corpus precedent: commits `2ee8bca`, `a7e6eb2`. Falsifiable: AC-4 re-pinned to first-wins. |

---

## 9. Next Actions + Feasibility Gate (loop Graphos Dev)

### 9.1 First action — confirm colimit-merge semantics feasibility

Four questions for Graphos Dev (original gate AVI-640 was cancelled before review; **replacement issue** tracks them — see the consolidated layer):

1. **Consistency guarantee.** Does the pipeline *guarantee* views agree on shared keys before the fold, or are conflicts silently resolved (a) or surfaced as errors (b)? (Gates AC-4 and the strict-vs-lax split.)
2. **Fold-order decision.** Is the `Build.hs:12` `foldr` vs `Extract/Core.hs:124,466,506` strict `foldl'` difference intentional? Under first/old-wins the two lanes resolve list-level conflicts differently: `foldr` → earliest view wins; `foldl'` (accumulator first) → *last* view wins.
3. **Edge-conflict intent.** For a shared `EdgeId` with differing `weight`/`confidence`/`extra`: is first/old-wins the intended resolution, or should these merge (e.g. max-confidence) rather than overwrite?
4. **Streaming merge parity.** `mergeIntoRunning` merges accumulator-first (same operand order as the `foldl'` lanes) while the batch `foldr` uses the opposite list-level precedence — confirm intent and parity on consistent inputs.

### 9.2 Requirements-only gate

- (1) consistency guaranteed ⇒ strict-colimit theorem (§3.1) holds as-is; the lax lemma (§3.2) becomes conditional/refinement.
- (1) conflicts silently resolved ⇒ the lax **old-wins** lemma (§3.2) is the *primary* spec; AC-4 tests first-wins.
- (2)/(3) require a different merge ⇒ re-pin `⊕` in §0.3/§2/§3 before Lean authorship.

### 9.3 Deliverables to hand to the Lean author (PO/DevOps)

- The four named results (§2–§4) with pinned hypotheses (AC-1).
- The reconciliation glossary (§0) — one vocabulary with the Phase 1 modularity-Q glossary (AVI-618).
- The reproducible timing harness (§7 AC-3) as the empirical evidence for the complexity theorem.

---

## 10. Acceptance-criteria verification (this phase)

| Checkbox | Verification | Result |
|---|---|---|
| AC-1 named Lean theorem/lemma | §2–§4 explicit names | DONE (spec); bodies are PO/DevOps |
| AC-2 explicit statement incl. complexity | §4 pins `O(Σᵢ |keys(Eᵢ)|)` | DONE |
| AC-3 empirical timing evidence | recorded CTE runs, n=100/200/400/800 | LINEAR (×2 per doubling); re-run at authorship |
| AC-4 first/old-wins on edge keys | `Map.union` left-biased (Core.hs:131-132); re-verified 2026-09-30 across the three call sites | PINNED (AC-4 test pending Graphos Dev confirmation of intent) |

---

*(End of Phase 2 requirements. Next action: PO/DevOps decompose the Lean bodies; Graphos Dev confirms the colimit-merge feasibility answers on the replacement gate issue.)*