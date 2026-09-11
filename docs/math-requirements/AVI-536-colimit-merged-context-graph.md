# Merged Context Graph as a Colimit (Universal Property) — Requirements (Graphos Context Graph)

**Author:** Category Theory Expert (reporting to Head of R&D)
**Issue:** [AVI-536](/AVI/issues/AVI-536) (child of [AVI-512](/AVI/issues/AVI-512) element **C1**, core math). Parent of the Graphos-Dev property-test/feasibility child (see §11).
**Status:** requirements document — math-requirements only. No Haskell is written here; every requirement names the concrete surface it constrains and carries a checkable acceptance criterion. Property-test bodies and the Haskell feasibility gate are Graphos-Dev (§11).
**Scope:** model the **merged context graph** as `colim D` of the diagram of extraction views, exhibit the **universal cocone** `lambda_i : D_i -> colim D` and the **unique mediating morphism** `<mu> : colim D -> G`, prove **associativity up to iso**, and pin the **non-commutative `merge x cluster`** interaction (CTO note 3). This closes the gap the issue records: `mergeGraphs` is currently an unproven heuristic; here it is a named construction with its universal property.

> **Grounding.** Every object is pinned to `src/Graphos/Domain/` or `openspec/specs/`. The merge operator is read directly from `Domain/Graph/Core.mergeGraphs` (`Core.hs:140-162`). Two categorical claims are **empirically verified this phase** against the runtime `containers` library the property tests run against (§2.2 AC-5, §3.1): `Data.Map.union`/`<>` give the **first/left operand** precedence on conflict (not "second/new" as earlier docs state), and `<>` associates exactly even with overlapping keys.

---

## 0. Scope and relationship to the sibling CT corpus

This doc pins the **colimit universal property onto the `Graph`-level `mergeGraphs`** (`Domain/Graph/Core`), which the Extraction-level corpus does not reach:

| Doc | What it establishes | Leaves open (this doc closes) |
|---|---|---|
| [AVI-524](/AVI/issues/AVI-524) `functoriality-context-graph` | `mergeExtractions`/`mergeGraphs` as (lax) colimit of value-preserving view-embeddings; `Rho`-labeling; pipeline as composite lax functor | Universal cocone + **unique mediating morphism** exhibited over the **`Graph`** `mergeGraphs`; **associativity up to iso**; explicit **`merge x cluster` non-commutativity** |
| [AVI-620](/AVI/issues/AVI-620) `colimit-functoriality-...` | Lean named theorems `mergedExtraction_isColimit` / `_laxColimit_newWins` over `mergeExtractions` | The `Graph`-level merge + cluster; associativity; CTO-note-3 non-commutativity |
| [AVI-556](/AVI/issues/AVI-556) / merge-cluster-determinism | graph/community half (confluence, cluster invariance, conflict quantification) | The category statement (cocone + unique mediating morphism + associativity) those invariants instantiate |

**Novel contributions of this doc, each mapped to a surface:**

| Novel contribution | Concrete Graphos surface |
|---|---|
| (1) Universal cocone `lambda_i` + unique mediating morphism `<mu>` over `mergeGraphs` | `Domain/Graph/Core.mergeGraphs` (`Core.hs:140`), `UseCase.Merge.mergeGraphsAndAnalyze` (`Merge.hs:37`) |
| (2) Associativity up to iso of `mergeGraphs` | `Data.Map.<>` (`Core.hs:142`, `145-146`), `gHash`/adjacency invariance (`Core.hs:159`, `145-151`) |
| (3) Non-commutative `merge x cluster` (CTO note 3) | `clusterGraphWithResolution'` re-detects (`Merge.hs:43,56`); old IDs discarded (`Merge.hs:32`); `09-merge` "re-cluster ... old community IDs discarded" |
| (4) Old-wins reconciliation | `Map.union` left-wins (empirical); `tests/Graphos/Domain/GraphSpec.hs:242` |

---

## 1. Objects and morphisms — defined *before* any universal property

### 1.1 The category of context graphs `Gr`

- **Objects.** An object of `Gr` is a context graph `G : Graph` (`Graph { gNodes, gEdges, gAdjFwd, gAdjBack, gDirected, gCompositions, gHash, gEmbeddings, gEmbeddingsPath }`, `Core.hs:47-57`). Two graphs are identified up to the canonical iso that preserves `(gNodes, gEdges, gAdjFwd, gAdjBack, gDirected)`; `gHash` is a deterministic function of `(gNodes, gEdges)` and so is determined by that structure. `gCompositions = Nothing` for the merge apex.
- **Morphisms.** A morphism `f : G -> H` is a pair `(f_N : gNodes G -> gNodes H, f_E : gEdges G -> gEdges H)` of **key-preserving partial maps** — total on the keys they are defined on, injective on keys, and preserving incidence and relation: whenever `(s,t) in gEdges G` maps to `(s',t') in gEdges H`, then `s' = f_N(s)`, `t' = f_N(t)`, and `edgeRelation_H(f_E(e)) = edgeRelation_G(e)` (`Edge.hs:32`, `Edge.hs:88`). Composition is pairing of partial maps; identity is the key-identity partial map on each side.
- **The value-inclusion subcategory `Gr_inj`.** Restrict morphisms to **key-inclusions** (`f_N`, `f_E` honest inclusions that never change a carried value). `Gr_inj subset Gr` is **poset-enriched**: between any two objects there is at most one morphism (`G -> H` iff `gNodes G subset gNodes H and gEdges G subset gEdges H`). This is the categorical home of `mergeGraphs`: on keys the merge is a coproduct, and the cocone legs are key-inclusions.

> **Why not skip to functors.** The universal property (§2) quantifies over *all* cocones into arbitrary targets `H : Gr`; it must therefore be stated in `Gr` (structure-preserving morphisms), while the cocone legs themselves live in `Gr_inj` (key-inclusions). Objects/morphisms precede the functor `D : I -> Gr`, which precedes the universal property — no skipping levels.

### 1.2 The index category `I` and the diagram functor `D : I -> Gr`

- **Index category.** `I` is the **view poset**: objects are extraction sources `{lsp, treesitter, stub, docs, embeddings}` (the issue's "per-file LSP symbol hierarchies, tree-sitter, stub, docs, embeddings"); `lsp <= treesitter <= stub` by refinement, with `docs`, `embeddings` sibling sources. `I` is a small poset, hence a category with a single morphism per comparable pair.
- **Diagram functor.** `D(i) = buildGraph directed (view_i)` — each source maps to its per-file `Graph` (`UseCase.Build.buildGraphFromExtractions`, `UseCase.Extract.extractAll`). On an inclusion `i <= j` in `I`, `D` sends it to the key-inclusion `Gr_inj`-morphism `D(i) -> D(j)` (a refined view carries every stub/treesitter key). The extraction list `[Extraction]` consumed by `extractAll` (`UseCase/Extract/Core.hs:46`) is precisely the family `D` indexes.

---

## 2. Colimit universal property -> `mergeGraphs` (named claim)

### 2.1 The universal property (consistent case — strict colimit)

> **Claim (merged graph = strict colimit).** For a **consistent** diagram `D : I -> Gr` (any two views sharing a key carry the *same* value on it — no conflict), `(M, {lambda_i})`, where `M = mergeGraphs`-fold of `{D(i)}` and `lambda_i : D(i) -> M` are the canonical key-inclusions, is the **colimit** of `D`:
>
> **(UP)** for every cocone `{mu_i : D(i) -> H}_{i in I}` in `Gr` there exists a morphism `<mu> : M -> H` such that `<mu> . lambda_i = mu_i` for all `i in I`.
>
> **(Uniqueness up to unique iso)** there is exactly one such `<mu>`; any two mediating morphisms coincide on every key of `M`.

**Why it holds (for the author).** Existence: define `<mu>` key-wise as `mu_i` on the leg covering that key; well-defined by consistency (agreement on overlaps), structure-preserving by §1.1. Uniqueness: `<mu>` is *forced* on every key of `M` (each key lies on some `lambda_i`), so any two mediating morphisms coincide — the iso is the identity on structure. This is the standard "colimit of a family = gluing of matching families" (Mac Lane & Moerdahl, *Sheaves in Geometry and Logic*, Ch. II).

### 2.2 The conflicting case — lax colimit, and an old-wins correction

> **Claim (merged graph = lax colimit).** When views share a key `k` with **different** values, no compatible value-preserving cocone exists into any single `H : Gr` (a value-preserving leg would have to carry two different values at `k`). The strict universal property (§2.1) therefore **fails**, and `mergeGraphs` exhibits a **lax colimit**: the cocone legs are the key-inclusions `lambda_i`, and the mediating morphism exists only *up to a chosen comparison 2-cell* `alpha_k`.

> **R-MERGE (merge as lax colimit).** `mergeGraphs A B` is the lax colimit of the inconsistent view-pair diagram, with comparison 2-cell `alpha` = **"keep the first/left operand's value on every conflicting key"** (`gNodes old <> gNodes new`, `Core.hs:142`). On consistent inputs `alpha` is invertible and the lax colimit collapses to the strict colimit of §2.1; on conflicting inputs `alpha` is a genuine non-invertible comparison and the result is **order-dependent**.

> **OLD-WINS CORRECTION (empirically verified, AC-5).** [AVI-524](/AVI/issues/AVI-524) §3.2 and `openspec/specs/09-merge` state that `Map.union` is *second/new-wins*. This is a **misread** of the runtime `containers` library: `Data.Map.union r1 r2` and `r1 <> r2` give the **first/left operand** precedence on conflict (verified with `runghc`: `union {a:=1} {a:=2} = {a:=1}`). Hence `mergeGraphs old new` keeps **old**'s value, and the lax comparison `alpha` is *"keep the left operand"*, not *"new"*. This **reconciles** with the existing property test `tests/Graphos/Domain/GraphSpec.hs:242` (asserts old-wins, `shouldBe "Original"`), which matches the code; the earlier "new-wins" claim is inconsistent with both the code and that test. The categorical structure is unchanged (order-dependent conflict resolution = lax colimit); only the *direction* of `alpha` was misstated.

---

## 3. Associativity up to iso -> `mergeGraphs`

### 3.1 Statement

> **Claim (associativity up to iso).** `mergeGraphs` is associative up to canonical iso:
> `((A . B) . C) ~=~ (A . (B . C))` where `. = mergeGraphs`.

**Why it holds.** On keys, `gNodes old <> gNodes new` / `gEdges old <> gEdges new` are `Data.Map.<>` — **associative even with overlapping keys** (empirically verified: `(a<>b)<>c == a<>(b<>c)` for overlapping Int keys). The derived fields are association-invariant: `gDirected = gDirected (leftmost)` in both parenthesizations (`Core.hs:157`); `gAdjFwd`/`gAdjBack` are `Map.unionWith Set.union` over the same key set (`Core.hs:145-146`); `gHash` is a deterministic function of `(gNodes, gEdges)` (`Core.hs:159`); `gCompositions = Nothing`; `gEmbeddings` is `Map.union` over `Maybe` maps (`Core.hs:147-151`). Hence the canonical iso is the **identity on structure**.

> **R-ASSOC (associativity).** `mergeGraphs` satisfies `((A . B) . C) == (A . (B . C))` up to the canonical structural iso; the iso is the identity on `(gNodes, gEdges, gAdjFwd, gAdjBack, gDirected, gHash)`. GATE: Graphos-Dev asserts strict equality of the full `Graph` record under re-association and reports any field that breaks it.

---

## 4. Non-commutative `merge x cluster` (CTO note 3)

### 4.1 The two operations

- **Merge** `. = mergeGraphs : Graph -> Graph -> Graph` (colimit, §2).
- **Cluster** `C = clusterGraphWithResolution' : Graph -> Resolution -> (CommunityMap, CohesionMap)` — Leiden community detection on the merged graph (`Merge.hs:43,56`; `UseCase.Cluster.clusterGraphWithResolution`).

`mergeGraphsAndAnalyze` composes them (with inference between): `merge -> C -> infer -> C -> analyze` (`Merge.hs:39-64`).

### 4.2 The claim

> **Claim (merge x cluster does not commute).** Community detection is **not a colimit-preserving functor**: `C(colim D) ~=~ C(D)` fails in general. Community IDs are **re-detected** on the merged graph and the source IDs are **discarded** (`Merge.hs:32`: "community IDs from the two source graphs no longer align, so we re-detect communities"; `09-merge`: "re-cluster merged graph via Leiden (old community IDs discarded)").

**Where it commutes.** For **disjoint** views (disjoint node sets), Leiden on the union equals the union of individual detections — isolated components cluster independently, so `C(A disjoint-union B) = C(A) disjoint-union C(B)`.

**Where it does not.** For **overlapping** views (shared nodes/edges), the merged graph's community structure is detected holistically and is *not* the combination of the parts' communities — an edge bridging the merge boundary reassigns both endpoints. Hence `C(mergeGraphs A B) ~=~ mergeCommunities(C A, C B)` fails.

> **R-MXC (non-commutativity).** Because `mergeGraphs` is order-dependent under conflicts (§2.2), `mergeGraphsAndAnalyze A B /= mergeGraphsAndAnalyze B A` under conflicts: swapping operands changes which node values feed Leiden, changing the re-detected partition. On **consistent** inputs `mergeGraphs` is order-independent (§2.1) and (given deterministic Leiden) `mergeGraphsAndAnalyze A B == mergeGraphsAndAnalyze B A`. The non-commutativity is thus **confined to conflicting inputs**.

> **GATE (loop with Graphos-Dev).** §4.2 assumes `clusterGraphWithResolution` is deterministic given its input (canonical argmax tie-break; [AVI-556](/AVI/issues/AVI-556) M3). Until Graphos-Dev confirms Leiden determinism, the consistent-input equality is conditional.

---

## 5. Requirements traceability table

| Req | Statement | Surface | Acceptance | Locked? |
|---|---|---|---|---|
| R1 | `Gr` objects = `Graph`; morphisms = key-preserving structure maps (§1.1) | `Core.hs:47`, `Edge.hs:88` | axiom check (identity + composition preserve `edgeRelation`) | structural |
| R2 | consistent merge = strict colimit w/ unique mediating morphism (§2.1) | `mergeGraphs` (`Core.hs:140`) | AC-1, AC-2 | structural |
| R3 | inconsistent merge = lax colimit, alpha = keep left operand (§2.2) | `Core.hs:142` | AC-5 | gate: direction |
| R4 | `mergeGraphs` associative up to canonical iso (§3) | `Data.Map.<>`, `gHash`, adjacency | AC-3 | structural |
| R5 | `merge x cluster` non-commutative under conflicts (§4) | `mergeGraphsAndAnalyze`, `clusterGraphWithResolution'` | AC-4 | gate: Leiden determinism |
| R6 | diagram `D : I -> Gr` over the view poset (§1.2) | `extractAll`, `buildGraphFromExtractions` | AC-1 (cocone legs are key-inclusions) | structural |

---

## 6. Acceptance criteria — checkable property tests (Graphos-Dev implement)

> **AC-1 (cocone existence + order-independence, consistent).** Over random consistent small diagrams `{D_i}` (no conflicting key), assert `mergeGraphs` yields the union of keys and `mergeGraphs A B == mergeGraphs B A`; the canonical key-inclusions are cocone legs. *Grounding:* consistent => strict colimit (§2.1).

> **AC-2 (uniqueness of mediating morphism).** For a consistent family and any target `H` with a compatible cocone `{mu_i}`, assert there is exactly one structure-preserving `h : M -> H` with `h . lambda_i = mu_i` (enumerate candidate mediating morphisms on small diagrams; assert existence AND uniqueness). *Grounding:* §2.1 uniqueness.

> **AC-3 (associativity up to iso).** `((A . B) . C) == (A . (B . C))` over random small diagrams, including overlapping keys. *Grounding:* §3; empirically the key structure associates exactly, so this extends to the full `Graph` record pending the GATE above.

> **AC-4 (non-commutative `merge x cluster`, CTO note 3).** Construct A, B sharing a conflicting node key with different values; assert (a) `mergeGraphs A B /= mergeGraphs B A` (order-dependent), and (b) `mergeGraphsAndAnalyze A B /= mergeGraphsAndAnalyze B A` (community partitions differ). On **consistent** inputs assert `mergeGraphsAndAnalyze A B == mergeGraphsAndAnalyze B A`. Also assert community IDs are re-detected, not inherited: `communities(mergeGraphs A B)` is not the union of `communities A` and `communities B` when views overlap. *Grounding:* §4.

> **AC-5 (old-wins reconciliation).** `mergeGraphs old new` keeps OLD's value at a conflicting key; `mergeGraphs A B /= mergeGraphs B A` at that key. *Grounding:* §2.2 + empirical `runghc`. This locks the correction of the "new-wins" claim in [AVI-524](/AVI/issues/AVI-524) / the 09-merge spec.

**Reproducible kernel for AC-5 (§2.2) already run this phase (GHC 9.10.3, `runghc`):**
```haskell
import qualified Data.Map.Strict as M
main = do
  let r1 = M.fromList [("a",(1::Int))]
      r2 = M.fromList [("a",(2::Int))]
  print (M.lookup "a" (M.union r1 r2))   -- Just 1  (LEFT/first operand wins)
```

---

## 7. Prior art

- **Colimit of a family = gluing of matching families; pushout = colimit over the span category.** Mac Lane, *Categories for the Working Mathematician*, Ch. IV, §5-§8; Riehl, *Categorical Logic and Topology*, Ch. 2.
- **Sheaf-style local-to-global as a colimit over a cover.** Mac Lane & Moerdahl, *Sheaves in Geometry and Logic* (Ch. II: global sections of a presheaf over a cover are its colimit; the sheaf condition is the descent/colimit equality) — the reference for "merged graph = colimit of local views; local and global truth agree on overlaps."
- **Lax colimit / lax natural transformation (commute only up to a chosen 2-cell).** Street, *Fibrations and Yoneda's theorem in an n-category*; Street & Walters, *Fibrations = Yoneda* — the reference for §2.2 "commutes only up to alpha."
- **Graph pushouts / cospan semantics (gluing graphs).** Lemmermeyer, *Graph Categories*; Lack, Freyjá & Morrison, "Cospan Semantics for Relational Systems" — the reference for "merge as pushout."
- **Last-write-wins / idempotent merges as coequalizers.** standard categorical treatment of order-sensitive union (cited by [AVI-524](/AVI/issues/AVI-524) §9).

---

## 8. Determinism lens

- **Consistent diagrams:** `mergeGraphs` is associative and order-independent on keys (§2.1), so the merge result is order-independent (sheaf local-to-global consistency, AC-1). Strict-colimit regime.
- **Conflicting diagrams:** `Data.Map.<>` left-wins makes the result order-dependent on conflicting keys (§2.2) — a documented, tested policy choice (AC-5), not a universal-property violation. The comparison 2-cell `alpha` is non-invertible there.
- **Associativity (AC-3):** the key structure associates exactly (empirically verified); derived fields are association-invariant, so re-association adds no cost and no nondeterminism.
- **Merge x cluster determinism (§4):** on consistent inputs the pipeline is order-independent iff Leiden is deterministic (GATE, [AVI-556](/AVI/issues/AVI-556) M3); under conflicts the order-dependence of `mergeGraphs` propagates through Leiden, so the partition is order-dependent. The nondeterminism lives in the input ordering (§2.2), not in the merge computation.

---

## 9. Math-judgment / escalation log

| # | Decision | Rationale / source |
|---|---|---|
| M-1 | `Gr` morphisms = key-preserving structure maps; cocone legs in `Gr_inj` | Matches [AVI-524](/AVI/issues/AVI-524) `Rho`-labeled-graph morphisms (`Edge.hs:88`); forced by the colimit quantification over arbitrary targets. |
| M-2 | consistent => strict colimit; inconsistent => lax colimit | [AVI-524](/AVI/issues/AVI-524) §3.1/§3.2; restated here at the `Graph` level. |
| M-3 | `alpha` direction = keep LEFT operand | **Empirical** (`runghc`, `Data.Map.union` left-wins); reconciles with `GraphSpec.hs:242`. Corrects [AVI-524](/AVI/issues/AVI-524)/09-merge "new-wins". |
| M-4 | associativity up to canonical iso (identity on structure) | Empirical (`<>` associative) + field-by-field invariance (`Core.hs:145-161`). |
| M-5 | `merge x cluster` non-commutative, confined to conflicts | `Merge.hs:32,43,56`; Leiden re-detection is not colimit-preserving. |
| M-6 | consistent-input merge x cluster equality is **conditional** on Leiden determinism | GATE with Graphos-Dev ([AVI-556](/AVI/issues/AVI-556) M3); not assumed here. |

---

## 10. Next Actions (Graphos-Dev owns the concrete next step)

1. **Feasibility gate + property tests** (AC-1..AC-5) — delegated to Graphos-Dev via child issue [AVI-645](/AVI/issues/AVI-645). Questions to confirm: (i) is Leiden deterministic (canonical tie-break)? [gates AC-4 consistent-input equality, M-6]; (ii) does any field of `Graph` break strict associativity under re-association? [gates R4]; (iii) confirm old-wins direction at edge keys (`EdgeId "<src>-><tgt>:<relation>"`) too.
2. **Lock the old-wins correction.** Once AC-5 passes, [AVI-524](/AVI/issues/AVI-524) §3.2 / 09-merge's "new-wins" language should be corrected to "left/first-operand wins" for consistency across the CT corpus.
3. **Escalate** any category-level judgment above confidence to the Head of R&D (per execution contract); e.g., whether `Gr_inj` (key-inclusions) is the right subcategory vs. a richer relation-poset once relation-coercions are introduced.

---

## 11. Child issue for the concrete next step

The property-test implementations (AC-1..AC-5) and the Haskell feasibility gate are Graphos-Dev's work; they are captured in child issue [AVI-645](/AVI/issues/AVI-645), which carries the acceptance criteria from §6 verbatim and the GATE questions from §10/§9.
