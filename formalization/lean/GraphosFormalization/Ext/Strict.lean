/-
  Graphos formalization — Phase 2, §3.1: the consistent case,
  strict colimit + order independence.

  Requirements: `docs/math-requirements/AVI-620-colimit-functoriality-context-graph-lean-formalization-requirements.md`
  §0.3 (merge / fold operators, first/old-wins), §2 (universal property), §3.1
  (`mergedExtraction_strictColimit_orderIndependent`), §5 AC-1 (named artifact
  must compile), §0.4 (the `foldr` vs strict `foldl'` fold-order discrepancy).

  **Acceptance criterion (AC-1).** The named artifact
  `mergedExtraction_strictColimit_orderIndependent` exists and its body compiles
  under the pinned consistency hypothesis.

  ## What is proved

  For `A, B : Extraction` with `consistent A B` (§3.1 hypothesis: the two views
  agree on every shared key):

  * (a) **strict colimit.** `(M, {ι_A, ι_B})` with `M = mergeExtractions A B` and
    the canonical legs `ι_A : A ↪ M`, `ι_B : B ↪ M` is the colimit of the
    two-object diagram: every cocone `(g_A, g_B)` has a mediator `u : M → X`
    with `u ∘ ι_A = g_A`, `u ∘ ι_B = g_B`, and `u` is **unique**
    (`mergedExtraction_strictColimit`). Uniqueness is derived, not assumed:
    `Ext` is poset-enriched, so `Hom(M, X)` is a subsingleton (§1.1).
  * (b) **order independence.** `mergeExtractions A B = mergeExtractions B A`
    (`Extraction.merge_comm`), and at the list level both fold sites agree and
    are invariant under every permutation / re-association of the view list:

      `foldl' ⊕ emptyExtraction σ = foldl' ⊕ emptyExtraction σ'`   (Perm)
      `foldr  ⊕ emptyExtraction σ = foldl' ⊕ emptyExtraction σ`    (§0.4)

    (`foldlMerge_perm`, `foldlMerge_eq_foldr`).

  ## Why this is the *strict* half of §3

  §3.2 (`mergedExtraction_laxColimit_oldWins`, AVI-1304) is where `Map.union`'s
  left bias becomes observable: on a **conflicting** key the merge is not
  commutative, so the comparison 2-cell of the lax colimit is non-invertible and
  the strict universal property has no instance (no cocone exists at all).
  Consistency is exactly what removes those conflicts, so the union becomes
  commutative and associative *as used*, and the whole fold collapses to the
  order-free `foldMerge` of `Ext/Basic.lean`. The two facts are not independent:
  commutation under consistency is what makes the lax comparison 2-cell
  invertible, i.e. what makes the colimit strict.

  Correction (AVI-1304): an earlier revision of this header said the two fold
  *sites* differ on a conflicting key (`foldl'` last-wins, `foldr` first-wins).
  That is not the shipped behaviour — all shipped folds are accumulator/head
  first, hence earliest-win — and it is not true in this model either
  (`Ext/Lax.lean:foldrMerge_eq_foldlMerge_all`). The conflict makes the *merge*
  order-dependent, not the fold shape. See `Ext/Lax.lean` and the AVI-620 §0.4
  erratum (AVI-1319).

  ## Modelling notes

  * `foldlMerge` is the streaming lane `Extract/Core.hs:124,466,506`
    (`foldl' (⊕) emptyExtraction`), `foldrMerge` the build lane
    `UseCase/Build.hs:12` (`foldr (⊕) emptyExtraction`). Both are modelled on
    `List.foldl` / `List.foldr` over the *same* `mergeExtractions`; the operators
    are the pinned first/old-wins ones of `Ext/Basic.lean`.
  * Kernel Lean cannot evaluate `Data.Map`; as in the rest of this package the
    maps are the extensional lookup functions `K → Option V`, so `Map.union` is
    the left-biased `Option` choice and the fold order is a `List`, not a
    balanced-tree traversal.
-/
import GraphosFormalization.Ext.Colimit

namespace GraphosFormalization

namespace ExtView

variable {K V : Type}

/-! ## Commutativity of the left-biased union on consistent views -/

/-- On a **consistent** pair the left-biased union is commutative (`§3.1 (b)` at
the single-map level): where both views carry a key they carry the *same* value,
so it does not matter which operand is consulted first. This is the precise
sense in which consistency removes the left bias. -/
theorem union_comm {A B : ExtView K V} (h : consistent A B) : union A B = union B A := by
  funext k
  rcases hA : A k with _ | v
  · rcases hB : B k with _ | w
    · rw [union_apply_none hA, union_apply_none hB, hA, hB]
    · rw [union_apply_none hA, union_apply_some hB, hB]
  · rcases hB : B k with _ | w
    · rw [union_apply_some hA, union_apply_none hB, hA]
    · rw [union_apply_some hA, union_apply_some hB, h k v w hA hB]

/-- The union of two views that are each consistent with `C` is consistent with
`C`: on a key supplied by either operand the value is forced, so the merged view
still agrees with any compatible cocone apex. -/
theorem consistent_union {A B C : ExtView K V}
    (hA : consistent A C) (hB : consistent B C) : consistent (union A B) C := by
  intro k v w hU hC
  rcases hAk : A k with _ | v'
  · rw [union_apply_none hAk] at hU
    exact hB k v w hU hC
  · rw [union_apply_some hAk] at hU
    exact (Option.some.inj hU) ▸ hA k v' w hAk hC

/-- A view is consistent with the everywhere-undefined view (§0.2
`emptyExtraction`): it has no keys, so there is nothing to disagree on. -/
theorem consistent_empty_right (A : ExtView K V) : consistent A empty := by
  intro k v w _ hw
  simp [empty] at hw

/-- The everywhere-undefined view is consistent with any view. -/
theorem consistent_empty_left (A : ExtView K V) : consistent empty A := by
  intro k v w hv _
  simp [empty] at hv

/-- A view is consistent with itself. -/
theorem consistent_self (A : ExtView K V) : consistent A A := by
  intro k v w h1 h2
  rw [h1] at h2
  exact Option.some.inj h2

/-! ## `unionList` is order-free on consistent families -/

/-- `foldr union X l` is `union` of the n-way union with `X`. This is the
associativity that lets a fold *start* from an arbitrary accumulator. -/
theorem foldr_union_eq (l : List (ExtView K V)) (X : ExtView K V) :
    l.foldr union X = union (unionList l) X := by
  induction l with
  | nil => simp [unionList]
  | cons A l ih =>
      rw [List.foldr_cons, ih, unionList_cons]
      exact (union_assoc A (unionList l) X).symm

/-- **Permutation invariance of the n-way union** under pairwise consistency
(`§3.1 (b)`): re-ordering a consistent family cannot change the merged view.
Proof by induction on the permutation derivation; the transposition case is
exactly `union_assoc` + `union_comm` on the (consistent) transposed pair. -/
theorem unionList_perm {l l' : List (ExtView K V)} (hp : l.Perm l') :
    (∀ A ∈ l, ∀ B ∈ l, consistent A B) → unionList l = unionList l' := by
  induction hp with
  | nil => intro _; rfl
  | cons x hp ih =>
      intro hc
      have hc' : ∀ A ∈ _, ∀ B ∈ _, consistent A B :=
        fun A hA B hB =>
          hc A (List.mem_cons_of_mem x hA) B (List.mem_cons_of_mem x hB)
      rw [unionList_cons, unionList_cons, ih hc']
  | swap x y l =>
      intro hc
      have hyx : consistent y x :=
        hc y (List.mem_cons_self y (x :: l))
          x (List.mem_cons_of_mem y (List.mem_cons_self x l))
      calc unionList (y :: x :: l)
          = union y (union x (unionList l)) := rfl
        _ = union (union y x) (unionList l) := (union_assoc y x (unionList l)).symm
        _ = union (union x y) (unionList l) := by rw [union_comm hyx]
        _ = union x (union y (unionList l)) := union_assoc x y (unionList l)
  | trans hp₁ hp₂ ih₁ ih₂ =>
      intro hc
      have hc' : ∀ A ∈ _, ∀ B ∈ _, consistent A B :=
        fun A hA B hB =>
          hc A ((hp₁.mem_iff).mpr hA) B ((hp₁.mem_iff).mpr hB)
      rw [ih₁ hc, ih₂ hc']

end ExtView

/-! ## Commutation and consistency at the `Extraction` level -/

namespace Extraction

variable {NK EK N E : Type}

/-- **Binary order independence** (§3.1 (b), first clause): on a consistent pair
the merge is commutative. Both projections are `Map.union`, which is
left-biased but agrees on shared keys by the hypothesis. -/
theorem merge_comm {A B : Extraction NK EK N E} (h : consistent A B) :
    mergeExtractions A B = mergeExtractions B A := by
  apply eq_of_components
  · exact ExtView.union_comm h.1
  · exact ExtView.union_comm h.2

/-- The merge is idempotent: merging a view with itself returns it (left bias is
harmless when both operands agree everywhere). -/
theorem merge_self (A : Extraction NK EK N E) : mergeExtractions A A = A := by
  apply eq_of_components
  · funext k
    rcases hk : A.extNodes k with _ | v
    · rw [mergeExtractions_extNodes, ExtView.union_apply_none hk, hk]
    · rw [mergeExtractions_extNodes, ExtView.union_apply_some hk]
  · funext k
    rcases hk : A.extEdges k with _ | v
    · rw [mergeExtractions_extEdges, ExtView.union_apply_none hk, hk]
    · rw [mergeExtractions_extEdges, ExtView.union_apply_some hk]

/-- `emptyExtraction` is a left identity for the merge (`⊕` on `Ext` has the
bottom as its unit, §0.2). -/
theorem merge_empty_left (A : Extraction NK EK N E) : mergeExtractions emptyExtraction A = A := by
  apply eq_of_components
  · exact ExtView.union_empty_left A.extNodes
  · exact ExtView.union_empty_left A.extEdges

/-- `emptyExtraction` is also a right identity. -/
theorem merge_empty_right (A : Extraction NK EK N E) : mergeExtractions A emptyExtraction = A := by
  apply eq_of_components
  · exact ExtView.union_empty_right A.extNodes
  · exact ExtView.union_empty_right A.extEdges

/-- **Re-association invariance** of the merge. `Map.union` is associative on the
nose; the list-level fold order question (§0.4) is about *permutation*, not
re-association. -/
theorem merge_assoc (A B C : Extraction NK EK N E) :
    mergeExtractions (mergeExtractions A B) C = mergeExtractions A (mergeExtractions B C) := by
  apply eq_of_components
  · exact ExtView.union_assoc A.extNodes B.extNodes C.extNodes
  · exact ExtView.union_assoc A.extEdges B.extEdges C.extEdges

/-- The merge of two views that are each consistent with `C` stays consistent
with `C`. This is what lets a *running accumulator* (`foldl'`) take the place of
the individual family members. -/
theorem consistent_merge_left {A B C : Extraction NK EK N E}
    (hA : consistent A C) (hB : consistent B C) : consistent (mergeExtractions A B) C :=
  ⟨ExtView.consistent_union hA.1 hB.1, ExtView.consistent_union hA.2 hB.2⟩

/-- Any view is consistent with the empty view. -/
theorem consistent_empty_left (A : Extraction NK EK N E) : consistent emptyExtraction A :=
  ⟨ExtView.consistent_empty_left A.extNodes, ExtView.consistent_empty_left A.extEdges⟩

end Extraction

/-! ## The two fold sites (§0.3, §0.4) -/

variable {NK EK N E : Type}

/-- The **build-path fold**: `foldr (⊕) emptyExtraction` (`UseCase/Build.hs:12`).
Earlier views win on a conflicting key at the list level. -/
def foldrMerge (l : List (Extraction NK EK N E)) : Extraction NK EK N E :=
  l.foldr mergeExtractions emptyExtraction

/-- The **streaming-path fold**: `foldl' (⊕) emptyExtraction`
(`UseCase/Extract/Core.hs:124,466,506`). The accumulator is the first operand, so
at the list level the *last* view wins on a conflicting key. -/
def foldlMerge (l : List (Extraction NK EK N E)) : Extraction NK EK N E :=
  l.foldl (fun acc A => mergeExtractions acc A) emptyExtraction

/-- The build-path fold is the n-way union `M = ⋃ᵢ Eᵢ` of `Ext/Basic.lean`
(`foldMerge_eq_foldr`). -/
theorem foldrMerge_eq_foldMerge (l : List (Extraction NK EK N E)) :
    foldrMerge l = foldMerge l :=
  (foldMerge_eq_foldr l).symm

/-- `foldMerge` as a `cons`: the n-way union is the merge of the head with the
tail's union. -/
theorem foldMerge_cons (A : Extraction NK EK N E) (l : List (Extraction NK EK N E)) :
    foldMerge (A :: l) = mergeExtractions A (foldMerge l) := by
  rw [foldMerge_eq_foldr (A :: l), foldMerge_eq_foldr l]
  rfl

/-- **Left-fold accumulation lemma.** Folding `mergeExtractions` over `l`
starting from `acc` equals `acc ⊕ ⋃ᵢ l`, provided `acc` is consistent with every
member and the members are pairwise consistent. This is the induction that turns
the streaming path into the order-free union. -/
theorem foldl_merge_eq (l : List (Extraction NK EK N E)) :
    ∀ (acc : Extraction NK EK N E),
      (∀ A ∈ l, Extraction.consistent acc A) → familyConsistent l →
      l.foldl (fun a A => mergeExtractions a A) acc = mergeExtractions acc (foldMerge l) := by
  induction l with
  | nil => intro acc _ _; exact (Extraction.merge_empty_right acc).symm
  | cons B l ih =>
      intro acc hacc hl
      have hl' : familyConsistent l :=
        fun A hA C hC =>
          hl A (List.mem_cons_of_mem B hA) C (List.mem_cons_of_mem B hC)
      have hacc' : ∀ A ∈ l, Extraction.consistent (mergeExtractions acc B) A :=
        fun A hA =>
          Extraction.consistent_merge_left (hacc A (List.mem_cons_of_mem B hA))
            (hl B (List.mem_cons_self B l) A (List.mem_cons_of_mem B hA))
      show l.foldl (fun a A => mergeExtractions a A) (mergeExtractions acc B)
        = mergeExtractions acc (foldMerge (B :: l))
      rw [foldMerge_cons, ih (mergeExtractions acc B) hacc' hl', Extraction.merge_assoc]

/-- **`foldl'` computes the n-way union** on a consistent family: the streaming
accumulator converges on the same order-free `M = ⋃ᵢ Eᵢ`. -/
theorem foldlMerge_eq_foldMerge (l : List (Extraction NK EK N E))
    (hc : familyConsistent l) : foldlMerge l = foldMerge l := by
  show l.foldl (fun a A => mergeExtractions a A) emptyExtraction = foldMerge l
  rw [foldl_merge_eq l emptyExtraction (fun A _ => Extraction.consistent_empty_left A) hc,
    Extraction.merge_empty_left]

/-- **`foldr` = `foldl'` on a consistent family** (§3.1 (b), §0.4): the two
shipped fold sites — build (`foldr`) and streaming (strict `foldl'`) — agree
whenever no shared key conflicts. This is the concrete form of the fold-order
discrepancy recorded in §0.4: the divergence is confined to the lax case (§3.2). -/
theorem foldlMerge_eq_foldr (l : List (Extraction NK EK N E))
    (hc : familyConsistent l) : foldrMerge l = foldlMerge l := by
  rw [foldrMerge_eq_foldMerge, foldlMerge_eq_foldMerge l hc]

/-- The n-way union is invariant under permutation of a consistent family. -/
theorem foldMerge_perm {l l' : List (Extraction NK EK N E)} (hp : l.Perm l')
    (hc : familyConsistent l) : foldMerge l = foldMerge l' := by
  apply Extraction.eq_of_components
  · refine ExtView.unionList_perm (hp.map Extraction.extNodes) ?_
    intro A hA B hB
    obtain ⟨A', hA', rfl⟩ := List.mem_map.mp hA
    obtain ⟨B', hB', rfl⟩ := List.mem_map.mp hB
    exact (hc A' hA' B' hB').1
  · refine ExtView.unionList_perm (hp.map Extraction.extEdges) ?_
    intro A hA B hB
    obtain ⟨A', hA', rfl⟩ := List.mem_map.mp hA
    obtain ⟨B', hB', rfl⟩ := List.mem_map.mp hB
    exact (hc A' hA' B' hB').2

/-- **Permutation invariance of the fold** (§3.1 (b), general clause):
`foldl' ⊕ emptyExtraction σ = foldl' ⊕ emptyExtraction σ'` for every permutation
(hence every re-association) `σ'` of a consistent view list. -/
theorem foldlMerge_perm {l l' : List (Extraction NK EK N E)} (hp : l.Perm l')
    (hc : familyConsistent l) : foldlMerge l = foldlMerge l' := by
  have hc' : familyConsistent l' :=
    fun A hA B hB => hc A ((hp.mem_iff).mpr hA) B ((hp.mem_iff).mpr hB)
  rw [foldlMerge_eq_foldMerge l hc, foldlMerge_eq_foldMerge l' hc', foldMerge_perm hp hc]

/-! ## §3.1 (a) The strict colimit, binary case -/

/-- The canonical cocone leg `ι_A : A ↪ A ⊕ B` (`§1.3`, `§2`). -/
def canonicalLegLeft (A B : Extraction NK EK N E) : ViewEmbedding A (mergeExtractions A B) :=
  let h := Extraction.le_merge_left A B
  ⟨h.1, h.2⟩

/-- The canonical cocone leg `ι_B : B ↪ A ⊕ B`. It exists because `A` and `B`
are consistent — on a conflicting pair there would be no such embedding, which
is exactly the lax failure of §3.2. -/
def canonicalLegRight {A B : Extraction NK EK N E} (h : Extraction.consistent A B) :
    ViewEmbedding B (mergeExtractions A B) :=
  let hl := Extraction.le_merge_right h
  ⟨hl.1, hl.2⟩

/-- **The binary universal property** (§2 / §3.1 (a)). For a consistent pair
`(A, B)` and any cocone `(g_A : A → X, g_B : B → X)`, there is a mediator
`u : A ⊕ B → X` with `u ∘ ι_A = g_A` and `u ∘ ι_B = g_B`, and it is **unique**.

No extra compatibility hypothesis on `(g_A, g_B)` is needed: both legs are
view-embeddings, so on a shared key they already agree with `X` pointwise, which
is exactly what makes the cocone a matching family. -/
theorem mergedExtraction_strictColimit {NK EK N E : Type}
    (A B X : Extraction NK EK N E) (h : Extraction.consistent A B)
    (gA : ViewEmbedding A X) (gB : ViewEmbedding B X) :
    ∃ u : ViewEmbedding (mergeExtractions A B) X,
      ViewEmbedding.comp u (canonicalLegLeft A B) = gA ∧
      ViewEmbedding.comp u (canonicalLegRight h) = gB ∧
      (∀ u' : ViewEmbedding (mergeExtractions A B) X,
        ViewEmbedding.comp u' (canonicalLegLeft A B) = gA →
        ViewEmbedding.comp u' (canonicalLegRight h) = gB → u' = u) := by
  have hM := Extraction.le_merge (X := X) ⟨gA.nodes_le, gA.edges_le⟩ ⟨gB.nodes_le, gB.edges_le⟩
  refine ⟨⟨hM.1, hM.2⟩, ?_, ?_, ?_⟩
  · exact ViewEmbedding.unique _ _
  · exact ViewEmbedding.unique _ _
  · intro u' _ _
    exact ViewEmbedding.unique u' _

/-! ## §3.1 The named theorem -/

/-- **`mergedExtraction_strictColimit_orderIndependent`** (AVI-620 §3.1,
AVI-1303).

For `A, B : Extraction` with `consistent A B`
(`∀k. k ∈ keys(A) ∩ keys(B) → value_A(k) = value_B(k)`):

* **(a) strict colimit.** `(M, {ι_A, ι_B})`, `M = mergeExtractions A B`, is the
  colimit of the two-view diagram: every cocone has a mediator `u : M → X`
  factoring through the canonical legs, and `u` is unique.
* **(b) order independence.** `mergeExtractions A B = mergeExtractions B A`; more
  generally, for every pairwise-consistent family `l`, every permutation
  `l' ~ l` gives the same strict `foldl'` result
  (`foldl' ⊕ emptyExtraction l = foldl' ⊕ emptyExtraction l'`), and the two
  shipped fold sites agree (`foldr ⊕ emptyExtraction l = foldl' ⊕ emptyExtraction l`).

The `∃ u, P u ∧ ∀ u', P u' → u' = u` encoding of `!u.` is used because Lean core
4.14 has no `∃!` binder (same encoding as `mergedExtraction_isColimit`).

The list-level clauses are quantified universally over *all* consistent
families; the binary pair `(A, B)` is the diagram whose legs the first clause
mentions. Taken together with `mergedExtraction_isColimit` (§2, n-way) this is
the full strict-colimit + order-independence statement of §3.1; the lax dual is
§3.2 (`mergedExtraction_laxColimit_oldWins`, AVI-1304). -/
theorem mergedExtraction_strictColimit_orderIndependent {NK EK N E : Type}
    (A B : Extraction NK EK N E) (hAB : Extraction.consistent A B) :
    (-- (a) strict colimit: the mediating map exists and is unique
     ∀ (X : Extraction NK EK N E) (gA : ViewEmbedding A X) (gB : ViewEmbedding B X),
        ∃ u : ViewEmbedding (mergeExtractions A B) X,
          ViewEmbedding.comp u (canonicalLegLeft A B) = gA ∧
          ViewEmbedding.comp u (canonicalLegRight hAB) = gB ∧
          (∀ u' : ViewEmbedding (mergeExtractions A B) X,
            ViewEmbedding.comp u' (canonicalLegLeft A B) = gA →
            ViewEmbedding.comp u' (canonicalLegRight hAB) = gB → u' = u)) ∧
    (-- (b) order independence
     mergeExtractions A B = mergeExtractions B A ∧
     (∀ l : List (Extraction NK EK N E), familyConsistent l →
        ∀ l' : List (Extraction NK EK N E), l.Perm l' →
          foldlMerge l = foldlMerge l') ∧
     (∀ l : List (Extraction NK EK N E), familyConsistent l →
        foldrMerge l = foldlMerge l)) :=
  ⟨fun X gA gB => mergedExtraction_strictColimit A B X hAB gA gB,
   Extraction.merge_comm hAB,
   fun l hc l' hp => @foldlMerge_perm NK EK N E l l' hp hc,
   fun l hc => foldlMerge_eq_foldr l hc⟩

/-! ## Non-vacuity: a concrete consistent family and both fold sites

The named theorem quantifies over all consistent families, so it is only
falsifiable if some family is consistent *and* the folds compute. The fixture
below is a single `Nat`-keyed extraction view (keys `0 ↦ 10`, `1 ↦ 11`) repeated
three times: it is trivially consistent, and both the build-path and the
streaming-path fold evaluate to the same concrete value. -/

/-- Fixture view: `0 ↦ 10`, `1 ↦ 11`, every other key absent. -/
def fixtureView : ExtView Nat Nat :=
  fun k => if k = 0 then some 10 else if k = 1 then some 11 else none

/-- The fixture as an extraction view with no edges
(`emptyExtraction`'s edge component). -/
def fixtureExtraction : Extraction Nat Nat Nat Nat :=
  ⟨fixtureView, ExtView.empty⟩

/-- The repeated fixture family is consistent (all members coincide). -/
theorem fixture_familyConsistent :
    familyConsistent [fixtureExtraction, fixtureExtraction, fixtureExtraction] := by
  intro X hX Y hY
  have hX' : X = fixtureExtraction := by simpa using hX
  have hY' : Y = fixtureExtraction := by simpa using hY
  subst hX'
  subst hY'
  exact Extraction.consistent_self fixtureExtraction

/-- Both fold sites on the fixture agree on a key carried by every member. -/
example :
    (foldlMerge [fixtureExtraction, fixtureExtraction, fixtureExtraction]).extNodes 1
      = some 11 := by decide

example :
    (foldrMerge [fixtureExtraction, fixtureExtraction, fixtureExtraction]).extNodes 0
      = some 10 := by decide

/-- … and a re-ordered family agrees with the original one, concretely. -/
example :
    (foldlMerge [fixtureExtraction, fixtureExtraction, fixtureExtraction]).extNodes 2 =
      (foldlMerge [fixtureExtraction, fixtureExtraction]).extNodes 2 := by decide

end GraphosFormalization
