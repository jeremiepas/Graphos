/-
  Graphos formalization — Phase 2, extraction views and the merge operator.

  Requirements: `docs/math-requirements/AVI-620-colimit-functoriality-context-graph-lean-formalization-requirements.md`
  §0.2 (extraction-view objects), §0.3 (merge / fold operators), §1.1 (the category
  `Ext`), §1.2 (diagrams), §1.3 (cocones), §2 (`mergedExtraction_isColimit`).

  Pinned Haskell surface:
    * `mergeExtractions` — `src/Graphos/Domain/Graph/Core.hs:129-136`
    * `emptyExtraction`   — `src/Graphos/UseCase/Build.hs:12`
    * build-path fold      — `src/Graphos/UseCase/Build.hs:12` (`foldr`)
    * streaming fold sites — `src/Graphos/UseCase/Extract/Core.hs:124,466,506` (`foldl'`)

  Design note (why functions instead of `Data.Map`).
  A Haskell `Map K V` is used by `mergeExtractions` only through `Map.union` and
  `Map.lookup`; both are *lookup-level* operations. We therefore model an
  extraction map by its lookup function `K → Option V`. This is the standard
  extensional model of a `Data.Map` (no balancing/representation invariant to
  carry around), and it makes `Map.union` exactly the left-biased `Option`
  choice below:

    `Map.union a b`  ≙  `fun k => (a k).or (b k)`   -- first (old) operand wins

  which is the head-rnd correction M-5 pinning **first/old-wins**
  (`Data.Map.union` is left-biased), not second/new-wins.

  This module contains no theorem about the merge beyond its defining
  equations; the universal property lives in `Ext/Colimit.lean`.
-/
namespace GraphosFormalization

/-! ## §1.1 One map of an extraction view -/

/-- A single map of an extraction view, represented extensionally by its lookup
function. `none` means "key absent from this view"; `some v` means "key present
carrying value `v`". -/
abbrev ExtView (K V : Type) : Type := K → Option V

namespace ExtView

variable {K V : Type}

/-- The everywhere-undefined view. This is the bottom of `Ext`, and the base case
of both fold sites (`emptyExtraction`, `Build.hs:12`). -/
def empty : ExtView K V := fun _ => none

/-- Key-wise left-biased union: the **first** operand wins on a shared key.
This is exactly `Data.Map.union` as used by `mergeExtractions`
(`Core.hs:131-132`), pinning the first/old-wins precedence of §0.3 / M-5. -/
def union (A B : ExtView K V) : ExtView K V :=
  fun k =>
    match A k with
    | some v => some v
    | none => B k

@[simp] theorem union_apply_none {A B : ExtView K V} {k : K} (h : A k = none) :
    union A B k = B k := by
  simp [union, h]

@[simp] theorem union_apply_some {A B : ExtView K V} {k : K} {v : V} (h : A k = some v) :
    union A B k = some v := by
  simp [union, h]

@[simp] theorem empty_apply (k : K) : empty (V := V) k = none := rfl

/-- A **value-preserving view-embedding** as a predicate on lookups (§1.1):
every key present in `A` is present in `B` carrying the same value. Equivalently
`A`'s map is a sub-map of `B`'s. `Ext` is poset-enriched, so this proposition
*is* the hom-set `A ⟶ B` (at most one morphism per pair). -/
def le (A B : ExtView K V) : Prop := ∀ k, A k = none ∨ A k = B k

/-- **Consistency** (§1.3, "matching family"): two views agree on every shared
key. This is the hypothesis that turns the lax merge into a strict colimit. -/
def consistent (A B : ExtView K V) : Prop :=
  ∀ k v w, A k = some v → B k = some w → v = w

/-! ### Basic order facts -/

theorem le_refl (A : ExtView K V) : le A A := fun _ => Or.inr rfl

theorem le_trans {A B C : ExtView K V} (h₁ : le A B) (h₂ : le B C) : le A C := by
  intro k
  rcases h₁ k with h | h
  · exact Or.inl h
  · rcases h₂ k with h' | h'
    · exact Or.inl (h.trans h')
    · exact Or.inr (h.trans h')

theorem le_antisymm {A B : ExtView K V} (h₁ : le A B) (h₂ : le B A) : A = B := by
  funext k
  rcases h₁ k with h | h
  · rcases h₂ k with h' | h'
    · exact h.trans h'.symm
    · exact h'.symm
  · exact h

theorem le_union_left (A B : ExtView K V) : le A (union A B) := by
  intro k
  rcases hA : A k with _ | v
  · exact Or.inl rfl
  · exact Or.inr (union_apply_some hA).symm

theorem le_union_right {A B : ExtView K V} (h : consistent A B) : le B (union A B) := by
  intro k
  rcases hB : B k with _ | w
  · exact Or.inl rfl
  · rcases hA : A k with _ | v
    · exact Or.inr (by rw [union_apply_none hA, hB])
    · have hvw : v = w := h k v w hA hB
      exact Or.inr (by rw [union_apply_some hA, hvw])

/-- Left-biased union has `empty` as a left identity. -/
@[simp] theorem union_empty_left (A : ExtView K V) : union empty A = A := by
  funext k
  simp [union]

/-- `emptyExtraction` is also a right identity (`A ∪ ∅ = A`). -/
@[simp] theorem union_empty_right (A : ExtView K V) : union A empty = A := by
  funext k
  cases hA : A k <;> simp [union, hA]

/-- Left-biased union is **associative**: re-associating a fold of views does
not change the pointwise result. Note this is re-association, not permutation:
permuting the family moves which view is *first*, which is where precedence
lives (see `mergedExtraction_isColimit` for the consistent case). -/
theorem union_assoc (A B C : ExtView K V) :
    union (union A B) C = union A (union B C) := by
  funext k
  cases hA : A k with
  | none => simp [union, hA]
  | some v => simp [union, hA]

/-- Two compatible cocone legs determine the merged leg: the colimit is the
*least* upper bound. -/
theorem le_union {A B X : ExtView K V} (hA : le A X) (hB : le B X) : le (union A B) X := by
  intro k
  rcases hAk : A k with _ | v
  · rw [union_apply_none hAk]
    exact hB k
  · rw [union_apply_some hAk]
    have := hA k
    rw [hAk] at this
    exact this

/-! ## §1.3 n-way union over a view family (the fold sites) -/

/-- The key-wise union of a view family: the exact reading of
`foldr mergeExtractions emptyExtraction` (`Build.hs:12`). Since `union` is
associative and has `empty` as identity, this is order-independent as a
*pointwise* operation; the list-level precedence question (which view dominates
a conflicting key) is AVI-1303's subject, not this module's. -/
def unionList (l : List (ExtView K V)) : ExtView K V := l.foldr union empty

@[simp] theorem unionList_nil : unionList ([] : List (ExtView K V)) = empty := rfl

@[simp] theorem unionList_cons (A : ExtView K V) (l : List (ExtView K V)) :
    unionList (A :: l) = union A (unionList l) := rfl

/-- Every value surviving the n-way union came from one specific member of the
family. This is the "gluing" half of the sheaf reading (§6): `M`'s keys are the
union of the members' keys. -/
theorem exists_mem_of_unionList_eq_some {l : List (ExtView K V)} {k : K} {v : V}
    (h : unionList l k = some v) : ∃ B ∈ l, B k = some v := by
  induction l with
  | nil => simp [unionList, empty] at h
  | cons A l ih =>
      rw [unionList_cons] at h
      rcases hA : A k with _ | w
      · rw [union_apply_none hA] at h
        obtain ⟨B, hB, hBk⟩ := ih h
        exact ⟨B, List.mem_cons_of_mem A hB, hBk⟩
      · have hSome : ((some w : Option V) = some v) := by
          rwa [union_apply_some hA] at h
        have hwv : w = v := Option.some.inj hSome
        exact ⟨A, List.mem_cons_self A l, by rw [hA, hwv]⟩

/-- A view consistent with every member of a family is consistent with the
family's union. -/
theorem consistent_unionList_right {l : List (ExtView K V)} {A : ExtView K V}
    (h : ∀ B ∈ l, consistent A B) : consistent A (unionList l) := by
  intro k v w hA hU
  obtain ⟨B, hB, hBk⟩ := exists_mem_of_unionList_eq_some hU
  exact h B hB k v w hA hBk

/-- Each family member embeds into the union — the leg `ιᵢ` of the canonical
cocone, under the family's pairwise consistency. -/
theorem le_unionList_of_mem {l : List (ExtView K V)}
    (hcons : ∀ A ∈ l, ∀ B ∈ l, consistent A B) {A : ExtView K V} (hA : A ∈ l) :
    le A (unionList l) := by
  induction l with
  | nil => simp at hA
  | cons B l ih =>
      rw [unionList_cons]
      rcases List.mem_cons.mp hA with rfl | hA
      · exact le_union_left A (unionList l)
      · have hcons' : ∀ A' ∈ l, ∀ B' ∈ l, consistent A' B' :=
          fun A' hA' B' hB' =>
            hcons A' (List.mem_cons_of_mem B hA') B' (List.mem_cons_of_mem B hB')
        have hB : consistent B (unionList l) :=
          consistent_unionList_right
            (fun C hC => hcons B (List.mem_cons_self B l) C (List.mem_cons_of_mem B hC))
        exact le_trans (ih hcons' hA) (le_union_right hB)

/-- The union is below any common upper bound of the family: this is the
mediating map, i.e. `M` is the *least* upper bound. -/
theorem le_unionList {l : List (ExtView K V)} {X : ExtView K V}
    (h : ∀ A ∈ l, le A X) : le (unionList l) X := by
  induction l with
  | nil => intro k; exact Or.inl rfl
  | cons A l ih =>
      rw [unionList_cons]
      exact le_union (h A (List.mem_cons_self A l))
        (ih (fun B hB => h B (List.mem_cons_of_mem A hB)))

end ExtView

/-! ## §1.1 The objects of `Ext`: extraction views -/

/-- An extraction view (§0.2): the node map and the edge map of one
`Extraction` (`Domain/Graph/Core.hs:100`). Node keys and edge keys are kept
separate because `EdgeId` is its own key space (`EdgeId Text`, `Convert.hs:165`),
and the merge acts independently on the two maps. -/
structure Extraction (NodeKey EdgeKey Node Edge : Type) where
  /-- `extractionNodes` — `extNodes` (`Core.hs:131`). -/
  extNodes : ExtView NodeKey Node
  /-- `extractionEdges` — `extEdges` (`Core.hs:132`). -/
  extEdges : ExtView EdgeKey Edge

namespace Extraction

variable {NK EK N E : Type}

/-- Componentwise equality of extraction views. -/
theorem eq_of_components {A B : Extraction NK EK N E}
    (h₁ : A.extNodes = B.extNodes) (h₂ : A.extEdges = B.extEdges) : A = B := by
  obtain ⟨a₁, a₂⟩ := A
  obtain ⟨b₁, b₂⟩ := B
  simp only at h₁ h₂ ⊢
  rw [h₁, h₂]

/-- `Ext` is poset-enriched (§1.1): a morphism `A ⟶ B` is the assertion that
`A`'s keys are present in `B` with the same values. At most one morphism per
pair, hence the `Prop`-valued structure. -/
def le (A B : Extraction NK EK N E) : Prop :=
  ExtView.le A.extNodes B.extNodes ∧ ExtView.le A.extEdges B.extEdges

/-- Two extraction views agree on every shared key (§3.1 hypothesis, §1.3
"compatible cocone"). -/
def consistent (A B : Extraction NK EK N E) : Prop :=
  ExtView.consistent A.extNodes B.extNodes ∧ ExtView.consistent A.extEdges B.extEdges

end Extraction

/-- The empty extraction view: the colimit bottom (`emptyExtraction`,
`Build.hs:12`). -/
def emptyExtraction {NK EK N E : Type} : Extraction NK EK N E :=
  ⟨ExtView.empty, ExtView.empty⟩

/-- `mergeExtractions`: key-wise `Map.union` on both maps, **first operand wins**
(`Domain/Graph/Core.hs:129-136`; head-rnd correction M-5). -/
def mergeExtractions {NK EK N E : Type} (A B : Extraction NK EK N E) : Extraction NK EK N E :=
  ⟨ExtView.union A.extNodes B.extNodes, ExtView.union A.extEdges B.extEdges⟩

@[simp] theorem mergeExtractions_extNodes {NK EK N E : Type}
    (A B : Extraction NK EK N E) :
    (mergeExtractions A B).extNodes = ExtView.union A.extNodes B.extNodes := rfl

@[simp] theorem mergeExtractions_extEdges {NK EK N E : Type}
    (A B : Extraction NK EK N E) :
    (mergeExtractions A B).extEdges = ExtView.union A.extEdges B.extEdges := rfl

/-- The n-way merge over an explicit family: `M = ⋃ᵢ Eᵢ` of §2, i.e. the
key-wise union of the family's node maps and edge maps. Defined componentwise
because the two maps merge independently; `foldMerge_eq_foldr` shows this is
exactly the build-path fold `foldr mergeExtractions emptyExtraction`
(`Build.hs:12`). -/
def foldMerge {NK EK N E : Type} (l : List (Extraction NK EK N E)) : Extraction NK EK N E :=
  ⟨ExtView.unionList (l.map Extraction.extNodes), ExtView.unionList (l.map Extraction.extEdges)⟩

@[simp] theorem foldMerge_nil {NK EK N E : Type} :
    foldMerge ([] : List (Extraction NK EK N E)) = emptyExtraction := rfl

/-- `M = ⋃ᵢ Eᵢ` is literally the build-path fold of `mergeExtractions` over the
family (`UseCase/Build.hs:12`: `foldr mergeExtractions emptyExtraction`), so the
colimit object formalized in `Ext/Colimit.lean` is the value the Haskell build
path actually computes on a consistent family. -/
theorem foldMerge_eq_foldr {NK EK N E : Type} (l : List (Extraction NK EK N E)) :
    foldMerge l = l.foldr mergeExtractions emptyExtraction := by
  induction l with
  | nil => rfl
  | cons A l ih =>
      show foldMerge (A :: l) = mergeExtractions A (List.foldr mergeExtractions emptyExtraction l)
      rw [← ih]
      rfl

end GraphosFormalization
