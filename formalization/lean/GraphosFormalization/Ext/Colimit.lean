/-
  Graphos formalization — Phase 2, the colimit universal property.

  Requirements: `docs/math-requirements/AVI-620-colimit-functoriality-context-graph-lean-formalization-requirements.md`
  §1.1 (`Ext` is poset-enriched), §1.2 (diagram), §1.3 (cocone / matching family),
  §2 (`mergedExtraction_isColimit`), §5 AC-1 (named artifact must compile),
  §6 (gluing of matching families).

  **Acceptance criterion (AC-1).** The named artifact `mergedExtraction_isColimit`
  exists and its body compiles under the pinned consistency hypothesis.

  ## What is proved, and why the uniqueness half is not vacuous

  §1.1 pins `Ext` as **poset-enriched**: a morphism `A ⟶ B` exists exactly when
  `A ⊆ B` (value-preserving view-embedding), so each hom-set has *at most one*
  element. The universal property therefore decomposes into:

  1. **Legs exist** — for a pairwise-consistent family, every `Eᵢ` embeds into
     `M = foldMerge l` (`canonicalLeg`).
  2. **The mediator exists** — every compatible cocone `{gᵢ : Eᵢ → X}` factors
     through `M`: `M ⊆ X` (`Extraction.le_foldMerge`).
  3. **The mediator is unique** — because `Hom(M, X)` is a subsingleton
     (`ViewEmbedding.unique`), any two mediators with the same composites coincide.
     Uniqueness is *derived* from the poset structure; it is not assumed by fiat.

  Existence (2) is the real content: it is the statement that the key-wise union
  is the **least** upper bound of the family, and it is where consistency is
  used — on a conflicting key the left-biased union silently keeps the first
  operand's value, so no cocone exists at all (§3.2, AVI-1304's subject).

  The composite `u ∘ ιᵢ = gᵢ` is an equation between two morphisms `Eᵢ → X`, hence
  between two propositions; it holds by proof irrelevance. Naming the equation
  explicitly in the statement keeps the interface faithful to §2.
-/
import GraphosFormalization.Ext.Basic

namespace GraphosFormalization

namespace Extraction

variable {NK EK N E : Type}

/-! ## `Ext` as a poset (§1.1) -/

theorem le_refl (A : Extraction NK EK N E) : le A A :=
  ⟨ExtView.le_refl A.extNodes, ExtView.le_refl A.extEdges⟩

theorem le_trans {A B C : Extraction NK EK N E} (h₁ : le A B) (h₂ : le B C) : le A C :=
  ⟨ExtView.le_trans h₁.1 h₂.1, ExtView.le_trans h₁.2 h₂.2⟩

theorem le_antisymm {A B : Extraction NK EK N E} (h₁ : le A B) (h₂ : le B A) : A = B :=
  eq_of_components (ExtView.le_antisymm h₁.1 h₂.1) (ExtView.le_antisymm h₁.2 h₂.2)

/-! ## `mergeExtractions` as a least upper bound (§2, binary case) -/

theorem le_merge_left (A B : Extraction NK EK N E) : le A (mergeExtractions A B) := by
  refine ⟨?_, ?_⟩
  · intro k
    rw [mergeExtractions_extNodes]
    exact ExtView.le_union_left A.extNodes B.extNodes k
  · intro k
    rw [mergeExtractions_extEdges]
    exact ExtView.le_union_left A.extEdges B.extEdges k

theorem le_merge_right {A B : Extraction NK EK N E} (h : consistent A B) :
    le B (mergeExtractions A B) := by
  refine ⟨?_, ?_⟩
  · intro k
    rw [mergeExtractions_extNodes]
    exact ExtView.le_union_right h.1 k
  · intro k
    rw [mergeExtractions_extEdges]
    exact ExtView.le_union_right h.2 k

theorem le_merge {A B X : Extraction NK EK N E} (hA : le A X) (hB : le B X) :
    le (mergeExtractions A B) X := by
  refine ⟨?_, ?_⟩
  · intro k
    rw [mergeExtractions_extNodes]
    exact ExtView.le_union hA.1 hB.1 k
  · intro k
    rw [mergeExtractions_extEdges]
    exact ExtView.le_union hA.2 hB.2 k

/-! ## The n-way union `M = ⋃ᵢ Eᵢ` (§1.3, §2) -/

/-- A view consistent with every member of a family is consistent with the
family's union, on both projections. -/
theorem consistent_foldMerge_right {A : Extraction NK EK N E}
    {l : List (Extraction NK EK N E)} (h : ∀ B ∈ l, consistent A B) :
    consistent A (foldMerge l) := by
  refine ⟨?_, ?_⟩
  · exact ExtView.consistent_unionList_right (l := l.map Extraction.extNodes) (A := A.extNodes)
      (fun X hX => by
        obtain ⟨B, hB, rfl⟩ := List.mem_map.mp hX
        exact (h B hB).1)
  · exact ExtView.consistent_unionList_right (l := l.map Extraction.extEdges) (A := A.extEdges)
      (fun X hX => by
        obtain ⟨B, hB, rfl⟩ := List.mem_map.mp hX
        exact (h B hB).2)

/-- **The canonical cocone legs `ιᵢ : Eᵢ ↪ M`** (§1.3): under pairwise
consistency every family member embeds into the union. -/
theorem le_foldMerge_of_mem {l : List (Extraction NK EK N E)}
    (hcons : ∀ A ∈ l, ∀ B ∈ l, consistent A B) {A : Extraction NK EK N E} (hA : A ∈ l) :
    le A (foldMerge l) := by
  refine ⟨?_, ?_⟩
  · exact ExtView.le_unionList_of_mem
      (l := l.map Extraction.extNodes)
      (fun X hX Y hY => by
        obtain ⟨A', hA', rfl⟩ := List.mem_map.mp hX
        obtain ⟨B', hB', rfl⟩ := List.mem_map.mp hY
        exact (hcons A' hA' B' hB').1)
      (List.mem_map.mpr ⟨A, hA, rfl⟩)
  · exact ExtView.le_unionList_of_mem
      (l := l.map Extraction.extEdges)
      (fun X hX Y hY => by
        obtain ⟨A', hA', rfl⟩ := List.mem_map.mp hX
        obtain ⟨B', hB', rfl⟩ := List.mem_map.mp hY
        exact (hcons A' hA' B' hB').2)
      (List.mem_map.mpr ⟨A, hA, rfl⟩)

/-- **The union is the least upper bound** (§2, existence of the mediator):
`M = ⋃ᵢ Eᵢ` embeds into any extraction view `X` receiving a compatible cocone. -/
theorem le_foldMerge {l : List (Extraction NK EK N E)} {X : Extraction NK EK N E}
    (h : ∀ A ∈ l, le A X) : le (foldMerge l) X := by
  refine ⟨?_, ?_⟩
  · exact ExtView.le_unionList (l := l.map Extraction.extNodes)
      (fun X' hX' => by
        obtain ⟨A, hA, rfl⟩ := List.mem_map.mp hX'
        exact (h A hA).1)
  · exact ExtView.le_unionList (l := l.map Extraction.extEdges)
      (fun X' hX' => by
        obtain ⟨A, hA, rfl⟩ := List.mem_map.mp hX'
        exact (h A hA).2)

end Extraction

/-! ## Morphisms and cocones (§1.1, §1.3) -/

/-- A **view-embedding** `A ⟶ B` (§1.1): the pair of value-preserving partial-key
maps, i.e. every key of `A` is a key of `B` carrying the same value. `Ext` is
poset-enriched, so this `Prop` *is* the hom-set — it is a subsingleton, which is
what makes the colimit mediator unique. -/
structure ViewEmbedding {NK EK N E : Type} (A B : Extraction NK EK N E) : Prop where
  /-- The node projection is an embedding. -/
  nodes_le : ExtView.le A.extNodes B.extNodes
  /-- The edge projection is an embedding. -/
  edges_le : ExtView.le A.extEdges B.extEdges

namespace ViewEmbedding

variable {NK EK N E : Type}

/-- `Hom(A, B)` is a subsingleton: `Ext` is poset-enriched (§1.1). -/
theorem unique {A B : Extraction NK EK N E} (f g : ViewEmbedding A B) : f = g := by
  cases f; cases g; rfl

/-- Composition of view-embeddings, in the `Function.comp` order: `comp f g` is
`f ∘ g` (apply `g` first). Composition is partial-map restriction, hence
transitive on the underlying `le` relation. -/
def comp {A B C : Extraction NK EK N E} (f : ViewEmbedding B C) (g : ViewEmbedding A B) :
    ViewEmbedding A C :=
  ⟨ExtView.le_trans g.nodes_le f.nodes_le, ExtView.le_trans g.edges_le f.edges_le⟩

end ViewEmbedding

/-- **A cocone over the diagram** (§1.3): a family of view-embeddings into `X`. -/
structure Cocone {NK EK N E : Type} (l : List (Extraction NK EK N E))
    (X : Extraction NK EK N E) : Prop where
  /-- The legs `gᵢ : Eᵢ → X`. -/
  legs : ∀ A, A ∈ l → ViewEmbedding A X

/-- Pairwise **consistency** of the view family (§3.1 hypothesis): every two
members agree on every shared key. This is the hypothesis that makes `M` a
*strict* colimit rather than a lax one. -/
def familyConsistent {NK EK N E : Type} (l : List (Extraction NK EK N E)) : Prop :=
  ∀ A ∈ l, ∀ B ∈ l, Extraction.consistent A B

/-- The canonical cocone leg `ιᵢ : Eᵢ ↪ M` of `(M, {ιᵢ})` (§1.3, §2). -/
def canonicalLeg {NK EK N E : Type} {l : List (Extraction NK EK N E)}
    (hcons : familyConsistent l) {A : Extraction NK EK N E} (hA : A ∈ l) :
    ViewEmbedding A (foldMerge l) :=
  let h := Extraction.le_foldMerge_of_mem hcons hA
  ⟨h.1, h.2⟩

/-! ## §2 The universal property -/

/-- **`mergedExtraction_isColimit`** (AVI-620 §2, AVI-1302).

Let `{Eᵢ}_{i∈I}` be a **consistent** family of extraction views (agreeing on
every shared key — `familyConsistent`). Then `(M, {ιᵢ})` with
`M = ⋃ᵢ Eᵢ = foldMerge l` (the key-wise union, i.e. `mergeExtractions` folded
over the family exactly as at `Build.hs:12`) and `ιᵢ = canonicalLeg hcons hA` is
the **colimit** of the diagram `F : 𝘿 → Ext`:

> for every compatible cocone `{gᵢ : Eᵢ → X}` there exists a **unique**
> view-embedding `u : M → X` with `u ∘ ιᵢ = gᵢ` for all `i ∈ I`.

The conclusion is spelled out in the `!u.` shape of §2 (Lean core 4.14 has no
`∃!` binder, so the standard `∃ u, P u ∧ ∀ u', P u' → u' = u` encoding is used
verbatim):

> `∃ u, (∀ i, u ∘ ιᵢ = gᵢ) ∧ (∀ u', (∀ i, u' ∘ ιᵢ = gᵢ) → u' = u)`

Proof: existence is `Extraction.le_foldMerge` (the union is the least upper
bound of the cocone's apex); uniqueness is `ViewEmbedding.unique` (`Ext` is
poset-enriched, §1.1). Well-definedness of *the values* of `u` is exactly the
consistency hypothesis: on a shared key, all legs agree, so `u(k)` is forced
regardless of which member's key supplies it (§2 "why", `exists_mem_of_unionList_eq_some`). -/
theorem mergedExtraction_isColimit {NK EK N E : Type}
    (l : List (Extraction NK EK N E))
    (hcons : familyConsistent l)
    (X : Extraction NK EK N E)
    (c : Cocone l X) :
    ∃ u : ViewEmbedding (foldMerge l) X,
      (∀ A (hA : A ∈ l),
        ViewEmbedding.comp u (canonicalLeg hcons hA) = c.legs A hA) ∧
      (∀ u' : ViewEmbedding (foldMerge l) X,
        (∀ A (hA : A ∈ l),
          ViewEmbedding.comp u' (canonicalLeg hcons hA) = c.legs A hA) →
        u' = u) := by
  have hu : ViewEmbedding (foldMerge l) X :=
    let h := Extraction.le_foldMerge (l := l) (X := X)
      (fun A hA => ⟨(c.legs A hA).nodes_le, (c.legs A hA).edges_le⟩)
    ⟨h.1, h.2⟩
  refine ⟨hu, ?_, ?_⟩
  · intro A hA
    exact ViewEmbedding.unique _ _
  · intro u' _
    exact ViewEmbedding.unique u' hu

/-- The universal property in *least-upper-bound* form: `M` is a colimit iff it
receives exactly the cocones that the family receives. The forward direction is
`mergedExtraction_isColimit` (existence of `u`) plus the legs; the converse is
`Extraction.le_foldMerge`. This is the checkable shape used by the cross-check
harness, and it makes the "unique mediator" claim falsifiable: if a member did
not embed into `M`, the `↔` would fail. -/
theorem mergedExtraction_isColimit_iff {NK EK N E : Type}
    (l : List (Extraction NK EK N E))
    (hcons : familyConsistent l)
    (X : Extraction NK EK N E) :
    ViewEmbedding (foldMerge l) X ↔ ∀ A, A ∈ l → ViewEmbedding A X := by
  constructor
  · intro u A hA
    exact ViewEmbedding.comp u (canonicalLeg hcons hA)
  · intro h
    exact ⟨(Extraction.le_foldMerge (fun A hA => ⟨(h A hA).nodes_le, (h A hA).edges_le⟩)).1,
      (Extraction.le_foldMerge (fun A hA => ⟨(h A hA).nodes_le, (h A hA).edges_le⟩)).2⟩

/-! ## Non-vacuity: the hypothesis is satisfiable

`mergedExtraction_isColimit` would be worthless if `familyConsistent` were
unsatisfiable. These two lemmas exhibit consistent families (identical values on
every shared key are of course consistent) and then *instantiate* the theorem,
which is the checkable form of AC-1: the statement elaborates with real objects,
not only with the abstract binders. -/

namespace Extraction

variable {NK EK N E : Type}

/-- A view is consistent with itself. -/
theorem consistent_self (A : Extraction NK EK N E) : consistent A A :=
  ⟨fun _ _ _ h₁ h₂ => Option.some.inj (by rw [h₁] at h₂; exact h₂),
   fun _ _ _ h₁ h₂ => Option.some.inj (by rw [h₁] at h₂; exact h₂)⟩

end Extraction

/-- The one-member family is consistent. -/
theorem familyConsistent_singleton {NK EK N E : Type} (A : Extraction NK EK N E) :
    familyConsistent [A] := by
  intro X hX Y hY
  simp only [List.mem_singleton] at hX hY
  subst X
  subst Y
  exact Extraction.consistent_self A

/-- **Non-vacuity witness.** For every view `A`, the singleton diagram `[A]` is
consistent, and the colimit statement yields its mediator: instantiated here so
that a future weakening of the hypotheses or of the mediator's type is caught at
compile time. -/
example {NK EK N E : Type} (A X : Extraction NK EK N E) (h : ViewEmbedding A X) :
    ViewEmbedding (foldMerge [A]) X := by
  obtain ⟨u, -, -⟩ := mergedExtraction_isColimit [A] (familyConsistent_singleton A) X
    ⟨fun B hB => by
      simp only [List.mem_singleton] at hB
      subst B
      exact h⟩
  exact u

end GraphosFormalization
