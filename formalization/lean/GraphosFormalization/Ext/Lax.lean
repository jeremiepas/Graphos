/-
  Graphos formalization — Phase 2, §3.2: the conflicting case,
  lax colimit with the **first/old-wins** comparison.

  Requirements: `docs/math-requirements/AVI-620-colimit-functoriality-context-graph-lean-formalization-requirements.md`
  §0.2 (`EdgeId` encoding, `Convert.hs:165`), §0.3 (first/old-wins `⊕`),
  §0.4 (fold-order note), §3.2 (`mergedExtraction_laxColimit_oldWins`), §5 AC-4
  (first/old-wins on edge keys); and the consolidated layer
  `docs/math-requirements/AVI-616-consolidated-lean-formalization-requirements.md`
  §1.3 (correction M-5).

  **Acceptance criterion (AC-4).** `mergeExtractions (mk [eA]) (mk [eB])` with
  `eA`, `eB` sharing `EdgeId` but differing in `weight` carries the **first**
  operand's value (`Core.hs:131-132`), and
  `mergeExtractions A B ≠ mergeExtractions B A` at that key. The concrete
  instance is `ac4_conflictingEdge_firstWins` at the end of this file.

  ## Correction banner (M-5) — first/old-wins, not new-wins

  `mergeExtractions a b = extNodes a Map.union extNodes b` and `Data.Map.union`
  is **left-biased**, so on a shared key the **first (old) operand** wins
  (`Core.hs:129-136`). The CTE Phase 2 draft pinned new-wins and named this
  artifact `mergedExtraction_laxColimit_newWins`; that pin was wrong and is
  superseded. Every statement below is about the actual, left-biased operator
  pinned in `Ext/Basic.lean` (`ExtView.union` = `(a k).or (b k)`).

  ## What is proved

  Let `A, B : Extraction` share a **conflicting** edge key `k`: both views carry
  `k`, with different payloads (`vA ≠ vB`). In the shipped encoding
  (`Convert.hs:165`) `k` is the `EdgeId` `"<src>-><tgt>:<relation>"`, so the
  conflict is *same `(src,tgt,relation)`, different `weight`/`confidence`/`extra`*.

  * **(a) no matching-family cocone ⇒ the strict universal property of §2 has no
    instance.** No extraction view `X` receives both `A` and `B` as
    view-embeddings (`Extraction.no_cocone_of_conflict`): the two legs would
    force `X k = some vA` and `X k = some vB` at the same key. Hence there is no
    cocone over the two-object diagram at all (`no_pairCocone_of_conflict`), and
    in particular the canonical cocone `(A ⊕ B, ι_A, ι_B)` of §3.1 is **not** a
    cocone — its second leg `ι_B : B ↪ A ⊕ B` does not exist
    (`not_leg_merge_of_conflict`), because `A ⊕ B` carries `A`'s value at `k`.
    Equivalently, `familyConsistent [A, B]` fails
    (`not_familyConsistent_pair_of_conflict`), so the hypotheses of
    `mergedExtraction_isColimit` (§2) and of
    `mergedExtraction_strictColimit_orderIndependent` (§3.1) are not met.

    In the requirement's prose the phrase "no `u : M → X` satisfies both
    `u ∘ ι_A = g_A` and `u ∘ ι_B = g_B`" presupposes that both legs exist; the
    sharp formal reading is the one above — *there is no `M` with both legs*, so
    no mediator is even well-typed. That is a strictly stronger failure than a
    missing/failing mediator.

  * **(b) lax colimit, first/old-wins.** `A ⊕ B` realises the **first** operand's
    value at `k` (`merge_apply_left_of_conflict`), the dual `B ⊕ A` realises the
    second's (`merge_apply_right_of_conflict`), so the canonical comparison
    2-cell is the first/old-wins projection, and it is **non-invertible**:
    `A ⊕ B ≠ B ⊕ A` (`merge_ne_merge_swap`), and — because `Ext` is
    poset-enriched — there is no view-embedding in *either* direction between
    the two merges (`not_le_merge_swap`, `not_le_merge_swap'`). The comparison
    is therefore not a 2-cell at all, let alone an invertible one; the lax
    structure is the left bias itself.

  The bundle is the named artifact `mergedExtraction_laxColimit_oldWins`.

  ## Relation to §3.1 (strict)

  `Ext/Strict.lean` proved that consistency makes `⊕` commutative, i.e. makes
  the comparison of (b) invertible; this file proves the converse direction:
  a single conflicting key destroys the strict colimit and exposes the left
  bias. Together they are the §2 dichotomy — `Map.union` is a *lax* colimit whose
  comparison is invertible exactly on the consistent subcategory.

  ## Modelling note on §0.4 (recorded, flagged to head-rnd)

  This package models both fold sites with the accumulator/head as the **first**
  operand (`foldrMerge l = l.foldr mergeExtractions emptyExtraction`,
  `foldlMerge l = l.foldl (fun acc A => acc ⊕ A) emptyExtraction`). Those two
  coincide on **every** list, conflicting keys included, by re-association alone
  (`foldrMerge_eq_foldlMerge_all` below) — consistency is not needed for *that*
  equality. The §0.4 prose ("`foldl'` makes the last view win, `foldr` the
  first") therefore does not describe the shipped call sites: `Build.hs:12`
  folds `foldr mergeExtractions` (element first ⇒ earliest view wins),
  `Extract/Core.hs:599,639` fold `List.foldl' mergeExtractions emptyExtraction`
  (accumulator first ⇒ earliest view wins), and the streaming accumulator is
  `mergeIntoRunning = mergeExtractions running ext` (accumulator first ⇒ oldest
  running view wins). All three are the same first/old-wins operator, so the
  lanes can only diverge if they are *fed different orders*, not because of the
  fold shape. The operator-level pin — that the first operand's value survives —
  is what AC-4 tests and what this file proves; the §0.4 divergence sentence is
  a requirements-doc erratum tracked separately.
-/
import GraphosFormalization.Ext.Strict

namespace GraphosFormalization

namespace ExtView

variable {K V : Type}

/-! ## §3.2 Conflict at the map level -/

/-- **Reading a view-embedding at a present key** (§1.1): if `A ⊆ B` and `A`
carries `v` at `k`, then `B` carries the same `v` at `k`. This is the step that
turns a conflicting key into the impossibility of a cocone. -/
theorem le_apply_of_some {A B : ExtView K V} (h : le A B) {k : K} {v : V}
    (hk : A k = some v) : B k = some v := by
  rcases h k with hnone | heq
  · rw [hk] at hnone
    cases hnone
  · rw [hk] at heq
    exact heq.symm

/-- **A conflicting key has no common upper bound.** If `A` and `B` both carry
`k`, with different values, then no view `X` can be a common upper bound: a leg
from `A` forces `X k = some v`, a leg from `B` forces `X k = some w`, and
`v ≠ w`. This is the map-level content of §3.2 (a) — the family is not
*matching*, so it has no cocone. -/
theorem no_upper_bound_of_conflict {A B X : ExtView K V} {k : K} {v w : V}
    (hA : A k = some v) (hB : B k = some w) (hne : v ≠ w) :
    ¬ (le A X ∧ le B X) := by
  rintro ⟨hAX, hBX⟩
  exact hne (Option.some.inj ((le_apply_of_some hAX hA).symm.trans (le_apply_of_some hBX hB)))

/-- **The left bias is observable** (§3.2 (b)): on a conflicting key the two
orders of the union differ, so `Map.union` is not commutative off the consistent
subcategory. -/
theorem union_ne_union_swap {A B : ExtView K V} {k : K} {v w : V}
    (hA : A k = some v) (hB : B k = some w) (hne : v ≠ w) :
    union A B ≠ union B A := by
  intro h
  have h1 : union A B k = some v := union_apply_some hA
  have h2 : union B A k = some w := union_apply_some hB
  rw [h] at h1
  exact hne (Option.some.inj (h1.symm.trans h2))

/-- **The comparison 2-cell is non-invertible, one direction.** There is no
view-embedding `A ⊕ B ⟶ B ⊕ A`: it would have to carry `A`'s value `v` at `k`
into the dual, which holds `B`'s value `w`. -/
theorem not_le_union_swap {A B : ExtView K V} {k : K} {v w : V}
    (hA : A k = some v) (hB : B k = some w) (hne : v ≠ w) :
    ¬ le (union A B) (union B A) := by
  intro h
  have h1 : union A B k = some v := union_apply_some hA
  have h2 : union B A k = some w := union_apply_some hB
  have h1' : union B A k = some v := le_apply_of_some h h1
  exact hne (Option.some.inj (h1'.symm.trans h2))

/-- … and not in the other direction either. -/
theorem not_le_union_swap' {A B : ExtView K V} {k : K} {v w : V}
    (hA : A k = some v) (hB : B k = some w) (hne : v ≠ w) :
    ¬ le (union B A) (union A B) := by
  intro h
  have h1 : union A B k = some v := union_apply_some hA
  have h2 : union B A k = some w := union_apply_some hB
  have h2' : union A B k = some w := le_apply_of_some h h2
  exact hne (Option.some.inj (h2'.symm.trans h1)).symm

end ExtView

/-! ## §3.2 Conflict at the `Extraction` level (edge component) -/

namespace Extraction

variable {NK EK N E : Type}

/-- `A` and `B` **conflict** at the edge key `k` (§3.2 hypothesis): both carry
`k`, with different values. In the shipped encoding `k` is the `EdgeId`
`"<src>-><tgt>:<relation>"` (`Convert.hs:165`), so this is the "same
`(src,tgt,relation)`, different `weight`/`confidence`/`extra`" case of AC-4. The
node component is treated identically (`extNodes` in place of `extEdges`). -/
abbrev edgeConflictAt (A B : Extraction NK EK N E) (k : EK) (v w : E) : Prop :=
  A.extEdges k = some v ∧ B.extEdges k = some w ∧ v ≠ w

/-- §3.2 (b), first-wins projection: `A ⊕ B` carries the **first (old)**
operand's value at a conflicting key (`Core.hs:131-132`, left-biased
`Map.union`). -/
theorem merge_apply_left_of_conflict {A B : Extraction NK EK N E} {k : EK} {v : E}
    (hA : A.extEdges k = some v) :
    (mergeExtractions A B).extEdges k = some v := by
  rw [mergeExtractions_extEdges]
  exact ExtView.union_apply_some hA

/-- … and the dual `B ⊕ A` carries the **second** operand's value: the two
compositions differ on the conflicting key. -/
theorem merge_apply_right_of_conflict {A B : Extraction NK EK N E} {k : EK} {w : E}
    (hB : B.extEdges k = some w) :
    (mergeExtractions B A).extEdges k = some w := by
  rw [mergeExtractions_extEdges]
  exact ExtView.union_apply_some hB

/-- §3.2 (a), cocone form: a conflicting pair has **no common upper bound**, so
no view receives both views as legs. -/
theorem no_cocone_of_conflict {A B : Extraction NK EK N E} {k : EK} {v w : E}
    (hA : A.extEdges k = some v) (hB : B.extEdges k = some w) (hne : v ≠ w) :
    ¬ ∃ X : Extraction NK EK N E, ViewEmbedding A X ∧ ViewEmbedding B X := by
  rintro ⟨X, hAX, hBX⟩
  exact hne (Option.some.inj
    ((ExtView.le_apply_of_some hAX.edges_le hA).symm.trans
      (ExtView.le_apply_of_some hBX.edges_le hB)))

/-- §3.2 (a), `Cocone` form: the two-object diagram `[A, B]` has **no cocone at
all** when the views conflict. This is the precise sense in which "no
matching-family cocone exists" and the strict universal property of §2 fails:
its premise (a cocone to factor) is unsatisfiable. -/
theorem no_pairCocone_of_conflict {A B : Extraction NK EK N E} {k : EK} {v w : E}
    (hA : A.extEdges k = some v) (hB : B.extEdges k = some w) (hne : v ≠ w) :
    ¬ ∃ X : Extraction NK EK N E, Cocone [A, B] X := by
  rintro ⟨X, c⟩
  exact no_cocone_of_conflict hA hB hne ⟨X, c.legs A (by simp), c.legs B (by simp)⟩

/-- §3.2 (a), leg form: the canonical cocone of §3.1 loses its **second leg**.
`A ⊕ B` carries `A`'s value at the conflicting key, so `ι_B : B ↪ A ⊕ B` cannot
exist — the canonical cocone is not a cocone. -/
theorem not_leg_merge_of_conflict {A B : Extraction NK EK N E} {k : EK} {v w : E}
    (hA : A.extEdges k = some v) (hB : B.extEdges k = some w) (hne : v ≠ w) :
    ¬ ViewEmbedding B (mergeExtractions A B) := by
  intro hg
  have hA' : (mergeExtractions A B).extEdges k = some v := merge_apply_left_of_conflict hA
  have hB' : (mergeExtractions A B).extEdges k = some w :=
    ExtView.le_apply_of_some hg.edges_le hB
  exact hne (Option.some.inj (hA'.symm.trans hB'))

/-- A conflicting pair is not consistent, so the consistency hypothesis of §2 /
§3.1 is not satisfied. -/
theorem not_consistent_of_conflict {A B : Extraction NK EK N E} {k : EK} {v w : E}
    (hA : A.extEdges k = some v) (hB : B.extEdges k = some w) (hne : v ≠ w) :
    ¬ consistent A B := by
  intro hc
  exact hne (hc.2 k v w hA hB)

/-- The two-view family is not pairwise consistent — the §3.1 hypothesis fails
on a conflicting pair. -/
theorem not_familyConsistent_pair_of_conflict {A B : Extraction NK EK N E} {k : EK}
    {v w : E} (hA : A.extEdges k = some v) (hB : B.extEdges k = some w) (hne : v ≠ w) :
    ¬ familyConsistent [A, B] := by
  intro hc
  exact not_consistent_of_conflict hA hB hne (hc A (by simp) B (by simp))

/-- §3.2 (b), non-invertibility: `A ⊕ B ≠ B ⊕ A` at the conflicting key, so the
comparison 2-cell is not invertible — the merge is only a **lax** colimit. -/
theorem merge_ne_merge_swap {A B : Extraction NK EK N E} {k : EK} {v w : E}
    (hA : A.extEdges k = some v) (hB : B.extEdges k = some w) (hne : v ≠ w) :
    mergeExtractions A B ≠ mergeExtractions B A := by
  intro h
  have h1 : (mergeExtractions A B).extEdges k = some v := merge_apply_left_of_conflict hA
  have h2 : (mergeExtractions B A).extEdges k = some w := merge_apply_right_of_conflict hB
  rw [h] at h1
  exact hne (Option.some.inj (h1.symm.trans h2))

/-- Non-invertibility in the sharp, `Ext`-internal form: **no view-embedding**
`A ⊕ B ⟶ B ⊕ A` exists. Since `Ext` is poset-enriched (§1.1), this says the
comparison is not an isomorphism — indeed not even a morphism. -/
theorem not_le_merge_swap {A B : Extraction NK EK N E} {k : EK} {v w : E}
    (hA : A.extEdges k = some v) (hB : B.extEdges k = some w) (hne : v ≠ w) :
    ¬ ViewEmbedding (mergeExtractions A B) (mergeExtractions B A) := by
  intro h
  have h1 : (mergeExtractions A B).extEdges k = some v := merge_apply_left_of_conflict hA
  have h2 : (mergeExtractions B A).extEdges k = some w := merge_apply_right_of_conflict hB
  have h1' : (mergeExtractions B A).extEdges k = some v := ExtView.le_apply_of_some h.edges_le h1
  exact hne (Option.some.inj (h1'.symm.trans h2))

/-- … nor the reverse. -/
theorem not_le_merge_swap' {A B : Extraction NK EK N E} {k : EK} {v w : E}
    (hA : A.extEdges k = some v) (hB : B.extEdges k = some w) (hne : v ≠ w) :
    ¬ ViewEmbedding (mergeExtractions B A) (mergeExtractions A B) := by
  intro h
  have h1 : (mergeExtractions A B).extEdges k = some v := merge_apply_left_of_conflict hA
  have h2 : (mergeExtractions B A).extEdges k = some w := merge_apply_right_of_conflict hB
  have h2' : (mergeExtractions A B).extEdges k = some w := ExtView.le_apply_of_some h.edges_le h2
  exact hne (Option.some.inj (h2'.symm.trans h1)).symm

end Extraction

/-! ## §3.2 The named artifact -/

/-- **`mergedExtraction_laxColimit_oldWins`** (AVI-620 §3.2, AVI-1304; renamed
from the CTE draft's `_newWins` by the head-rnd correction M-5).

Let `A, B : Extraction` share a **conflicting** edge key `k` — both carry `k`
with different values (`vA ≠ vB`). With `k = EdgeId "<src>-><tgt>:<relation>"`
(`Convert.hs:165`) this is exactly AC-4's "same `(src,tgt,relation)`, different
`weight`/`confidence`/`extra`". Then:

* **(a) the strict universal property of §2 fails.** No view receives both `A`
  and `B` (no common upper bound), hence the two-object diagram has no cocone at
  all; in particular the canonical cocone `(A ⊕ B, ι_A, ι_B)` is not a cocone
  because `ι_B` does not exist, and `familyConsistent [A, B]` is false — so the
  §2/§3.1 theorems are not applicable.
* **(b) `Map.union` realises a lax colimit with a first/old-wins comparison.**
  `(A ⊕ B) k = some vA` (the first operand's value survives,
  `Core.hs:131-132`), `(B ⊕ A) k = some vB`, the two merges are distinct, and no
  view-embedding relates them in either direction: the comparison 2-cell is the
  first/old-wins projection and it is non-invertible.

The `Ext`-level statement is given for the edge component, which is AC-4's case;
the node component is identical (`extNodes` for `extEdges`), and
`ExtView.union_ne_union_swap` / `ExtView.not_le_union_swap` give the map-level
form directly. -/
theorem mergedExtraction_laxColimit_oldWins {NK EK N E : Type}
    (A B : Extraction NK EK N E) {k : EK} {vA vB : E}
    (hconf : Extraction.edgeConflictAt A B k vA vB) :
    (-- (a) no matching-family cocone: the strict universal property has no instance
     (¬ ∃ X : Extraction NK EK N E, ViewEmbedding A X ∧ ViewEmbedding B X) ∧
     (¬ ∃ X : Extraction NK EK N E, Cocone [A, B] X) ∧
     (¬ ViewEmbedding B (mergeExtractions A B)) ∧
     (¬ familyConsistent [A, B])) ∧
    (-- (b) lax colimit: first/old-wins comparison, non-invertible
     (mergeExtractions A B).extEdges k = some vA ∧
     (mergeExtractions B A).extEdges k = some vB ∧
     mergeExtractions A B ≠ mergeExtractions B A ∧
     (¬ ViewEmbedding (mergeExtractions A B) (mergeExtractions B A)) ∧
     (¬ ViewEmbedding (mergeExtractions B A) (mergeExtractions A B))) :=
  ⟨⟨Extraction.no_cocone_of_conflict hconf.1 hconf.2.1 hconf.2.2,
    Extraction.no_pairCocone_of_conflict hconf.1 hconf.2.1 hconf.2.2,
    Extraction.not_leg_merge_of_conflict hconf.1 hconf.2.1 hconf.2.2,
    Extraction.not_familyConsistent_pair_of_conflict hconf.1 hconf.2.1 hconf.2.2⟩,
   ⟨Extraction.merge_apply_left_of_conflict hconf.1,
    Extraction.merge_apply_right_of_conflict hconf.2.1,
    Extraction.merge_ne_merge_swap hconf.1 hconf.2.1 hconf.2.2,
    Extraction.not_le_merge_swap hconf.1 hconf.2.1 hconf.2.2,
    Extraction.not_le_merge_swap' hconf.1 hconf.2.1 hconf.2.2⟩⟩

/-! ## §0.4 fold-shape note: the two shipped fold sites coincide on every input

The §0.4 divergence sentence ("`foldl'` makes the last view win, `foldr` the
first") is *not* a property of the fold shape: both shipped folds put the
accumulator / head in the **first** operand position, and re-association alone
identifies them. The equality below is therefore unconditional — consistency,
and even conflict-freeness, is irrelevant — which is exactly why the §0.4 claim
is recorded as a requirements-doc erratum (see the header and the AVI-1304
report). -/

variable {NK EK N E : Type}

/-- Accumulation lemma for the **element-agnostic** case: folding `⊕` over `l`
from `acc` is `acc ⊕ (⋃ l)`, with no consistency hypothesis (associativity of the
left-biased union suffices, `ExtView.union_assoc`). -/
theorem foldl_merge_eq_all (l : List (Extraction NK EK N E)) (acc : Extraction NK EK N E) :
    l.foldl (fun a A => mergeExtractions a A) acc = mergeExtractions acc (foldrMerge l) := by
  induction l generalizing acc with
  | nil => exact (Extraction.merge_empty_right acc).symm
  | cons B l ih =>
      rw [foldrMerge, List.foldr_cons]
      show l.foldl (fun a A => mergeExtractions a A) (mergeExtractions acc B)
        = mergeExtractions acc (mergeExtractions B (foldrMerge l))
      rw [ih (mergeExtractions acc B), Extraction.merge_assoc]

/-- **The build-path `foldr` and the streaming-path `foldl'` agree on every
list**, conflicting keys included. Under the shipped first/old-wins operator both
are "earliest view wins", so their only possible divergence is the *order in
which views are fed*, not the fold shape. This is the formal counterpart of the
§0.4 erratum noted above. -/
theorem foldrMerge_eq_foldlMerge_all (l : List (Extraction NK EK N E)) :
    foldrMerge l = foldlMerge l := by
  rw [foldlMerge, foldl_merge_eq_all l emptyExtraction, Extraction.merge_empty_left]

/-! ## AC-4: a concrete conflicting `EdgeId` fixture

The fixture is the AC-4 witness: two edges that agree on the whole `EdgeId`
encoding (`(src,tgt,relation)`) and differ only in payload, merged in both
orders. The first operand's value survives in each composition and the two
compositions differ — the left bias is observable, not vacuous.

`EdgeId` is modelled as the encoded `String` of `Convert.hs:165`
(`"<src>-><tgt>:<relation>"`), and a node map keyed by `Unit` (i.e. empty) keeps
the conflict confined to the edge component, which is AC-4's subject. -/

/-- The `EdgeId` encoding of `Convert.hs:165`: `"<src>-><tgt>:<relation>"`. -/
def encodeEdgeId (src tgt relation : String) : String :=
  src ++ "->" ++ tgt ++ ":" ++ relation

/-- One extraction edge (`Types/Graph.hs:73`, `Convert.hs:165`): the `EdgeId`
components plus the payload that `Map.union` discards on a collision. -/
structure Edge where
  /-- `edgeSource` — first `EdgeId` component. -/
  source : String
  /-- `edgeTarget` — second `EdgeId` component. -/
  target : String
  /-- `edgeRelation` — third `EdgeId` component. -/
  relation : String
  /-- The payload component AC-4 varies. -/
  weight : Nat
  /-- Further payload that a collision also discards. -/
  confidence : Nat
  deriving DecidableEq, Repr

namespace Edge

/-- The `EdgeId` of an edge: two edges collide exactly when their `EdgeId`s
agree, i.e. when all three key components agree. -/
def toEdgeId (e : Edge) : String := encodeEdgeId e.source e.target e.relation

end Edge

/-- The one-edge extraction view `{EdgeId e ↦ e}`, with an empty node map: the
AC-4 `mk [e]` of the requirement. -/
def singleEdge (e : Edge) : Extraction Unit String Unit Edge :=
  ⟨fun _ => none, fun k => if k = e.toEdgeId then some e else none⟩

/-- AC-4's `eA`: `src/A.hs -> src/B.hs : imports`, weight 1. -/
def edgeA : Edge := ⟨"src/A.hs", "src/B.hs", "imports", 1, 100⟩

/-- AC-4's `eB`: **the same `EdgeId`**, weight 2. -/
def edgeB : Edge := ⟨"src/A.hs", "src/B.hs", "imports", 2, 100⟩

/-- The shared `EdgeId` — the conflicting key `k` of §3.2. -/
def keyAB : String := edgeA.toEdgeId

/-- The two fixture edges share their whole `EdgeId` and differ only in payload:
the AC-4 conflict shape (`same (src,tgt,relation), different weight`). -/
theorem edgeA_edgeB_sameEdgeId : edgeA.toEdgeId = edgeB.toEdgeId ∧ edgeA.weight ≠ edgeB.weight := by
  decide

theorem edgeA_at_key : (singleEdge edgeA).extEdges keyAB = some edgeA := by
  decide

theorem edgeB_at_key : (singleEdge edgeB).extEdges keyAB = some edgeB := by
  decide

/-- **AC-4.** For the conflicting `EdgeId` fixture:
`mergeExtractions (mk [eA]) (mk [eB])` carries the **first** operand's value
(`Core.hs:131-132`), the dual carries the second's, and the two merges differ —
the left bias is observable on a real `EdgeId` conflict, i.e. §3.2 (b) is not
vacuous. -/
theorem ac4_conflictingEdge_firstWins :
    (mergeExtractions (singleEdge edgeA) (singleEdge edgeB)).extEdges keyAB = some edgeA ∧
    (mergeExtractions (singleEdge edgeB) (singleEdge edgeA)).extEdges keyAB = some edgeB ∧
    mergeExtractions (singleEdge edgeA) (singleEdge edgeB) ≠
      mergeExtractions (singleEdge edgeB) (singleEdge edgeA) := by
  obtain ⟨h1, h2, h3, -, -⟩ :=
    (mergedExtraction_laxColimit_oldWins (singleEdge edgeA) (singleEdge edgeB)
      ⟨edgeA_at_key, edgeB_at_key, by decide⟩).2
  exact ⟨h1, h2, h3⟩

/-- … and the §3.2 (a) half on the same fixture: the family `[mk [eA], mk [eB]]`
has no cocone, and the §3.1 canonical second leg does not exist. -/
theorem ac4_conflictingEdge_noCocone :
    (¬ ∃ X : Extraction Unit String Unit Edge,
        Cocone [singleEdge edgeA, singleEdge edgeB] X) ∧
    ¬ ViewEmbedding (singleEdge edgeB)
        (mergeExtractions (singleEdge edgeA) (singleEdge edgeB)) := by
  obtain ⟨⟨-, h1, h2, -⟩, -⟩ :=
    mergedExtraction_laxColimit_oldWins (singleEdge edgeA) (singleEdge edgeB)
      ⟨edgeA_at_key, edgeB_at_key, by decide⟩
  exact ⟨h1, h2⟩

end GraphosFormalization
