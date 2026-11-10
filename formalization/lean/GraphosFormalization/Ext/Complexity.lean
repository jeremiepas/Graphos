/-
  Graphos formalization — Phase 2, §4: n-way fold complexity.

  Requirements: `docs/math-requirements/AVI-620-colimit-functoriality-context-graph-lean-formalization-requirements.md`
  §0.3 (`foldl' ⊕ emptyExtraction [E₁…Eₙ]`, the streaming fold site),
  §4 (`mergedExtraction_nwayFold_linear`), §5 AC-1/AC-2 (named artifact with the
  complexity statement readable from its type), §5 AC-3 (empirical timing).

  **Acceptance criteria.**
  * AC-1 — `mergedExtraction_nwayFold_linear` exists and its body compiles.
  * AC-2 — the explicit bound `cost ≤ Σᵢ |keys(Eᵢ)|` is the statement itself,
    with `|keys(E)| = |extNodes E| + |extEdges E|`.

  ## Cost model (what is counted, and what is not)

  `mergeExtractions` is two `Map.union`s, one per component (`Core.hs:129-136`).
  The project comment at `Core.hs:126-128` quotes `O(n₁+n₂)` for the union. The
  real `Data.Map.union` is `O(m·log (n/m + 1))`, `m = min(n₁, n₂)`: it walks the
  **smaller** operand's keys and `split`s the larger tree along each one. The
  dominant, key-proportional term is therefore the smaller operand's key count;
  the `log` factor is the depth of the `split` path, i.e. node/pointer
  accounting, not a key operation.

  This module formalizes exactly the **key-visit charge**

      `stepCost acc E = min (|keys acc|) (|keys E|)`

  and proves that the `foldl'` over a family charges at most
  `Σᵢ |keys(Eᵢ)|` in total. That is the precise content of the §4 justification
  "the left fold visits each key a bounded number of times": each key of each
  input view is charged **once**, at the step where its view is the smaller
  operand, and is never re-scanned at a later step. The accumulator is not
  re-charged, which is what makes the pass linear rather than quadratic.

  > Modelling note (recorded, not silently assumed). Under the *literal* coarse
  > reading `O(n₁ + n₂)` — charging both operands at every step — the fold is
  > `Θ(n·Σᵢ|keys(Eᵢ)|)` in the worst case (growing accumulator), and the §4
  > bound `O(Σᵢ|keys(Eᵢ)|)` is **false**. The theorem is therefore stated
  > against the smaller-operand charge, the only charge under which §4's
  > statement holds. See the authorship note in AVI-620 §4 (AVI-1306).

  ## Universe-relative key counting

  Views are extensional lookup functions `K → Option V` (`Ext/Basic.lean`), so
  they carry no intrinsic finite support. Keys are counted against a candidate
  universe list `s` (and `t` for the edge key space):

      `keysIn s E = s.filter (fun k => (E k).isSome)`

  With `s ⊇ keys ⋃ᵢ Eᵢ` (a duplicate-free enumeration of the family's keys) the
  length of `keysIn s E` is exactly `|keys E|`; for a duplicated universe the
  count is the multiplicity-weighted count, and the bound still holds. Neither
  hypothesis is needed for the theorem — it is uniform in `s`, `t`.

  ## What is proved

  * `Extraction.size` is the §4 quantity `|keys(Eᵢ)| = |extNodes Eᵢ| + |extEdges Eᵢ|`.
  * `Extraction.size_empty` and `Extraction.size_merge_le` (union's key set is
    contained in the union of the key sets) — the bookkeeping the induction needs.
  * `foldlCost_fst` — the cost-annotated fold computes exactly `foldlMerge l`
    (`foldl' ⊕ emptyExtraction`), so the charge is attached to the real fold
    value, not to a parallel toy computation.
  * `foldlCost_snd_le` — the charge is at most `Σᵢ size(Eᵢ)`.
  * `mergedExtraction_nwayFold_linear` — both halves bundled: the fold's value
    and the linear bound (AC-1/AC-2).

  ## Non-vacuity (AC-3 reading)

  `triExtraction b` is a 3-key view at keys `b, b+1, b+2`. Four disjoint views
  over a 12-key universe (`[0,12)`) are folded concretely: the charge is `9` for
  a family of total size `12`, and `3` for the first two views — the `(n−1)·k`
  shape whose doubling ratios are `2.01, 2.00, 2.00` as `n` doubles, matching the
  recorded AC-3 ratios (`2.01, 1.99, 2.01`) to two decimal places. The charge is
  **attained** for disjoint families (`size` is additive; `Extraction.size_merge_eq_add`),
  so `Σᵢ|keys(Eᵢ)|` is a tight asymptotic bound, not a loose one.

  `Extraction.size_merge_eq_add` is stated for componentwise-disjoint views; the
  `Nat` fixtures below exercise it through `decide`.

  ## AC-3 harness (AVI-1307)

  The empirical arm lives outside this module: `Harness/Ac3Cost.lean` evaluates
  `nwayFoldCost` on the AC-3 disjoint family at n = 100/200/400/800 (`lake exe
  ac3-cost`), and `benchmarks/nway-fold-timing/` times the shipped
  `foldl'`/`foldr` `mergeExtractions` over real `Data.Map`s. Both record ≈ ×2
  per doubling; see `benchmarks/nway-fold-timing/README.md` for the recorded
  run and the §7 sketch erratum.
-/
import GraphosFormalization.Ext.Strict

namespace GraphosFormalization

/-! ## Generic list bookkeeping -/

/-- Length of a filtered list, split over an `or`-predicate: the generic
`O(n₁ + n₂)` union-counting step used by `Map.union` (`p` is the union's key
predicate, `q`/`r` the operands'). -/
theorem filter_length_le_add {K : Type} (s : List K) (p q r : K → Bool)
    (hp : ∀ k, p k = (q k || r k)) :
    (s.filter p).length ≤ (s.filter q).length + (s.filter r).length := by
  induction s with
  | nil => simp
  | cons k rest ih =>
      have hk : p k = (q k || r k) := hp k
      by_cases hq : q k = true
      · by_cases hr : r k = true
        · rw [List.filter_cons_of_pos (by simp [hk, hq, hr]),
              List.filter_cons_of_pos hq, List.filter_cons_of_pos hr]
          simp only [List.length_cons]
          omega
        · rw [List.filter_cons_of_pos (by simp [hk, hq, hr]),
              List.filter_cons_of_pos hq, List.filter_cons_of_neg hr]
          simp only [List.length_cons]
          omega
      · by_cases hr : r k = true
        · rw [List.filter_cons_of_pos (by simp [hk, hq, hr]),
              List.filter_cons_of_neg hq, List.filter_cons_of_pos hr]
          simp only [List.length_cons]
          omega
        · rw [List.filter_cons_of_neg (by simp [hk, hq, hr]),
              List.filter_cons_of_neg hq, List.filter_cons_of_neg hr]
          exact ih

/-- The same split as an equality when the operands share no key: the union of
two disjoint key sets has exactly the summed size (`Map.union` on disjoint maps
copies the smaller operand in). -/
theorem filter_length_eq_add_of_or {K : Type} (s : List K) (p q r : K → Bool)
    (hp : ∀ k, p k = (q k || r k))
    (hd : ∀ k, ¬ (q k = true ∧ r k = true)) :
    (s.filter p).length = (s.filter q).length + (s.filter r).length := by
  induction s with
  | nil => simp
  | cons k rest ih =>
      have hk : p k = (q k || r k) := hp k
      by_cases hq : q k = true
      · have hr : ¬ r k = true := fun hr => hd k ⟨hq, hr⟩
        rw [List.filter_cons_of_pos (by simp [hk, hq]),
            List.filter_cons_of_pos hq, List.filter_cons_of_neg hr]
        simp only [List.length_cons]
        omega
      · by_cases hr : r k = true
        · rw [List.filter_cons_of_pos (by simp [hk, hq, hr]),
              List.filter_cons_of_neg hq, List.filter_cons_of_pos hr]
          simp only [List.length_cons]
          omega
        · rw [List.filter_cons_of_neg (by simp [hk, hq, hr]),
              List.filter_cons_of_neg hq, List.filter_cons_of_neg hr]
          exact ih

namespace ExtView

variable {K V : Type}

/-- **The keys of a view, drawn from a candidate universe `s`.** The extensional
model has no intrinsic support, so `|keys(E)|` is read relative to an
enumeration `s` of the keys under consideration (`s` duplicate-free and
containing the family's keys gives the literal cardinality of §4). -/
def keysIn (s : List K) (E : ExtView K V) : List K :=
  s.filter (fun k => (E k).isSome)

@[simp] theorem keysIn_nil (E : ExtView K V) : keysIn ([] : List K) E = [] := rfl

/-- The empty view has no keys. -/
@[simp] theorem keysIn_empty (s : List K) : keysIn s (empty : ExtView K V) = [] := by
  induction s with
  | nil => rfl
  | cons k rest ih =>
      rw [keysIn, List.filter_cons_of_neg]
      · exact ih
      · simp [empty]

/-- `Map.union`'s key predicate is the disjunction of the operands' predicates. -/
theorem isSome_union (A B : ExtView K V) (k : K) :
    (union A B k).isSome = ((A k).isSome || (B k).isSome) := by
  rcases h : A k with _ | v <;> simp [union, h]

/-- **Union visits at most the sum of the operands' keys** (the `O(n₁+n₂)` step,
restricted to the key-proportional term). -/
theorem keysIn_union_le (s : List K) (A B : ExtView K V) :
    (keysIn s (union A B)).length ≤ (keysIn s A).length + (keysIn s B).length :=
  filter_length_le_add s _ _ _
    (fun k => isSome_union A B k)

/-- On **componentwise-disjoint** views the union's key set has exactly the
summed size: nothing is deduplicated, so the charge is attained. -/
theorem keysIn_union_eq_add (s : List K) (A B : ExtView K V)
    (h : ∀ k, ¬ ((A k).isSome = true ∧ (B k).isSome = true)) :
    (keysIn s (union A B)).length = (keysIn s A).length + (keysIn s B).length :=
  filter_length_eq_add_of_or s _ _ _
    (fun k => isSome_union A B k) h

/-- The empty view shares no key with any view (left operand). -/
theorem empty_disjoint_left (A : ExtView K V) :
    ∀ k, ¬ (((empty : ExtView K V) k).isSome = true ∧ (A k).isSome = true) := by
  intro k hk
  simp [empty] at hk

/-- The empty view shares no key with any view (right operand). -/
theorem empty_disjoint_right (A : ExtView K V) :
    ∀ k, ¬ ((A k).isSome = true ∧ ((empty : ExtView K V) k).isSome = true) := by
  intro k hk
  simp [empty] at hk

end ExtView

/-! ## §4 `|keys(E)| = |extNodes E| + |extEdges E|` (§0.2, §4) -/

namespace Extraction

variable {NK EK N E : Type}

/-- Node keys of `A`, counted against the node-key universe `s`. -/
def nodeKeys (s : List NK) (A : Extraction NK EK N E) : List NK :=
  ExtView.keysIn s A.extNodes

/-- Edge keys of `A`, counted against the edge-key universe `t`. -/
def edgeKeys (t : List EK) (A : Extraction NK EK N E) : List EK :=
  ExtView.keysIn t A.extEdges

/-- **§4's `|keys(E)|`**: the node-key count plus the edge-key count (the two
key spaces merge independently, `Core.hs:131-132`). -/
def size (s : List NK) (t : List EK) (A : Extraction NK EK N E) : Nat :=
  (nodeKeys s A).length + (edgeKeys t A).length

@[simp] theorem size_empty (s : List NK) (t : List EK) :
    size s t (emptyExtraction (NK := NK) (EK := EK) (N := N) (E := E)) = 0 := by
  show (ExtView.keysIn s (ExtView.empty : ExtView NK N)).length
      + (ExtView.keysIn t (ExtView.empty : ExtView EK E)).length = 0
  simp

/-- **Union's size is subadditive** — the induction invariant of the fold bound. -/
theorem size_merge_le (s : List NK) (t : List EK) (A B : Extraction NK EK N E) :
    size s t (mergeExtractions A B) ≤ size s t A + size s t B := by
  unfold size nodeKeys edgeKeys
  rw [mergeExtractions_extNodes, mergeExtractions_extEdges]
  have h₁ := ExtView.keysIn_union_le s A.extNodes B.extNodes
  have h₂ := ExtView.keysIn_union_le t A.extEdges B.extEdges
  omega

/-- On componentwise-disjoint views the union's size is **exactly** the sum, so
the `Σᵢ |keys(Eᵢ)|` bound of §4 is attained by disjoint families (AC-3's
harness uses disjoint views by construction). -/
theorem size_merge_eq_add (s : List NK) (t : List EK) (A B : Extraction NK EK N E)
    (hn : ∀ k, ¬ ((A.extNodes k).isSome = true ∧ (B.extNodes k).isSome = true))
    (he : ∀ k, ¬ ((A.extEdges k).isSome = true ∧ (B.extEdges k).isSome = true)) :
    size s t (mergeExtractions A B) = size s t A + size s t B := by
  unfold size nodeKeys edgeKeys
  rw [mergeExtractions_extNodes, mergeExtractions_extEdges]
  have h₁ := ExtView.keysIn_union_eq_add s A.extNodes B.extNodes hn
  have h₂ := ExtView.keysIn_union_eq_add t A.extEdges B.extEdges he
  omega

end Extraction

/-! ## §4 The cost-annotated streaming fold -/

variable {NK EK N E : Type}

/-- **Per-step key-visit charge** of `mergeExtractions A B`: the smaller
operand's key count. `Data.Map.union` is `O(m·log(n/m+1))` with `m = min`; this
is its key-proportional term (§4 justification, see the module header). -/
def mergeStepCost (s : List NK) (t : List EK) (A B : Extraction NK EK N E) : Nat :=
  min (Extraction.size s t A) (Extraction.size s t B)

/-- The cost-annotated `foldl' (⊕) init es`: returns the real fold value together
with the total key-visit charge. -/
def foldlCost (s : List NK) (t : List EK) (init : Extraction NK EK N E) :
    List (Extraction NK EK N E) → Extraction NK EK N E × Nat
  | [] => (init, 0)
  | E :: es =>
      let rest := foldlCost s t (mergeExtractions init E) es
      (rest.1, mergeStepCost s t init E + rest.2)

/-- The charge of `foldl' ⊕ emptyExtraction l` — §4's `cost`. -/
def nwayFoldCost (s : List NK) (t : List EK) (l : List (Extraction NK EK N E)) : Nat :=
  (foldlCost s t emptyExtraction l).2

/-- **The annotated fold computes the real fold**: `foldl' (⊕) init l`
(`Extract/Core.hs:124,466,506`). The charge is attached to the actual merge
sequence, not to a parallel toy computation. -/
theorem foldlCost_fst (s : List NK) (t : List EK) (init : Extraction NK EK N E)
    (l : List (Extraction NK EK N E)) :
    (foldlCost s t init l).1
      = l.foldl (fun acc A => mergeExtractions acc A) init := by
  induction l generalizing init with
  | nil => rfl
  | cons E es ih =>
      simp only [foldlCost]
      rw [ih]
      rfl

/-- **Linear total charge.** The key-visit charge of the whole fold is at most
the family's total key count. In words: every key of every input view is charged
once (when its view is the smaller operand of a step) and is never re-scanned;
the accumulator is not re-charged. -/
theorem foldlCost_snd_le (s : List NK) (t : List EK) (init : Extraction NK EK N E)
    (l : List (Extraction NK EK N E)) :
    (foldlCost s t init l).2 ≤ (l.map (Extraction.size s t)).sum := by
  induction l generalizing init with
  | nil => simp [foldlCost]
  | cons E es ih =>
      simp only [foldlCost, List.map_cons, List.sum_cons]
      have h₁ := ih (mergeExtractions init E)
      have h₂ : mergeStepCost s t init E ≤ Extraction.size s t E :=
        Nat.min_le_right _ _
      omega

/-! ## §4 The named theorem -/

/-- **`mergedExtraction_nwayFold_linear`** (AVI-620 §4, AVI-1306).

For a view family `{Eᵢ}_{i∈I}` and any key universes `s` (nodes), `t` (edges):

> `cost (foldl' ⊕ emptyExtraction [E₁…Eₙ]) = O( Σᵢ |keys(Eᵢ)| )`

spelled out with the explicit constant `1` and the §4 size
`|keys(Eᵢ)| = |extNodes Eᵢ| + |extEdges Eᵢ|`:

> `cost ≤ Σᵢ (|extNodes Eᵢ| + |extEdges Eᵢ|)`

and, at the same time, the annotated fold's value is exactly the streaming fold
`foldlMerge l = foldl' (⊕) emptyExtraction l` of `Ext/Strict.lean` — so the bound
is about the fold Graphos actually runs (`Extract/Core.hs:124,466,506`), not a
surrogate.

The bound is a single linear pass: the proof is `foldlCost_snd_le`, whose only
inequality is `min (|acc|) (|Eᵢ|) ≤ |Eᵢ|` — no key is re-scanned in a later step.
It is attained on componentwise-disjoint families (`Extraction.size_merge_eq_add`),
so the constant cannot be improved asymptotically.

**Reading of `O`.** This is a `Big-O` statement with constant `1` in the key
model; the `log` factor of `Data.Map.union`'s `split` path (node/pointer depth,
not key operations) is outside the counted unit — see the module header's
modelling note and the AVI-620 §4 authorship note. -/
theorem mergedExtraction_nwayFold_linear (s : List NK) (t : List EK)
    (l : List (Extraction NK EK N E)) :
    (foldlCost s t emptyExtraction l).1 = foldlMerge l ∧
      nwayFoldCost s t l ≤ (l.map (Extraction.size s t)).sum := by
  refine ⟨?_, foldlCost_snd_le s t emptyExtraction l⟩
  rw [foldlCost_fst]
  rfl

/-! ## Non-vacuity: concrete families (AC-3 reading)

`ExtView`/`Extraction` over `Nat` keys, so the cost can be *computed* by kernel
reduction. `triExtraction b` carries three node keys `b, b+1, b+2` and no edge
keys; `threeKeyFamily` places them at disjoint offsets over `[0,12)`, mirroring
the AC-3 harness (disjoint views, fixed `viewSize`). -/

/-- The three-key view at offsets `base, base+1, base+2`. -/
def triView (base : Nat) : ExtView Nat Nat :=
  fun k =>
    if k = base then some 0
    else if k = base + 1 then some 1
    else if k = base + 2 then some 2
    else none

/-- The three-key extraction view `⟨triView base, ∅⟩`. -/
def triExtraction (base : Nat) : Extraction Nat Nat Nat Nat :=
  ⟨triView base, ExtView.empty⟩

/-- The single-node-key view used to exercise the disjoint-size lemma. -/
def oneView (k₀ v : Nat) : ExtView Nat Nat :=
  fun k => if k = k₀ then some v else none

/-- The one-key extraction view `⟨oneView k₀ v, ∅⟩`. -/
def oneExtraction (k₀ v : Nat) : Extraction Nat Nat Nat Nat :=
  ⟨oneView k₀ v, ExtView.empty⟩

/-- Distinct `oneView`s are componentwise disjoint. -/
theorem oneView_disjoint {a b v w : Nat} (h : a ≠ b) :
    ∀ k, ¬ ((oneView a v k).isSome = true ∧ (oneView b w k).isSome = true) := by
  intro k hk
  unfold oneView at hk
  by_cases ha : k = a <;> by_cases hb : k = b <;> simp [ha, hb] at hk
  all_goals exact h (by omega)

example : Extraction.size (List.range 12) (List.range 12) (triExtraction 0) = 3 := by
  decide

/-- Two disjoint three-key views: charge `min 0 3 + min 3 3 = 3`, total size `6`. -/
example :
    nwayFoldCost (List.range 12) (List.range 12)
      [triExtraction 0, triExtraction 3] = 3 := by
  decide

/-- Four disjoint three-key views: charge `0 + 3 + 3 + 3 = 9`, total size `12`.
Doubling the family size (`2 → 4` views) increases the charge `3 → 9`; the
asymptotic ratio per doubling is `(2n−1)/(n−1) → 2`, i.e. the AC-3 recorded
`≈ ×2` per doubling at `n = 100…800`. -/
example :
    nwayFoldCost (List.range 12) (List.range 12)
      [triExtraction 0, triExtraction 3, triExtraction 6, triExtraction 9] = 9 := by
  decide

/-- The charge is bounded by the family's total key count (the named theorem,
instantiated). -/
example :
    nwayFoldCost (List.range 12) (List.range 12)
      [triExtraction 0, triExtraction 3, triExtraction 6, triExtraction 9]
      ≤ ([triExtraction 0, triExtraction 3, triExtraction 6, triExtraction 9].map
          (Extraction.size (List.range 12) (List.range 12))).sum :=
  (mergedExtraction_nwayFold_linear _ _ _).2

/-- The disjoint-size lemma, instantiated: merging the two disjoint one-key
views costs exactly the summed size (`1 + 1`). -/
example :
    Extraction.size (List.range 4) (List.range 4)
        (mergeExtractions (oneExtraction 0 7) (oneExtraction 1 9))
      = Extraction.size (List.range 4) (List.range 4) (oneExtraction 0 7)
        + Extraction.size (List.range 4) (List.range 4) (oneExtraction 1 9) :=
  Extraction.size_merge_eq_add _ _ _ _
    (oneView_disjoint (by decide))
    (ExtView.empty_disjoint_left (ExtView.empty : ExtView Nat Nat))

end GraphosFormalization
