/-
  Graphos formalization — Phase 1, INV-Q2: `Q` depends only on the partition's
  blocks (order independence).

  Requirements:
  `docs/math-requirements/AVI-618-auditable-modularity-lean-formalization-requirements.md`
  §2 INV-Q2 (`theorem modularity_partitionOnly`), §1.2 (the functional `(Q)`),
  and the INV-Q4 note that the *summation order is fixed*.

  ## What is proved

  If `π'` is obtained from `π` by permuting the member list of every block —
  the `permuteMembers` acceptance property — then

  ```
    modularity g π = modularity g π' .
  ```

  This is `theorem modularity_partitionOnly` plus two corollaries:

  * `modularity_reverseMembers`: reversing every community's `[NodeId]` list
    (the literal "swap member lists of every community" fixture) leaves `Q`
    unchanged;
  * `EdgeGraph.modularityNum_blocksPerm`: on the **exact** layer the numerator
    is invariant even under a reordering of the blocks themselves (the
    `CommunityMap` key order), not just of members inside a block.

  ## Why the model has no `CommunityMap` key order

  `Partition.blocks` is a list of blocks with the community ids stripped, so a
  `CommunityMap` key-order difference is *not representable* at this layer: two
  maps whose ids differ only in insertion order still iterate their
  communities in the same ascending-id order (`Map.keys`, `Data.Map.Strict`),
  so the fold sees the *same block sequence* in both. INV-Q2's "key order
  differs" case is therefore automatic in the implementation (the canonical
  order is part of the fold, INV-Q4), and the only content left to prove is
  member-list invariance — which is what `PermuteMembers` captures. The exact
  layer lemma below additionally records block-reordering invariance of
  `Qnum`, where integer/real arithmetic makes it genuinely true.

  ## Why `Float` block reordering is *not* claimed

  `modularityRaw` is a left fold of `Float` additions, and IEEE-754 addition is
  not associative, so a reordering of the *summands* is not value-preserving
  (`Modularity.lean` §1.2, and AVI-618 §2 INV-Q4 explicitly relies on a fixed
  fold order). That is exactly why the implementation folds `Map.keys` in
  ascending order. We prove member-list invariance for the `Float` rendering,
  which is the `permuteMembers` property the Haskell side exposes, and keep
  block-order invariance on the exact `modularityNum` layer where it holds.

  ## Proof shape

  `σ_tot(b)` is `(b.map deg).sum`, so it only depends on the multiset of
  members; `σ_in(b)` is a double sum `Σ_{v∈b} Σ_{w∈b} [adj v w]`, so it only
  depends on the members as a set/multiset as well. Both are therefore
  invariant under `List.Perm`, the per-block summands `communityTerm` are
  equal, and the fold is equal because the block sequence is unchanged.
-/
import GraphosFormalization.Phase1.Modularity
import GraphosFormalization.Phase1.RangeBound

namespace GraphosFormalization

/-! ## §0. Permutation-invariant `Nat` sums (core Lean ships no `Perm.sum_eq`) -/

/-- A `Nat`-valued list sum is invariant under permutation. Proven directly by
induction on the `List.Perm` derivation because core Lean has no
`List.Perm.sum_eq`. -/
theorem sum_map_perm {α : Type} (f : α → Nat) {l₁ l₂ : List α}
    (h : l₁.Perm l₂) : (l₁.map f).sum = (l₂.map f).sum := by
  induction h with
  | nil => rfl
  | cons x _ ih => simp [ih]
  | swap x y l =>
      simp only [List.map_cons, List.sum_cons]
      omega
  | trans _ _ ih₁ ih₂ => exact ih₁.trans ih₂

/-! ## §1. The block aggregates see a block only through its members -/

/-- `σ_tot(b)` only depends on the multiset of members: `(b.map deg).sum`. -/
theorem sigmaTot_perm {V : Type} (g : SupportGraph V) {b b' : List V}
    (h : b.Perm b') : sigmaTot g b = sigmaTot g b' := by
  unfold sigmaTot
  exact sum_map_perm g.deg h

/-- `σ_in(b)` only depends on the multiset of members. It is the double sum
`Σ_{v∈b} Σ_{w∈b} [adj v w]`; permuting the outer list permutes the summands,
permuting the inner list leaves each count unchanged. -/
theorem sigmaIn_perm {V : Type} (g : SupportGraph V) {b b' : List V}
    (h : b.Perm b') : sigmaIn g b = sigmaIn g b' := by
  unfold sigmaIn
  have hinner : ∀ v : V,
      (b.filter (fun w => g.adj v w)).length
        = (b'.filter (fun w => g.adj v w)).length :=
    fun v => (h.filter (fun w => g.adj v w)).length_eq
  calc (b.map (fun v => (b.filter (fun w => g.adj v w)).length)).sum
      = (b.map (fun v => (b'.filter (fun w => g.adj v w)).length)).sum := by
        rw [List.map_congr_left]
        exact fun v _ => hinner v
    _ = (b'.map (fun v => (b'.filter (fun w => g.adj v w)).length)).sum :=
        sum_map_perm (fun v => (b'.filter (fun w => g.adj v w)).length) h

/-- The summand of `(Q)` is unchanged when the block's members are permuted. -/
theorem communityTerm_perm {V : Type} (g : SupportGraph V) {b b' : List V}
    (h : b.Perm b') : communityTerm g b = communityTerm g b' := by
  unfold communityTerm
  rw [sigmaIn_perm g h, sigmaTot_perm g h]

/-! ## §2. `PermuteMembers`: the `permuteMembers` relation on partition blocks -/

/-- `π'` is obtained from `π` by permuting the member list of every block while
keeping the block sequence (the canonical `CommunityMap` key order) unchanged.

This is the Lean counterpart of the Haskell `permuteMembers` acceptance
property: community ids and their relative order are untouched, only the
`[NodeId]` order inside each block may differ. -/
inductive PermuteMembers : List (List V) → List (List V) → Prop
  | nil : PermuteMembers [] []
  | cons {b b' : List V} {bs bs' : List (List V)}
      (hb : b.Perm b') (hbs : PermuteMembers bs bs') :
      PermuteMembers (b :: bs) (b' :: bs')

/-- A left fold whose step respects block membership is invariant under a
`PermuteMembers` relation, for every starting accumulator. -/
theorem foldl_permuteMembers {V : Type} {f : Float → List V → Float}
    (hf : ∀ q b b', b.Perm b' → f q b = f q b') :
    ∀ {bs bs' : List (List V)}, PermuteMembers bs bs' →
      ∀ q : Float, bs.foldl f q = bs'.foldl f q := by
  intro bs bs' h
  induction h with
  | nil => intro q; rfl
  | cons hb _ ih =>
      intro q
      simp only [List.foldl_cons]
      rw [hf q _ _ hb]
      exact ih (f q _)

/-! ## §3. INV-Q2 — `theorem modularity_partitionOnly` -/

/-- **INV-Q2 (AVI-618 §2) — `modularity_partitionOnly`.**

For a support graph `G_s` and two partitions `π`, `π'` that induce the same
blocks and differ only by a permutation of the members *inside* each block,
`Q(π) = Q(π')`.

This is the Lean form of the acceptance property
`∀ g cm. let cm' = permuteMembers cm in modularity g cm == modularity g cm'`.
The `m = 0` pin (`Modularity.lean`, `Community.hs:165-166`) makes both sides
equal on an edgeless graph; for `m ≠ 0`, `modularityRaw` is a fold over the same
block sequence with pointwise-equal summands (`communityTerm_perm`), hence
equal. -/
theorem modularity_partitionOnly {V : Type} (g : SupportGraph V)
    {π π' : Partition V} (h : PermuteMembers π.blocks π'.blocks) :
    modularity g π = modularity g π' := by
  unfold modularity
  by_cases hm : g.m = 0
  · rw [if_pos hm, if_pos hm]
  · rw [if_neg hm, if_neg hm]
    unfold modularityRaw
    exact foldl_permuteMembers
      (fun q b b' hb => by rw [communityTerm_perm g hb]) h 0.0

/-- The `permuteMembers` fixture operation as a partition map: reverse each
community's member list (on a two-member block this is exactly a swap). -/
def Partition.reverseMembers {V : Type} (π : Partition V) : Partition V :=
  ⟨π.blocks.map List.reverse⟩

/-- Reversing every member list is a `PermuteMembers` relation. -/
theorem permuteMembers_map_reverse {V : Type} (bs : List (List V)) :
    PermuteMembers (bs.map List.reverse) bs := by
  induction bs with
  | nil => exact PermuteMembers.nil
  | cons b bs ih =>
      exact PermuteMembers.cons (List.reverse_perm b) ih

/-- **INV-Q2 corollary** — the acceptance fixture's operation: permuting the
members of *every* community (`[NodeId]` reversal, e.g. swapping the two
members of each block) leaves `Q` unchanged. -/
theorem modularity_reverseMembers {V : Type} (g : SupportGraph V)
    (π : Partition V) : modularity g π.reverseMembers = modularity g π :=
  modularity_partitionOnly g (permuteMembers_map_reverse π.blocks)

/-! ## §4. Exact layer: block reordering (`CommunityMap` key order) is invariant

`modularityNum` (`RangeBound.lean`) is the exact integer numerator
`Qnum = (2m)²·Q`. Over integers the block sum is genuinely order-insensitive, so
this records the "key order differs" half of INV-Q2 where it is true. The
`Float` rendering deliberately keeps the canonical fold order (INV-Q4), so it
never needs to reorder summands. -/

namespace EdgeGraph

variable {V : Type} [DecidableEq V]

/-- **INV-Q2, exact layer.** Reordering the blocks of a partition (the
`CommunityMap` key order) does not change the modularity numerator. -/
theorem modularityNum_blocksPerm (g : EdgeGraph V) {π π' : Partition V}
    (h : π.blocks.Perm π'.blocks) : modularityNum g π = modularityNum g π' := by
  unfold modularityNum
  have hA : (π.blocks.map (fun b => 2 * g.m * g.sigmaIn b)).sum
      = (π'.blocks.map (fun b => 2 * g.m * g.sigmaIn b)).sum :=
    sum_map_perm (fun b => 2 * g.m * g.sigmaIn b) h
  have hT : (π.blocks.map (fun b => g.sigmaTot b * g.sigmaTot b)).sum
      = (π'.blocks.map (fun b => g.sigmaTot b * g.sigmaTot b)).sum :=
    sum_map_perm (fun b => g.sigmaTot b * g.sigmaTot b) h
  rw [hA, hT]

end EdgeGraph

/-! ## §5. Fixture: swap every community's member list ⇒ `Q` unchanged

A 4-node path `0—1—2—3` (`m = 3`) partitioned as `{{0,1}, {2,3}}`. Swapping the
member lists gives `{{1,0}, {3,2}}`; the fixture records that `Q` is unchanged
and that the graph is non-trivial (`m ≠ 0`, so the `modularity` guard does not
hide the claim). -/

/-- The 4-node path `0—1—2—3`. -/
def path4 : SupportGraph (Fin 4) where
  vertices := [0, 1, 2, 3]
  nodup := by decide
  complete := by decide
  adj := fun u v =>
    decide ((u, v) ∈ ([(0, 1), (1, 0), (1, 2), (2, 1), (2, 3), (3, 2)] :
      List (Fin 4 × Fin 4)))
  symm := by decide
  irrefl := by decide

/-- `{{0,1}, {2,3}}` — the two blocks `[0,1]` and `[2,3]`. -/
def path4Blocks : Partition (Fin 4) where
  blocks := [[0, 1], [2, 3]]

/-- `{{1,0}, {3,2}}` — every member list swapped. -/
def path4BlocksSwapped : Partition (Fin 4) where
  blocks := [[1, 0], [3, 2]]

/-- The fixture graph has three edges, so `m ≠ 0` and `Q` is not pinned to
`0.0` by the INV-Q0 guard. -/
theorem path4_m : path4.m = 3 := by decide

theorem path4_permuteMembers :
    PermuteMembers path4BlocksSwapped.blocks path4Blocks.blocks :=
  PermuteMembers.cons (List.reverse_perm [0, 1])
    (PermuteMembers.cons (List.reverse_perm [2, 3]) PermuteMembers.nil)

/-- **INV-Q2 fixture.** Swapping the member list of every community leaves `Q`
unchanged on a graph with edges. -/
theorem path4_q_unchanged :
    modularity path4 path4BlocksSwapped = modularity path4 path4Blocks :=
  modularity_partitionOnly path4 path4_permuteMembers

end GraphosFormalization
