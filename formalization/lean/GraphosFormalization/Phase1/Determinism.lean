/-
  Graphos formalization — Phase 1, INV-Q4: deterministic summation order.

  Requirements:
  `docs/math-requirements/AVI-618-auditable-modularity-lean-formalization-requirements.md`
  §2 INV-Q4 (`theorem modularity_deterministic`), §5 fixture 7 (two calls
  bit-equal), §0.1 (`CommunityMap = Map CommunityId [NodeId]`).

  Pinned Haskell surface:
    * `modularity` fold       — `src/Graphos/Domain/Community.hs:151-167`
                                (`foldl' acc 0.0 (Map.keys cm)`)
    * `CommunityMap`          — `src/Graphos/Domain/Types/Graph.hs:43`
    * `computeCommunityStats` — `src/Graphos/Domain/Community.hs:92-107`

  ## What is proved

  ```
    modularity_deterministic          : ∀ g π, modularity g π = modularity g π
    modularity_map_deterministic      : the same on the `CommunityMap` surface
    modularity_foldOrder_perm         : same map bindings, different insertion
                                        order ⇒ bit-equal `Q`
    modularity_canonicalOrder_stable  : re-canonicalising a map cannot change `Q`
    keysInFoldOrder_sorted            : the fold order is ascending by
                                        `CommunityId`
    keysInFoldOrder_unique            : ascending order is the *same list* for
                                        every enumeration of the same domain
    fold_order_matters                : a non-associative combiner gives
                                        different results for different orders
  ```

  ## Why the fold order is part of the statement

  IEEE-754 `Float`/`Double` addition is not associative, so `Σ_c` is only a
  function of `(g, cm)` once the summation order is fixed. The implementation
  fixes it by folding `Map.keys cm`, and `Data.Map.Strict` iterates keys in
  ascending order — an order that depends only on the *key set*, never on
  insertion order. Here a `CommunityMap` is therefore modelled as the finite
  function a `Data.Map` denotes: a duplicate-free key list plus a
  `block : CommunityId → List V`. The fold order is

  ```
    keysInFoldOrder cm := cm.keys.mergeSort (fun a b => decide (a ≤ b)) ,
  ```

  and the lemmas below establish that it is (a) ascending, (b) a permutation of
  the map's domain, and (c) the *same list* for any two enumerations of that
  domain, so the fold — and hence `Q` — is a deterministic function of
  `(g, cm)`.

  ## Bit equality vs `Float.beq`

  Lean's `Float` is a wrapper around the native binary64 value
  (`Init/Data/Float.lean`), so `=` on `Float` is structural equality of the
  IEEE-754 payload — bit equality. We deliberately state the theorem with `=`
  and not `Float.beq` (`==`), because `Float.beq` is *not* reflexive on `NaN`
  whereas `=` is (and `modularity` never needs the `NaN` case: `m = 0` is
  pinned by INV-Q0 and the divisors are `2m > 0` otherwise). Kernel Lean cannot
  reduce `Float` arithmetic (`Float.add` is `opaque @[extern]`), so the
  non-associativity *witness* below is stated on `Nat`, where reduction is
  complete; the `Float`-level determinism claim is `rfl` and needs no
  computation.

  ## What is *not* claimed

  A general `Float` order-insensitivity claim is false (that is the whole point
  of INV-Q4), and Lean 4.14 core provides no `Float` order or associativity
  lemmas — `Float` has only an opaque `BEq` and no `DecidableEq`. The exact
  (integer) layer is where order-invariance genuinely holds, and that is
  recorded in `Phase1/PartitionOnly.lean` (`modularityNum_blocksPerm`).
-/
import GraphosFormalization.Phase1.PartitionOnly

namespace GraphosFormalization

/-! ## §1. `CommunityMap` as the finite function `Data.Map` denotes -/

/-- `CommunityId = Int` in Haskell (`Types/Graph.hs:41`). Kept as `Int` here:
the fold order only needs the total order on community ids, and `Int` carries
it in core Lean (`Int.le_total`, `Int.le_antisymm`). -/
abbrev CommunityId := Int

/-- A `CommunityMap = Map CommunityId [NodeId]` (`Types/Graph.hs:43`).
`Data.Map` is an ordered tree, so a map *is* its key→block function: `keys` is
the (duplicate-free) domain and `block` the members of one community. Insertion
order is not part of the value — the ascending-key enumeration below is what
makes the fold canonical. -/
structure CommunityMap (V : Type) where
  /-- The domain `Map.keys cm`, in the map's own stored order (irrelevant). -/
  keys : List CommunityId
  /-- `Data.Map` has no duplicate keys. -/
  keys_nodup : keys.Nodup
  /-- The members of community `k` (`Map.lookup k cm`, defaulting to `[]`). -/
  block : CommunityId → List V

namespace CommunityMap

variable {V : Type}

/-- The fold order: `Map.keys`/`Map.elems` iterate in ascending `CommunityId`
order (`Data.Map.Strict`), which is exactly `mergeSort` by the key's `≤`. -/
def keysInFoldOrder (cm : CommunityMap V) : List CommunityId :=
  cm.keys.mergeSort (fun a b => decide (a ≤ b))

/-- The ascending enumeration is an enumeration of *all* keys — no community is
dropped from, or duplicated in, the fold. -/
theorem keysInFoldOrder_perm (cm : CommunityMap V) :
    (cm.keysInFoldOrder).Perm cm.keys :=
  List.mergeSort_perm cm.keys _

/-- Membership characterisation of the fold order (used by the exact-layer
cross-check: the fold sees exactly the map's communities). -/
theorem mem_keysInFoldOrder {cm : CommunityMap V} {k : CommunityId} :
    k ∈ cm.keysInFoldOrder ↔ k ∈ cm.keys := by
  simp [keysInFoldOrder,
    (List.mem_mergeSort (l := cm.keys) (le := fun a b : CommunityId => decide (a ≤ b))
      (a := k))]

/-- The fold order is **ascending**: `Map.elems` order is sorted by
`CommunityId`. This is the "fixed order" half of INV-Q4. -/
theorem keysInFoldOrder_sorted (cm : CommunityMap V) :
    (cm.keysInFoldOrder).Pairwise (fun a b => a ≤ b) := by
  have h := List.sorted_mergeSort
    (le := fun a b : CommunityId => decide (a ≤ b))
    (by
      intro a b c hab hbc
      rw [decide_eq_true_eq] at hab hbc ⊢
      exact Int.le_trans hab hbc)
    (by
      intro a b
      rw [Bool.or_eq_true, decide_eq_true_eq, decide_eq_true_eq]
      exact Int.le_total a b)
    cm.keys
  simpa [keysInFoldOrder, decide_eq_true_eq] using h

/-- The ascending order is **total**: any two community ids are comparable, so
the fold order has no ties to break and no traversal-order dependence. -/
theorem keysInFoldOrder_total (a b : CommunityId) : a ≤ b ∨ b ≤ a :=
  Int.le_total a b

/-- The ascending order is **stable**: `mergeSort` leaves an already ascending
key list untouched, so a second canonicalisation cannot reorder the fold. -/
theorem keysInFoldOrder_idempotent (cm : CommunityMap V) :
    (cm.keysInFoldOrder).mergeSort (fun a b => decide (a ≤ b))
      = cm.keysInFoldOrder := by
  apply List.mergeSort_of_sorted
  simpa [decide_eq_true_eq] using cm.keysInFoldOrder_sorted

/-- **The fold order is a function of the map, not of insertion order.** Any two
enumerations of the same key set are permutations of each other; the ascending
enumeration is the unique sorted one (`Perm.eq_of_sorted` with the antisymmetric
`≤` on `Int`), so both maps fold their communities in the *same* order.

This is the INV-Q4 claim that `Data.Map.Strict.elems` order is "total and stable
across calls on equal inputs": equal maps have equal domains up to permutation,
hence literally equal ascending enumerations. -/
theorem keysInFoldOrder_unique {cm₁ cm₂ : CommunityMap V}
    (h : cm₁.keys.Perm cm₂.keys) :
    cm₁.keysInFoldOrder = cm₂.keysInFoldOrder := by
  apply List.Perm.eq_of_sorted (le := fun a b => a ≤ b)
  · intro a b _ _ hab hba
    exact Int.le_antisymm hab hba
  · exact cm₁.keysInFoldOrder_sorted
  · exact cm₂.keysInFoldOrder_sorted
  · exact (cm₁.keysInFoldOrder_perm).trans
      (h.trans (cm₂.keysInFoldOrder_perm).symm)

/-- The block sequence that the `modularity` fold consumes, in ascending-key
order. This is `Map.elems cm` as a list of blocks: no `IntMap`/`Set` iteration
enters here — the only traversals are the sort of `cm.keys` and the `map` of
`cm.block` over the result. -/
def blocksInFoldOrder (cm : CommunityMap V) : List (List V) :=
  (cm.keysInFoldOrder).map cm.block

/-- The partition a `CommunityMap` induces, in the canonical fold order. -/
def toPartition (cm : CommunityMap V) : Partition V :=
  ⟨cm.blocksInFoldOrder⟩

/-- Canonicalise the stored key order: the map with its keys already ascending.
Re-canonicalising an already canonical map is the identity. -/
def canonicalize (cm : CommunityMap V) : CommunityMap V where
  keys := cm.keysInFoldOrder
  keys_nodup := cm.keysInFoldOrder_perm.symm.nodup cm.keys_nodup
  block := cm.block

/-- Canonicalising does not change which blocks the fold sees. -/
theorem canonicalize_blocks (cm : CommunityMap V) :
    cm.canonicalize.blocksInFoldOrder = cm.blocksInFoldOrder := by
  unfold blocksInFoldOrder
  show ((cm.keysInFoldOrder).mergeSort
        (fun a b : CommunityId => decide (a ≤ b))).map cm.block
      = (cm.keysInFoldOrder).map cm.block
  rw [keysInFoldOrder_idempotent]

/-- Two maps with the same bindings produce the same block sequence (and hence
the same fold): the order of `cm.keys` is irrelevant, and `block` is the map's
own key→members function. -/
theorem blocksInFoldOrder_unique {cm₁ cm₂ : CommunityMap V}
    (hkeys : cm₁.keys.Perm cm₂.keys)
    (hblock : ∀ k, cm₁.block k = cm₂.block k) :
    cm₁.blocksInFoldOrder = cm₂.blocksInFoldOrder := by
  simp only [blocksInFoldOrder, keysInFoldOrder_unique hkeys]
  exact List.map_congr_left (fun k _ => hblock k)

end CommunityMap

/-! ## §2. INV-Q4 — `theorem modularity_deterministic` -/

/-- **INV-Q4 (AVI-618 §2) — `modularity_deterministic`.**

`modularity g π` is a deterministic pure function of `(g, π)`: for every graph
and partition, two calls return the same `Float`. Because Lean's `Float`
equality is structural equality of the IEEE-754 binary64 payload, this is bit
equality (see the module header for why we use `=` rather than `Float.beq`).

The statement is `rfl`: `modularity` is a *definition*, and the real content of
INV-Q4 is that the definition folds a fixed, canonical order — proved by
`CommunityMap.keysInFoldOrder_unique` / `blocksInFoldOrder_unique` below, which
show that the order is a function of the map alone. -/
theorem modularity_deterministic {V : Type} (g : SupportGraph V)
    (π : Partition V) : modularity g π = modularity g π :=
  rfl

/-- Two independent evaluations on equal inputs agree (the congruence closure of
`modularity_deterministic`: determinism is preserved by substituting equal
arguments). -/
theorem modularity_deterministic_eq {V : Type} {g g' : SupportGraph V}
    {π π' : Partition V} (hg : g = g') (hπ : π = π') :
    modularity g π = modularity g' π' := by
  rw [hg, hπ]

/-- INV-Q4 on the Haskell surface: the fold over `Map.keys cm` makes `Q` a
deterministic function of the `CommunityMap`. -/
theorem modularity_map_deterministic {V : Type} (g : SupportGraph V)
    (cm : CommunityMap V) :
    modularity g cm.toPartition = modularity g cm.toPartition :=
  rfl

/-- **INV-Q4, fold-order form.** Two `CommunityMap`s that denote the same map —
the same bindings, hence the same key set up to insertion order and the same
block per key — give the identical fold order and therefore bit-equal `Q`.

This is the substantive half of INV-Q4: the summation order is not merely
"reproducible on the same data structure instance", it is determined by the
map value. -/
theorem modularity_foldOrder_perm {V : Type} (g : SupportGraph V)
    {cm₁ cm₂ : CommunityMap V} (hkeys : cm₁.keys.Perm cm₂.keys)
    (hblock : ∀ k, cm₁.block k = cm₂.block k) :
    modularity g cm₁.toPartition = modularity g cm₂.toPartition := by
  unfold CommunityMap.toPartition
  rw [CommunityMap.blocksInFoldOrder_unique hkeys hblock]

/-- **INV-Q4, stability form.** Re-entering the canonical order is a no-op for
the fold: the ascending-key enumeration is idempotent, so canonicalising a map
a second time cannot change `Q`. -/
theorem modularity_canonicalOrder_stable {V : Type} (g : SupportGraph V)
    (cm : CommunityMap V) :
    modularity g cm.canonicalize.toPartition = modularity g cm.toPartition := by
  unfold CommunityMap.toPartition
  rw [CommunityMap.canonicalize_blocks]

/-! ## §3. Why the order has to be fixed (abstract witness)

Kernel Lean cannot evaluate `Float` arithmetic (`Float.add` is `opaque
@[extern]`, and `Float` has no `DecidableEq`), so a *concrete* IEEE-754
non-associativity witness cannot be proved here. The structural reason is
nevertheless formal and cheap on `Nat`: a non-associative combiner applied to
the same multiset in two different orders gives different results, which is
exactly what INV-Q4's "fix the fold order" requirement avoids for `Double`. -/

/-- A non-associative combiner on `Nat`: `f a b = 2a + b`. -/
def nonAssociative (a b : Nat) : Nat := 2 * a + b

/-- **Order matters for a non-associative fold.** The same summands
`{0, 1, 2}` in ascending versus descending order give `4` versus `10`. This is
the mechanism that forces `modularity` to pin `Map.elems` order: `Double`
addition has the same defect, so an unordered fold would not be a function of
`(g, cm)`. -/
theorem fold_order_matters :
    [0, 1, 2].foldl nonAssociative 0 ≠ [2, 1, 0].foldl nonAssociative 0 := by
  decide

/-! ## §4. Fixture 7 — two calls are bit-equal (AVI-618 §5)

Fixture 7 of §5: "Two calls `modularity g cm` on the same `cm` are bit-equal".
The fixture below uses the triangle `0—1—2` (fixture 1's graph) with the
all-in-one community, and additionally exercises the `CommunityMap` path with
two different key insertion orders. -/

/-- The triangle `0—1—2` (3 edges), the graph of AVI-618 §5 fixture 1. -/
def fixtureTriangle : SupportGraph (Fin 3) where
  vertices := [0, 1, 2]
  nodup := by decide
  complete := by decide
  adj := fun u v =>
    decide ((u, v) ∈ ([(0, 1), (1, 0), (1, 2), (2, 1), (0, 2), (2, 0)] :
      List (Fin 3 × Fin 3)))
  symm := by decide
  irrefl := by decide

/-- The single community `{0 ↦ [0,1,2]}` over the triangle. -/
def fixtureTriangleSingle : Partition (Fin 3) where
  blocks := [[0, 1, 2]]

/-- The triangle has three edges, so `m ≠ 0` and the INV-Q0 pin does not hide
the fixture's value. -/
theorem fixtureTriangle_m : fixtureTriangle.m = 3 := by decide

/-- **INV-Q4 fixture 7.** Two calls of `modularity` on the same `(g, cm)` are
bit-equal. -/
theorem fixture7_twoCallsBitEqual :
    modularity fixtureTriangle fixtureTriangleSingle
      = modularity fixtureTriangle fixtureTriangleSingle :=
  modularity_deterministic fixtureTriangle fixtureTriangleSingle

/-- Fixture 7's `CommunityMap`, keys inserted in ascending order:
`{0 ↦ [0,1], 1 ↦ [2]}`. -/
def fixtureMapAscending : CommunityMap (Fin 3) where
  keys := [0, 1]
  keys_nodup := by decide
  block := fun k => if k = 0 then [0, 1] else [2]

/-- The same `CommunityMap` with its keys inserted in descending order. -/
def fixtureMapDescending : CommunityMap (Fin 3) where
  keys := [1, 0]
  keys_nodup := by decide
  block := fun k => if k = 0 then [0, 1] else [2]

/-- The two fixture maps enumerate the same domain (only insertion order
differs). -/
theorem fixtureMap_keys_perm :
    fixtureMapAscending.keys.Perm fixtureMapDescending.keys :=
  (List.Perm.swap 0 1 []).symm

/-- **INV-Q4 fixture 7, map form.** Two `CommunityMap`s with the same bindings
but opposite insertion order are folded in the *same* ascending-key order, so
their `Q` values are bit-equal. -/
theorem fixture7_insertionOrderBitEqual :
    modularity fixtureTriangle fixtureMapAscending.toPartition
      = modularity fixtureTriangle fixtureMapDescending.toPartition :=
  modularity_foldOrder_perm fixtureTriangle fixtureMap_keys_perm (fun _ => rfl)

/-- The ascending-insertion fixture is already canonical, so its fold order is
the stored key list. -/
theorem fixtureMapAscending_foldOrder :
    fixtureMapAscending.keysInFoldOrder = [0, 1] := by
  unfold CommunityMap.keysInFoldOrder
  apply List.mergeSort_of_sorted
  decide

/-- The canonical fold order of the descending-insertion fixture is the
ascending key order `[0, 1]` — sorting, not insertion order, decides the fold. -/
theorem fixtureMapDescending_foldOrder :
    fixtureMapDescending.keysInFoldOrder = [0, 1] := by
  rw [CommunityMap.keysInFoldOrder_unique fixtureMap_keys_perm.symm,
    fixtureMapAscending_foldOrder]

end GraphosFormalization
