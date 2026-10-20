/-
  Graphos formalization — Phase 1, AVI-618 §6: the pinned cross-check fixtures.

  Requirements:
  `docs/math-requirements/AVI-618-auditable-modularity-lean-formalization-requirements.md`
  §5 (the fixture table) and §6 (the cross-check contract).

  ## What this module is

  The §6 contract says: for every pinned fixture `f` of §5,

  ```
    | Q_lean(f) − Q_hs(f) | ≤ eps ,      Q_hs(f) = modularity g_f cm_f
  ```

  where the Lean side is *exact* (`ℚ`) and the Haskell side is `Double`. This
  module is the Lean half: it pins each fixture of §5 as concrete
  `EdgeGraph`/`Partition` data in the same exact (integer) layer as INV-Q1/INV-Q5,
  proves its exact numerator `Qnum = (2m)²·Q` (`by decide`), and exposes the
  rows as `crossCheckRows` so the executable arm
  (`Harness/CrossCheck.lean`) can print them.

  The Haskell half lives in
  `tests/Graphos/Domain/ModularityCrossCheckSpec.hs`; it builds the same
  fixtures through `extractionFromLists`/`buildGraph`, calls `modularity`, and
  asserts the tolerance against the table this module emits. Regenerate the
  checked-in table with:

  ```
  cd formalization/lean && lake exe modularity-crosscheck --tsv
  ```

  ## Why the exact layer again

  Kernel Lean cannot evaluate `Float` arithmetic (`Float.add`/`Float.div` are
  `opaque @[extern]`, `Float` has no `DecidableEq` and core Lean ships no
  `Float` order lemmas), so `Q_lean` is the exact rational
  `Qnum / (2m)²`. AVI-618 §6 pins the contract to rational fixtures precisely
  so that both sides reduce to the same value; the `Double` rounding is what
  `eps = 2⁻²⁴` absorbs.

  ## Fixture ↔ §5 row map

  | §5 row | fixture data | partition | exact `Q` |
  |---|---|---|---|
  | 1 | `triangle` | `triangleSingle` | `0` |
  | 2 | `triangle` | `triangleSingletons` | `−1/3` |
  | 3 | `twoCliquesBridge` | `twoCliquesSplit` (Leiden output) | `11/26` |
  | 4 | `path6` | `path6Singletons` → `path6AfterLocalMove` | `−9/50` → `11/50` |
  | 5 | `twoCliquesBridge` | four reachable partitions | each in `[−½,1]` |
  | 6 | `twoCliquesBridge` | `twoCliquesSplit` | `11/26` (per-edge oracle) |
  | 7 | `twoCliquesBridge` | `twoCliquesSplit` / block-permuted | `11/26` (bit-equal) |
-/
import GraphosFormalization.Phase1.FixturesExact
import GraphosFormalization.Phase1.PartitionOnly

namespace GraphosFormalization

/-- One row of the §6 cross-check table: a `(fixture, partition)` pair, its edge
count `m`, and the exact numerator `qnum = (2m)²·Q`. -/
structure CrossCheckRow where
  /-- §5 row identifier (`"1"` … `"7"`, with lettered sub-rows for fixture 4/5). -/
  fixture : String
  /-- Name of the pinned partition. -/
  partition : String
  /-- `m`, the number of edges of `G_s`. -/
  m : Nat
  /-- `Qnum = (2m)²·Q`, the exact numerator (`modularityNum`). -/
  qnum : Int
  deriving Repr

namespace EdgeGraph

variable {V : Type} [DecidableEq V]

/-! ## §1. Fixtures 1 & 2 — the triangle `{a,b,c}` with 3 edges

The triangle itself (`triangleGraph`, `triangleSingle`, `triangleSingletons`) is
pinned by INV-Q5 in `Phase1/FixturesExact.lean` §5; this section only adds the
cross-check instantiations. The Haskell arm labels the vertices `"a"`, `"b"`,
`"c"` in vertex order. -/

theorem triangle_single_isSetPartition :
    IsSetPartition triangleSingle triangleGraph.vertices := by
  unfold IsSetPartition Partition.Subset Partition.Covers Partition.PairwiseDisjoint
  decide

theorem triangle_singletons_isSetPartition :
    IsSetPartition triangleSingletons triangleGraph.vertices := by
  unfold IsSetPartition Partition.Subset Partition.Covers Partition.PairwiseDisjoint
  decide

/-- **INV-Q1 on fixture 1** — the range bound instantiated on the single-block
triangle partition (`m = 3 ≥ 1`). -/
theorem triangleSingle_range :
    -(2 * (triangleGraph.m : Int) * (triangleGraph.m : Int))
        ≤ modularityNum triangleGraph triangleSingle ∧
      modularityNum triangleGraph triangleSingle
        ≤ 4 * (triangleGraph.m : Int) * (triangleGraph.m : Int) :=
  modularity_range_bound triangleGraph triangleSingle
    triangle_single_isSetPartition (by rw [triangleGraph_m]; decide)

/-! ## §2. Fixtures 3, 5, 6, 7 — two 4-cliques joined by one bridge -/

/-- Fixtures 3/5/6/7 graph: two 4-cliques `{0,1,2,3}` and `{4,5,6,7}` joined by
the single bridge `3–4` (`m = 2·6 + 1 = 13`). Vertex order matches the Haskell
arm's `"a".."h"`. -/
def twoCliquesBridge : EdgeGraph (Fin 8) where
  vertices := [0, 1, 2, 3, 4, 5, 6, 7]
  nodup := by decide
  complete := by decide
  edges :=
    [ (0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)
    , (4, 5), (4, 6), (4, 7), (5, 6), (5, 7), (6, 7)
    , (3, 4) ]
  edges_nodup := by decide
  edges_noLoop := by decide
  edges_mem := by decide
  edges_noRev := by decide

theorem twoCliquesBridge_m : twoCliquesBridge.m = 13 := rfl

/-- §5 fixture 3 partition — the Leiden output on this graph: the two cliques
separately. (The Haskell arm asserts `detectCommunities g` equals this block
set, so the pinned table cannot drift from the implementation.) -/
def twoCliquesSplit : Partition (Fin 8) := ⟨[[0, 1, 2, 3], [4, 5, 6, 7]]⟩

/-- Fixture 5 row (a): one block containing every vertex. -/
def twoCliquesSingle : Partition (Fin 8) := ⟨[[0, 1, 2, 3, 4, 5, 6, 7]]⟩

/-- Fixture 5 row (b): all singletons. -/
def twoCliquesSingletons : Partition (Fin 8) :=
  ⟨[[0], [1], [2], [3], [4], [5], [6], [7]]⟩

/-- Fixture 5 row (c): a reachable *cross* partition (two vertices of the first
clique with one of the second, and vice versa). -/
def twoCliquesCross : Partition (Fin 8) := ⟨[[0, 1, 2, 4], [3, 5, 6, 7]]⟩

theorem twoCliquesSplit_isSetPartition :
    IsSetPartition twoCliquesSplit twoCliquesBridge.vertices := by
  unfold IsSetPartition Partition.Subset Partition.Covers Partition.PairwiseDisjoint
  decide

theorem twoCliquesSingle_isSetPartition :
    IsSetPartition twoCliquesSingle twoCliquesBridge.vertices := by
  unfold IsSetPartition Partition.Subset Partition.Covers Partition.PairwiseDisjoint
  decide

theorem twoCliquesSingletons_isSetPartition :
    IsSetPartition twoCliquesSingletons twoCliquesBridge.vertices := by
  unfold IsSetPartition Partition.Subset Partition.Covers Partition.PairwiseDisjoint
  decide

theorem twoCliquesCross_isSetPartition :
    IsSetPartition twoCliquesCross twoCliquesBridge.vertices := by
  unfold IsSetPartition Partition.Subset Partition.Covers Partition.PairwiseDisjoint
  decide

/-- §5 fixture 3: `σ_in = 12` per block, `σ_tot = 13` per block, `2m = 26`, so
`Qnum = 2·(26·12 − 13²) = 286`, i.e. `Q = 286/676 = 11/26` (in `[−½, 1]`). -/
theorem twoCliquesSplit_qnum :
    modularityNum twoCliquesBridge twoCliquesSplit = 286 := by decide

/-- Fixture 5 row (a): the single block gives `Qnum = 0` (and `Q = 0`). -/
theorem twoCliquesSingle_qnum :
    modularityNum twoCliquesBridge twoCliquesSingle = 0 := by decide

/-- Fixture 5 row (b): all singletons give `Qnum = −Σ_v k_v² = −86`
(`k = 3,3,3,4,4,3,3,3`), i.e. `Q = −86/676 = −43/338 ≈ −0.1272`. -/
theorem twoCliquesSingletons_qnum :
    modularityNum twoCliquesBridge twoCliquesSingletons = -86 := by decide

/-- Fixture 5 row (c): the cross partition gives `Qnum = −26`, i.e.
`Q = −26/676 = −1/26 ≈ −0.0385`. -/
theorem twoCliquesCross_qnum :
    modularityNum twoCliquesBridge twoCliquesCross = -26 := by decide

/-- **INV-Q1 on fixture 3** — the exact range bound instantiated on the pinned
Leiden partition (`m = 13 ≥ 1`). -/
theorem twoCliquesSplit_range :
    -(2 * (twoCliquesBridge.m : Int) * (twoCliquesBridge.m : Int))
        ≤ modularityNum twoCliquesBridge twoCliquesSplit ∧
      modularityNum twoCliquesBridge twoCliquesSplit
        ≤ 4 * (twoCliquesBridge.m : Int) * (twoCliquesBridge.m : Int) :=
  modularity_range_bound twoCliquesBridge twoCliquesSplit
    twoCliquesSplit_isSetPartition (by rw [twoCliquesBridge_m]; decide)

/-- **Fixture 5** — the range bound on every pinned "reachable" partition of the
fixture-3 graph (single, all-singletons, cross, Leiden split). -/
theorem twoCliquesReachable_range :
    (∀ π ∈ [twoCliquesSingle, twoCliquesSingletons, twoCliquesCross, twoCliquesSplit],
        -(2 * (twoCliquesBridge.m : Int) * (twoCliquesBridge.m : Int))
            ≤ modularityNum twoCliquesBridge π ∧
          modularityNum twoCliquesBridge π
            ≤ 4 * (twoCliquesBridge.m : Int) * (twoCliquesBridge.m : Int)) := by
  intro π hπ
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hπ
  rcases hπ with rfl | rfl | rfl | rfl
  · exact modularity_range_bound _ _ twoCliquesSingle_isSetPartition (by rw [twoCliquesBridge_m]; decide)
  · exact modularity_range_bound _ _ twoCliquesSingletons_isSetPartition (by rw [twoCliquesBridge_m]; decide)
  · exact modularity_range_bound _ _ twoCliquesCross_isSetPartition (by rw [twoCliquesBridge_m]; decide)
  · exact twoCliquesSplit_range

/-- **Fixture 7 (Lean half)** — swapping the two blocks leaves `Qnum` unchanged,
mirroring INV-Q2/INV-Q4's order-independence at the exact layer. -/
theorem twoCliquesSplit_blocksPerm :
    modularityNum twoCliquesBridge twoCliquesSplit
      = modularityNum twoCliquesBridge ⟨[[4, 5, 6, 7], [0, 1, 2, 3]]⟩ :=
  modularityNum_blocksPerm twoCliquesBridge (List.Perm.swap _ _ _)

/-! ## §3. Fixture 4 — the path `p1..p6`, local move from `cm0` -/

/-- Fixture 4 graph: the path `p1—p2—p3—p4—p5—p6` (`m = 5`); vertex `i` is the
Haskell arm's `"p" ++ show (i+1)`. -/
def path6 : EdgeGraph (Fin 6) where
  vertices := [0, 1, 2, 3, 4, 5]
  nodup := by decide
  complete := by decide
  edges := [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5)]
  edges_nodup := by decide
  edges_noLoop := by decide
  edges_mem := by decide
  edges_noRev := by decide

theorem path6_m : path6.m = 5 := rfl

/-- Fixture 4's `cm0`: all singletons. -/
def path6Singletons : Partition (Fin 6) := ⟨[[0], [1], [2], [3], [4], [5]]⟩

/-- Fixture 4's `Q(after)`: the partition the shipped `localMovesFrom` reaches
from `cm0` — `{p1,p2,p3}`, `{p4}`, `{p5,p6}` (the Haskell arm asserts this
equality against `localMovesFrom g cm0`, so the table tracks the code). -/
def path6AfterLocalMove : Partition (Fin 6) := ⟨[[0, 1, 2], [3], [4, 5]]⟩

/-- Fixture 4 "before": `Qnum = −(1²+2²+2²+2²+2²+1²) = −18`, i.e.
`Q = −18/100 = −9/50 ≈ −0.18`. -/
theorem path6Singletons_qnum : modularityNum path6 path6Singletons = -18 := by decide

/-- Fixture 4 "after": `Qnum = 22`, i.e. `Q = 22/100 = 11/50 = 0.22`. -/
theorem path6AfterLocalMove_qnum :
    modularityNum path6 path6AfterLocalMove = 22 := by decide

/-- **INV-Q3 on fixture 4** — the local move is non-decreasing *exactly*:
`Qnum(after) ≥ Qnum(before)` on the pinned rational data. -/
theorem path6_localMove_nonDecreasing :
    modularityNum path6 path6Singletons ≤ modularityNum path6 path6AfterLocalMove := by
  decide

/-- The move is strict on this fixture (`Qnum` rises by `40`). -/
theorem path6_localMove_strict :
    modularityNum path6 path6Singletons < modularityNum path6 path6AfterLocalMove := by
  decide

/-! ## §4. The §6 cross-check table -/

/-- The seven §5 fixtures as (fixture, partition) rows with `qnum` taken from the
*theorems' own definition* (`modularityNum`), so the emitted table is the Lean
computation, not a hand-copied constant. -/
def crossCheckRows : List CrossCheckRow :=
  [ { fixture := "1",  partition := "single",           m := triangleGraph.m, qnum := modularityNum triangleGraph triangleSingle }
  , { fixture := "2",  partition := "singletons",       m := triangleGraph.m, qnum := modularityNum triangleGraph triangleSingletons }
  , { fixture := "3",  partition := "split (leiden)",   m := twoCliquesBridge.m, qnum := modularityNum twoCliquesBridge twoCliquesSplit }
  , { fixture := "4a", partition := "path6 singletons", m := path6.m, qnum := modularityNum path6 path6Singletons }
  , { fixture := "4b", partition := "path6 after move", m := path6.m, qnum := modularityNum path6 path6AfterLocalMove }
  , { fixture := "5a", partition := "single",           m := twoCliquesBridge.m, qnum := modularityNum twoCliquesBridge twoCliquesSingle }
  , { fixture := "5b", partition := "singletons",       m := twoCliquesBridge.m, qnum := modularityNum twoCliquesBridge twoCliquesSingletons }
  , { fixture := "5c", partition := "cross",            m := twoCliquesBridge.m, qnum := modularityNum twoCliquesBridge twoCliquesCross }
  , { fixture := "5d", partition := "split (leiden)",   m := twoCliquesBridge.m, qnum := modularityNum twoCliquesBridge twoCliquesSplit }
  , { fixture := "6",  partition := "split (leiden)",   m := twoCliquesBridge.m, qnum := modularityNum twoCliquesBridge twoCliquesSplit }
  , { fixture := "7",  partition := "split (leiden)",   m := twoCliquesBridge.m, qnum := modularityNum twoCliquesBridge twoCliquesSplit }
  ]

/-- Every row's `qnum` is the exact numerator over `den = (2m)²` — the value the
Haskell arm compares against `modularity` at tolerance `2⁻²⁴`. -/
theorem crossCheckRows_qnum_eq :
    crossCheckRows.map CrossCheckRow.qnum
      = [0, -12, 286, -18, 22, 0, -86, -26, 286, 286, 286] := by
  decide

end EdgeGraph

end GraphosFormalization
