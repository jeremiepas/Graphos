/-
  Graphos formalization — **undirected scoring of a partition** (Lean arm of
  `openspec/changes/modularity-undirected-view`).

  Pinned Haskell surface:

  | Surface | Location |
  | --- | --- |
  | `partitionQuality` (report score) | `src/Graphos/UseCase/Cluster.hs:273-283` |
  | `modularity` | `src/Graphos/Domain/Community.hs:296-306` |
  | `computeCommunityStats` | `src/Graphos/Domain/Community.hs:256-270` |
  | `neighbors` (forward-only when directed) | `src/Graphos/Domain/Graph/Query.hs:62-66` |
  | `undirectedView` | `src/Graphos/Domain/Community.hs:239-240` |

  ## The defect

  `partitionQuality` calls `modularity g cm` on the pipeline graph, which is
  directed under `--directed`. `computeCommunityStats` then mixes two
  incompatible counts:

  * degrees, `σ_tot` and `m` come from `neighbors`, i.e. the **out**-neighbour
    sets when `gDirected g`;
  * `σ_in` comes from the **edge records** `Map.elems (gEdges g)`, two per
    internal record, whatever the direction.

  `gEdges` is keyed by the ordered pair `(source, target)`
  (`Seam/SupportGraph.lean`), so even on the undirected projection one
  unordered pair `{u, v}` may be stored twice — `(u, v)` and `(v, u)` — and then
  counts `4` towards `σ_in` while `u` and `v` each see one neighbour.

  Phase 1 proves `-1/2 ≤ Q ≤ 1` (`EdgeGraph.modularity_range_bound`) only for a
  simple undirected edge set with one record per unordered pair
  (`edges_noRev`). Both mismatches leave that model, and the report printed
  `modularity = 1.2149` on solario-core (`Q ≤ 1` is violated).

  ## What this module proves

  * `RecordGraph` models `gEdges` as its key list (ordered pairs, no duplicate
    key, no self-loop) and `legacyNum` is the exact numerator of today's
    `modularity` under a `directed` flag: `Q_legacy = legacyNum / twoM²`.
  * **The defect is real** (exact witnesses, `Q_legacy = 3/2 > 1`):
    `twoArcs_legacy_directed_exceeds_one` (the `--directed` case) and
    `bothWays_legacy_undirected_exceeds_one` (the projection alone is not
    enough when both orientations are stored).
  * **The fix is in range.** `supportPairs` keeps one record per unordered
    pair; `toEdgeGraph` packages it as a Phase 1 `EdgeGraph` (all five
    invariants proved, `edges_noRev` included), so the fixed score is exactly
    `EdgeGraph.modularityNum` and inherits `fixed_range_bound`:
    `-2m² ≤ Qnum ≤ 4m²`, i.e. `-1/2 ≤ Q ≤ 1`.
  * **No change on simple inputs.** `supportPairs_eq_self`: when no unordered
    pair is stored twice, the support list *is* the record list, so the fix
    changes nothing for graphs that already satisfy the Phase 1 model.
  * `fixedM_pos`: a non-empty record list gives `m ≥ 1`, the hypothesis of the
    range bound (the `m = 0` pin covers the empty case, INV-Q0).

  ## Modelling choices

  1. Exact integers, no `Float`: same posture as `Phase1/RangeBound.lean`.
     The Haskell `twoM = 2.0 * (Σ deg / 2.0)` equals `Σ deg` exactly in
     `Double` below `2^53`, so `twoM` here is the degree sum.
  2. Partitions are Phase 1 `Partition`s; nodes absent from the
     `CommunityMap` (Haskell's `findWithDefault 0`) are out of scope — the
     fixed scoring is stated for a genuine `IsSetPartition`.
  3. `supportPairs` keeps the *last* orientation met; the Haskell fix may
     choose another canonical orientation (`(min, max)`). Every aggregate of
     `EdgeGraph` (`endpointsIn`, `bothIn`) is symmetric in the endpoints, so the
     choice does not change `Qnum`; the cross-check fixtures pin it on data.
-/
import GraphosFormalization.Phase1.RangeBound

namespace GraphosFormalization.UndirectedScoring

open GraphosFormalization

variable {V : Type} [DecidableEq V]

/-! ## §1. The record graph (`gEdges` keys) and today's score -/

/-- The keys of `gEdges`: ordered pairs, one per key, no self-loop. -/
structure RecordGraph (V : Type) where
  /-- `Map.keys (gNodes g)`, duplicate-free. -/
  vertices : List V
  nodup : vertices.Nodup
  complete : ∀ v : V, v ∈ vertices
  /-- `Map.keys (gEdges g)`: ordered `(source, target)` pairs. -/
  records : List (V × V)
  records_nodup : records.Nodup
  records_noLoop : ∀ e ∈ records, e.1 ≠ e.2

namespace RecordGraph

/-- `v ∈ neighbors g u` (`Query.hs:62-66`): forward targets, plus backward
ones unless the graph is directed. -/
def adj (g : RecordGraph V) (directed : Bool) (u v : V) : Bool :=
  g.records.contains (u, v) || (!directed && g.records.contains (v, u))

/-- `Set.size (neighbors g v)`. -/
def deg (g : RecordGraph V) (directed : Bool) (v : V) : Nat :=
  (g.vertices.filter (fun w => g.adj directed v w)).length

/-- `twoM = 2.0 * csM = Σ_v deg(v)` (`Community.hs:259,299`). -/
def twoM (g : RecordGraph V) (directed : Bool) : Nat :=
  (g.vertices.map (g.deg directed)).sum

/-- `σ_tot(b)` from neighbour-set degrees (`Community.hs:260-263`). -/
def sigmaTot (g : RecordGraph V) (directed : Bool) (b : List V) : Nat :=
  (b.map (g.deg directed)).sum

/-- Record `e` lies inside block `b`. -/
def bothIn (b : List V) (e : V × V) : Bool :=
  decide (e.1 ∈ b) && decide (e.2 ∈ b)

/-- `σ_in(b)`: `2.0` per internal **edge record** (`Community.hs:264-269`),
independent of the direction flag. -/
def sigmaIn (g : RecordGraph V) (b : List V) : Nat :=
  (g.records.map (fun e => if bothIn b e then 2 else 0)).sum

/-- Exact numerator of today's `modularity`: `Q_legacy = legacyNum / twoM²`. -/
def legacyNum (g : RecordGraph V) (directed : Bool) (π : Partition V) : Int :=
  ((π.blocks.map (fun b => g.twoM directed * g.sigmaIn b)).sum : Int)
    - ((π.blocks.map (fun b => g.sigmaTot directed b * g.sigmaTot directed b)).sum : Int)

end RecordGraph

/-! ## §2. One record per unordered pair -/

/-- Keep one orientation of every unordered pair: a record is dropped when its
reverse survives further down the list. -/
def supportPairs : List (V × V) → List (V × V)
  | [] => []
  | e :: es =>
      if (e.2, e.1) ∈ supportPairs es then supportPairs es else e :: supportPairs es

theorem mem_of_mem_supportPairs :
    ∀ {rs : List (V × V)} {e : V × V}, e ∈ supportPairs rs → e ∈ rs
  | [], _, h => by simp [supportPairs] at h
  | x :: xs, e, h => by
      by_cases hc : (x.2, x.1) ∈ supportPairs xs
      · rw [supportPairs, if_pos hc] at h
        exact List.mem_cons_of_mem _ (mem_of_mem_supportPairs h)
      · rw [supportPairs, if_neg hc] at h
        rcases List.mem_cons.mp h with h | h
        · rw [h]; exact List.mem_cons_self _ _
        · exact List.mem_cons_of_mem _ (mem_of_mem_supportPairs h)

theorem supportPairs_nodup :
    ∀ {rs : List (V × V)}, rs.Nodup → (supportPairs rs).Nodup
  | [], _ => List.nodup_nil
  | x :: xs, h => by
      have hx : x ∉ xs := (List.nodup_cons.mp h).1
      have ih := supportPairs_nodup (List.nodup_cons.mp h).2
      by_cases hc : (x.2, x.1) ∈ supportPairs xs
      · rw [supportPairs, if_pos hc]; exact ih
      · rw [supportPairs, if_neg hc]
        exact List.nodup_cons.mpr ⟨fun hm => hx (mem_of_mem_supportPairs hm), ih⟩

omit [DecidableEq V] in
/-- The reverse of a pair `(a, b)` is `(b, a)`; stated on components so the
proofs never need `Prod` eta. -/
theorem pair_eq_swap_iff {a b c d : V} : ((a, b) : V × V) = (d, c) ↔ a = d ∧ b = c := by
  constructor
  · intro h; exact ⟨congrArg Prod.fst h, congrArg Prod.snd h⟩
  · rintro ⟨rfl, rfl⟩; rfl

theorem supportPairs_noRev :
    ∀ {rs : List (V × V)}, (∀ e ∈ rs, e.1 ≠ e.2) →
      ∀ e ∈ supportPairs rs, (e.2, e.1) ∉ supportPairs rs
  | [], _, e, he => by simp [supportPairs] at he
  | (p, q) :: xs, hl, (a, b), he => by
      have hl' : ∀ e ∈ xs, e.1 ≠ e.2 := fun e h => hl e (List.mem_cons_of_mem _ h)
      have hpq : p ≠ q := hl (p, q) (List.mem_cons_self _ _)
      have ih := supportPairs_noRev hl'
      by_cases hc : ((p, q).2, (p, q).1) ∈ supportPairs xs
      · rw [supportPairs, if_pos hc] at he ⊢
        exact ih (a, b) he
      · rw [supportPairs, if_neg hc] at he ⊢
        intro hm
        rcases List.mem_cons.mp he with hx | ht
        · -- `(a, b) = (p, q)`: its reverse is neither `(p, q)` (no loop) nor in
          -- the tail (`hc`).
          obtain ⟨rfl, rfl⟩ := pair_eq_swap_iff.mp hx
          rcases List.mem_cons.mp hm with h1 | h1
          · exact hpq (pair_eq_swap_iff.mp h1).2
          · exact hc h1
        · rcases List.mem_cons.mp hm with h1 | h1
          · -- `(b, a) = (p, q)`, so `(a, b)` is the reverse of `(p, q)`,
            -- excluded by `hc`.
            obtain ⟨rfl, rfl⟩ := pair_eq_swap_iff.mp h1
            exact hc ht
          · exact ih (a, b) ht h1

/-- Every record survives, possibly as its reverse: the projection keeps the
whole support relation. -/
theorem mem_or_rev_mem_supportPairs :
    ∀ {rs : List (V × V)} {e : V × V}, e ∈ rs →
      e ∈ supportPairs rs ∨ (e.2, e.1) ∈ supportPairs rs
  | [], _, he => absurd he (List.not_mem_nil _)
  | x :: xs, e, he => by
      by_cases hc : (x.2, x.1) ∈ supportPairs xs
      · rw [supportPairs, if_pos hc]
        rcases List.mem_cons.mp he with rfl | he'
        · exact Or.inr hc
        · exact mem_or_rev_mem_supportPairs he'
      · rw [supportPairs, if_neg hc]
        rcases List.mem_cons.mp he with rfl | he'
        · exact Or.inl (List.mem_cons_self _ _)
        · rcases mem_or_rev_mem_supportPairs he' with h | h
          · exact Or.inl (List.mem_cons_of_mem _ h)
          · exact Or.inr (List.mem_cons_of_mem _ h)

/-- **No change on simple inputs**: when no unordered pair is stored in both
orientations, `supportPairs` is the identity. -/
theorem supportPairs_eq_self :
    ∀ {rs : List (V × V)}, (∀ e ∈ rs, (e.2, e.1) ∉ rs) → supportPairs rs = rs
  | [], _ => rfl
  | x :: xs, h => by
      have h' : ∀ e ∈ xs, (e.2, e.1) ∉ xs := fun e he hm =>
        h e (List.mem_cons_of_mem _ he) (List.mem_cons_of_mem _ hm)
      have hx : (x.2, x.1) ∉ supportPairs xs := fun hm =>
        h x (List.mem_cons_self _ _) (List.mem_cons_of_mem _ (mem_of_mem_supportPairs hm))
      rw [supportPairs, if_neg hx, supportPairs_eq_self h']

/-! ## §3. The fixed score is the Phase 1 modularity -/

namespace RecordGraph

/-- The simple undirected support graph of the records: Phase 1's `EdgeGraph`
over `supportPairs`. Every invariant of the range bound holds. -/
def toEdgeGraph (g : RecordGraph V) : EdgeGraph V where
  vertices := g.vertices
  nodup := g.nodup
  complete := g.complete
  edges := supportPairs g.records
  edges_nodup := supportPairs_nodup g.records_nodup
  edges_noLoop := fun e he => g.records_noLoop e (mem_of_mem_supportPairs he)
  edges_mem := fun _ _ => ⟨g.complete _, g.complete _⟩
  edges_noRev := supportPairs_noRev g.records_noLoop

/-- `m` of the fixed score: the number of unordered support pairs. -/
def fixedM (g : RecordGraph V) : Nat := g.toEdgeGraph.m

/-- Exact numerator of the fixed score: `Q_fixed = fixedNum / (2m)²`. -/
def fixedNum (g : RecordGraph V) (π : Partition V) : Int :=
  EdgeGraph.modularityNum g.toEdgeGraph π

/-- A non-empty record list has at least one support pair. -/
theorem fixedM_pos (g : RecordGraph V) (h : g.records ≠ []) : 1 ≤ g.fixedM := by
  obtain ⟨e, es, hes⟩ : ∃ e es, g.records = e :: es := by
    cases hr : g.records with
    | nil => exact absurd hr h
    | cons e es => exact ⟨e, es, rfl⟩
  have he : e ∈ g.records := by rw [hes]; exact List.mem_cons_self _ _
  have hne : supportPairs g.records ≠ [] := by
    intro hnil
    rcases mem_or_rev_mem_supportPairs he with hm | hm <;> rw [hnil] at hm <;>
      exact List.not_mem_nil _ hm
  exact List.length_pos.mpr hne

/-- **Fixed score is in range**: `-1/2 ≤ Q_fixed ≤ 1`, stated division-free
on `(2m)²` exactly as `EdgeGraph.modularity_range_bound`. Holds for directed
and undirected builds alike — the direction flag never reaches it. -/
theorem fixed_range_bound (g : RecordGraph V) (π : Partition V)
    (hpart : EdgeGraph.IsSetPartition π g.vertices) (hm : 1 ≤ g.fixedM) :
    -(2 * (g.fixedM : Int) * (g.fixedM : Int)) ≤ g.fixedNum π ∧
      g.fixedNum π ≤ 4 * (g.fixedM : Int) * (g.fixedM : Int) :=
  EdgeGraph.modularity_range_bound g.toEdgeGraph π hpart hm

/-- On a record graph that stores each unordered pair once, the fixed score is
computed over the records themselves. -/
theorem toEdgeGraph_edges_of_simple (g : RecordGraph V)
    (h : ∀ e ∈ g.records, (e.2, e.1) ∉ g.records) :
    g.toEdgeGraph.edges = g.records :=
  supportPairs_eq_self h

end RecordGraph

/-! ## §4. Exact witnesses -/

open RecordGraph

/-- The two-block partition `{{0,1}, {2,3}}` of `Fin 4`. -/
def twoBlocks : Partition (Fin 4) := ⟨[[0, 1], [2, 3]]⟩

theorem twoBlocks_isSetPartition :
    EdgeGraph.IsSetPartition twoBlocks [0, 1, 2, 3] := by
  unfold EdgeGraph.IsSetPartition Partition.Subset Partition.Covers
    Partition.PairwiseDisjoint
  decide

/-- Two arcs `0 → 1`, `2 → 3`, each inside its block. -/
def twoArcs : RecordGraph (Fin 4) where
  vertices := [0, 1, 2, 3]
  nodup := by decide
  complete := by decide
  records := [(0, 1), (2, 3)]
  records_nodup := by decide
  records_noLoop := by decide

/-- `--directed`: out-degrees give `twoM = 2` while `σ_in = 2` per block, so
`legacyNum = 6` over `twoM² = 4` — `Q_legacy = 3/2`. -/
theorem twoArcs_legacy_directed :
    twoArcs.twoM true = 2 ∧ twoArcs.legacyNum true twoBlocks = 6 := by decide

/-- **The `--directed` defect**: today's score exceeds the `Q ≤ 1` bound. -/
theorem twoArcs_legacy_directed_exceeds_one :
    twoArcs.legacyNum true twoBlocks >
      (twoArcs.twoM true : Int) * (twoArcs.twoM true : Int) := by decide

/-- The undirected projection alone repairs this graph: `8 / 16 = 1/2`. -/
theorem twoArcs_legacy_undirected :
    twoArcs.twoM false = 4 ∧ twoArcs.legacyNum false twoBlocks = 8 := by decide

/-- The fixed score agrees: `m = 2`, `Qnum = 8` over `(2m)² = 16`. -/
theorem twoArcs_fixed :
    twoArcs.fixedM = 2 ∧ twoArcs.fixedNum twoBlocks = 8 := by decide

/-- Both orientations of `{0,1}` and of `{2,3}` stored as two records each —
what `gEdges` holds when both `u → v` and `v → u` were extracted. -/
def bothWays : RecordGraph (Fin 4) where
  vertices := [0, 1, 2, 3]
  nodup := by decide
  complete := by decide
  records := [(0, 1), (1, 0), (2, 3), (3, 2)]
  records_nodup := by decide
  records_noLoop := by decide

/-- Undirected projection, record-counted `σ_in`: `twoM = 4`, `σ_in = 4` per
block, `legacyNum = 24` over `16` — `Q_legacy = 3/2`. -/
theorem bothWays_legacy_undirected :
    bothWays.twoM false = 4 ∧ bothWays.legacyNum false twoBlocks = 24 := by decide

/-- **`undirectedView` is necessary but not sufficient**: with both
orientations stored, the projected score still exceeds `1`. -/
theorem bothWays_legacy_undirected_exceeds_one :
    bothWays.legacyNum false twoBlocks >
      (bothWays.twoM false : Int) * (bothWays.twoM false : Int) := by decide

/-- One record per unordered pair restores `Q = 8 / 16 = 1/2`. -/
theorem bothWays_fixed :
    bothWays.fixedM = 2 ∧ bothWays.fixedNum twoBlocks = 8 := by decide

/-- The fixed score on the defect witness, inside the proven range. -/
theorem bothWays_fixed_in_range :
    -(2 * (bothWays.fixedM : Int) * (bothWays.fixedM : Int)) ≤ bothWays.fixedNum twoBlocks ∧
      bothWays.fixedNum twoBlocks ≤ 4 * (bothWays.fixedM : Int) * (bothWays.fixedM : Int) :=
  bothWays.fixed_range_bound twoBlocks twoBlocks_isSetPartition
    (bothWays.fixedM_pos (by decide))

end GraphosFormalization.UndirectedScoring
