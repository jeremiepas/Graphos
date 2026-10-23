/-
  Graphos formalization — Phase 1, INV-Q5: canonical fixtures pin exact values.

  Requirements:
  `docs/math-requirements/AVI-618-auditable-modularity-lean-formalization-requirements.md`
  §2 INV-Q5 (`theorem modularity_fixturesExact`), §5 fixtures 1 & 2, §1.2 (the
  functional `(Q)`), §6 (cross-check contract).

  Pinned Haskell surface:
    * `modularity`            — `src/Graphos/Domain/Community.hs:151-167`
    * `computeCommunityStats` — `src/Graphos/Domain/Community.hs:92-107`
    * `degree`                — `src/Graphos/Domain/Graph/Query.hs:44`

  ## What is proved

  On the pinned fixtures 1 and 2 of §5, for an undirected simple support graph
  `G_s = (V, A)` with `m = |A|` edges and `k_v = deg_s(v)`:

  ```
    Fixture 1 (single block `{V}`) : Qnum = 0            ⇔ Q = 0
    Fixture 2 (all singletons)     : Qnum = -Σ_v k_v²    ⇔ Q = -(1/(2m))²·Σ_v k_v²
    Fixture 2 with `m ≥ 1`         : Qnum < 0            ⇔ Q < 0
  ```

  where

  ```
    Qnum(π) = Σ_c [ 2m·σ_in(c) − σ_tot(c)² ] = (2m)² · Q(π)
  ```

  is the exact numerator over the common denominator `(2m)²` (AVI-618 §3,
  shared with INV-Q1). The bundled artifact `modularity_fixturesExact` carries
  all three claims; the per-fixture lemmas are `singleBlock_modularityNum`,
  `singletons_modularityNum` and `singletons_modularityNum_neg`.

  ## Why the exact layer

  `modularity` computes `Q` in `Float` (mirroring Haskell's `Double`). Kernel
  Lean cannot evaluate `Float` arithmetic: `Float.add`/`Float.div` are
  `opaque @[extern]`, `Float` has no `DecidableEq`, and core Lean ships no
  `Float` order or field lemmas — so `modularity g π = 0.0` and
  `modularity g π < 0.0` are not provable in a dependency-free package. AVI-618
  §6 already pins the Lean side to exact arithmetic ("proven over ℚ/ℝ; the
  Haskell side computes in Double"), and §5 fixes the fixtures to rationals
  where both sides reduce to the same value. This module therefore proves
  INV-Q5 over the exact integers; the `Float` rendering is the implementation's
  rounded evaluation and is pinned to these values by the AVI-1301 fixture
  cross-check (`|Q_lean − Q_hs| ≤ 2⁻²⁴`, exactly `0` on rational fixtures).

  ## Model

  `EdgeGraph` (`RangeBound.lean`) is the edge-set model of `G_s = (V, A)`:
  one record per unordered pair, matching `gEdges` and the `neighbors`
  multi-edge collapse (`Graph/Query.hs:37`). The per-vertex degree is
  `k_v = σ_tot([v])` (`deg`): for a singleton block `σ_tot` counts exactly the
  incident edges, and `Σ_v k_v = 2m` (`singletons_sum_deg`).

  ## Proof shapes

  * **Fixture 1.** `g.vertices` contains both endpoints of every edge
    (`edges_mem`), so every edge is internal to the single block: `σ_in = 2m`
    and `σ_tot = 2m`, hence `Qnum = 2m·2m − (2m)² = 0`.
  * **Fixture 2.** A singleton block is internal to no edge (`edges_noLoop`),
    so `σ_in([v]) = 0`; and `σ_tot([v]) = k_v`. Hence
    `Qnum = 0 − Σ_v k_v²`. With `m ≥ 1` some edge `e` exists, so
    `k_{e.1} ≥ 1`, the sum of squares is positive, and `Qnum < 0`.
  * Both are proved for the *general* graph and then instantiated on the §5
    triangle (`m = 3`): fixture 1 gives `Qnum = 0`; fixture 2 gives
    `Qnum = −12 = −(2²·3)`, i.e. `Q = −12/36 = −1/3`.

  ## Honesty caveats (inherited from AVI-618 §2/§3)

  * Directed graphs are out of scope (`Q` is defined on the undirected support
    graph `G_s` only).
  * Weighted modularity is out of scope: `edgeWeight` is ignored by `Q`.
  * The strict negativity of fixture 2 needs `m ≥ 1`; for `m = 0` both fixtures
    coincide at `Q = 0` (that is INV-Q0, `modularity_edgesAbsent_zero`).
-/
import GraphosFormalization.Phase1.RangeBound

namespace GraphosFormalization

namespace EdgeGraph

variable {V : Type} [DecidableEq V]

/-! ## §0. Sum helpers -/

/-- A `Nat`-valued map-sum dominates each of its summands. Core Lean ships no
`List.le_sum_of_mem` for a mapped list. -/
theorem le_sum_map {α : Type} (l : List α) (f : α → Nat) {a : α} (ha : a ∈ l) :
    f a ≤ (l.map f).sum := by
  induction l with
  | nil => simp at ha
  | cons b bs ih =>
      rcases List.mem_cons.mp ha with rfl | hbs
      · rw [List.map_cons, List.sum_cons]
        omega
      · have h := ih hbs
        rw [List.map_cons, List.sum_cons]
        omega

/-! ## §1. Per-vertex degree on the edge-set model -/

/-- `k_v = deg_s(v)` (`Graph/Query.hs:44`) on the edge-set model: for the
singleton block `[v]`, `σ_tot` counts exactly the edges incident to `v`
(a simple graph has no loop that could contribute `2` to one vertex). -/
def deg (g : EdgeGraph V) (v : V) : Nat := g.sigmaTot [v]

/-- A singleton block is internal to no edge: `bothIn [v] e` would force
`e.1 = e.2 = v`, contradicting `edges_noLoop`. -/
theorem internalIndicator_singleton {v : V} {e : V × V} (h : e.1 ≠ e.2) :
    internalIndicator [v] e = 0 := by
  unfold internalIndicator bothIn
  by_cases h1 : e.1 = v
  · by_cases h2 : e.2 = v
    · exact absurd (h1.trans h2.symm) h
    · simp [h1, h2]
  · simp [h1]

/-- Fixture 2, per block: every block is a singleton, so `σ_in = 0`. -/
theorem sigmaIn_singleton (g : EdgeGraph V) (v : V) : g.sigmaIn [v] = 0 := by
  unfold sigmaIn
  apply sum_map_eq_zero_of_forall
  intro e he
  rw [internalIndicator_singleton (g.edges_noLoop e he)]

/-- An edge with an endpoint in `[v]` contributes at least one endpoint count. -/
theorem one_le_endpointsIn_of_mem {v : V} {e : V × V} (h : e.1 ∈ [v]) :
    1 ≤ endpointsIn [v] e := by
  have h1 : e.1 = v := List.mem_singleton.mp h
  unfold endpointsIn
  by_cases h2 : e.2 = v <;> simp [h1, h2]

/-- A vertex incident to an edge has positive degree. -/
theorem deg_pos {g : EdgeGraph V} {v : V} (h : ∃ e ∈ g.edges, e.1 = v) :
    0 < g.deg v := by
  obtain ⟨e, he, hv⟩ := h
  have hle := le_sum_map g.edges (fun e => endpointsIn [v] e) he
  have hone : 1 ≤ endpointsIn [v] e := by
    apply one_le_endpointsIn_of_mem
    rw [hv]
    simp
  have hsum : 1 ≤ (g.edges.map (fun e => endpointsIn [v] e)).sum :=
    Nat.le_trans hone hle
  unfold deg sigmaTot
  exact Nat.lt_of_lt_of_le Nat.zero_lt_one hsum

/-! ## §2. Fixture 1 — the single block `{V}` gives `Q = 0` -/

/-- Fixture 1, `σ_in`: every edge is internal to the single block, since
`g.vertices` contains both endpoints. -/
theorem sigmaIn_all (g : EdgeGraph V) : g.sigmaIn g.vertices = 2 * g.m := by
  unfold sigmaIn m
  rw [show (g.edges.map (fun e => 2 * internalIndicator g.vertices e)).sum
        = (g.edges.map (fun _ => 2)).sum from
        sum_map_congr _ _ _ (fun e he => by
          have h := g.edges_mem e he
          unfold internalIndicator bothIn
          simp [h.1, h.2])]
  rw [sum_map_const]

/-- Fixture 1, `σ_tot`: both endpoints of every edge lie in the single block. -/
theorem sigmaTot_all (g : EdgeGraph V) : g.sigmaTot g.vertices = 2 * g.m := by
  unfold sigmaTot m
  rw [show (g.edges.map (fun e => endpointsIn g.vertices e)).sum
        = (g.edges.map (fun _ => 2)).sum from
        sum_map_congr _ _ _ (fun e he => by
          have h := g.edges_mem e he
          unfold endpointsIn
          simp [h.1, h.2])]
  rw [sum_map_const]

omit [DecidableEq V] in
/-- The single block `{V}` is a genuine partition of `V`. -/
theorem singleBlock_isSetPartition (g : EdgeGraph V) :
    IsSetPartition ⟨[g.vertices]⟩ g.vertices := by
  unfold IsSetPartition Partition.Subset Partition.Covers Partition.PairwiseDisjoint
  refine ⟨?_, ?_, ?_, ?_, ?_⟩
  · intro b hb v hv
    rw [List.mem_singleton] at hb
    rw [hb] at hv
    exact hv
  · intro v hv
    exact ⟨g.vertices, List.mem_singleton_self _, hv⟩
  · intro b₁ hb₁ b₂ hb₂ hne
    rw [List.mem_singleton] at hb₁ hb₂
    rw [hb₁, hb₂] at hne
    exact absurd rfl hne
  · simp [g.nodup]
  · intro b hb
    rw [List.mem_singleton] at hb
    rw [hb]
    exact g.nodup

/-- **Fixture 1 (AVI-618 §5).** The single community containing all nodes gives
`Qnum = 2m·2m − (2m)² = 0`, i.e. `Q = 0`. -/
theorem singleBlock_modularityNum (g : EdgeGraph V) :
    modularityNum g ⟨[g.vertices]⟩ = 0 := by
  unfold modularityNum
  simp only [List.map_cons, List.map_nil, List.sum_cons, List.sum_nil, Nat.add_zero]
  rw [sigmaIn_all, sigmaTot_all]
  omega

/-! ## §3. Fixture 2 — the all-singletons partition gives `Q = −(1/2m)²·Σ k_v²` -/

omit [DecidableEq V] in
/-- The all-singletons block list `{ [v] | v ∈ V }` is a genuine partition. -/
theorem singletons_isSetPartition (g : EdgeGraph V) :
    IsSetPartition ⟨g.vertices.map (fun v => [v])⟩ g.vertices := by
  unfold IsSetPartition Partition.Subset Partition.Covers Partition.PairwiseDisjoint
  refine ⟨?_, ?_, ?_, ?_, ?_⟩
  · intro b hb v hv
    obtain ⟨u, hu, rfl⟩ := List.mem_map.mp hb
    rw [List.mem_singleton] at hv
    rw [hv]
    exact hu
  · intro v hv
    exact ⟨[v], List.mem_map_of_mem (fun v => [v]) hv, List.mem_singleton_self v⟩
  · intro b₁ hb₁ b₂ hb₂ hne v h1 h2
    obtain ⟨u, _, rfl⟩ := List.mem_map.mp hb₁
    obtain ⟨w, _, rfl⟩ := List.mem_map.mp hb₂
    rw [List.mem_singleton] at h1 h2
    exact hne (by rw [← h1, ← h2])
  · have h : (g.vertices.map (fun v : V => [v])).Nodup :=
      (show List.Pairwise (fun a b : V => a ≠ b) g.vertices from g.nodup).map
        (fun v : V => [v])
        (fun a b (hne : a ≠ b) (heq : [a] = [b]) =>
          absurd (by simpa using heq : a = b) hne)
    simpa using h
  · intro b hb
    obtain ⟨u, _, rfl⟩ := List.mem_map.mp hb
    simp

/-- The degree sum is `2m` on the all-singletons partition — the sanity check
that `deg` really is `deg_s` (handshake lemma, `Σ_v k_v = 2m`). -/
theorem singletons_sum_deg (g : EdgeGraph V) :
    (g.vertices.map (fun v => g.deg v)).sum = 2 * g.m := by
  have h := sum_sigmaTot_eq_two_m g (singletons_isSetPartition g)
  rw [show (g.vertices.map (fun v => g.deg v)).sum
        = ((g.vertices.map (fun v => [v])).map (fun b => g.sigmaTot b)).sum from by
        rw [List.map_map]
        rfl] at *
  exact h

/-- **Fixture 2, exact value.** With every block a singleton,
`σ_in = 0` and `σ_tot([v]) = k_v`, so
`Qnum = −Σ_v k_v²` — equivalently `Q = −(1/(2m))²·Σ_v k_v²`. -/
theorem singletons_modularityNum (g : EdgeGraph V) :
    modularityNum g ⟨g.vertices.map (fun v => [v])⟩
      = -(((g.vertices.map (fun v => g.deg v * g.deg v)).sum : Nat) : Int) := by
  unfold modularityNum
  have hzero : ((g.vertices.map (fun v => [v])).map
      (fun b => 2 * g.m * g.sigmaIn b)).sum = 0 := by
    rw [List.map_map]
    apply sum_map_eq_zero_of_forall
    intro v _
    show 2 * g.m * g.sigmaIn [v] = 0
    rw [sigmaIn_singleton]
    simp
  have hsecond : ((g.vertices.map (fun v => [v])).map
      (fun b => g.sigmaTot b * g.sigmaTot b)).sum
      = (g.vertices.map (fun v => g.deg v * g.deg v)).sum := by
    rw [List.map_map]
    rfl
  rw [hzero, hsecond, Int.ofNat_zero, Int.zero_sub]

/-- **Fixture 2, strict negativity.** With at least one edge, some `k_v ≥ 1`,
so `Σ_v k_v² > 0` and `Qnum < 0` — equivalently `Q < 0` since `(2m)² > 0`. -/
theorem singletons_modularityNum_neg (g : EdgeGraph V) (hm : 1 ≤ g.m) :
    modularityNum g ⟨g.vertices.map (fun v => [v])⟩ < 0 := by
  rw [singletons_modularityNum]
  have hpos : 0 < (g.vertices.map (fun v => g.deg v * g.deg v)).sum := by
    have hne : g.edges ≠ [] := by
      intro h
      have hm0 : g.m = 0 := by
        unfold m
        rw [h]
        rfl
      omega
    obtain ⟨e, he⟩ := List.exists_mem_of_ne_nil g.edges hne
    have hmem : e.1 ∈ g.vertices := (g.edges_mem e he).1
    have hd : 0 < g.deg e.1 := deg_pos ⟨e, he, rfl⟩
    have hterm : 0 < g.deg e.1 * g.deg e.1 := Nat.mul_pos hd hd
    have hle : g.deg e.1 * g.deg e.1 ≤
        (g.vertices.map (fun v => g.deg v * g.deg v)).sum :=
      le_sum_map g.vertices (fun v => g.deg v * g.deg v) hmem
    exact Nat.lt_of_lt_of_le hterm hle
  have hcast : (0 : Int) < ((g.vertices.map (fun v => g.deg v * g.deg v)).sum : Int) := by
    exact_mod_cast hpos
  omega

/-! ## §4. The named artifact `modularity_fixturesExact` -/

/-- **INV-Q5 (AVI-618 §2) — `modularity_fixturesExact`.**

On the pinned fixtures of §5, for an undirected simple support graph with
`m ≥ 1`:

* fixture 1 (single block containing all nodes, no dangling edges) has
  `Qnum = 0`, i.e. `Q = 0`;
* fixture 2 (all-singletons partition) has `Qnum = −Σ_v k_v²`, i.e.
  `Q = −(1/(2m))²·Σ_v k_v²`, and `Qnum < 0` (`Q < 0`).

`Qnum` is the exact numerator over the common denominator `(2m)²`
(`Qnum = (2m)²·Q`), so every claim is the exact-rational statement; the
`Float` rendering of `modularity` is covered by the AVI-1301 cross-check (see
the module header). Both fixture values are then instantiated on the §5
triangle in §5 below. -/
theorem modularity_fixturesExact (g : EdgeGraph V) (hm : 1 ≤ g.m) :
    modularityNum g ⟨[g.vertices]⟩ = 0 ∧
      modularityNum g ⟨g.vertices.map (fun v => [v])⟩
        = -(((g.vertices.map (fun v => g.deg v * g.deg v)).sum : Nat) : Int) ∧
      modularityNum g ⟨g.vertices.map (fun v => [v])⟩ < 0 :=
  ⟨singleBlock_modularityNum g, singletons_modularityNum g,
    singletons_modularityNum_neg g hm⟩

/-! ## §5. Pinned §5 fixtures: the triangle `{a,b,c}` -/

/-- The §5 triangle: `V = {0,1,2}`, `A = {(0,1), (0,2), (1,2)}`, so `m = 3`. -/
def triangleGraph : EdgeGraph (Fin 3) where
  vertices := [0, 1, 2]
  nodup := by decide
  complete := by decide
  edges := [(0, 1), (0, 2), (1, 2)]
  edges_nodup := by decide
  edges_noLoop := by decide
  edges_mem := by decide
  edges_noRev := by decide

/-- Fixture 1's partition on the triangle: one community holding all nodes. -/
def triangleSingle : Partition (Fin 3) := ⟨[triangleGraph.vertices]⟩

/-- Fixture 2's partition on the triangle: all singletons. -/
def triangleSingletons : Partition (Fin 3) :=
  ⟨triangleGraph.vertices.map (fun v => [v])⟩

theorem triangleGraph_m : triangleGraph.m = 3 := rfl

/-- Fixture 1 on the §5 triangle: `Qnum = 0` (`Q = 0`). -/
theorem triangleSingle_modularityNum :
    modularityNum triangleGraph triangleSingle = 0 :=
  singleBlock_modularityNum triangleGraph

/-- The triangle's degrees are all `2`, so `Σ_v k_v² = 3·4 = 12`. -/
theorem triangleSingletons_deg_sq_sum :
    (triangleGraph.vertices.map
      (fun v => triangleGraph.deg v * triangleGraph.deg v)).sum = 12 := by
  decide

/-- Fixture 2 on the §5 triangle: `Qnum = −12`. -/
theorem triangleSingletons_modularityNum :
    modularityNum triangleGraph triangleSingletons = -12 := by
  decide

/-- Fixture 2's acceptance value on the triangle: `Qnum = −(1/(2m))²·Σ k_v²`
multiplied through by `(2m)²`. With `m = 3` and `Σ k_v² = 12` this is
`Qnum = −12`, i.e. `Q = −12/36 = −1/3`, exactly the §5 table value. -/
theorem triangleSingletons_Q_eq_neg_third :
    3 * modularityNum triangleGraph triangleSingletons
      = -((2 * (triangleGraph.m : Int)) * (2 * (triangleGraph.m : Int))) := by
  rw [triangleSingletons_modularityNum, triangleGraph_m]
  decide

/-- Fixture 2 is strictly negative on the triangle. -/
theorem triangleSingletons_modularityNum_neg :
    modularityNum triangleGraph triangleSingletons < 0 := by
  rw [triangleSingletons_modularityNum]
  decide

/-- The bundled INV-Q5 artifact instantiated on the §5 triangle. -/
theorem triangle_fixturesExact :
    modularityNum triangleGraph triangleSingle = 0 ∧
      modularityNum triangleGraph triangleSingletons
        = -(((triangleGraph.vertices.map
              (fun v => triangleGraph.deg v * triangleGraph.deg v)).sum : Nat) : Int) ∧
      modularityNum triangleGraph triangleSingletons < 0 :=
  modularity_fixturesExact triangleGraph (by rw [triangleGraph_m]; decide)

end EdgeGraph

end GraphosFormalization
