/-
  Graphos formalization — Phase 1, auditable global modularity `Q`.

  Requirements:
  `docs/math-requirements/AVI-618-auditable-modularity-lean-formalization-requirements.md`
  §1.2 (the functional `(Q)`), §1.3 (well-definedness / `m = 0` pinning),
  §2 INV-Q0 (`theorem modularity_edgesAbsent_zero`), §6 (cross-check contract).

  Pinned Haskell surface:
    * `modularity`         — `src/Graphos/Domain/Community.hs:151-167`
    * `m == 0` pinning     — `src/Graphos/Domain/Community.hs:165-166` (`if csM stats == 0.0`)
    * `m <= 0` guard       — `src/Graphos/Domain/Community.hs:232` (`bestCommunityFor`)
    * `computeCommunityStats` — `src/Graphos/Domain/Community.hs:92-107`
    * `degree`             — `src/Graphos/Domain/Graph/Query.hs:44`

  ## Modeling choices (why this shape)

  1. **No Mathlib, no `Finset`, no `Rat`.** The lake package is dependency-free
     (`lakefile.toml`: no `[[require]]`), so this module only uses the Lean core
     library. A `SimpleGraph`/`Finset`/`Rat` dependency would drag in the whole
     Mathlib build and break the report-only CI budget. The graph is therefore
     modeled by an explicit vertex enumeration plus a decidable adjacency
     relation, and `Q` is computed in `Float` (64-bit IEEE, the same arithmetic
     family as Haskell's `Double`).
  2. **`Q` mirrors the Haskell fold.** `modularity` sums one term per block of
     the partition in list order and pins the value to `0.0` when `m = 0`,
     exactly like `modularity`'s `if csM stats == 0.0 then 0.0 else foldl' ...`.
     That guard is *necessary*: `0.0 / 0.0` is `NaN` in `Float`, so the raw
     formula is not defined when the graph has no edges. INV-Q0 is precisely the
     claim that the guard fires for every partition of an edgeless `G_s`.
  3. **`deg` counts over the declared vertex set.** `SupportGraph.complete`
     states that `vertices` enumerates all of `V`, so no adjacency can hide
     outside the enumeration. This keeps the graph's degree function faithful to
     `Set.size (neighbors g nid)` (`Query.hs:37-46`) without needing a `Fintype`
     instance.
-/
namespace GraphosFormalization

/-! ## §1.1 Objects: the undirected simple support graph `G_s` -/

/-- Helper: a list of naturals summing to zero is a list of zeros (core Lean has
no `List.sum_eq_zero` lemma). -/
theorem list_sum_eq_zero_of_forall_zero {l : List Nat} (h : ∀ x ∈ l, x = 0) :
    l.sum = 0 := by
  induction l with
  | nil => rfl
  | cons a t ih =>
      have ha : a = 0 := h a (by simp)
      have ht : t.sum = 0 := ih (fun x hx => h x (by simp [hx]))
      rw [List.sum_cons, ha, ht, Nat.zero_add]

/-- The **simple support graph** `G_s` that `Q` is defined over (AVI-618 §0.2).

`vertices` is a duplicate-free enumeration of `V`; `adj` is a symmetric,
irreflexive adjacency relation, so each unordered edge is represented exactly
once per endpoint and multi-edges collapse — the `Set` semantics of
`neighbors` (`Query.hs:37`). `complete` pins `vertices` to be *all* of `V`. -/
structure SupportGraph (V : Type) where
  /-- The vertex set `V`, as a duplicate-free list. -/
  vertices : List V
  /-- `V` is a set: no vertex appears twice in the enumeration. -/
  nodup : vertices.Nodup
  /-- The enumeration is exhaustive: every `v : V` is a vertex. -/
  complete : ∀ v : V, v ∈ vertices
  /-- Decidable adjacency of the support graph. -/
  adj : V → V → Bool
  /-- `G_s` is undirected (`¬gDirected g`): adjacency is symmetric. -/
  symm : ∀ u v, adj u v = adj v u
  /-- `G_s` is simple: no self-loops. -/
  irrefl : ∀ v, adj v v = false

namespace SupportGraph

variable {V : Type}

/-- `deg_s(v)` — the support-graph degree (`Graph/Query.hs:44`), a `Set.size` in
Haskell and a filtered count here. -/
def deg (g : SupportGraph V) (v : V) : Nat :=
  (g.vertices.filter (fun w => g.adj v w)).length

/-- `Σ_v deg_s(v)` — twice the edge count, by the handshake lemma. -/
def totalDegree (g : SupportGraph V) : Nat :=
  (g.vertices.map g.deg).sum

/-- `m`, the number of edges of `G_s` (`csM`, `Community.hs:95`):
`m = (Σ_v deg_s(v)) / 2`. -/
def m (g : SupportGraph V) : Nat :=
  g.totalDegree / 2

/-- `2m` as it appears in `(Q)` (`twoM`, `Community.hs:155`). -/
def twoM (g : SupportGraph V) : Float :=
  2.0 * Float.ofNat g.m

/-- **INV-Q0 hypothesis**: every vertex of `G_s` has degree zero, i.e. `E_s = 0`. -/
def AllDegreesZero (g : SupportGraph V) : Prop :=
  ∀ v : V, g.deg v = 0

/-- A vertex of degree zero has no neighbour at all. Uses `complete`: there is
no vertex outside the enumeration where an adjacency could hide. -/
theorem not_adj_of_deg_eq_zero {g : SupportGraph V} {v w : V} (h : g.deg v = 0) :
    g.adj v w = false := by
  have hnil : g.vertices.filter (fun w => g.adj v w) = [] :=
    List.eq_nil_of_length_eq_zero h
  have hnot : ¬(g.adj v w = true) :=
    (List.filter_eq_nil_iff.mp hnil) w (g.complete w)
  cases hAdj : g.adj v w with
  | false => rfl
  | true => exact absurd hAdj hnot

/-- Under INV-Q0 the degree sum vanishes, hence so does `m` (`csM = 0`). -/
theorem totalDegree_eq_zero (g : SupportGraph V) (h : g.AllDegreesZero) :
    g.totalDegree = 0 := by
  unfold totalDegree
  apply list_sum_eq_zero_of_forall_zero
  intro x hx
  obtain ⟨v, _, rfl⟩ := List.mem_map.mp hx
  exact h v

/-- The `m = 0` half of INV-Q0: `allDegreesZero g → csM = 0`. -/
theorem m_eq_zero (g : SupportGraph V) (h : g.AllDegreesZero) : g.m = 0 := by
  unfold m
  rw [g.totalDegree_eq_zero h]

end SupportGraph

/-! ## §1.1 Partitions of `V` -/

/-- A partition `π` of `V`, encoded as a list of blocks (the blocks of
`CommunityMap = Map CommunityId [NodeId]` in `Types/Graph.hs:43`, stripped of the
community ids because `Q` only depends on the induced blocks, INV-Q2). -/
structure Partition (V : Type) where
  /-- The blocks `B_c` of the partition. -/
  blocks : List (List V)

namespace Partition

variable {V : Type}

/-- Every member of every block is a vertex of `V`. -/
def Subset (π : Partition V) (vs : List V) : Prop :=
  ∀ b ∈ π.blocks, ∀ v ∈ b, v ∈ vs

/-- The blocks cover `V`. -/
def Covers (π : Partition V) (vs : List V) : Prop :=
  ∀ v ∈ vs, ∃ b ∈ π.blocks, v ∈ b

/-- Distinct blocks are disjoint. -/
def PairwiseDisjoint (π : Partition V) : Prop :=
  ∀ b₁ ∈ π.blocks, ∀ b₂ ∈ π.blocks, b₁ ≠ b₂ →
    ∀ v, v ∈ b₁ → v ∈ b₂ → False

/-- A genuine partition of `V` (INV-1 of AVI-510 §1.3): blocks are subsets of
`V`, cover it, and are pairwise disjoint. Blocks are non-empty by construction
in the fixtures; non-emptiness is not needed for INV-Q0. -/
def IsPartition (π : Partition V) (vs : List V) : Prop :=
  π.Subset vs ∧ π.Covers vs ∧ π.PairwiseDisjoint

end Partition

/-! ## §1.2 The auditable functional `(Q)` -/

/-- `σ_in(b)` — twice the number of internal edges of block `b`:
`Σ_{v∈b} #{w ∈ b : {v,w} ∈ E_s}` (`csSigmaIn`, `Community.hs:100-105`). -/
def sigmaIn {V : Type} (g : SupportGraph V) (b : List V) : Nat :=
  (b.map (fun v => (b.filter (fun w => g.adj v w)).length)).sum

/-- `σ_tot(b)` — the total degree of block `b` (`csSigmaTot`,
`Community.hs:96-99`). -/
def sigmaTot {V : Type} (g : SupportGraph V) (b : List V) : Nat :=
  (b.map g.deg).sum

/-- One summand of `(Q)`:
`σ_in(b)/(2m) − (σ_tot(b)/(2m))²` (AVI-618 §1.2). -/
def communityTerm {V : Type} (g : SupportGraph V) (b : List V) : Float :=
  let twoM := g.twoM
  let si := Float.ofNat (sigmaIn g b)
  let st := Float.ofNat (sigmaTot g b)
  si / twoM - (st / twoM) * (st / twoM)

/-- The raw fold of `(Q)` over the partition's blocks, mirroring
`foldl' acc 0.0 (Map.keys cm)` (`Community.hs:161-167`) without the `m = 0`
pin. On an edgeless graph this expression is `NaN` (0/0), which is exactly why
the implementation pins the value instead of evaluating it. -/
def modularityRaw {V : Type} (g : SupportGraph V) (π : Partition V) : Float :=
  π.blocks.foldl (fun q b => q + communityTerm g b) 0.0

/-- **`modularity`** — auditable global modularity `Q` at resolution `γ = 1`
(`Community.hs:151`). Exactly the Haskell wiring: compute the block aggregates,
then fold the per-community terms in partition order; when `m = 0` the result is
pinned to `0.0` (`if csM stats == 0.0 then 0.0`, `Community.hs:165-166`), which
avoids the `0/0` in `(Q)` (AVI-618 §1.3, INV-Q0). -/
def modularity {V : Type} (g : SupportGraph V) (π : Partition V) : Float :=
  if g.m = 0 then 0.0 else modularityRaw g π

/-! ## §2 INV-Q0 — `theorem modularity_edgesAbsent_zero` -/

/-- Under INV-Q0 every block has `σ_in = 0`: a member with an internal neighbour
would have positive degree. -/
theorem sigmaIn_eq_zero {V : Type} (g : SupportGraph V) (h : g.AllDegreesZero)
    (b : List V) : sigmaIn g b = 0 := by
  unfold sigmaIn
  apply list_sum_eq_zero_of_forall_zero
  intro x hx
  obtain ⟨v, _, rfl⟩ := List.mem_map.mp hx
  have hnil : b.filter (fun w => g.adj v w) = [] := by
    rw [List.filter_eq_nil_iff]
    intro w _ hAdj
    have hfalse : g.adj v w = false :=
      SupportGraph.not_adj_of_deg_eq_zero (v := v) (w := w) (h v)
    rw [hfalse] at hAdj
    exact Bool.false_ne_true hAdj
  simp [hnil]

/-- Under INV-Q0 every block also has `σ_tot = 0`. -/
theorem sigmaTot_eq_zero {V : Type} (g : SupportGraph V) (h : g.AllDegreesZero)
    (b : List V) : sigmaTot g b = 0 := by
  unfold sigmaTot
  apply list_sum_eq_zero_of_forall_zero
  intro x hx
  obtain ⟨v, _, rfl⟩ := List.mem_map.mp hx
  exact h v

/-- **INV-Q0 (AVI-618 §2).** If every vertex of the undirected simple support
graph `G_s` has degree zero — equivalently `E_s = 0`, equivalently `csM = 0` —
then for *every* partition `π` of `V`, `Q(π) = 0`.

This matches the Haskell acceptance property
`∀ g cm. allDegreesZero g ⇒ modularity g cm == 0.0`: the `m = 0` guard in
`modularity` (`Community.hs:165-166`) is what makes the value well defined, and
the only content needed is that the hypothesis really forces `csM = 0`
(`SupportGraph.m_eq_zero`). The companion lemmas `sigmaIn_eq_zero` /
`sigmaTot_eq_zero` record the underlying mathematics: both terms of every
summand vanish before the pin is applied. -/
theorem modularity_edgesAbsent_zero {V : Type} (g : SupportGraph V)
    (π : Partition V) (h : g.AllDegreesZero) : modularity g π = 0.0 := by
  have hm : g.m = 0 := g.m_eq_zero h
  simp [modularity, hm]

/-! ## INV-Q0 fixture: four isolated nodes, one community ⇒ `Q = 0` -/

/-- Fixture required by AVI-618 §2 INV-Q0: four isolated nodes. -/
def isolatedFour : SupportGraph (Fin 4) where
  vertices := [0, 1, 2, 3]
  nodup := by decide
  complete := by decide
  adj := fun _ _ => false
  symm := by intros; rfl
  irrefl := by intro; rfl

/-- The single-community partition `{0 ↦ [0,1,2,3]}` of the four-node fixture. -/
def isolatedFourSingle : Partition (Fin 4) where
  blocks := [[0, 1, 2, 3]]

/-- The fixture is edgeless in the INV-Q0 sense. -/
theorem isolatedFour_allDegreesZero : isolatedFour.AllDegreesZero := by
  intro v
  rfl

/-- **INV-Q0 fixture.** Four isolated nodes in a single community give `Q = 0`,
mirroring the acceptance property in AVI-618 §2. -/
theorem isolatedFour_modularity_zero :
    modularity isolatedFour isolatedFourSingle = 0.0 :=
  modularity_edgesAbsent_zero isolatedFour isolatedFourSingle
    isolatedFour_allDegreesZero

end GraphosFormalization
