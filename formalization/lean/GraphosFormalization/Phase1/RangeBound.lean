/-
  Graphos formalization — Phase 1, INV-Q1: the modularity range bound.

  Requirements:
  `docs/math-requirements/AVI-618-auditable-modularity-lean-formalization-requirements.md`
  §1.4 (algebraic identities), §3 (range-bound lemma), §5 fixture 5, §6 (exact
  rationals on the Lean side).

  ## What is proved

  For a simple undirected support graph `G_s = (V, A)` with `m = |A| ≥ 1` and any
  partition `π` of `V`,

  ```
    -1/2 ≤ Q(π) ≤ 1 ,     Q(π) = Σ_c [ σ_in(c)/(2m) − (σ_tot(c)/(2m))² ] .
  ```

  We state the claim **without division**, on the common denominator `(2m)²`:

  ```
    Qnum(π) := Σ_c [ 2m·σ_in(c) − σ_tot(c)² ]  =  (2m)² · Q(π)
    -2m² ≤ Qnum(π) ≤ 4m²                 (m ≥ 1)
  ```

  For `m ≥ 1` the denominator `(2m)²` is positive, so this is *exactly*
  `-1/2 ≤ Q(π) ≤ 1`. (In quotient form: `-2m²/(2m)² = -1/2` and
  `4m²/(2m)² = 1`.)

  ## Why the exact (integer) layer, and why the edge set is primary

  The implementation `modularity` computes `Q` in `Float` (`Modularity.lean`,
  mirroring Haskell's `Double`). A general inequality about an opaque `Float`
  expression is not provable in dependency-free core Lean (no `Rat`, no
  `Mathlib`, no `Float` order lemmas), and AVI-618 §6 already pins the Lean side
  to *exact* arithmetic: "The Lean side is proven over ℚ/ℝ (exact); the Haskell
  side computes in `Double`". This module therefore proves INV-Q1 over the
  exact integer quantities; the Float rendering is the implementation's rounded
  evaluation and is covered by the AVI-1301 cross-check on pinned fixtures.

  The support graph `G_s = (V, A)` (AVI-618 §0.2) is represented here by its
  **edge set `A`**, exactly as `Graph` stores it in `gEdges` (`Core.hs:103`) and
  as the requirement pins the one-record-per-unordered-pair invariant. The
  aggregates are the requirement's own definitions:

  * `σ_in(c) = 2·|internal edges of c|`   (`= Σ_{v∈B_c} deg^c_s(v)`),
  * `σ_tot(c) =` the number of edge endpoints in `B_c`
    (`= Σ_{v∈B_c} deg_s(v)`),
  * `m = |A|` (`= (Σ_v deg_s(v))/2 = csM`, `Community.hs:95`).

  `EdgeGraph` is the simple-graph edge-set representation: `edges` has no
  duplicates (`edges_nodup`), no self-loops (`edges_noLoop`), and never stores
  both orientations of one undirected edge (`edges_noRev`); every endpoint is a
  vertex (`edges_mem`). This is the `neighbors`-collapses-multi-edges,
  one-record-per-pair semantics of `Graph/Query.hs:37`.

  ## Proof shape

  Per block `c`, let `t = σ_tot(c)`, `a = σ_in(c)` and `M = m`.

  * Counting: `2t = a + 2k_c`, where `k_c` is the number of edges with at least
    one endpoint in `c` (an edge contributes `2` endpoint counts when internal,
    `1` when cut). Since `k_c ≤ m`, `2t ≤ a + 2M`.
  * Hence `t ≤ M + a/2`, and with `a ≤ t ≤ 2M` this gives the per-block
    quadratic bound `t² ≤ 2M·a + M·t`.
  * Summing over blocks: `Σ_c t_c = 2M` (each edge has two endpoints and every
    vertex lies in exactly one block), `Σ_c a_c ≤ Σ_c t_c`, and the two summed
    inequalities give `-2M² ≤ Qnum` and `Qnum ≤ 4M²`.

  The single non-trivial list fact is *unique block membership*: for every
  vertex `v`, `Σ_c [v ∈ B_c] = 1`, from cover + pairwise disjointness +
  duplicate-free block list. Everything else is per-edge indicator arithmetic.

  ## Honesty caveats (inherited from AVI-618 §3)

  * Directed graphs are out of scope: the `-1/2` floor fails for `gDirected`.
  * Weighted modularity is out of scope: the bounds are for the unweighted
    simple support graph.
-/
import GraphosFormalization.Phase1.Modularity

namespace GraphosFormalization

/-! ## §0. List-sum helpers (core Lean ships none of these) -/

theorem sum_map_zero {V : Type} (l : List V) : (l.map (fun _ => 0)).sum = 0 := by
  induction l with
  | nil => rfl
  | cons a t ih => simp [ih]

theorem sum_map_add {V : Type} (l : List V) (f g : V → Nat) :
    (l.map (fun x => f x + g x)).sum = (l.map f).sum + (l.map g).sum := by
  induction l with
  | nil => rfl
  | cons a t ih =>
      simp only [List.map_cons, List.sum_cons, ih]
      omega

theorem sum_map_mul_left {V : Type} (l : List V) (c : Nat) (f : V → Nat) :
    (l.map (fun x => c * f x)).sum = c * (l.map f).sum := by
  induction l with
  | nil => simp
  | cons a t ih => simp only [List.map_cons, List.sum_cons, ih, Nat.mul_add]

theorem sum_map_le_sum {V : Type} (l : List V) (f g : V → Nat)
    (h : ∀ x ∈ l, f x ≤ g x) : (l.map f).sum ≤ (l.map g).sum := by
  induction l with
  | nil => simp
  | cons a t ih =>
      simp only [List.map_cons, List.sum_cons]
      exact Nat.add_le_add (h a (by simp)) (ih (fun x hx => h x (by simp [hx])))

theorem sum_map_congr {V : Type} (l : List V) (f g : V → Nat)
    (h : ∀ x ∈ l, f x = g x) : (l.map f).sum = (l.map g).sum := by
  induction l with
  | nil => rfl
  | cons a t ih =>
      simp only [List.map_cons, List.sum_cons]
      rw [h a (by simp), ih (fun x hx => h x (by simp [hx]))]

theorem sum_comm {α β : Type} (l₁ : List α) (l₂ : List β) (f : α → β → Nat) :
    (l₁.map (fun a => (l₂.map (fun b => f a b)).sum)).sum
      = (l₂.map (fun b => (l₁.map (fun a => f a b)).sum)).sum := by
  induction l₁ with
  | nil => simp [sum_map_zero]
  | cons a t ih =>
      simp only [List.map_cons, List.sum_cons]
      rw [ih, sum_map_add]

theorem sum_map_const {V : Type} (l : List V) (c : Nat) :
    (l.map (fun _ => c)).sum = c * l.length := by
  induction l with
  | nil => simp
  | cons a t ih =>
      simp only [List.map_cons, List.sum_cons, List.length_cons, ih, Nat.mul_add, Nat.mul_one]
      omega

theorem sum_map_eq_zero_of_forall {V : Type} (l : List V) (f : V → Nat)
    (h : ∀ x ∈ l, f x = 0) : (l.map f).sum = 0 := by
  induction l with
  | nil => rfl
  | cons a t ih =>
      simp only [List.map_cons, List.sum_cons, h a (by simp)]
      rw [ih (fun x hx => h x (by simp [hx]))]

/-! ## §1. The simple support graph `G_s = (V, A)` -/

/-- The simple support graph as a vertex enumeration plus its **edge set**
`A = edges` (one record per unordered pair, an arbitrary but fixed orientation).

The fields pin the simple-undirected semantics of AVI-618 §0.2 and
`Graph/Query.hs:37`: `edges` is duplicate-free, loop-free, never stores both
orientations of one undirected edge, and every endpoint is a vertex. `m` is
therefore *definitionally* `|A|`, the edge count (`csM`). -/
structure EdgeGraph (V : Type) where
  /-- The vertex set `V`, as a duplicate-free enumeration. -/
  vertices : List V
  /-- `V` is a set. -/
  nodup : vertices.Nodup
  /-- The enumeration is exhaustive. -/
  complete : ∀ v : V, v ∈ vertices
  /-- The edge set `A`, one record per unordered pair. -/
  edges : List (V × V)
  /-- No multi-edges. -/
  edges_nodup : edges.Nodup
  /-- No self-loops (simple graph). -/
  edges_noLoop : ∀ e ∈ edges, e.1 ≠ e.2
  /-- Every endpoint is a vertex. -/
  edges_mem : ∀ e ∈ edges, e.1 ∈ vertices ∧ e.2 ∈ vertices
  /-- No reversed duplicate: `{u,v}` is stored in exactly one orientation. -/
  edges_noRev : ∀ e ∈ edges, (e.2, e.1) ∉ edges

namespace EdgeGraph

variable {V : Type} [DecidableEq V]

/-- `m = |A|`, the number of edges of `G_s` (`csM`, `Community.hs:95`). -/
def m (g : EdgeGraph V) : Nat := g.edges.length

/-- Number of endpoints of edge `e` that lie in block `b` (`0`, `1` or `2`). -/
def endpointsIn (b : List V) (e : V × V) : Nat :=
  (if e.1 ∈ b then 1 else 0) + (if e.2 ∈ b then 1 else 0)

/-- Edge `e` is internal to block `b`. -/
def bothIn (b : List V) (e : V × V) : Bool :=
  decide (e.1 ∈ b) && decide (e.2 ∈ b)

/-- Edge `e` has at least one endpoint in block `b`. -/
def touches (b : List V) (e : V × V) : Bool :=
  decide (e.1 ∈ b) || decide (e.2 ∈ b)

/-- Indicator (as a `Nat`) of "edge internal to `b`". This is the per-edge
summand of `σ_in`: an internal edge has two internal degree endpoints. -/
def internalIndicator (b : List V) (e : V × V) : Nat :=
  if bothIn b e then 1 else 0

/-- Indicator of "edge has an endpoint in `b`". -/
def touchIndicator (b : List V) (e : V × V) : Nat :=
  if touches b e then 1 else 0

/-- `σ_tot(b)` — total degree of block `b`, counted as edge endpoints in `b`
(`Σ_{v∈B_c} deg_s(v)`, `csSigmaTot`, `Community.hs:96-99`). -/
def sigmaTot (g : EdgeGraph V) (b : List V) : Nat :=
  (g.edges.map (fun e => endpointsIn b e)).sum

/-- `σ_in(b)` — internal degree sum of block `b`, i.e. `2 · |internal edges|`
(`Σ_{v∈B_c} deg^c_s(v)`, `csSigmaIn`, `Community.hs:100-105`). -/
def sigmaIn (g : EdgeGraph V) (b : List V) : Nat :=
  (g.edges.map (fun e => 2 * internalIndicator b e)).sum

/-- Number of edges with at least one endpoint in `b` (internal + cut). -/
def touchTotal (g : EdgeGraph V) (b : List V) : Nat :=
  (g.edges.map (touchIndicator b)).sum

/-! ### §1.1 Per-edge indicator algebra -/

theorem internalIndicator_eq (b : List V) (e : V × V) :
    internalIndicator b e =
      (if e.1 ∈ b then 1 else 0) * (if e.2 ∈ b then 1 else 0) := by
  unfold internalIndicator bothIn
  by_cases h1 : e.1 ∈ b <;> by_cases h2 : e.2 ∈ b <;> simp [h1, h2]

theorem endpoints_eq (b : List V) (e : V × V) :
    endpointsIn b e = internalIndicator b e + touchIndicator b e := by
  unfold endpointsIn internalIndicator touchIndicator bothIn touches
  by_cases h1 : e.1 ∈ b <;> by_cases h2 : e.2 ∈ b <;> simp [h1, h2]

theorem endpoints_two (b : List V) (e : V × V) :
    2 * endpointsIn b e = 2 * internalIndicator b e + 2 * touchIndicator b e := by
  unfold endpointsIn internalIndicator touchIndicator bothIn touches
  by_cases h1 : e.1 ∈ b <;> by_cases h2 : e.2 ∈ b <;> simp [h1, h2]

theorem two_mul_endpoints_le (b : List V) (e : V × V) :
    2 * internalIndicator b e ≤ endpointsIn b e := by
  unfold endpointsIn internalIndicator bothIn
  by_cases h1 : e.1 ∈ b <;> by_cases h2 : e.2 ∈ b <;> simp [h1, h2]

theorem touchIndicator_le_one (b : List V) (e : V × V) : touchIndicator b e ≤ 1 := by
  unfold touchIndicator
  split <;> omega

theorem endpoints_le_two (b : List V) (e : V × V) : endpointsIn b e ≤ 2 := by
  unfold endpointsIn
  split <;> split <;> omega

/-! ### §1.2 Per-block identities -/

/-- `2·σ_tot(b) = σ_in(b) + 2·k_b`, where `k_b` counts edges touching `b`. -/
theorem two_sigmaTot_eq (g : EdgeGraph V) (b : List V) :
    2 * g.sigmaTot b = g.sigmaIn b + 2 * g.touchTotal b := by
  unfold sigmaTot sigmaIn touchTotal
  rw [← sum_map_mul_left (g.edges) 2 (fun e => endpointsIn b e)]
  rw [show (g.edges.map (fun e => 2 * internalIndicator b e)).sum
        + 2 * (g.edges.map (touchIndicator b)).sum
      = (g.edges.map (fun e =>
          2 * internalIndicator b e + 2 * touchIndicator b e)).sum from by
        rw [← sum_map_mul_left (g.edges) 2 (fun e => touchIndicator b e)]
        rw [← sum_map_add]]
  exact sum_map_congr _ _ _ (fun e _ => endpoints_two b e)

/-- `σ_in(b) ≤ σ_tot(b)`: every internal endpoint is a total endpoint. -/
theorem sigmaIn_le_sigmaTot (g : EdgeGraph V) (b : List V) :
    g.sigmaIn b ≤ g.sigmaTot b := by
  unfold sigmaIn sigmaTot
  exact sum_map_le_sum _ _ _ (fun e _ => two_mul_endpoints_le b e)

/-- `k_b ≤ m`: at most all edges touch `b`. -/
theorem touchTotal_le_m (g : EdgeGraph V) (b : List V) : g.touchTotal b ≤ g.m := by
  unfold touchTotal m
  calc (g.edges.map (touchIndicator b)).sum
      ≤ (g.edges.map (fun _ => 1)).sum :=
        sum_map_le_sum _ _ _ (fun e _ => touchIndicator_le_one b e)
    _ = 1 * g.edges.length := sum_map_const _ 1
    _ = g.edges.length := by rw [Nat.one_mul]

/-- `σ_tot(b) ≤ 2m`: each edge contributes at most two endpoints to any block. -/
theorem sigmaTot_le_two_m (g : EdgeGraph V) (b : List V) :
    g.sigmaTot b ≤ 2 * g.m := by
  unfold sigmaTot m
  calc (g.edges.map (fun e => endpointsIn b e)).sum
      ≤ (g.edges.map (fun _ => 2)).sum :=
        sum_map_le_sum _ _ _ (fun e _ => endpoints_le_two b e)
    _ = 2 * g.edges.length := sum_map_const _ 2

/-! ## §2. Partitions and unique block membership -/

/-- A genuine **set partition** of `vs`: blocks are subsets of `vs`, cover it,
  are pairwise disjoint, the block list has no duplicate entries, and each
  block is duplicate-free. Non-emptiness is not needed for the bound (empty
  blocks contribute `0`), so it is deliberately not required — the theorem
  therefore covers the pinned "no empty block" hypothesis as a special case. -/
def IsSetPartition (π : Partition V) (vs : List V) : Prop :=
  π.Subset vs ∧ π.Covers vs ∧ π.PairwiseDisjoint ∧
    π.blocks.Nodup ∧ (∀ b ∈ π.blocks, b.Nodup)

theorem mem_of_indicator_pos {v : V} {b : List V}
    (h : (if v ∈ b then 1 else 0) = 1) : v ∈ b := by
  by_cases hv : v ∈ b
  · exact hv
  · simp [hv] at h

theorem indicator_eq_zero_of_not_mem {v : V} {b : List V} (h : v ∉ b) :
    (if v ∈ b then 1 else 0) = 0 := by simp [h]

/-- At most one block contains `v` (pairwise disjointness + no duplicate block
entries). -/
theorem indicator_sum_le_one {v : V} {blocks : List (List V)}
    (hnodup : blocks.Nodup)
    (hdisj : ∀ b₁ ∈ blocks, ∀ b₂ ∈ blocks, b₁ ≠ b₂ →
      ∀ x, x ∈ b₁ → x ∈ b₂ → False) :
    (blocks.map (fun b => if v ∈ b then 1 else 0)).sum ≤ 1 := by
  induction blocks with
  | nil => simp
  | cons b bs ih =>
      obtain ⟨hb_notin, hbs_nodup⟩ := List.nodup_cons.mp hnodup
      have hdisj_bs : ∀ b₁ ∈ bs, ∀ b₂ ∈ bs, b₁ ≠ b₂ →
          ∀ x, x ∈ b₁ → x ∈ b₂ → False := by
        intro b₁ h₁ b₂ h₂ hne
        exact hdisj b₁ (by simp [h₁]) b₂ (by simp [h₂]) hne
      by_cases hvb : v ∈ b
      · have hzero : (bs.map (fun b' => if v ∈ b' then 1 else 0)).sum = 0 := by
          apply sum_map_eq_zero_of_forall
          intro b' hb'
          have hne : b' ≠ b := by
            intro heq
            rw [heq] at hb'
            exact hb_notin hb'
          have hnot : v ∉ b' := fun hv' =>
            hdisj b (by simp) b' (by simp [hb']) (Ne.symm hne) v hvb hv'
          simp [hnot]
        simp only [List.map_cons, List.sum_cons]
        rw [if_pos hvb, hzero]
        omega
      · have hih := ih hbs_nodup hdisj_bs
        simp only [List.map_cons, List.sum_cons]
        rw [if_neg hvb]
        simpa using hih

/-- At least one block contains `v` when `v` is covered. -/
theorem indicator_sum_ge_one {v : V} {blocks : List (List V)}
    (h : ∃ b, b ∈ blocks ∧ v ∈ b) :
    1 ≤ (blocks.map (fun b => if v ∈ b then 1 else 0)).sum := by
  induction blocks with
  | nil => simp at h
  | cons c cs ih =>
      obtain ⟨b, hb, hvb⟩ := h
      by_cases hcb : v ∈ c
      · simp only [List.map_cons, List.sum_cons]
        rw [if_pos hcb]
        omega
      · have hmem : ∃ b, b ∈ cs ∧ v ∈ b := by
          rcases List.mem_cons.mp hb with heq | hmem
          · rw [heq] at hvb
            exact absurd hvb hcb
          · exact ⟨b, hmem, hvb⟩
        have hih := ih hmem
        simp only [List.map_cons, List.sum_cons]
        rw [if_neg hcb]
        simpa using hih

/-- **Unique block membership.** Every vertex of `vs` lies in exactly one block
of a set partition. -/
theorem indicator_sum_eq_one {vs : List V} {π : Partition V}
    (hpart : IsSetPartition π vs) {v : V} (hv : v ∈ vs) :
    (π.blocks.map (fun b => if v ∈ b then 1 else 0)).sum = 1 := by
  obtain ⟨_, hcover, hdisj, hnodup, _⟩ := hpart
  have hle := indicator_sum_le_one (v := v) hnodup hdisj
  have hge := indicator_sum_ge_one (v := v) (hcover v hv)
  omega

/-- For any edge `e`, the endpoint counts over the blocks sum to `2`: each of
its two endpoints lies in exactly one block. -/
theorem endpoints_sum_eq_two (g : EdgeGraph V) {π : Partition V}
    (hpart : IsSetPartition π g.vertices) {e : V × V} (he : e ∈ g.edges) :
    (π.blocks.map (fun b => endpointsIn b e)).sum = 2 := by
  obtain ⟨hmem1, hmem2⟩ := g.edges_mem e he
  have h1 : (π.blocks.map (fun b => if e.1 ∈ b then 1 else 0)).sum = 1 :=
    indicator_sum_eq_one hpart hmem1
  have h2 : (π.blocks.map (fun b => if e.2 ∈ b then 1 else 0)).sum = 1 :=
    indicator_sum_eq_one hpart hmem2
  unfold endpointsIn
  rw [sum_map_add, h1, h2]

/-! ## §3. The aggregate identity `Σ_c σ_tot(c) = 2m` -/

theorem sum_sigmaTot_eq_two_m (g : EdgeGraph V) {π : Partition V}
    (hpart : IsSetPartition π g.vertices) :
    (π.blocks.map (fun b => g.sigmaTot b)).sum = 2 * g.m := by
  unfold sigmaTot m
  rw [sum_comm (π.blocks) (g.edges) (fun b e => endpointsIn b e)]
  calc (g.edges.map (fun e => (π.blocks.map (fun b => endpointsIn b e)).sum)).sum
      = (g.edges.map (fun _ => 2)).sum :=
        sum_map_congr _ _ _ (fun e he => endpoints_sum_eq_two g hpart he)
    _ = 2 * g.edges.length := sum_map_const _ 2

/-! ## §4. Per-block quadratic bound -/

/-- Pure arithmetic core: if `2t ≤ a + 2M`, `a ≤ t` and `t ≤ 2M`, then
`t² ≤ 2M·a + M·t`. -/
theorem quadratic_bound (M a t : Nat) (h2t : 2 * t ≤ a + 2 * M)
    (_hat : a ≤ t) (ht : t ≤ 2 * M) : t * t ≤ 2 * M * a + M * t := by
  by_cases h : t ≤ M
  · have h1 : t * t ≤ M * t := Nat.mul_le_mul_right t h
    omega
  · have hMt : M ≤ t := by omega
    have h2 : 2 * (t - M) ≤ a := by
      rw [Nat.mul_sub_left_distrib]
      omega
    have h3 : 2 * (t * (t - M)) ≤ t * a := by
      have hmul := Nat.mul_le_mul_right t h2
      calc 2 * (t * (t - M)) = (2 * (t - M)) * t := by
            rw [Nat.mul_assoc, Nat.mul_comm (t - M) t, ← Nat.mul_assoc]
        _ ≤ a * t := hmul
        _ = t * a := by rw [Nat.mul_comm]
    have h4 : t * (t - M) = t * t - M * t := by
      rw [Nat.mul_sub_left_distrib, Nat.mul_comm M t]
    have h6 : t * a ≤ 2 * M * a := by
      calc t * a ≤ (2 * M) * a := Nat.mul_le_mul_right a ht
        _ = 2 * M * a := rfl
    have h7 : 2 * (t * t - M * t) ≤ 2 * (M * a) := by
      calc 2 * (t * t - M * t) = 2 * (t * (t - M)) := by rw [h4]
        _ ≤ t * a := h3
        _ ≤ 2 * M * a := h6
        _ = 2 * (M * a) := by rw [Nat.mul_assoc]
    have h8 : t * t - M * t ≤ M * a :=
      Nat.le_of_mul_le_mul_left h7 (by decide : 0 < 2)
    have h8' : t * t - M * t ≤ 2 * M * a := by
      have hmul : M * a ≤ 2 * M * a := by
        have hle := Nat.le_mul_of_pos_left (M * a) (by decide : 0 < 2)
        calc M * a ≤ 2 * (M * a) := hle
          _ = 2 * M * a := (Nat.mul_assoc 2 M a).symm
      omega
    have h9 : M * t ≤ t * t :=
      Nat.mul_le_mul_right t hMt
    have h10 : t * t = (t * t - M * t) + M * t := (Nat.sub_add_cancel h9).symm
    rw [h10]
    omega

/-- Per-block INV-Q1 core: `σ_tot(b)² ≤ 2m·σ_in(b) + m·σ_tot(b)`. -/
theorem block_quadratic_bound (g : EdgeGraph V) (b : List V) :
    g.sigmaTot b * g.sigmaTot b
      ≤ 2 * g.m * g.sigmaIn b + g.m * g.sigmaTot b := by
  apply quadratic_bound g.m (g.sigmaIn b) (g.sigmaTot b)
  · have h := g.two_sigmaTot_eq b
    have hk := g.touchTotal_le_m b
    omega
  · exact g.sigmaIn_le_sigmaTot b
  · exact g.sigmaTot_le_two_m b

/-! ## §5. The range bound -/

/-- The exact numerator of `Q` over the common denominator `(2m)²`:
`Qnum(π) = Σ_c [2m·σ_in(c) − σ_tot(c)²] = (2m)² · Q(π)`. -/
def modularityNum (g : EdgeGraph V) (π : Partition V) : Int :=
  ((π.blocks.map (fun b => 2 * g.m * g.sigmaIn b)).sum : Int)
    - ((π.blocks.map (fun b => g.sigmaTot b * g.sigmaTot b)).sum : Int)

/-- **INV-Q1 (AVI-618 §3) — `modularity_range_bound`.**

For a simple undirected support graph with at least one edge and any partition
of its vertex set, the exact modularity numerator lies in `[-2m², 4m²]`.
Dividing by `(2m)² > 0` this is exactly `-1/2 ≤ Q(π) ≤ 1`. The upper bound is
not attained for `m ≥ 1` (it is the supremum); the lower bound is tight, e.g.
the bipartition of `K_{n,n}` gives exactly `-1/2`. -/
theorem modularity_range_bound (g : EdgeGraph V) (π : Partition V)
    (hpart : IsSetPartition π g.vertices) (_hm : 1 ≤ g.m) :
    -(2 * (g.m : Int) * (g.m : Int)) ≤ modularityNum g π ∧
      modularityNum g π ≤ 4 * (g.m : Int) * (g.m : Int) := by
  -- Aggregate facts.
  have hsumT : (π.blocks.map (fun b => g.sigmaTot b)).sum = 2 * g.m :=
    sum_sigmaTot_eq_two_m g hpart
  have hsumA : (π.blocks.map (fun b => g.sigmaIn b)).sum ≤ 2 * g.m := by
    calc (π.blocks.map (fun b => g.sigmaIn b)).sum
        ≤ (π.blocks.map (fun b => g.sigmaTot b)).sum :=
          sum_map_le_sum _ _ _ (fun b _ => g.sigmaIn_le_sigmaTot b)
      _ = 2 * g.m := hsumT
  -- The two summed quadratic inequalities.
  have hupper : (π.blocks.map (fun b => g.sigmaTot b * g.sigmaTot b)).sum
      ≤ 2 * g.m * (π.blocks.map (fun b => g.sigmaIn b)).sum + g.m * (2 * g.m) := by
    calc (π.blocks.map (fun b => g.sigmaTot b * g.sigmaTot b)).sum
        ≤ (π.blocks.map (fun b => 2 * g.m * g.sigmaIn b + g.m * g.sigmaTot b)).sum :=
          sum_map_le_sum _ _ _ (fun b _ => g.block_quadratic_bound b)
      _ = (π.blocks.map (fun b => 2 * g.m * g.sigmaIn b)).sum
            + (π.blocks.map (fun b => g.m * g.sigmaTot b)).sum := sum_map_add _ _ _
      _ = 2 * g.m * (π.blocks.map (fun b => g.sigmaIn b)).sum
            + g.m * (π.blocks.map (fun b => g.sigmaTot b)).sum := by
          rw [sum_map_mul_left, sum_map_mul_left]
      _ = 2 * g.m * (π.blocks.map (fun b => g.sigmaIn b)).sum + g.m * (2 * g.m) := by
          rw [hsumT]
  -- Normalise to the common-denominator form and finish in `Nat`, then cast.
  have hmul2 : g.m * (2 * g.m) = 2 * (g.m * g.m) := by
    rw [Nat.mul_comm g.m (2 * g.m), Nat.mul_assoc]
  have hA_sum : (π.blocks.map (fun b => g.sigmaIn b)).sum ≤ 2 * g.m := hsumA
  have hT2n : (π.blocks.map (fun b => g.sigmaTot b * g.sigmaTot b)).sum
      ≤ 2 * (g.m * (π.blocks.map (fun b => g.sigmaIn b)).sum) + 2 * (g.m * g.m) := by
    have h := hupper
    rw [hmul2] at h
    rwa [Nat.mul_assoc] at h
  have hUn : 2 * (g.m * (π.blocks.map (fun b => g.sigmaIn b)).sum) ≤ 4 * (g.m * g.m) := by
    have hstep : g.m * (π.blocks.map (fun b => g.sigmaIn b)).sum ≤ g.m * (2 * g.m) :=
      Nat.mul_le_mul_left g.m hA_sum
    calc 2 * (g.m * (π.blocks.map (fun b => g.sigmaIn b)).sum)
        ≤ 2 * (g.m * (2 * g.m)) := Nat.mul_le_mul_left 2 hstep
      _ = 2 * (2 * (g.m * g.m)) := by rw [hmul2]
      _ = 4 * (g.m * g.m) := by omega
  have hT2i : ((π.blocks.map (fun b => g.sigmaTot b * g.sigmaTot b)).sum : Int)
      ≤ 2 * ((g.m : Int) * ((π.blocks.map (fun b => g.sigmaIn b)).sum : Int))
        + 2 * ((g.m : Int) * (g.m : Int)) := by
    exact_mod_cast hT2n
  have hUi : 2 * ((g.m : Int) * ((π.blocks.map (fun b => g.sigmaIn b)).sum : Int))
      ≤ 4 * ((g.m : Int) * (g.m : Int)) := by
    exact_mod_cast hUn
  have hT2nonneg : (0 : Int)
      ≤ ((π.blocks.map (fun b => g.sigmaTot b * g.sigmaTot b)).sum : Int) :=
    Int.ofNat_nonneg _
  have hNum : modularityNum g π
      = 2 * ((g.m : Int) * ((π.blocks.map (fun b => g.sigmaIn b)).sum : Int))
        - ((π.blocks.map (fun b => g.sigmaTot b * g.sigmaTot b)).sum : Int) := by
    unfold modularityNum
    have hnat : (π.blocks.map (fun b => 2 * g.m * g.sigmaIn b)).sum
        = 2 * (g.m * (π.blocks.map (fun b => g.sigmaIn b)).sum) := by
      rw [sum_map_mul_left _ (2 * g.m) _, Nat.mul_assoc]
    rw [show ((π.blocks.map (fun b => 2 * g.m * g.sigmaIn b)).sum : Int)
          = ((2 * (g.m * (π.blocks.map (fun b => g.sigmaIn b)).sum) : Nat) : Int) from by
        exact_mod_cast hnat]
    simp
  have hb1 : 2 * (g.m : Int) * (g.m : Int) = 2 * ((g.m : Int) * (g.m : Int)) := by
    rw [Int.mul_assoc]
  have hb2 : 4 * (g.m : Int) * (g.m : Int) = 4 * ((g.m : Int) * (g.m : Int)) := by
    rw [Int.mul_assoc]
  constructor
  · rw [hNum, hb1]
    omega
  · rw [hNum, hb2]
    omega

/-- Alias matching the AVI-618 §2 artifact name (`theorem modularity_bounded`). -/
theorem modularity_bounded (g : EdgeGraph V) (π : Partition V)
    (hpart : IsSetPartition π g.vertices) (_hm : 1 ≤ g.m) :
    -(2 * (g.m : Int) * (g.m : Int)) ≤ modularityNum g π ∧
      modularityNum g π ≤ 4 * (g.m : Int) * (g.m : Int) :=
  modularity_range_bound g π hpart _hm


/-! ## §6. Tightness fixture: the bipartition of `K_{2,2}` attains `-1/2` -/

/-- `K_{2,2}` on `Fin 4`: the 4-cycle with edges `(0,2), (0,3), (1,2), (1,3)`. -/
def k22 : EdgeGraph (Fin 4) where
  vertices := [0, 1, 2, 3]
  nodup := by decide
  complete := by decide
  edges := [(0, 2), (0, 3), (1, 2), (1, 3)]
  edges_nodup := by decide
  edges_noLoop := by decide
  edges_mem := by decide
  edges_noRev := by decide

/-- The bipartition `{{0,1}, {2,3}}` of `K_{2,2}`. -/
def k22Bipartition : Partition (Fin 4) where
  blocks := [[0, 1], [2, 3]]

theorem k22_isSetPartition : IsSetPartition k22Bipartition k22.vertices := by
  unfold IsSetPartition Partition.Subset Partition.Covers Partition.PairwiseDisjoint
  decide

/-- `m = 4` for `K_{2,2}`. -/
theorem k22_m : k22.m = 4 := rfl

/-- Both blocks are independent, so `σ_in = 0` per block, and both have
`σ_tot = 4`; hence `Qnum = -2·16 = -32 = -2m²`. -/
theorem k22_modularityNum : modularityNum k22 k22Bipartition = -32 := by decide

/-- **The lower bound is attained.** `K_{2,2}` with its bipartition gives
`Qnum = -2m²`, i.e. `Q = -1/2` exactly. -/
theorem k22_attains_lower_bound :
    modularityNum k22 k22Bipartition = -(2 * (k22.m : Int) * (k22.m : Int)) := by
  rw [k22_modularityNum, k22_m]
  decide

/-- The range bound instantiated on the tight fixture (sanity check that the
statement is non-vacuous on a graph with edges). -/
theorem k22_range_bound :
    -(2 * (k22.m : Int) * (k22.m : Int)) ≤ modularityNum k22 k22Bipartition ∧
      modularityNum k22 k22Bipartition ≤ 4 * (k22.m : Int) * (k22.m : Int) :=
  modularity_range_bound k22 k22Bipartition k22_isSetPartition (by rw [k22_m]; decide)

end EdgeGraph

end GraphosFormalization
