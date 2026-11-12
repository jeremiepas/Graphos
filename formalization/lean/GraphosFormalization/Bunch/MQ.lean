/-
  bunch-search-based-modularization — Lean arm of the `modularization-quality`
  capability (openspec/changes/archive/2026-11-11-bunch-search-based-modularization).

  Source: Mitchell & Mancoridis, *On the evaluation of the Bunch search-based
  software modularization algorithm*, Soft Computing 12(1):77–93, 2008, §2.2:

    MQ = Σ_{i=1..k} CF_i ,   CF_i = 0            if µ_i = 0
                             CF_i = 2µ_i / (2µ_i + Σ_{j≠i}(ε_ij + ε_ji))  otherwise

  `µ_i` is the intra-edge weight of cluster i and `ε_ij` the weight of the
  edges from cluster i to cluster j; the undirected fold only ever sees the sum
  `ext_i = Σ_{j≠i}(ε_ij + ε_ji)`, the weight of the edges with exactly one
  endpoint in i.

  ## What is proved (mirrors the spec's requirements)

  Exact, division-free layer (AVI-618 §6 posture: no `Rat`, no `Mathlib`, no
  `Float` order lemmas). A cluster factor is the fraction `cfNum / cfDen`, and
  MQ over a cluster list is the fraction `mqNum / mqDen` built by
  `a/b + c/d = (ad + cb)/(bd)`.

  * `cf_le_one`, `cfDen_pos`           — `0 ≤ CF_i ≤ 1`  ("ranges" scenario)
  * `cf_closed`                        — no external edge ⇒ `CF_i = 1`
                                          ("closed cluster" scenario)
  * `cf_paper_fixture`                 — 1 intra-edge, 3 inter-edges ⇒ `2/5`
                                          (paper Fig. 2, subsystem 2)
  * `mq_le_k`, `mqDen_pos`             — `0 ≤ MQ ≤ k`   ("ranges" scenario)
  * `intra_le_total`                   — intra-edge fraction `≤ 1`
  * `mqNum_eq_zero_of_noIntra`         — no intra-edge ⇒ `MQ = 0`
                                          ("singleton partition" scenario)
  * `mu_moveTo`, `ext_moveTo`,
    `cf_moveTo`                        — moving one node changes `µ`, `ext`
                                          and hence `CF` of no cluster other
                                          than its source and destination:
                                          the paper's O(1) incremental update
                                          (design D3, "incremental update
                                          agrees with full recompute")
  * `twoTriangles_mq`                  — the two-clique fixture of the
                                          `bunch-clustering` spec at size 3:
                                          `MQ = 2·(6/7) = 84/49`

  Toolchain: leanprover/lean4:v4.14.0 (formalization companion); written
  against core only so it also elaborates on newer releases.
-/

namespace GraphosFormalization.Bunch

/-- A weighted undirected edge of the file-level MDG. -/
structure Edge where
  u : Nat
  v : Nat
  w : Nat

/-- The MDG is its edge list (one record per unordered file pair). -/
abbrev MDG := List Edge

/-- A partition is the cluster index of every node. -/
abbrev Assign := Nat → Nat

/-- Total weight of the edges satisfying `p`. -/
def weightWhere (p : Edge → Bool) : MDG → Nat
  | [] => 0
  | e :: es => (if p e then e.w else 0) + weightWhere p es

/-- `n` lies in cluster `c`. -/
def inC (a : Assign) (c n : Nat) : Bool := decide (a n = c)

/-- `µ_c`: weight of the edges with both endpoints in `c`. -/
def mu (g : MDG) (a : Assign) (c : Nat) : Nat :=
  weightWhere (fun e => inC a c e.u && inC a c e.v) g

/-- `ext_c = Σ_{j≠c}(ε_cj + ε_jc)`: weight of the edges with exactly one
    endpoint in `c`. -/
def ext (g : MDG) (a : Assign) (c : Nat) : Nat :=
  weightWhere (fun e => inC a c e.u != inC a c e.v) g

/-- Numerator of the cluster factor: `0` when `µ = 0`, else `2µ`. -/
def cfNum (m _x : Nat) : Nat := if m = 0 then 0 else 2 * m

/-- Denominator of the cluster factor: `1` when `µ = 0`, else `2µ + ext`. -/
def cfDen (m x : Nat) : Nat := if m = 0 then 1 else 2 * m + x

/-- Denominator of `MQ` over a cluster list: the product of the cluster
    denominators. -/
def mqDen (g : MDG) (a : Assign) : List Nat → Nat
  | [] => 1
  | c :: cs => cfDen (mu g a c) (ext g a c) * mqDen g a cs

/-- Numerator of `MQ` over a cluster list, with `mqDen` as denominator:
    `a/b + c/d = (a·d + c·b)/(b·d)`. -/
def mqNum (g : MDG) (a : Assign) : List Nat → Nat
  | [] => 0
  | c :: cs =>
      cfNum (mu g a c) (ext g a c) * mqDen g a cs
        + mqNum g a cs * cfDen (mu g a c) (ext g a c)

/-- Weight of the edges whose endpoints share a cluster (`Σ_c µ_c`). -/
def intra (g : MDG) (a : Assign) : Nat :=
  weightWhere (fun e => decide (a e.u = a e.v)) g

/-- Total edge weight `Σ_e w_e`. -/
def total : MDG → Nat
  | [] => 0
  | e :: es => e.w + total es

-- ───────────────────────────────────────────────
-- Cluster factor: 0 ≤ CF ≤ 1, closed cluster, paper fixture
-- ───────────────────────────────────────────────

theorem cfDen_pos (m x : Nat) : 0 < cfDen m x := by
  unfold cfDen
  split <;> omega

/-- `CF ≤ 1`: the numerator never exceeds the denominator. -/
theorem cf_le_one (m x : Nat) : cfNum m x ≤ cfDen m x := by
  unfold cfNum cfDen
  split <;> omega

/-- A cluster with intra-edges and no external edge has `CF = 1`. -/
theorem cf_closed (m : Nat) (h : m ≠ 0) : cfNum m 0 = cfDen m 0 := by
  unfold cfNum cfDen
  split <;> omega

/-- Paper Fig. 2, subsystem 2: one intra-edge and three inter-edges give
    `CF = 2·1 / (2·1 + 3) = 2/5 = 0.4`. -/
theorem cf_paper_fixture : cfNum 1 3 = 2 ∧ cfDen 1 3 = 5 := by
  decide

-- ───────────────────────────────────────────────
-- MQ: 0 ≤ MQ ≤ k
-- ───────────────────────────────────────────────

theorem mqDen_pos (g : MDG) (a : Assign) : ∀ cs : List Nat, 0 < mqDen g a cs
  | [] => Nat.zero_lt_one
  | c :: cs => by
      simp only [mqDen]
      exact Nat.mul_pos (cfDen_pos _ _) (mqDen_pos g a cs)

/-- Adding a fraction `A/D ≤ 1` to a fraction `N/d ≤ k` gives at most `k + 1`,
    in cross-multiplied form. -/
theorem add_frac_le {A D N d k : Nat} (hA : A ≤ D) (hN : N ≤ k * d) :
    A * d + N * D ≤ (k + 1) * (D * d) := by
  have h1 : A * d ≤ D * d := Nat.mul_le_mul_right d hA
  have h2 : N * D ≤ k * d * D := Nat.mul_le_mul_right D hN
  have h3 : k * d * D = k * (D * d) := by
    rw [Nat.mul_assoc, Nat.mul_comm d D]
  rw [h3] at h2
  calc A * d + N * D ≤ D * d + k * (D * d) := Nat.add_le_add h1 h2
    _ = k * (D * d) + D * d := Nat.add_comm _ _
    _ = (k + 1) * (D * d) := by rw [Nat.add_mul, Nat.one_mul]

/-- `MQ ≤ k`: `mqNum ≤ k · mqDen` for a list of `k` clusters. -/
theorem mq_le_k (g : MDG) (a : Assign) :
    ∀ cs : List Nat, mqNum g a cs ≤ cs.length * mqDen g a cs
  | [] => by simp [mqNum, mqDen]
  | c :: cs => by
      have ih := mq_le_k g a cs
      simp only [mqNum, mqDen, List.length_cons]
      exact add_frac_le (cf_le_one _ _) ih

-- ───────────────────────────────────────────────
-- Intra-edge fraction ≤ 1 and the all-singletons case
-- ───────────────────────────────────────────────

theorem weightWhere_le_total (p : Edge → Bool) :
    ∀ g : MDG, weightWhere p g ≤ total g
  | [] => Nat.le_refl 0
  | e :: es => by
      have ih := weightWhere_le_total p es
      simp only [weightWhere, total]
      split <;> omega

/-- The intra-edge weight never exceeds the total weight, so the intra-edge
    fraction lies in `[0, 1]`. -/
theorem intra_le_total (g : MDG) (a : Assign) : intra g a ≤ total g :=
  weightWhere_le_total _ g

theorem weightWhere_eq_zero_of_false {p : Edge → Bool} :
    ∀ g : MDG, (∀ e, e ∈ g → p e = false) → weightWhere p g = 0
  | [] => fun _ => rfl
  | e :: es => fun h => by
      have he : p e = false := h e (List.Mem.head _)
      have hes : ∀ e', e' ∈ es → p e' = false := fun e' m => h e' (List.Mem.tail _ m)
      simp [weightWhere, he, weightWhere_eq_zero_of_false es hes]

/-- No edge inside any cluster (every node its own cluster, no self-loops) ⇒
    every `µ_c = 0`. -/
theorem mu_eq_zero_of_noIntra (g : MDG) (a : Assign)
    (h : ∀ e, e ∈ g → a e.u ≠ a e.v) (c : Nat) : mu g a c = 0 := by
  unfold mu
  apply weightWhere_eq_zero_of_false
  intro e he
  unfold inC
  by_cases hu : a e.u = c
  · have hv : ¬ (a e.v = c) := fun hv => h e he (hu.trans hv.symm)
    simp [hu, hv]
  · simp [hu]

/-- No intra-edge ⇒ `MQ = 0` (the "singleton partition" scenario). -/
theorem mqNum_eq_zero_of_noIntra (g : MDG) (a : Assign)
    (h : ∀ e, e ∈ g → a e.u ≠ a e.v) :
    ∀ cs : List Nat, mqNum g a cs = 0
  | [] => rfl
  | c :: cs => by
      have ih := mqNum_eq_zero_of_noIntra g a h cs
      have hm : mu g a c = 0 := mu_eq_zero_of_noIntra g a h c
      simp [mqNum, hm, ih, cfNum]

-- ───────────────────────────────────────────────
-- Locality of a single move: only the source and destination clusters change
-- ───────────────────────────────────────────────

/-- Move node `x` to cluster `b`. -/
def moveTo (a : Assign) (x b : Nat) : Assign :=
  fun n => if n = x then b else a n

theorem inC_moveTo {a : Assign} {x b c : Nat} (hc : c ≠ a x) (hb : c ≠ b) (n : Nat) :
    inC (moveTo a x b) c n = inC a c n := by
  unfold inC moveTo
  by_cases h : n = x
  · subst h
    have h1 : ¬ (b = c) := fun e => hb e.symm
    have h2 : ¬ (a n = c) := fun e => hc e.symm
    simp [h1, h2]
  · simp [h]

theorem weightWhere_congr {p q : Edge → Bool} (h : ∀ e, p e = q e) :
    ∀ g : MDG, weightWhere p g = weightWhere q g
  | [] => rfl
  | e :: es => by
      simp only [weightWhere, h e, weightWhere_congr h es]

/-- `µ_c` is unchanged by a move whose source and destination are not `c`. -/
theorem mu_moveTo (g : MDG) (a : Assign) {x b c : Nat} (hc : c ≠ a x) (hb : c ≠ b) :
    mu g (moveTo a x b) c = mu g a c := by
  unfold mu
  apply weightWhere_congr
  intro e
  simp only [inC_moveTo hc hb]

/-- `ext_c` is unchanged by a move whose source and destination are not `c`. -/
theorem ext_moveTo (g : MDG) (a : Assign) {x b c : Nat} (hc : c ≠ a x) (hb : c ≠ b) :
    ext g (moveTo a x b) c = ext g a c := by
  unfold ext
  apply weightWhere_congr
  intro e
  simp only [inC_moveTo hc hb]

/-- The cluster factor of every cluster other than the move's source and
    destination is unchanged: the paper's O(1) incremental MQ update only has
    to recompute two cluster factors. -/
theorem cf_moveTo (g : MDG) (a : Assign) {x b c : Nat} (hc : c ≠ a x) (hb : c ≠ b) :
    cfNum (mu g (moveTo a x b) c) (ext g (moveTo a x b) c)
        = cfNum (mu g a c) (ext g a c)
    ∧ cfDen (mu g (moveTo a x b) c) (ext g (moveTo a x b) c)
        = cfDen (mu g a c) (ext g a c) := by
  rw [mu_moveTo g a hc hb, ext_moveTo g a hc hb]
  exact ⟨rfl, rfl⟩

-- ───────────────────────────────────────────────
-- Fixture: two triangles joined by one edge, partitioned into the triangles
-- ───────────────────────────────────────────────

/-- Nodes 0,1,2 form a triangle, nodes 3,4,5 form a triangle, and edge 2–3
    joins them; unit weights. -/
def twoTriangles : MDG :=
  [ ⟨0, 1, 1⟩, ⟨1, 2, 1⟩, ⟨0, 2, 1⟩
  , ⟨3, 4, 1⟩, ⟨4, 5, 1⟩, ⟨3, 5, 1⟩
  , ⟨2, 3, 1⟩ ]

/-- The intended partition: cluster = node / 3. -/
def byTriangle : Assign := fun n => n / 3

/-- Each triangle has `µ = 3`, `ext = 1`, so `CF = 6/7` and
    `MQ = 6/7 + 6/7 = 84/49` in the cross-multiplied representation. -/
theorem twoTriangles_mq :
    mqNum twoTriangles byTriangle [0, 1] = 84
    ∧ mqDen twoTriangles byTriangle [0, 1] = 49 := by
  decide

/-- Six of the seven unit edges are intra-edges: intra-edge fraction `6/7`. -/
theorem twoTriangles_intra : intra twoTriangles byTriangle = 6 ∧ total twoTriangles = 7 := by
  decide

end GraphosFormalization.Bunch
