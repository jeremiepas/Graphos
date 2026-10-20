/-
  Graphos formalization — Phase 1, INV-Q3: modularity does not decrease under one
  Leiden local-moving pass.

  Requirements:
  `docs/math-requirements/AVI-618-auditable-modularity-lean-formalization-requirements.md`
  §2 INV-Q3 (`theorem modularity_localMoveNonDecreasing`), §5 fixture 4.

  Pinned Haskell surface:
    * `localMovingPass`   — `src/Graphos/Domain/Community.hs:494`
    * `localMovingLoop`   — `src/Graphos/Domain/Community.hs:502` (σ_tot update 527-530)
    * `bestCommunityFor`  — `src/Graphos/Domain/Community.hs:551` (gain rule 558-566)
    * `localMovesFrom`    — `src/Graphos/Domain/Community.hs:374` (seeded single pass)

  ## What is proved

  At the exact (integer) layer `Qnum = (2m)²·Q` (`RangeBound.lean`,
  `modularityNum`), a local move of node `i` (degree `k`) from block `c` to
  block `d` changes the numerator by

  ```
    ΔQnum = 4m·(e_d − e_c) + 2k·(σ_tot(c) − σ_tot(d) − k)
  ```

  where `e_c` (resp. `e_d`) is the number of edges from `i` into `c` (resp. `d`),
  `σ_tot(c)` includes `i`, and `σ_tot(d)` does not (`moveDelta`). That is
  *exactly* twice the difference of the code's own gain numerators

  ```
    gateNum m B k e = 2m·e − B·k        (the numerator of `deltaQ`,
                                        Community.hs:558-561, over 2m²)
    moveDelta = 2·(gateNum m σ_tot(d) k e_d − gateNum m (σ_tot(c) − k) k e_c)
  ```

  i.e. `moveDelta = 0` exactly at the "stay put" reference: the source block is
  scored *after* `i` has been vacated (`σ_tot(c) ↦ σ_tot(c) − k`,
  `σ_in(c) ↦ σ_in(c) − 2e_c`). Under that corrected reference, "accept the
  argmax move iff its score is `> 0`" is *equivalent* to `ΔQnum > 0`, so every
  accepted move strictly increases `Q`. A pass is a chain of such moves, hence
  `Q` is non-decreasing across a pass and strictly increasing whenever at least
  one move is accepted — the INV-Q3 statement.

  ## Why the statement is about the corrected gate, not the literal one

  `bestCommunityFor` (`Community.hs:551-566`) compares the *isolation-relative*
  gain `deltaQ(d) > 0` but does **not** vacate `i` from its current community
  first (`sigmaTotMap` still contains `i` in `currentComm`), and
  `localMovingLoop` (`Community.hs:527-530`) does not maintain `σ_tot` at all:
  it stores `σ_tot(c) − k_i + e_c` for the source instead of `σ_tot(c) − k_i`.
  Both make the implementation's accept test strictly weaker than `ΔQ > 0`.
  `isolationGate_not_sound` and `codeSigmaTotUpdate_drifts` below are exact
  witnesses of the two defects, and `starK13_literal_move_decreases_q` shows the
  shipped `localMovesFrom` lowering `Q` on a concrete fixture (verified against
  the Haskell build: `Q` goes from `−1/18` to `−2/9`). The Haskell fix is tracked
  as a separate dev ticket; until it lands, the acceptance property
  `Q (localMovesFrom g cm0) ≥ Q cm0` does **not** hold for the shipped code.

  ## Modeling choices

  1. **Aggregate layer.** A move touches only two summands of `(Q)`, so the pass
     is modeled on the *block-aggregate list* `[(σ_in(c), σ_tot(c))]`, exactly
     the quantities `computeCommunityStats` recomputes and `localMovingLoop`
     patches. `qNumOf_blocksOf` ties that model back to `modularityNum` on a
     real partition, so nothing is lost: `Qnum` is a function of the partition.
  2. **Integer arithmetic.** As in `RangeBound.lean`, the claim is proven over
     exact integers (`Qnum`), not over the `Float` evaluation (AVI-618 §6). No
     `Rat`/`Mathlib`; the package stays dependency-free.
  3. **No claim across `mergeSmallCommunities`.** Merges may decrease `Q`; this
     module constrains itself to `localMovesFrom`, i.e. one local-moving pass
     (`Community.hs:374-377`), as AVI-618 §2 requires.
-/
import GraphosFormalization.Phase1.Modularity
import GraphosFormalization.Phase1.RangeBound

namespace GraphosFormalization

/-! ## §0. The aggregate layer: `(Q)` as a function of the block aggregates -/

/-- `(2m)²·Q(π)` seen through per-block aggregates `(σ_in(c), σ_tot(c))`. This is
the exact shape of `modularityNum` (`RangeBound.lean`), so no cast gymnastics are
needed to connect the two. -/
def qNumOf (m : Nat) (blocks : List (Nat × Nat)) : Int :=
  (blocks.map (fun p => ((2 * m * p.1 : Nat) : Int))).sum
    - (blocks.map (fun p => ((p.2 * p.2 : Nat) : Int))).sum

/-- Exact numerator of the change in `(Qnum)` when node `i` (degree `k`) moves
from block `c` to block `d`.

* `e_c` / `e_d` — edges from `i` into `c \ {i}` / into `d`;
* `A` — `σ_tot(c)` **including** `i`; `σ_tot(c \ {i}) = A - k`;
* `B` — `σ_tot(d)` (which excludes `i`). -/
def moveDelta (m k ec ed A B : Nat) : Int :=
  4 * (m : Int) * ((ed : Int) - (ec : Int))
    + 2 * (k : Int) * ((A : Int) - (B : Int) - (k : Int))

/-- Numerator of the code's `deltaQ` (`Community.hs:558-561`) scaled by the
positive factor `2m²`: `deltaQ(target) = sigmaIn/m − sigmaTot·k_i/(2m²)`, so
`sign (gateNum m sigmaTot k sigmaIn) = sign (deltaQ target)`. -/
def gateNum (m B k e : Nat) : Int :=
  2 * (m : Int) * (e : Int) - (B : Int) * (k : Int)

/-! ### §0.1 Core-Lean algebra helpers (no `ring`, no `Mathlib`) -/

theorem int_sq_sub_sq (x y : Int) : x * x - y * y = (x - y) * (x + y) := by
  simp only [Int.mul_add, Int.add_mul, Int.sub_mul, Int.mul_sub, Int.mul_comm,
    Int.mul_left_comm, Int.mul_assoc]
  omega

theorem int_mul_two (x : Int) : x * 2 = x + x := by omega

theorem int_two_mul (x : Int) : 2 * x = x + x := by omega

theorem int_mul_four (x : Int) : x * 4 = (x + x) + (x + x) := by omega

theorem int_four_mul (x : Int) : 4 * x = (x + x) + (x + x) := by omega

theorem int_two_two : (2 : Int) * 2 = 4 := by omega

theorem int_cast_two : ((2 : Nat) : Int) = 2 := rfl

/-- A `Nat`-valued list sum, cast to `Int`, is the sum of the cast summands. -/
theorem sum_map_cast {V : Type} (l : List V) (f : V → Nat) :
    (l.map (fun x => (f x : Int))).sum = ((l.map f).sum : Int) := by
  induction l with
  | nil => rfl
  | cons a t ih =>
      simp only [List.map_cons, List.sum_cons, Int.natCast_add, ih]

/-- `omega` sees only linear arithmetic, so distributivity and `x*2 = x+x` are
pushed in first; this is the "poor man's ring" used by this module. -/
macro "normarith" : tactic =>
  `(tactic|
    (simp only [Int.mul_add, Int.add_mul, Int.sub_mul, Int.mul_sub, Int.mul_comm,
       Int.mul_left_comm, Int.mul_assoc, Int.natCast_add, Int.natCast_mul,
       int_mul_two, int_two_mul, int_mul_four, int_four_mul, int_two_two,
       int_cast_two] <;>
     omega))

/-- **Per-move identity.** Applying the move to the two affected summands
changes `Qnum` by exactly `moveDelta`. The context `rest` is untouched, so the
lemma covers a move in a partition of any size (block order is irrelevant:
`Qnum` is a sum, see `PartitionOnly.modularityNum_blocksPerm`). -/
theorem qNumOf_pair_move (m k ec ed a A b B : Nat) (rest : List (Nat × Nat))
    (h1 : 2 * ec ≤ a) (h2 : k ≤ A) :
    qNumOf m ([(a - 2 * ec, A - k), (b + 2 * ed, B + k)] ++ rest)
      = qNumOf m ([(a, A), (b, B)] ++ rest) + moveDelta m k ec ed A B := by
  unfold qNumOf
  simp only [List.map_cons, List.map_nil, List.sum_cons, List.sum_nil, Nat.zero_add,
    List.cons_append, List.nil_append]
  simp only [Int.natCast_add, Int.natCast_mul]
  have h1' : ((a - 2 * ec : Nat) : Int) = (a : Int) - 2 * (ec : Int) := by omega
  have h2' : ((A - k : Nat) : Int) = (A : Int) - (k : Int) := by omega
  rw [h1', h2']
  unfold moveDelta
  normarith

/-- The code's gain numerator, compared on the **vacated** source block, IS the
per-move change in `Q`: `ΔQnum = 2·(gate(d) − gate(c vacated))`. -/
theorem moveDelta_eq_gate (m k ec ed A B : Nat) (h : k ≤ A) :
    moveDelta m k ec ed A B
      = 2 * (gateNum m B k ed - gateNum m (A - k) k ec) := by
  unfold moveDelta gateNum
  have hA : ((A - k : Nat) : Int) = (A : Int) - (k : Int) := by omega
  rw [hA]
  normarith

/-- **Acceptance ⇒ strict increase.** If the move's gain beats the (vacated)
stay-put reference — the corrected accept rule — then `Q` strictly increases. -/
theorem gate_accept_pos (m k ec ed A B : Nat) (h : k ≤ A)
    (hgate : gateNum m (A - k) k ec < gateNum m B k ed) :
    0 < moveDelta m k ec ed A B := by
  rw [moveDelta_eq_gate m k ec ed A B h]
  omega

/-! ## §1. A local-moving pass as a chain of accepted, exact moves -/

/-- A pass from aggregate state `p` to state `t` whose recorded `Qnum` change is
`s`. Every constructor step is an *accepted* move of the corrected rule (the
gate condition `hgate`), and its exact effect on `Qnum` is `moveDelta`. -/
inductive Pass (m : Nat) : List (Nat × Nat) → Int → List (Nat × Nat) → Prop
  | nil (s : List (Nat × Nat)) : Pass m s 0 s
  | step {p t : List (Nat × Nat)} {s : Int}
      (k ec ed a A b B : Nat) (rest : List (Nat × Nat))
      (h1 : 2 * ec ≤ a) (h2 : k ≤ A)
      (hgate : gateNum m (A - k) k ec < gateNum m B k ed)
      (ht : Pass m ([(a - 2 * ec, A - k), (b + 2 * ed, B + k)] ++ rest) s t) :
      Pass m ([(a, A), (b, B)] ++ rest) (moveDelta m k ec ed A B + s) t

/-- The recorded change of a pass is the sum of its per-move `moveDelta`s. -/
theorem pass_qNum {m : Nat} {p : List (Nat × Nat)} {s : Int} {t : List (Nat × Nat)}
    (h : Pass m p s t) : qNumOf m t = qNumOf m p + s := by
  induction h with
  | nil s => simp [qNumOf]
  | step k ec ed a A b B rest h1 h2 hgate ht ih =>
      rw [ih, qNumOf_pair_move m k ec ed a A b B rest h1 h2]
      omega

/-- Every step of a pass is an accepted move, so every step increases `Qnum`:
the recorded total change is non-negative. -/
theorem pass_nonneg {m : Nat} {p : List (Nat × Nat)} {s : Int} {t : List (Nat × Nat)}
    (h : Pass m p s t) : 0 ≤ s := by
  induction h with
  | nil s => omega
  | step k ec ed a A b B rest h1 h2 hgate ht ih =>
      have hpos : 0 < moveDelta m k ec ed A B := gate_accept_pos m k ec ed A B h2 hgate
      omega

/-- **INV-Q3, pass level.** A local-moving pass never decreases `Qnum`. -/
theorem pass_nondec {m : Nat} {p : List (Nat × Nat)} {s : Int} {t : List (Nat × Nat)}
    (h : Pass m p s t) : qNumOf m p ≤ qNumOf m t := by
  rw [pass_qNum h]
  have := pass_nonneg h
  omega

/-- **INV-Q3, strict form.** If the pass accepted at least one move
(`0 < s`), `Qnum` strictly increases. -/
theorem pass_strict {m : Nat} {p : List (Nat × Nat)} {s : Int} {t : List (Nat × Nat)}
    (h : Pass m p s t) (hs : 0 < s) : qNumOf m p < qNumOf m t := by
  rw [pass_qNum h]
  omega

/-- Strictness read directly off "the pass accepts ≥ 1 move": a pass with a
leading accepted move strictly increases `Qnum` (the rest of the pass is
non-negative by `pass_nonneg`). -/
theorem pass_strict_of_step {m : Nat} (k ec ed a A b B : Nat) (rest : List (Nat × Nat))
    {s : Int} {t : List (Nat × Nat)}
    (h1 : 2 * ec ≤ a) (h2 : k ≤ A)
    (hgate : gateNum m (A - k) k ec < gateNum m B k ed)
    (ht : Pass m ([(a - 2 * ec, A - k), (b + 2 * ed, B + k)] ++ rest) s t) :
    qNumOf m ([(a, A), (b, B)] ++ rest) < qNumOf m t := by
  have hstep : Pass m ([(a, A), (b, B)] ++ rest) (moveDelta m k ec ed A B + s) t :=
    @Pass.step m ([(a, A), (b, B)] ++ rest) t s
      k ec ed a A b B rest h1 h2 hgate ht
  have hpos : 0 < moveDelta m k ec ed A B := gate_accept_pos m k ec ed A B h2 hgate
  have hnn : 0 ≤ s := pass_nonneg ht
  exact pass_strict hstep (by omega)

/-! ## §2. Back to partitions: `theorem modularity_localMoveNonDecreasing` -/

namespace EdgeGraph

variable {V : Type} [DecidableEq V]

/-- The aggregate view of a partition: one `(σ_in, σ_tot)` pair per block. -/
def blocksOf (g : EdgeGraph V) (π : Partition V) : List (Nat × Nat) :=
  π.blocks.map (fun b => (g.sigmaIn b, g.sigmaTot b))

/-- The aggregate layer computes the same numerator as `modularityNum`
(`RangeBound.lean`), so INV-Q3 on `qNumOf` is INV-Q3 on `(Q)`. -/
theorem qNumOf_blocksOf (g : EdgeGraph V) (π : Partition V) :
    qNumOf g.m (blocksOf g π) = modularityNum g π := by
  unfold qNumOf blocksOf modularityNum
  rw [sum_map_cast (π.blocks.map (fun b => (g.sigmaIn b, g.sigmaTot b)))
        (fun p => 2 * g.m * p.1),
      sum_map_cast (π.blocks.map (fun b => (g.sigmaIn b, g.sigmaTot b)))
        (fun p => p.2 * p.2),
      List.map_map, List.map_map]
  rfl

/-- **INV-Q3 (AVI-618 §2) — `modularity_localMoveNonDecreasing`.**

For a simple undirected support graph `G_s` (`m = |E_s|`) and any starting
partition `π₀`, the assignment after one full local-moving pass — every accepted
move of the pass satisfying the corrected gain rule `ΔQ > 0`
(`gate_accept_pos`) — has `Q(π₁) ≥ Q(π₀)`.

The pass argument is the aggregate trace (`Pass`): a chain of accepted moves
starting at `π₀`'s block aggregates and ending at `π₁`'s. No claim is made across
`mergeSmallCommunities` (merges can lower `Q`; INV-Q3 constrains the local-move
step alone). -/
theorem modularity_localMoveNonDecreasing (g : EdgeGraph V) {π₀ π₁ : Partition V}
    {s : Int} (h : Pass g.m (blocksOf g π₀) s (blocksOf g π₁)) :
    modularityNum g π₀ ≤ modularityNum g π₁ := by
  rw [← qNumOf_blocksOf g π₀, ← qNumOf_blocksOf g π₁]
  exact pass_nondec h

/-- **INV-Q3, strict form.** If the pass accepted at least one move, `Q` strictly
increases. -/
theorem modularity_localMoveNonDecreasing_strict (g : EdgeGraph V) {π₀ π₁ : Partition V}
    {s : Int} (h : Pass g.m (blocksOf g π₀) s (blocksOf g π₁)) (hs : 0 < s) :
    modularityNum g π₀ < modularityNum g π₁ := by
  rw [← qNumOf_blocksOf g π₀, ← qNumOf_blocksOf g π₁]
  exact pass_strict h hs

/-! ## §3. Witness: an accepted move of the corrected rule strictly raises `Q`

`K₃` on `{0,1,2}` plus a pendant edge `1—3` (`m = 4`), starting from
`{{0,1,2}, {3}}`. Node `1` (`k = 3`) has `e_c = 2`, `e_d = 1`, so the vacated
source scores `2·4·2 − (7−3)·3 = 4` and the target scores `2·4·1 − 1·3 = 5`:
the move is accepted and `ΔQnum = 2·(5−4) = 2 > 0`. -/

/-- `K₃` on `{0,1,2}` plus the pendant edge `1—3`. -/
def k3Pendant : EdgeGraph (Fin 4) where
  vertices := [0, 1, 2, 3]
  nodup := by decide
  complete := by decide
  edges := [(0, 1), (0, 2), (1, 2), (1, 3)]
  edges_nodup := by decide
  edges_noLoop := by decide
  edges_mem := by decide
  edges_noRev := by decide

/-- The starting partition `{{0,1,2}, {3}}`. -/
def k3PendantStart : Partition (Fin 4) where
  blocks := [[0, 1, 2], [3]]

/-- The partition after node `1` moves to `{3}`, i.e. `{{0,2}, {1,3}}`. -/
def k3PendantMoved : Partition (Fin 4) where
  blocks := [[0, 2], [1, 3]]

theorem k3Pendant_m : k3Pendant.m = 4 := rfl

theorem k3Pendant_start_blocks :
    blocksOf k3Pendant k3PendantStart = [(6, 7), (0, 1)] := by decide

theorem k3Pendant_moved_blocks :
    blocksOf k3Pendant k3PendantMoved = [(2, 4), (2, 4)] := by decide

/-- The corrected gate accepts node `1`'s move (`4 < 5`), and the aggregate
trace is a valid one-step `Pass` with recorded change `2`. -/
theorem k3Pendant_move_pass_conc :
    Pass 4 [(6, 7), (0, 1)] 2 [(2, 4), (2, 4)] :=
  Pass.step (m := 4) (p := [(6, 7), (0, 1)])
    (t := ([(6 - 2 * 2, 7 - 3), (0 + 2 * 1, 1 + 3)] ++ [])) (s := 0)
    (k := 3) (ec := 2) (ed := 1) (a := 6) (A := 7) (b := 0) (B := 1) (rest := [])
    (by decide) (by decide) (by decide)
    (Pass.nil (m := 4) ([(6 - 2 * 2, 7 - 3), (0 + 2 * 1, 1 + 3)] ++ []))

/-- The same pass, expressed on the partition itself. -/
theorem k3Pendant_move_pass :
    Pass k3Pendant.m (blocksOf k3Pendant k3PendantStart) 2
      (blocksOf k3Pendant k3PendantMoved) := by
  rw [k3Pendant_m, k3Pendant_start_blocks, k3Pendant_moved_blocks]
  exact k3Pendant_move_pass_conc

/-- **INV-Q3 witness.** One accepted move of the corrected rule raises `Qnum`
by exactly `2` (i.e. `Q` by `2/(2m)² = 1/32`). -/
theorem k3Pendant_q_strict :
    modularityNum k3Pendant k3PendantStart < modularityNum k3Pendant k3PendantMoved :=
  modularity_localMoveNonDecreasing_strict k3Pendant k3Pendant_move_pass (by decide)

/-- The same witness, in the exact `(2m)²·Q` units the code's gain uses. -/
theorem k3Pendant_qnum_delta :
    modularityNum k3Pendant k3PendantMoved
      = modularityNum k3Pendant k3PendantStart + 2 := by decide

/-! ## §4. Two defects in the literal implementation's accept rule

The requirement's proof sketch assumes `bestScore > 0` (`Community.hs:566`) is
equivalent to `ΔQ > 0`. It is not, for two independent reasons. Both witnesses
below are exact, and both were confirmed against the built Haskell library
(`localMovesFrom` + `modularity`) on the same fixtures.

### §4.1 The gate is relative to isolation, not to the current partition

`bestCommunityFor` scores candidate communities with
`deltaQ(d) = e_d/m − γ·σ_tot(d)·k_i/(2m²)` and accepts iff `max_d deltaQ(d) > 0`,
*without* first removing `i` from its current community. With the true `σ_tot`,
`ΔQnum(d) = 2·(gate(d) − gate(c vacated))`, so `gate(d) > 0` can hold while
`gate(d) < gate(c vacated)` and the move *lowers* `Q`.

Star `K₁,` on `{0,1,2}` centred at `2` plus leaf `3` (`m = 3`), start
`{{0,1,2}, {3}}`: node `2` has `k = 3`, `e_c = 2`, `e_d = 1`, `σ_tot(c) = 5`,
`σ_tot(d) = 1`, so

* literal gate: `2·3·1 − 1·3 = 3 > 0` → the code **accepts**;
* true change: `ΔQnum = 4·3·(1−2) + 2·3·(5−1−3) = −6 < 0` → `Q` **decreases**
  (from `−2/36 = −1/18` to `−8/36 = −2/9`). -/

/-- `K₁,₃` on `{0,1,2}` centred at node `2`, plus the leaf edge `2—3`. -/
def starK13 : EdgeGraph (Fin 4) where
  vertices := [0, 1, 2, 3]
  nodup := by decide
  complete := by decide
  edges := [(0, 2), (1, 2), (2, 3)]
  edges_nodup := by decide
  edges_noLoop := by decide
  edges_mem := by decide
  edges_noRev := by decide

/-- Starting partition `{{0,1,2}, {3}}`. -/
def starK13Start : Partition (Fin 4) where
  blocks := [[0, 1, 2], [3]]

/-- Partition after the code moves the centre `2` into `{3}`: `{{0,1}, {2,3}}`. -/
def starK13After : Partition (Fin 4) where
  blocks := [[0, 1], [2, 3]]

theorem starK13_m : starK13.m = 3 := rfl

/-- **Defect 1 (exact).** The literal, isolation-relative gate is satisfied
(`3 > 0`) while the true change in `Qnum` is negative (`−6`). -/
theorem isolationGate_not_sound :
    0 < gateNum 3 1 3 1 ∧ moveDelta 3 3 2 1 5 1 < 0 := by decide

/-- The same defect, stated on `Q` itself: the literal rule's move lowers the
modularity of an actual partition. -/
theorem starK13_literal_move_decreases_q :
    modularityNum starK13 starK13After < modularityNum starK13 starK13Start := by
  decide

/-- The exact values, for the record: `Qnum(π₀) = −2`, `Qnum(π₁) = −8`. -/
theorem starK13_qnum_values :
    modularityNum starK13 starK13Start = -2 ∧
      modularityNum starK13 starK13After = -8 := by
  constructor <;> decide

/-! ### §4.2 The `σ_tot` map is not maintained by `localMovingLoop`

`localMovingLoop` (`Community.hs:527-530`) updates the source community's
`σ_tot` to `σ_tot(c) − k_i + e_c` instead of `σ_tot(c) − k_i` (and symmetrically
for the target), so the map drifts away from the true degree sum as soon as a
move happens. On the `K₃`+pendant fixture above, after node `1` (`k = 3`,
`e_c = 2`) leaves `{0,1,2}` (`σ_tot = 7`) the code stores `7 − 3 + 2 = 6` while
the true `σ_tot` of the remaining block `{0,2}` is `deg 0 + deg 2 = 2 + 2 = 4`,
so the very next decision is taken on a stale aggregate. -/

/-- The `σ_tot` update actually performed by `localMovingLoop`
(`Community.hs:528-529`), parameterised by the old total, the node degree and
the number of edges from the node into the community. -/
def codeSigmaTotUpdate (oldST ki eInto : Nat) : Nat := oldST - ki + eInto

/-- The exact `σ_tot` of the source community after the node leaves it. -/
def exactSigmaTotUpdate (oldST ki : Nat) : Nat := oldST - ki

/-- **Defect 2 (exact).** The code's update drifts from the true `σ_tot` on the
`K₃`+pendant witness (`7 − 3 + 2 = 6`, true value `7 − 3 = 4`). -/
theorem codeSigmaTotUpdate_drifts :
    codeSigmaTotUpdate 7 3 2 ≠ exactSigmaTotUpdate 7 3 := by decide

end EdgeGraph

end GraphosFormalization
