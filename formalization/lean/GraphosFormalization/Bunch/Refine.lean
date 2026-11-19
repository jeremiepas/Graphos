/-
  bunch-real-projects D3 — Lean arm of the global refinement pass
  (`refinePartition`, `src/Graphos/Domain/Community/Bunch.hs`).

  The refinement moves one file at a time to a neighbouring module and keeps a
  move only when it raises MQ. It evaluates the move on the two clusters it
  touches (source and destination); `cf_moveTo` (`Bunch/MQ.lean`) says no
  other cluster factor changes. This file closes the argument:

  * `moveTo_covers`, `moveTo_other`      — a move keeps a partition: every node
                                           is still in a listed cluster, and
                                           only the moved node changes cluster.
  * `Accepted`                           — the move's exact gain over the two
                                           touched clusters is positive (the
                                           refinement's `gain > 1e-12` test,
                                           without rounding).
  * `accepted_increases_mq`              — an accepted move strictly increases
                                           the exact MQ of the whole partition
                                           (cross-multiplied, as in `MQ.lean`).
  * `misplaced_move_accepted`,
    `misplaced_move_restores`            — fixture: node 2 of `twoTriangles`
                                           placed in the wrong triangle; moving
                                           it home is accepted and gives back
                                           `MQ = 84/49`.

  Because MQ is bounded (`mq_le_k`) and every accepted move strictly raises
  it, a pass sequence cannot cycle; the Haskell pass cap (50) is a safety net.

  Exact, division-free, core only (no `Mathlib`), like `Bunch/MQ.lean`.
-/
import GraphosFormalization.Bunch.MQ

namespace GraphosFormalization.Bunch

-- ───────────────────────────────────────────────
-- A move keeps a partition
-- ───────────────────────────────────────────────

/-- Every node of the moved assignment lies in a listed cluster, provided the
    old assignment did and the destination is listed. -/
theorem moveTo_covers {a : Assign} {x b : Nat} {cs : List Nat}
    (ha : ∀ n, a n ∈ cs) (hb : b ∈ cs) : ∀ n, moveTo a x b n ∈ cs := by
  intro n
  by_cases h : n = x
  · simp only [moveTo, h, if_true]; exact hb
  · simp only [moveTo, h, if_false]; exact ha n

/-- A move changes the cluster of the moved node only. -/
theorem moveTo_other (a : Assign) {x n : Nat} (b : Nat) (h : n ≠ x) :
    moveTo a x b n = a n := by
  simp only [moveTo, h, if_false]

/-- The moved node is in its destination. -/
theorem moveTo_self (a : Assign) (x b : Nat) : moveTo a x b x = b := by
  simp only [moveTo, if_true]

-- ───────────────────────────────────────────────
-- Fraction order under a common summand (cross-multiplied)
-- ───────────────────────────────────────────────

/-- `p/q + r₁/s₁ < p/q + r₂/s₂` whenever `r₁/s₁ < r₂/s₂` and `q > 0`, in the
    cross-multiplied form of `mqNum` / `mqDen`:
    `(p·s₁ + r₁·q)/(q·s₁)` against `(p·s₂ + r₂·q)/(q·s₂)`. -/
theorem fracLt_addLeft {p q r₁ s₁ r₂ s₂ : Nat} (hq : 0 < q)
    (h : r₁ * s₂ < r₂ * s₁) :
    (p * s₁ + r₁ * q) * (q * s₂) < (p * s₂ + r₂ * q) * (q * s₁) := by
  have h1 : r₁ * s₂ * (q * q) < r₂ * s₁ * (q * q) :=
    Nat.mul_lt_mul_of_pos_right h (Nat.mul_pos hq hq)
  have eL : (p * s₁ + r₁ * q) * (q * s₂) = p * q * s₁ * s₂ + r₁ * s₂ * (q * q) := by
    simp only [Nat.add_mul, Nat.mul_add, Nat.mul_assoc, Nat.mul_comm, Nat.mul_left_comm]
  have eR : (p * s₂ + r₂ * q) * (q * s₁) = p * q * s₁ * s₂ + r₂ * s₁ * (q * q) := by
    simp only [Nat.add_mul, Nat.mul_add, Nat.mul_assoc, Nat.mul_comm, Nat.mul_left_comm]
  rw [eL, eR]
  exact Nat.add_lt_add_left h1 _

-- ───────────────────────────────────────────────
-- An accepted move strictly increases MQ
-- ───────────────────────────────────────────────

/-- Prepending clusters the move does not touch preserves a strict MQ
    increase: their cluster factors are unchanged (`cf_moveTo`). -/
theorem mq_lt_of_prefix (g : MDG) (a : Assign) (x b : Nat) (L : List Nat)
    (hL : mqNum g a L * mqDen g (moveTo a x b) L
            < mqNum g (moveTo a x b) L * mqDen g a L) :
    ∀ pre : List Nat, (∀ c ∈ pre, c ≠ a x ∧ c ≠ b) →
      mqNum g a (pre ++ L) * mqDen g (moveTo a x b) (pre ++ L)
        < mqNum g (moveTo a x b) (pre ++ L) * mqDen g a (pre ++ L)
  | [], _ => by simpa using hL
  | c :: cs, h => by
      have hcb := h c (List.mem_cons_self c cs)
      have ih := mq_lt_of_prefix g a x b L hL cs
        (fun d hd => h d (List.mem_cons_of_mem c hd))
      have hcf := cf_moveTo g a hcb.1 hcb.2
      simp only [List.cons_append, mqNum, mqDen, hcf.1, hcf.2]
      exact fracLt_addLeft (cfDen_pos _ _) ih

/-- The refinement's acceptance test, exactly: moving `x` to `b` raises the
    sum of the cluster factors of the source `a x` and the destination `b`. -/
def Accepted (g : MDG) (a : Assign) (x b : Nat) : Prop :=
  mqNum g a [a x, b] * mqDen g (moveTo a x b) [a x, b]
    < mqNum g (moveTo a x b) [a x, b] * mqDen g a [a x, b]

/-- **An accepted move strictly increases MQ.** For any listing of the other
    clusters, the exact MQ of the moved partition is larger. -/
theorem accepted_increases_mq (g : MDG) (a : Assign) (x b : Nat)
    (hacc : Accepted g a x b) (rest : List Nat)
    (hrest : ∀ c ∈ rest, c ≠ a x ∧ c ≠ b) :
    mqNum g a (rest ++ [a x, b]) * mqDen g (moveTo a x b) (rest ++ [a x, b])
      < mqNum g (moveTo a x b) (rest ++ [a x, b]) * mqDen g a (rest ++ [a x, b]) :=
  mq_lt_of_prefix g a x b [a x, b] hacc rest hrest

-- ───────────────────────────────────────────────
-- Fixture: a misplaced node is pulled back into its triangle
-- ───────────────────────────────────────────────

/-- `byTriangle` with node 2 put in the second triangle's cluster. -/
def misplaced : Assign := fun n => if n = 2 then 1 else n / 3

/-- Moving node 2 back to cluster 0 is accepted. -/
theorem misplaced_move_accepted : Accepted twoTriangles misplaced 2 0 := by
  unfold Accepted
  decide

/-- After the move the partition is the two triangles again, `MQ = 84/49`
    (before it: `{0,1}` has `µ = 1, ext = 2` and `{2,3,4,5}` has `µ = 4,
    ext = 2`, so `MQ = 2/4 + 8/10 = 52/40`). -/
theorem misplaced_move_restores :
    mqNum twoTriangles (moveTo misplaced 2 0) [0, 1] = 84
    ∧ mqDen twoTriangles (moveTo misplaced 2 0) [0, 1] = 49 := by
  decide

/-- The move increases MQ over the full cluster list `[1, 0]` (no other
    cluster). -/
theorem misplaced_move_increases :
    mqNum twoTriangles misplaced [1, 0] * mqDen twoTriangles (moveTo misplaced 2 0) [1, 0]
      < mqNum twoTriangles (moveTo misplaced 2 0) [1, 0] * mqDen twoTriangles misplaced [1, 0] :=
  accepted_increases_mq twoTriangles misplaced 2 0 misplaced_move_accepted []
    (fun _ h => absurd h (List.not_mem_nil _))

end GraphosFormalization.Bunch
