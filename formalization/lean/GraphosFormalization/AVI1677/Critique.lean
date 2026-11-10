/-
  AVI-1677: Lean 4 critique predicates for the Graphos code-review path.

  This module mirrors the numeric arm of `Graphos.Domain.Analysis.surpriseScore`
  (`src/Graphos/Domain/Analysis.hs`).  The Haskell side keeps reason strings and
  candidate ordering; Lean owns only the crisp numeric predicate, per
  AVI-1677-D6.

  Toolchain: leanprover/lean4:v4.14.0 (formalization companion).
-/

namespace GraphosFormalization.AVI1677.Critique

/-- Confidence bucket score, matching the Haskell `confBonus`:
    c < 0.3  -> 3, c < 0.8 -> 2, otherwise -> 1.
    `confX10` is confidence multiplied by ten (0..10). -/
def confBonus (confX10 : Nat) : Nat :=
  if confX10 < 3 then 3
  else if confX10 < 8 then 2
  else 1

/-- Cross-file-type bonus (2) when the two endpoints differ in file category. -/
def crossFiletypeBonus (crossFiletype : Nat) : Nat :=
  if crossFiletype = 1 then 2 else 0

/-- Cross-community bonus (1) when the endpoints live in different communities. -/
def crossCommunityBonus (crossComm : Nat) : Nat :=
  if crossComm = 1 then 1 else 0

/-- Semantic bonus for an inferred edge: round (confBonus * 1.5).
    Haskell uses `round @Double`, which is round-half-to-even; the three
    possible inputs give 4, 3, and 2 respectively. -/
def semBonus (inferred : Nat) (cb : Nat) : Nat :=
  if inferred = 1 then
    if cb = 3 then 4 else if cb = 2 then 3 else 2
  else 0

/-- Numeric surprise score, matching the Lean-exported ABI
    `graphos_surprise_score`. -/
def surpriseScore (confX10 : Nat) (crossFiletype : Nat) (crossComm : Nat) (inferred : Nat) : Nat :=
  let cb := confBonus confX10
  cb + crossFiletypeBonus crossFiletype + crossCommunityBonus crossComm + semBonus inferred cb

/-- ABI version reported by `graphos_lean_critique_abi_version`. -/
def abiVersion : Nat := 0

-- Theorems

theorem confBonus_lt3 (x : Nat) (h : x < 3) : confBonus x = 3 := by
  unfold confBonus
  simp [h]

theorem confBonus_mid (x : Nat) (h1 : 3 ≤ x) (h2 : x < 8) : confBonus x = 2 := by
  unfold confBonus
  have hx : ¬ (x < 3) := by omega
  simp [hx, h2]

theorem confBonus_high (x : Nat) (h : 8 ≤ x) : confBonus x = 1 := by
  unfold confBonus
  have hx : ¬ (x < 3) := by omega
  have hy : ¬ (x < 8) := by omega
  simp [hx, hy]

theorem crossFiletypeBonus_nonneg (x : Nat) : 0 ≤ crossFiletypeBonus x := by
  unfold crossFiletypeBonus
  by_cases h : x = 1
  · simp [h]
  · simp [h]

theorem crossCommunityBonus_nonneg (x : Nat) : 0 ≤ crossCommunityBonus x := by
  unfold crossCommunityBonus
  by_cases h : x = 1
  · simp [h]
  · simp [h]

theorem semBonus_nonneg (inferred cb : Nat) : 0 ≤ semBonus inferred cb := by
  unfold semBonus
  by_cases h : inferred = 1
  · by_cases hcb : cb = 3
    · simp [h, hcb]
    · by_cases hcb2 : cb = 2
      · simp [h, hcb, hcb2]
      · simp [h, hcb, hcb2]
  · simp [h]

theorem surpriseScore_total (confX10 crossFiletype crossComm inferred : Nat) :
    ∃ n : Nat, surpriseScore confX10 crossFiletype crossComm inferred = n := by
  exact ⟨surpriseScore confX10 crossFiletype crossComm inferred, rfl⟩

-- Pinned fixture parity (score arm only).
theorem fixture_conf_low_cross : surpriseScore 0 1 1 0 = 6 := by
  native_decide

theorem fixture_inferred : surpriseScore 0 1 1 1 = 10 := by
  native_decide

theorem fixture_high : surpriseScore 9 0 0 0 = 1 := by
  native_decide

theorem abiVersion_zero : abiVersion = 0 := by
  rfl

end GraphosFormalization.AVI1677.Critique
