-- Stage 0 feasibility spike for AVI-1677.
--
-- A tiny, dependency-free Lean 4 computation that mirrors the numeric arm
-- of `Graphos.Domain.Analysis.surpriseScore` (src/Graphos/Domain/Analysis.hs).
-- Only the score is computed here; the accompanying reason strings stay on
-- the Haskell side until the ABI is frozen in Stage 2.

/-- Confidence bucket score, matching the Haskell:
    c < 0.3  -> 3, c < 0.8 -> 2, otherwise -> 1.
    `confX10` is confidence multiplied by ten (0..10) so the ABI stays integer. -/
def confBonus (confX10 : UInt8) : UInt64 :=
  if confX10 < 3 then 3
  else if confX10 < 8 then 2
  else 1

/-- Cross-file-type bonus (2) when the two endpoints differ in file category. -/
def crossFiletypeBonus (crossFiletype : UInt8) : UInt64 :=
  if crossFiletype == 1 then 2 else 0

/-- Cross-community bonus (1) when the endpoints live in different communities. -/
def crossCommunityBonus (crossComm : UInt8) : UInt64 :=
  if crossComm == 1 then 1 else 0

/-- Semantic bonus for an inferred edge: round (confBonus * 1.5).
    Haskell uses `round @Double`, which is round-half-to-even; the three
    possible inputs give 4, 3, and 2 respectively. -/
def semBonus (inferred : UInt8) (cb : UInt64) : UInt64 :=
  if inferred == 1 then
    if cb == 3 then 4 else if cb == 2 then 3 else 2
  else 0

@[export graphos_surprise_score]
def surpriseScore (confX10 : UInt8) (crossFiletype : UInt8) (crossComm : UInt8) (inferred : UInt8) : UInt64 :=
  let cb := confBonus confX10
  cb + crossFiletypeBonus crossFiletype + crossCommunityBonus crossComm + semBonus inferred cb
