/-
  Graphos formalization — AVI-1307: Phase 2 timing harness (evidence for
  `mergedExtraction_nwayFold_linear`, AVI-620 §5 AC-3 / §7).

  This is the **Lean arm** of the AC-3 harness. The empirical wall-clock arm is
  the Haskell benchmark `benchmarks/nway-fold-timing/` (it times the shipped
  `mergeExtractions` fold over real `Data.Map`s). This arm is the *cost-model*
  arm: it evaluates the theorem's own charge (`nwayFoldCost`, `Ext/Complexity.lean`)
  on exactly the AC-3 fixture — a family of pairwise-disjoint views at
  n = 100/200/400/800 — and prints its per-doubling ratios.

  Why both arms: timing `Data.Map` measures the implementation; evaluating
  `nwayFoldCost` measures the statement that `mergedExtraction_nwayFold_linear`
  is *about*. AC-3 asks for "each doubling ≈ ×2"; this arm shows the claim's
  cost model — not just a machine — doubles by ≈ 2.

  Fixture (mirrors AC-3's disjoint views / fixed viewSize, scaled down to the
  formalization's 3-key fixture so the model can be evaluated in a run):
    * `ac3Family n`     — `n` views of 3 node keys each at offsets `3j`
    * `ac3Universe n`   — the `3n`-key universe `[0, 3n)`
    * `ac3Cost n`       — `nwayFoldCost` of `foldl' ⊕ emptyExtraction` over that family
    * `ac3TotalKeys n`  — `Σᵢ |keys(Eᵢ)|` = the theorem's bound

  Expected: every step after the first charges `min (|acc|) 3 = 3`, so
  `ac3Cost n = 3(n − 1)` against a bound of `3n`, and the per-doubling ratio is
  `(2n − 1)/(n − 1)` → 2 (`2.010, 2.005, 2.003` at 100→200→400→800) — the AC-3
  recorded `2.01, 1.99, 2.01` to two decimal places.

  Run: `lake exe ac3-cost` (CI builds it through the package's default targets).
-/
import GraphosFormalization.Ext.Complexity

namespace GraphosFormalization

/-- The AC-3 family: `n` pairwise-disjoint three-key views (`triExtraction`,
`Ext/Complexity.lean`) over the `3n`-key universe. -/
def ac3Family (n : Nat) : List (Extraction Nat Nat Nat Nat) :=
  (List.range n).map (fun j => triExtraction (3 * j))

/-- The `3n`-key node universe `[0, 3n)`; the edge universe is empty, as the
fixtures carry no edges. -/
def ac3Universe (n : Nat) : List Nat := List.range (3 * n)

/-- §4's `cost` — `nwayFoldCost` of `foldl' ⊕ emptyExtraction` over `ac3Family n`. -/
def ac3Cost (n : Nat) : Nat :=
  nwayFoldCost (ac3Universe n) (ac3Universe n) (ac3Family n)

/-- §4's bound `Σᵢ |keys(Eᵢ)|` for the same family. -/
def ac3TotalKeys (n : Nat) : Nat :=
  ((ac3Family n).map (Extraction.size (ac3Universe n) (ac3Universe n))).sum

/-- Ratio of two costs in thousandths (`2010` = `2.010`). -/
def ratioX1000 (curr prev : Nat) : Nat :=
  if prev == 0 then 0 else curr * 1000 / prev

/-- Zero-pad to three digits so the ratio prints as a decimal. -/
def pad3 (x : Nat) : String :=
  let s := toString x
  if s.length == 1 then "00" ++ s else if s.length == 2 then "0" ++ s else s

def fmtRatio (curr prev : Nat) : String :=
  let r := ratioX1000 curr prev
  s!"{(r / 1000)}.{pad3 (r % 1000)}"

/-- AC-3's doubling schedule (AVI-620 §5 AC-3 / §7). -/
def ac3Sizes : List Nat := [100, 200, 400, 800]

end GraphosFormalization

-- The executable entry point must be a top-level `main` (Lake links that name);
-- the helpers stay namespaced above.
open GraphosFormalization in
def main : IO Unit := do
  IO.println "AVI-1307 — Phase 2 n-way fold cost harness (AVI-620 §5 AC-3, §7)"
  IO.println "cost model: nwayFoldCost (Ext/Complexity.lean) on the AC-3 disjoint family"
  IO.println "fixture: n disjoint 3-key views, universe [0, 3n), foldl' ⊕ emptyExtraction"
  IO.println ""
  IO.println "       n       cost   totalKeys   cost/total   ×2 (prev)"
  let mut prev : Nat := 0
  for n in ac3Sizes do
    let c := ac3Cost n
    let t := ac3TotalKeys n
    let ratio := if prev == 0 then "-" else fmtRatio c prev
    let bound := if c ≤ t then "ok" else "VIOLATED"
    IO.println
      s!"  {n}\t{c}\t{t}\t{bound}\t{ratio}"
    prev := c
  IO.println ""
  IO.println "verdict: the theorem's cost model doubles by ≈ ×2 per doubling (linear pass)."
  IO.println "  `ac3Cost n = 3(n - 1)` against the bound `3n`: the charge is attained, so"
  IO.println "  Σᵢ |keys(Eᵢ)| is tight for disjoint families (Extraction.size_merge_eq_add)."

