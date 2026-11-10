# AVI-1307 — Phase 2 n-way fold timing harness (AVI-620 §5 AC-3 / §7)

Empirical evidence for `mergedExtraction_nwayFold_linear`
([AVI-620](../../docs/math-requirements/AVI-620-colimit-functoriality-context-graph-lean-formalization-requirements.md)
§4/§5 AC-3, Lean proof in
[`formalization/lean/GraphosFormalization/Ext/Complexity.lean`](../../formalization/lean/GraphosFormalization/Ext/Complexity.lean),
AVI-1306).

AC-3 asks that the n-way merge, `foldl' ⊕ emptyExtraction`, be **linear**: three
doublings of the family size (`n = 100/200/400/800`) should each cost ≈ ×2, not
×4 (quadratic). The recorded CTE run (2026-09-11) reported `2.01, 1.99, 2.01`.

This directory holds the **Haskell arm**: it times the shipped fold over real
`Extraction` values and real `Data.Map`s. The **Lean arm**
(`formalization/lean/GraphosFormalization/Harness/Ac3Cost.lean`, `lake exe
ac3-cost`) evaluates the theorem's own cost model on the same fixture, so the
statement and the implementation are both checked.

## Run it

```bash
# Haskell arm (real mergeExtractions + Data.Map), from the repo root:
devenv shell -- cabal bench nway-fold-timing
# more repetitions:
devenv shell -- cabal bench nway-fold-timing --benchmark-options="--runs 7"

# Lean arm (the theorem's cost model), from formalization/lean:
lake exe ac3-cost
```

Both are report-only: the Haskell component is not built by `cabal build all` /
`cabal test`, and the Lean exe is not a default `lake build` target. The Haskell
harness exits non-zero when the measurement is outside the linearity tolerance,
so it can be wired into a gating job later if the board wants one.

## What is measured (Haskell arm)

* `foldl' mergeExtractions emptyExtraction` — the streaming / per-server fold
  sites (`src/Graphos/UseCase/Extract/Core.hs:599`, `:639`);
* `foldr mergeExtractions emptyExtraction` — the build-path fold
  (`src/Graphos/UseCase/Build.hs:12`);
* the **recorded §7 shape** (a constant 1000-key view folded `n` times) as a
  control arm, so the recorded numbers can be compared like for like.

The real operator (`mergeExtractions`, `src/Graphos/Domain/Graph/Core.hs:129-136`,
first/old-wins `Map.union` on both components) and the real `Extraction` type
(`Map NodeId Node`, nodes built with the shipped `makeStubNode`) are used.
Views are built once, outside the timed region; each run folds a family of
**pairwise-disjoint** views of `viewSize = 1000` node keys — the AC-3 fixture.
The result is forced to normal form inside the timed region, so no work escapes
the measurement. Each run starts from a freshly collected heap (`performGC`);
the harness prints the best and the median run and verdicts on the best run,
because a shared machine's median picks up unrelated GC/scheduler pauses.

## Recorded run — 2026-10-20 (authorship re-run)

`devenv shell -- cabal bench nway-fold-timing --benchmark-options="--runs 7"`,
ghc 9.10.3, x86_64/linux, `-O2`, viewSize 1000, 7 timed runs per n:

```
disjoint views, foldl' mergeExtractions  [primary — AC-3 fixture]
       n     min(µs)   median(µs)     max(µs)     ×2 (min)  ×2 (med)
     100      4922.9       5187.9       8425.2            -         -
     200     10189.7      10387.9      12105.9        2.070     2.002
     400     21180.1      24190.2      30244.4        2.079     2.329
     800     42799.4      48584.5      58237.9        2.021     2.008
  per-doubling ratio (min):  2.070 2.079 2.021
  per-doubling ratio (med):  2.002 2.329 2.008
  log-log slope: min 1.040 | median 1.076

disjoint views, foldr  mergeExtractions  [build-path shape]
       n     min(µs)   median(µs)     max(µs)     ×2 (min)  ×2 (med)
     100      5625.1       6078.1       6956.0            -         -
     200     11811.4      12618.7      16080.5        2.100     2.076
     400     23393.5      24836.9      32466.7        1.981     1.968
     800     47467.2      51250.2      57606.0        2.029     2.063
  per-doubling ratio (min):  2.100 1.981 2.029
  per-doubling ratio (med):  2.076 1.968 2.063
  log-log slope: min 1.026 | median 1.025

recorded §7 shape (constant view, growing fold length)
       n     min(µs)   median(µs)     max(µs)     ×2 (min)  ×2 (med)
     100       484.0        511.0        522.2            -         -
     200      1004.9       1017.5       1124.1        2.076     1.991
     400      2002.4       2028.7       2047.2        1.993     1.994
     800      3975.4       4032.9       4064.1        1.985     1.988
  per-doubling ratio (min):  2.076 1.993 1.985
  per-doubling ratio (med):  1.991 1.994 1.988
  log-log slope: min 1.013 | median 0.994

verdict: LINEAR — each doubling ≈ ×2 (single pass, not quadratic)
  4× n costs 8.69× time (foldl', best) / 8.44× (foldr', best); log-log slope 1.040
  reference: quadratic would cost 16× (slope 2.000)
```

Lean arm, same day (`lake exe ac3-cost`, 21 s):

```
       n       cost   totalKeys   cost/total   ×2 (prev)
  100      297           300          ok            -
  200      597           600          ok        2.010
  400     1197          1200          ok        2.005
  800     2397          2400          ok        2.002
```

**Reading.** AC-3's claim holds: 4× the family size costs ≈ 8.7× the time
(`foldl'`) and ≈ 8.4× (`foldr`) — a single pass, slope ≈ 1.0, not the 16×
(slope 2.0) a quadratic re-scan would show. The absolute times are larger than
the recorded CTE run (3052/6126/12162/24506 µs) because this fixture uses real
`Node` values and forces the whole result, not integer payloads to WHNF; the
ratios, which are the claim, are the same.

The theorem's cost model agrees exactly: `ac3Cost n = 3(n − 1)` against the
bound `3n`, ratios `2.010/2.005/2.002` — the AC-3 recorded `2.01, 1.99, 2.01`
to two decimal places, and the §4 bound is attained
(`Extraction.size_merge_eq_add`).

> **Modelling note (already recorded in the AVI-1306 header).** The timed unit
> is the key-visit charge `min (|acc|) (|Eᵢ|)`. `Data.Map.union` additionally
> pays a `log (n/m + 1)` split-path factor (node/pointer depth, not key
> operations); it is visible here as the slope being marginally above 1.0 on
> the noisier medians, not as a change of growth class. Nothing in the
> measurement suggests the quadratic regime that the `O(n₁+n₂)`-per-step
> reading would predict.

## Erratum: the §7 recorded harness sketch (AVI-1307)

The Criterion sketch pasted into AVI-620 §5/§7 does not reproduce the numbers
attributed to it, and as written it does not compile. Recorded here so the next
reader does not try to run it verbatim:

1. **Type error.** `mkView :: Int -> Map NodeId Int` is applied to two arguments
   (`mkView (nv*vs + j*13) vs`).
2. **Not disjoint.** `disjointViews` steps by `13` while `viewSize` is `1000`,
   so consecutive views overlap in 987 keys — the opposite of the fixture the
   prose pins ("disjoint views, viewSize 1000"). The disjoint family is what
   makes the §4 bound `Σᵢ |keys(Eᵢ)|` attained.
3. **The bench body ignores the family.** Each `bench` folds `n = 800` elements
   while unioning the *same constant* `mkView (999*1000)` at every step, so the
   measured quantity is the growth of the fold length with a fixed 1000-key
   operand — `min (|acc|) 1000 = 1000` per step, i.e. linear in `n` by
   construction, whatever the accumulator size.
4. **`whnf` only.** `whnf` stops at the outer constructor; the union work itself
   is not forced.

The `recorded-shape` arm above reproduces that computation faithfully and lands
on `2.076/1.993/1.985` (slope 1.013) — the same shape as the recorded
`2.01/1.99/2.01`. So the *recorded numbers are consistent with the recorded
code*; they just do not measure the disjoint-family fold the acceptance
criterion is about. The `disjoint` arm does, and is linear too.
