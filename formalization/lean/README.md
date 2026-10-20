# Graphos Lean formalization (`formalization/lean/`)

Lean 4 **verification companion** for the Graphos context graph. This package is
built independently of the Haskell project: it is **not** a cabal component, has
**no** FFI, and no runtime coupling with the `graphos` executable.

- Requirements: [`docs/math-requirements/AVI-616-consolidated-lean-formalization-requirements.md`](../../docs/math-requirements/AVI-616-consolidated-lean-formalization-requirements.md)
- Board scope: Phase 1 + 2 + 3 · CI posture **report-only** · maintenance owner **R&D cell**
- Toolchain: pinned in [`lean-toolchain`](./lean-toolchain) (`leanprover/lean4:v4.14.0`); `elan`/`lake` read it automatically.

## Layout

| Path | Purpose |
| --- | --- |
| `lakefile.toml` | Lake package `GraphosFormalization`, no dependencies |
| `lean-toolchain` | Pinned Lean 4 toolchain |
| `GraphosFormalization.lean` | Root module (library entry point) |
| `GraphosFormalization/Basic.lean` | Package skeleton / build smoke check |
| `GraphosFormalization/Phase1/Modularity.lean` | Phase 1 modularity `Q`, INV-Q0: `modularity_edgesAbsent_zero` (AVI-618 §2) |
| `GraphosFormalization/Phase1/RangeBound.lean` | Phase 1 exact INV-Q1 range bound: `modularity_range_bound` / `modularity_bounded`, plus the tight `K_{2,2}` fixture (AVI-618 §3) |
| `GraphosFormalization/Phase1/PartitionOnly.lean` | Phase 1 INV-Q2 partition-only invariance: `modularity_partitionOnly`, member-list corollary and exact-layer key-order invariance (AVI-618 §2) |
| `GraphosFormalization/Phase1/LocalMove.lean` | Phase 1 INV-Q3 local-moving monotonicity: `modularity_localMoveNonDecreasing` (+ strict form), per-move `ΔQnum` identity, and exact witnesses for the two shipped-gate defects (AVI-618 §2, §5 fixture 4) |
| `GraphosFormalization/Phase1/Determinism.lean` | Phase 1 INV-Q4 deterministic fold order: `modularity_deterministic`, insertion-order independence of the `Map.elems` fold, canonicalisation stability, and fixture 7 bit-equality (AVI-618 §2, §5 fixture 7) |
| `GraphosFormalization/Phase1/FixturesExact.lean` | Phase 1 INV-Q5 fixture exactness: `modularity_fixturesExact` — single block ⇒ `Qnum = 0`, all-singletons ⇒ `Qnum = −Σ_v k_v² < 0` (AVI-618 §2, §5 fixtures 1 & 2) |
| `GraphosFormalization/Ext/Basic.lean` | Phase 2 objects: `Extraction`, `mergeExtractions`, `emptyExtraction`, `foldMerge`, consistency, `ViewEmbedding` |
| `GraphosFormalization/Ext/Colimit.lean` | Phase 2 universal property: `mergedExtraction_isColimit` (AVI-620 §2) |
| `GraphosFormalization/Ext/Strict.lean` | Phase 2 consistent case: strict colimit + fold order-independence, `mergedExtraction_strictColimit_orderIndependent` (AVI-620 §3.1) |

## Build

```bash
cd formalization/lean
lake build
```

`elan` downloads the pinned toolchain on first run. `.lake/` is a build directory
and is gitignored.

## CI posture

`.github/workflows/lean.yml` runs `lake build` with `continue-on-error: true`.
The job surfaces results on the PR but **never blocks a merge** — a failing Lean
build is a signal to the R&D cell, not a release gate.

## Status

Phase 1 modularity layer started; Phase 2 colimit layer in progress. Both are
authored under the requirements-layer authorship decomposition (AVI-948) with
per-module requirement citations.

### Phase 1 — auditable modularity `Q` (AVI-618)

| Artifact | Lean name | State |
| --- | --- | --- |
| §2 INV-Q0 — no-divide-by-zero / edges absent | `modularity_edgesAbsent_zero` | **proved** (`Phase1/Modularity.lean`), incl. 4-isolated-node fixture |
| §2 INV-Q1 — range bound | `modularity_range_bound` (alias `modularity_bounded`) | **proved** (`Phase1/RangeBound.lean`), exact division-free form `-2m² ≤ Qnum ≤ 4m²`; `K_{2,2}` bipartition attains `-1/2` exactly |
| §2 INV-Q2 — partition-only | `modularity_partitionOnly` | **proved** (`Phase1/PartitionOnly.lean`): member-list invariance of `modularity`, plus exact-layer block-order (`Qnum`) invariance |
| §2 INV-Q3 — local move non-decreasing | `modularity_localMoveNonDecreasing` (+ `_strict`) | **proved** (`Phase1/LocalMove.lean`) for the corrected gain rule `ΔQ > 0`; see the caveat below |
| §2 INV-Q4 — deterministic summation | `modularity_deterministic` | **proved** (`Phase1/Determinism.lean`): fold order is the unique ascending-`CommunityId` enumeration of the map's domain, so `Q` is bit-identical across calls and across insertion orders; see the caveat below |
| §2 INV-Q5 — fixtures exact | `modularity_fixturesExact` | **proved** (`Phase1/FixturesExact.lean`) on the exact layer: single block ⇒ `Qnum = 0`; all-singletons ⇒ `Qnum = −Σ_v k_v² < 0`; §5 triangle gives `Q = −1/3` exactly |

`Phase1/Modularity.lean` also carries the shared objects those theorems build
on (`SupportGraph`, `Partition`, `sigmaIn`, `sigmaTot`, `modularity`). `Q` is
computed in `Float` (no Mathlib/Rat dependency) and pins `m = 0` to `0.0`,
mirroring `Domain/Community.hs:151-167`.

`Phase1/LocalMove.lean` proves INV-Q3 at the exact layer: a move of node `i`
(`k = deg i`) from block `c` to block `d` changes `Qnum` by
`4m(e_d − e_c) + 2k(σ_tot(c) − σ_tot(d) − k)`, which is exactly
`2(gate(d) − gate(c))` for the code's own gain numerator
`gate = 2m·e − σ_tot·k` — provided the source block is scored **after** `i` is
vacated. A pass is a chain of accepted moves, hence `Q` never decreases, and
strictly increases if any move is accepted (AVI-618 §2, §5 fixture 4).

> **Caveat (AVI-1296).** The shipped `bestCommunityFor` gate does *not* vacate
> the source block, and `localMovingLoop` stores
> `σ_tot(c) − k_i + e_c` instead of `σ_tot(c) − k_i`. Both are proven unsound
> here on exact fixtures (`starK13_literal_move_decreases_q`,
> `codeSigmaTotUpdate_drifts`); the Haskell fix is a separate dev ticket. Until
> it lands, `Q (localMovesFrom g cm0) ≥ Q cm0` does not hold for the shipped
> code, so the AVI-1301 fixture cross-check must pin the *corrected* rule.

`Phase1/Determinism.lean` models `CommunityMap` as the finite function a
`Data.Map` denotes (a duplicate-free key list plus `block : CommunityId →
List V`), so the fold order is `keysInFoldOrder cm = mergeSort cm.keys (· ≤ ·)`.
It proves that this order is ascending, is a permutation of the domain, and is
*the same list* for any two enumerations of that domain
(`keysInFoldOrder_unique`) — hence `modularity_foldOrder_perm` (two maps with
different insertion order give bit-equal `Q`) and
`modularity_canonicalOrder_stable` (re-canonicalising never changes `Q`). The
acceptance property `∀ g cm. modularity g cm = modularity g cm` is
`modularity_deterministic`, plus fixture 7 in both its `Partition` and
`CommunityMap` forms.

> **Caveat (AVI-1297).** Lean's `Float` is the native binary64 payload, so `=`
> on `Float` *is* bit equality; the theorem deliberately uses `=` rather than
> `Float.beq` (`==`), which is not reflexive on `NaN`. Kernel Lean cannot
> evaluate `Float` arithmetic (`Float.add` is `opaque @[extern]` and `Float` has
> no `DecidableEq`), so the concrete non-associativity witness is stated on
> `Nat` (`fold_order_matters`); the `Float`-level determinism claim needs no
> computation. General `Float` order-invariance is *false* by design — the exact
> layer's order-invariance is `modularityNum_blocksPerm` in
> `Phase1/PartitionOnly.lean`.

`Phase1/RangeBound.lean` adds the **exact** layer INV-Q1 needs: core Lean has
no `Rat`/`Finset` and no `Float` order lemmas, so a general inequality over the
opaque `Float` expression is not provable without Mathlib. Following AVI-618 §6
("the Lean side is proven over ℚ/ℝ (exact)"), the bound is stated division-free
on the common denominator `(2m)²`:

```
Qnum(π) = Σ_c [2m·σ_in(c) − σ_tot(c)²] = (2m)²·Q(π)
-2m² ≤ Qnum(π) ≤ 4m²        (m ≥ 1)   ⇔   -1/2 ≤ Q(π) ≤ 1
```

`G_s` is represented there by its edge set `A` (one record per unordered pair,
`EdgeGraph`), matching `gEdges`/`csM` and the `neighbors` multi-edge collapse.
The Float rendering remains the implementation's rounded evaluation and is
covered by the AVI-1301 fixture cross-check.

`Phase1/FixturesExact.lean` proves INV-Q5 on that same exact layer: on the §5
fixtures, `Qnum = 0` for the single block containing all nodes
(`singleBlock_modularityNum`) and `Qnum = −Σ_v k_v²` for the all-singletons
partition (`singletons_modularityNum`), which is strictly negative as soon as
`m ≥ 1` (`singletons_modularityNum_neg`). The bundle is
`modularity_fixturesExact`. `k_v` is `deg v = σ_tot([v])`; the handshake check
`singletons_sum_deg` (`Σ_v k_v = 2m`) exercises it against
`sum_sigmaTot_eq_two_m`. On the §5 triangle (`m = 3`, all degrees `2`) both
fixture values are also computed concretely: `Qnum = 0` and
`Qnum = −12 = −(2²·3)`, i.e. `Q = −1/3`.

> **Caveat (AVI-1298).** As for INV-Q1, the claims are stated on the exact
> numerator `Qnum = (2m)²·Q` rather than on the `Float` rendering: kernel Lean
> cannot reduce `Float` arithmetic (opaque `Float.add`/`Float.div`, no
> `DecidableEq`, no order lemmas), and AVI-618 §6 pins the Lean side to exact
> ℚ/ℝ arithmetic. The `Float`/`Double` values on the pinned fixtures are the
> AVI-1301 cross-check's job (`|Q_lean − Q_hs| ≤ 2⁻²⁴`, exactly `0` on rational
> fixtures).

### Phase 2 — merged context graph as colimit (AVI-620)

| Artifact (AVI-620) | Lean name | State |
| --- | --- | --- |
| §2 colimit universal property | `mergedExtraction_isColimit` | **proved** (`Ext/Colimit.lean`) |
| §2 least-upper-bound form | `mergedExtraction_isColimit_iff` | **proved** |
| §3.1 strict colimit + order independence | `mergedExtraction_strictColimit_orderIndependent` | **proved** (`Ext/Strict.lean`) |
| §3.2 lax colimit / first-wins | `mergedExtraction_laxColimit_oldWins` | pending (AVI-1304) |
| §4 n-way fold complexity | `mergedExtraction_nwayFold_linear` | pending (AVI-1306) |

The colimit layer carries the consistency hypothesis explicitly
(`familyConsistent`), so it stays valid regardless of how the AVI-949
feasibility answers land: inconsistent families satisfy its premise only when
they genuinely agree on every shared key.

`Ext/Strict.lean` proves the §3.1 pair of claims as one named bundle. (a) is the
*binary* universal property: for consistent `A, B`, `M = mergeExtractions A B`
with the canonical legs `ι_A : A ↪ M`, `ι_B : B ↪ M` receives every cocone and
the mediator is unique (`mergedExtraction_strictColimit`; uniqueness again comes
from `Ext` being poset-enriched, so no extra compatibility hypothesis on the
cocone legs is needed — two view-embeddings into `X` already agree on shared
keys). (b) is order independence at both levels: `mergeExtractions A B =
mergeExtractions B A` (`Extraction.merge_comm`) and, for every pairwise-consistent
family, `foldl'` is invariant under permutation of the view list
(`foldlMerge_perm`) and agrees with the build-path `foldr`
(`foldlMerge_eq_foldr`). The intermediate lemmas are reusable:
`ExtView.union_comm` (left-biased `Map.union` commutes on consistent views),
`unionList_perm` (the n-way union is order-free on a consistent family),
`merge_assoc`/`merge_self`/`merge_empty_left`/`merge_empty_right`, and
`consistent_merge_left` (a `foldl'` accumulator stays consistent).

> **Scope caveat (AVI-1303).** The order-independence half is *conditional on
> consistency by design*: on a conflicting key the folds genuinely differ
> (`foldl'` makes the **last** view win, `foldr` the **first** — AVI-620 §0.4),
> which is exactly the lax case `mergedExtraction_laxColimit_oldWins`
> (AVI-1304). The AVI-949 consistency guarantee is therefore what makes the
> streaming and build lanes interchangeable; without it this theorem says
> nothing about the shipped folds. The concrete fixture at the end of
> `Ext/Strict.lean` evaluates both folds on a `Nat`-keyed view (`0 ↦ 10`,
> `1 ↦ 11`) to the same value, so the statements are not vacuous.
