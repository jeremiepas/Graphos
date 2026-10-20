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
| `GraphosFormalization/Ext/Lax.lean` | Phase 2 conflicting case: no matching-family cocone + lax colimit with the first/old-wins non-invertible comparison, `mergedExtraction_laxColimit_oldWins` (AVI-620 §3.2) |
| `GraphosFormalization/Ext/Complexity.lean` | Phase 2 §4 n-way fold complexity: `mergedExtraction_nwayFold_linear` — the key-visit cost of `foldl' ⊕ emptyExtraction` is `≤ Σᵢ \|keys(Eᵢ)\|` (AVI-620 §4) |
| `GraphosFormalization/Harness/Ac3Cost.lean` | Phase 2 AC-3 cost harness: evaluates `nwayFoldCost` on the AC-3 disjoint family (n = 100/200/400/800) and prints its doubling ratios (AVI-620 §5 AC-3 / §7, AVI-1307) |
| `GraphosFormalization/Seam/SupportGraph.lean` | Phase 1/2 **seam**: merged `M` → `buildGraph` → `Graph` → `neighbors` → `G_s`, pinned so that `Q` only sees the support relation (AVI-616 §1.1, AVI-1308) |

## Build

```bash
cd formalization/lean
lake build
```

`elan` downloads the pinned toolchain on first run. `.lake/` is a build directory
and is gitignored.

The AC-3 timing harness is a separate executable (not a default target, so
`lake build` stays library-only):

```bash
lake build ac3-cost      # or: lake exe ac3-cost
```

On NixOS the elan-bundled `clang` cannot run, so the executable must be linked
with the system C compiler and the toolchain's bundled `libc++`/`libuv`:

```bash
T="$HOME/.elan/toolchains/leanprover--lean4---v4.14.0"
printf '#!/bin/sh\nexec cc -L%s/lib "$@"\n' "$T" > /tmp/leancc && chmod +x /tmp/leancc
LEAN_CC=/tmp/leancc lake build ac3-cost
```

The Haskell arm of the same harness lives in `benchmarks/nway-fold-timing/`
(`devenv shell -- cabal bench nway-fold-timing`).

## CI posture

`.github/workflows/lean.yml` runs `lake build` with `continue-on-error: true`.
The job surfaces results on the PR but **never blocks a merge** — a failing Lean
build is a signal to the R&D cell, not a release gate.

## Status

Phase 1 modularity layer started; Phase 2 colimit layer in progress; the Phase
1/2 **seam** between them is pinned ([`Seam/SupportGraph.lean`](./GraphosFormalization/Seam/SupportGraph.lean),
AVI-1308). All are authored under the requirements-layer authorship
decomposition (AVI-948) with per-module requirement citations.

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
| §3.2 lax colimit / first-wins | `mergedExtraction_laxColimit_oldWins` | **proved** (`Ext/Lax.lean`), incl. the AC-4 conflicting-`EdgeId` fixture |
| §4 n-way fold complexity | `mergedExtraction_nwayFold_linear` | **proved** (`Ext/Complexity.lean`), `cost ≤ Σᵢ \|keys(Eᵢ)\|` with constant `1` |
| §5 AC-3 empirical timing | `ac3-cost` harness (Lean arm) + `benchmarks/nway-fold-timing` (Haskell arm) | **recorded** (`Harness/Ac3Cost.lean`, AVI-1307): cost model `2.010/2.005/2.002` per doubling; shipped fold measured `2.062/2.045/2.087` (slope 1.046, not quadratic) |

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

> **Scope caveat (AVI-1303, corrected by AVI-1304).** The order-independence half
> is *conditional on consistency by design*, but not for the reason originally
> recorded here. On a conflicting key the merge genuinely loses information
> (`A ⊕ B ≠ B ⊕ A`), which is the lax case `mergedExtraction_laxColimit_oldWins`
> (AVI-1304, `Ext/Lax.lean`); what is *not* true is that the two fold sites
> differ on conflicts. Both shipped folds put the accumulator / list head in the
> **first** operand position, so under first/old-wins they all agree
> (`Ext/Lax.lean:foldrMerge_eq_foldlMerge_all`, unconditional). The AVI-949
> consistency guarantee is what makes the *result* order-independent
> (commutation + permutation invariance), not what makes `foldr` and `foldl'`
> agree. The §0.4 prose that claimed a `foldl'`-last-wins / `foldr`-first-wins
> divergence is a requirements erratum tracked as AVI-1319.

`Ext/Lax.lean` proves the §3.2 dual. For `A, B` sharing a conflicting **edge**
key (same `EdgeId` encoding `(src,tgt,relation)`, different payload —
`Convert.hs:165`): (a) no view receives both `A` and `B`, so the two-object
diagram has **no cocone** (`Extraction.no_cocone_of_conflict`,
`no_pairCocone_of_conflict`), the canonical cocone of §3.1 loses its second leg
(`not_leg_merge_of_conflict`), and `familyConsistent [A, B]` fails — the §2/§3.1
hypotheses are unsatisfiable, a strictly stronger failure than a missing
mediator; (b) `A ⊕ B` carries the **first** operand's value at `k`
(`merge_apply_left_of_conflict`), `B ⊕ A` the second's, `A ⊕ B ≠ B ⊕ A`, and no
view-embedding relates the two merges in either direction
(`not_le_merge_swap`, `not_le_merge_swap'`) — the first/old-wins comparison is
non-invertible. AC-4 is instantiated on a concrete conflicting-`EdgeId` fixture
(`ac4_conflictingEdge_firstWins`), so the statement is falsifiable, not vacuous.

`Ext/Complexity.lean` proves the §4 linear-pass claim. The fold is priced by a
cost-annotated `foldl'` (`foldlCost`) that returns *the very same value* as
`foldlMerge` (`foldlCost_fst`), so the bound is about the fold Graphos runs. Each
step is charged the **smaller operand's** key count — the key-proportional term
of `Data.Map.union`'s `O(m·log(n/m+1))` — and the induction `foldlCost_snd_le`
then gives `cost ≤ Σᵢ (|extNodes Eᵢ| + |extEdges Eᵢ|)` (`Extraction.size`, §4's
`|keys(Eᵢ)|`). Keys are counted against a candidate universe list, since the
extensional view model has no intrinsic support; with a duplicate-free universe
containing the family's keys this is the literal cardinality.

> **Modelling note (AVI-1306).** Charging *both* operands at every step — the
> literal reading of the coarse `O(n₁+n₂)` comment at `Core.hs:126-128` — makes
> the fold `Θ(n·Σᵢ|keys(Eᵢ)|)` for a growing accumulator, i.e. the §4 bound would
> be false. The theorem therefore prices the smaller operand, which is what
> `Data.Map.union` actually traverses and what "each key is visited a bounded
> number of times" means: every key is charged once, at the step where its view
> is the smaller operand, and never re-scanned. The `log` of the `split` path is
> node/pointer depth, outside the key-operation unit that §4 and M-3 pin. The
> charge is attained on componentwise-disjoint views
> (`Extraction.size_merge_eq_add`: four disjoint three-key views over `[0,12)`
> charge `9` against `12`), so the constant cannot be improved asymptotically;
> the `(2n−1)/(n−1) → 2` doubling ratio matches the AC-3 recorded
> `2.01, 1.99, 2.01` to two decimal places. The timing harness itself is
> AVI-1307 (`Harness/Ac3Cost.lean`, Lean arm, plus the Haskell arm in
> `benchmarks/nway-fold-timing/`).

### Seam — `M → Graph → G_s` (AVI-616 §1.1, AVI-1308)

`Seam/SupportGraph.lean` pins the one spec seam Phase 2 left open: how the
colimit object `M` becomes the simple support graph `G_s` that Phase 1's `Q`
consumes.

```
M : Extraction ──buildGraph gDirected──▶ Graph ──neighbors──▶ G_s = (V, A) ──▶ Q
```

Objects and functions:

| Seam item | Lean name | Haskell surface |
| --- | --- | --- |
| extraction edge (endpoints + opaque payload) | `SeamEdge` | `Types/Graph.hs:73` (`edgeSource`/`edgeTarget`) |
| graph observables `neighbors`/`degree` read | `BuiltGraph` | `Core.hs:47-57` (`gEdges :: Map (NodeId,NodeId) Edge`) |
| dangling drop | `validEdge`, `validEdges` | `Core.hs:100-103` |
| `buildGraph` at those observables | `buildGraph` | `Core.hs:96-122` |
| `neighbors` membership | `BuiltGraph.neighbors` (= `supportAdj` when undirected) | `Query.hs:37-41` |
| simplicity precondition | `NoSelfLoops` | implicit in Phase 1's `SupportGraph.irrefl` |
| `G_s` as Phase 1's object | `buildGraph_toSupportGraph` | `Community.hs:92-107,151` |

Named results:

* `buildGraph_supportAdj_eq_true_iff` — `G_s`'s support relation is exactly
  "some valid edge joins `u` and `v` in either orientation"; the statement names
  no `EdgeId`, relation, weight or confidence, so none of them can reach `Q`.
* `buildGraph_supportAdj_merge_swap` — **the seam theorem**: if two views agree
  on edge endpoints at every shared key (guaranteed by the `EdgeId` encoding,
  `Convert.hs:165`), then `G_s(A ⊕ B) = G_s(B ⊕ A)` even though
  `A ⊕ B ≠ B ⊕ A`. Phase 2's first/old-wins precedence cannot leak into `Q`.
* `ac4_seam_modularity_eq` — the same claim instantiated on the AC-4
  conflicting-`EdgeId` fixture and closed against `modularity`: the two merges
  are different extractions, yet `Q` agrees on **every** partition.
* `buildGraph_supportAdj_merge_left` — per-view containment:
  `G_s(Eᵢ) ⊆ G_s(M)` on the same key enumeration.
* `buildGraph_supportAdj_symm`, `buildGraph_supportAdj_irrefl` — `G_s` is
  undirected and (under `NoSelfLoops`) simple, so Phase 1's `SupportGraph`
  axioms are discharged, not assumed.
* `buildGraph_foldMerge_eq_buildFold` — `M` is literally
  `foldr mergeExtractions emptyExtraction`, the fold `Build.hs:11-13` runs.
* `dangling_resurrection` (fixture) — the dangling drop happens **once, after**
  the colimit: an edge dangling inside its own view survives in `G_s(M)` when
  its endpoint arrives from another view. Hence `G_s(M) ≠ ⋃ᵢ G_s(Eᵢ)`; `G_s` is
  a function of `M`, not of the family's per-view graphs.
* `modularity_congr_adj` — `Q` is a function of `(V, A)` alone, so the four
  `SupportGraph` proof fields are irrelevant to any modularity claim.

> **Citation note (AVI-1308).** AVI-616 §1.1 cites `Graph` as
> `(LabeledGraph, Types/Graph.hs:83)`. That line lands in the `Extraction`
> accessor block (`extNodes`, `Types/Graph.hs:89`); the `LabeledGraph` record is
> `Types/Graph.hs:106-110` and keys `gEdges` by `EdgeId`. The pipeline uses
> `Domain/Graph/Core.hs:47-57`, whose `gEdges` is keyed by `(NodeId, NodeId)` —
> that endpoint key is what makes the multi-edge collapse immediate, and it is
> the record this seam is pinned against.
>
> **Precondition note (AVI-1308).** `buildGraph` does not drop self-edges, while
> Phase 1's `SupportGraph` is simple (`irrefl`). The seam therefore carries
> `NoSelfLoops` explicitly. Nothing in the extraction lane forbids a self-edge,
> so if the shipped pipeline can emit one, that is a Haskell-side guard
> question, not a Lean gap.
