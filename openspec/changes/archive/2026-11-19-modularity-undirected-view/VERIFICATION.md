# Verification — Lean arm

Artifact: `formalization/lean/GraphosFormalization/Seam/UndirectedScoring.lean`
(namespace `GraphosFormalization.UndirectedScoring`), imported from
`formalization/lean/GraphosFormalization.lean`.

## Toolchain

- Pinned: `formalization/lean/lean-toolchain` → `leanprover/lean4:v4.14.0`.
- Lean core only: no `Mathlib`, no `Rat`, no `Float` lemmas. Exact integer
  numerators over `(2m)²`, as in `Phase1/RangeBound.lean`.
- The nix `lean4` (4.35.0) does **not** build the companion: `Ext.Basic` and
  `Phase1.RangeBound` fail on it. Use elan with the pinned release; on this
  machine `nix shell nixpkgs#elan` fetched it.

## Commands

```bash
cd formalization/lean
nix shell nixpkgs#elan -c lake build
```

Result (2026-11-12): `Build completed successfully`, 20 targets, no error and
no warning in `GraphosFormalization.Seam.UndirectedScoring`.

Axiom audit:

```lean
import GraphosFormalization.Seam.UndirectedScoring
open GraphosFormalization.UndirectedScoring
#print axioms RecordGraph.fixed_range_bound           -- [propext, Quot.sound]
#print axioms supportPairs_eq_self                    -- [propext]
#print axioms twoArcs_legacy_directed_exceeds_one     -- [propext]
#print axioms bothWays_legacy_undirected_exceeds_one  -- [propext]
#print axioms bothWays_fixed_in_range                 -- [propext, Quot.sound]
```

No `sorry` (`grep -c sorry` = 0).

## What the theorems establish

| Claim | Theorem |
|---|---|
| Today's score can exceed `1` under `--directed` | `twoArcs_legacy_directed_exceeds_one` (`Q = 3/2`) |
| The undirected projection alone is not enough | `bothWays_legacy_undirected_exceeds_one` (`Q = 3/2`) |
| The fixed score is Phase 1 `Q` on a valid `EdgeGraph` | `RecordGraph.toEdgeGraph` (all invariants) |
| The fixed score is in `[-1/2, 1]` | `RecordGraph.fixed_range_bound` |
| Simple inputs are unchanged | `supportPairs_eq_self`, `RecordGraph.toEdgeGraph_edges_of_simple` |
| No support edge is lost | `mem_or_rev_mem_supportPairs` |

## Gap between the proof and the code

- The Lean `supportPairs` keeps the last orientation it meets; the Haskell fix
  uses `(min, max)`. `Qnum` depends only on unordered pairs, because
  `endpointsIn` and `bothIn` are symmetric. This is not stated as a Lean
  theorem; the cross-check rows `twoArcs` and `bothWays` (task 1.3) pin it on
  data.
- The theorem needs a genuine set partition. `partitionQuality` scores
  `finalComm`; task 2.3 asserts that it covers every node (design D3).
- `Float` versus exact arithmetic: `twoM = 2.0 * (Σdeg / 2.0)` is exact in
  `Double` below `2^53`. The rounding of the final quotient is covered by the
  existing `2^-24` cross-check tolerance.
