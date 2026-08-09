# Plan: Integration verification

## Goal
Confirm all changes work together on the Graphos repository and meet the proposal PDCA targets.

## Approach
- Run default pipeline (`cabal run graphos -- .`).
- Run traced pipeline (`cabal run graphos -- . --debug-trace graphos-out/traces --debug`).
- Run `scripts/audit_graph.py`.
- Compare metrics to baseline.

## Check Criteria
- Connected components ≪ 165; ≥ 1 cross-file `imports` edge.
- Zero truncated junk labels.
- `kind=None` nodes reduced ≥ 80% from baseline (5,900).
- Report/export parity.
- No `traces/` on default run; trace file on debug run.
- `span_build`/`span_cluster` ≥ 1 ms.
- `cabal test` and `cabal build` pass.
