# Check: Canonical module IDs + relation semantics

## Test Results
- `cabal test`: PASS — 142 examples, 0 failures.
- `cabal build`: PASS with `-Wall -Werror`.

## Verification
- Cross-file import specs passed: shared module node and a cross-file `imports` edge.
- Integration run produced 712 cross-file `imports` edges (baseline: 0).
