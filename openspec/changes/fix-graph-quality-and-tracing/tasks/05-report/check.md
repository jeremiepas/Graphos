# Check: Report/export consistency

## Test Results
- `cabal test`: PASS — 142 examples, 0 failures.
- `cabal build`: PASS with `-Wall -Werror`.
- Integration run: report totals (4,333 / 49,819 / 11) exactly match `graph.json`.
- Audit script: PASS.

## Verification
- No duplicate surprising connections in GRAPH_REPORT.md.
