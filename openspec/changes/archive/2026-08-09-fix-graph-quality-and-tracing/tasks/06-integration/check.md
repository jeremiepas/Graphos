# Check: Integration verification

## Results
- `cabal test`: PASS — 142 examples, 0 failures.
- `cabal build`: PASS with `-Wall -Werror`.
- `scripts/audit_graph.py`: PASS.

## Metrics vs Baseline
| Metric | Baseline | New |
|---|---|---|
| Nodes | 8,366 | 4,333 |
| Edges | 9,307 | 49,819 |
| Communities | 276 | 11 |
| Components | 165 | 62 |
| Cross-file imports | 0 | 712 |
| `kind=None` | 5,900 | 0 |
| Truncated junk labels | 2,933 | 0 |

## PDCA Targets Met
- Components dropped from 165 to 62 (well below source-file count).
- Cross-file imports exist.
- Truncated junk labels eliminated.
- `kind=None` reduced by 100%.
- Report/export totals match.
- Span durations real (ms).
- No unconditional `traces/` folder.
