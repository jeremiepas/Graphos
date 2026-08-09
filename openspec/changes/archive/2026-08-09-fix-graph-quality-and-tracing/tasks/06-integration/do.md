# Do: Integration verification

## Changes Made
- Ran both default and traced full pipeline executions on the Graphos repository.
- Ran `scripts/audit_graph.py` against the generated output.
- Captured span durations from `graphos-out/traces/*.jsonl`.

## Results
- Default run: 4,333 nodes / 49,819 edges / 11 communities / 62 components.
- Cross-file imports: 712.
- Truncated junk labels: 0.
- `kind=None` nodes: 0 (down from 5,900).
- No `traces/` directory created on default run.
- Traced run: `span_build` = 16.2 ms, `span_cluster` = 232 ms.
