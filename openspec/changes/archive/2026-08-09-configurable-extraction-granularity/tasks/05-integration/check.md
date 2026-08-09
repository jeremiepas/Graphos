# Check: Integration verification + CHANGELOG

| Metric | fine | function |
|---|---|---|
| TS example files (7 files, tree-sitter) | 227 nodes | 104 nodes |
| TS node kinds at function level | statements included | Module/Type/Function/Class/Method/Property/Import/Export only |
| Repo total (LSP-dominated) | 4,627 | 4,504 |

- No `.json` file contributes >1 node on a default run; 1-node guarantee fixture-verified.
- `scripts/audit_graph.py`: PASS at function level.
- `cabal build` + `cabal test`: PASS (170 examples, 0 failures).
- Active-level log: "Granularity: function" / "Granularity: fine (CLI override)".
