# Check: Integration + performance verification

| Measurement | Old | New | Verdict |
|---|---|---|---|
| `span_cluster`, this repo (4.5k nodes) | 232 ms | **140 ms** | 1.65×, no regression ✓ |
| 100k nodes / 120k edges, compiled -O2 | **169.2 s** | **10.5 s** | **16× faster** ✓ |
| PRD §16.1: Leiden < 30 s @ 100k | violated (5.6× over) | met (3× headroom) | ✓ |

- `cabal test`: PASS — 148 examples, 0 failures.
- `cabal build`: clean with `-Wall -Werror`.
- `scripts/audit_graph.py`: PASS.
- Note: 19,591 (old) vs 19,641 (new) communities on the synthetic graph — expected drift from the merge node-loss bugfix; deterministic goldens identical.
