# Do: Integration + performance verification

- Traced pipeline run on this repo with `--debug-trace`.
- Benchmark harness (temp file, compiled `ghc -O2` against the inplace library): 100,000 nodes / 120,000 edges (ring + deterministic chords, ~1.2 edges/node like the real corpus).
- Ran against NEW implementation, then OLD (via `git stash push src/Graphos/Domain/Community.hs`), then restored.
- Final `cabal test` + `scripts/audit_graph.py`.
