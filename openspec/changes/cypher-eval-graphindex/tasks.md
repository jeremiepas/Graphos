## 1. Anchor node candidates via `GraphIndex`

- [ ] 1.1 In `nodeCandidates`, use `giLabelIndex` (via `Map.findWithDefault` on the lowercased label) to get candidate node IDs for a label filter; fall back to the full node list when the pattern has no label constraint
- [ ] 1.2 Thread the `GraphIndex` (`_idx`) through the evaluation helpers so `nodeCandidates` can use it (currently `_idx` is unused)
- [ ] 1.3 Tests: label-filtered node candidates return the same results as the full-scan (existing `EvalSpec` golden results must remain unchanged)

## 2. Variable-length paths via `CachedFGL`

- [ ] 2.1 Build `CachedFGL` once per query in `evaluate` (via `toCachedFGL g`); skip it for single-hop queries
- [ ] 2.2 Precompute an edge adjacency index (source → edges, target → edges) from the FGL graph once per query
- [ ] 2.3 Use the adjacency index in `hop` instead of full-scanning `Map.toList (gEdges g)`
- [ ] 2.4 Tests: variable-length path enumeration returns the same results as the recursive full-scan (existing `EvalSpec` golden results must remain unchanged)

## 3. Verification

- [ ] 3.1 `cabal build --flag dev` green (with `-Werror`)
- [ ] 3.2 `cabal test --flag dev` green (558 examples, 0 failures, 3 pending)
- [ ] 3.3 E2E: re-run the `/tmp/opencode/graph.json` e2e queries (label filter, relation hop, var-length, regex, count, budget) and confirm identical output
