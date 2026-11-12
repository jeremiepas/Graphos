## Why

The cypher evaluator (`src/Graphos/Domain/Query/Cypher/Eval.hs`) diverges from the design's GraphIndex/CachedFGL strategy. It ignores the `GraphIndex` (`_idx`), full-scans `gNodes`/`gEdges` for every query, and uses recursive full-scan for variable-length paths instead of CachedFGL. This is functionally correct (all tests pass) but does not match the design and will not scale to large graphs.

## What Changes

- Use the `GraphIndex` (`giLabelIndex`) to anchor node candidate lookups instead of full-scanning `gNodes`.
- Use CachedFGL for variable-length path enumeration instead of recursive full-scan of `gEdges`.

## Capabilities

### New Capabilities
(none — pure implementation fix, no new capabilities)

### Modified Capabilities
(none — no spec-level behavior changes; the `cypher-query` spec is already correct)

## Impact

- `src/Graphos/Domain/Query/Cypher/Eval.hs`: Use GraphIndex for node candidates; use CachedFGL for variable-length paths.
- No API changes, no behavior changes, no dependency changes.
