# AVI-961: intent code-graph projection (classifier + well-formedness)

## Schema

- `Domain.Graph` — pure domain layer, zero IO.
- New surface functions in `Graphos.Domain.Graph`:
  - `cond(G)` — Condensation DAG of SCCs.
  - `isAcyclicDirected`
  - `connectedComponents`
  - `weaklyConnectedComponents`
  - `isolatedNodes`
  - `hasCycleUndirected`
  - `*WithCached` variants (14 exports in `Graphos.Domain.Graph.Query`, re-exported from `Graphos.Domain.Graph`).
- Existing functions preserved: `stronglyConnectedComponents`, `stronglyConnectedComponentsWithCached`.

## Requirements

- Implement the six pure surface functions above using FGL 5.8+ (Patricia Tree) on raw Maps/Sets; do **not** import `Domain.Graph` to avoid cycles.
- Provide property tests for INV-7 uniqueness / condensation-is-DAG.
- Golden fixture tests following AVI-523 pattern covering cond, isAcyclicDirected, isolatedNodes, connectedComponents, weaklyConnectedComponents, and INV-C6 coarsening.

## Impact

- Affected code: `Domain.Graph` module (new pure functions + cached variants).
- APIs: New exported functions in `Graphos.Domain.Graph` and re-exports from `Graphos.Domain.Graph`.
- No new Haskell dependencies; uses existing FGL 5.8+ library.
- Systems: No pipeline/wiring changes beyond the specification; no MCP server modifications.

## Integration

- Build on existing `stronglyConnectedComponents` function.
- Integrate with `stronglyConnectedComponentsWithCached`.
