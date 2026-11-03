# AVI-961: intent code-graph projection (classifier + well-formedness)

This change implements the missing pure `Domain.Graph` surface functions required for AVI-961 (intent projection tasks 3.1, 3.2 complete). Provides a `*WithCached` variant layer.

## Why

LLMs waste tokens re-reading entire codebases; developers lack navigable maps of how code connects. G5 intent projection introduces new graph operations on the knowledge-graph domain, but no pure surface functions exist yet.

## What Changes

- Add pure `Domain.Graph` surface functions:
  - `cond(G)` — Condensation DAG of SCCs.
  - `isAcyclicDirected`
  - `connectedComponents`
  - `weaklyConnectedComponents`
  - `isolatedNodes`
  - `hasCycleUndirected`
  - `*WithCached` variants of all above functions (14 exports in `Graphos.Domain.Graph.Query`, re-exported from `Graphos.Domain.Graph`).
- Provide property tests for INV-7 uniqueness / condensation-is-DAG.
- Golden fixture tests following AVI-523 pattern.

## Capabilities

### New Capabilities
- `cond(G)` — Condensation DAG of SCCs.
- `isAcyclicDirected` — Check if directed graph is acyclic.
- `connectedComponents` — Get connected components (undirected).
- `weaklyConnectedComponents` — Get weakly connected components (directed).
- `isolatedNodes` — Find isolated nodes in graph.
- `hasCycleUndirected` — Detect cycles in undirected graphs.
- `*WithCached` variants of all functions for caching intermediate results.

## Impact

- Affected code: `Domain.Graph` module (new pure functions + cached variants).
- APIs: New exported functions in `Graphos.Domain.Graph` and re-exports from `Graphos.Domain.Graph`.
- Dependencies: None new; uses existing FGL 5.8+ library.
- Systems: No pipeline/wiring changes beyond the specification; no MCP server modifications.

## Integration

- Build on existing `stronglyConnectedComponents` function.
- Integrate with `stronglyConnectedComponentsWithCached`.

## Property Tests & Golden Fixtures

- Property tests for INV-7 uniqueness / condensation-is-DAG (3 property checks).
- Golden fixture tests following AVI-523 pattern covering cond, isAcyclicDirected, isolatedNodes, connectedComponents, weaklyConnectedComponents, and INV-C6 coarsening.

## Commit / PR

- Committed to `feature/AVI-961-intent-projection` (per heartbeat AVI-1348: G5 lives under the math-adoption branch).
- PR deferred: no managed GitHub identity available this run; commit-only work on the branch. Push + open PR on next run with identity.
