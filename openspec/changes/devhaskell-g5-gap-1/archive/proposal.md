# devhaskell-g5-gap-1: Implement G5 GAP-1 Surface Functions

This change implements the missing pure `Domain.Graph` surface functions required for AVI-1291 (math-requirements adoption) and provides a `*WithCached` variant layer.

## Why

LLMs waste tokens re-reading entire codebases; developers lack navigable maps of how code connects. G5 GAP-1 introduces new graph operations on the knowledge-graph domain, but no pure surface functions exist yet. Implementing them now lets us expose condensation-DAG (`cond`), cycle detection (`isAcyclicDirected`, `hasCycleUndirected`), component analysis (`connectedComponents`, `weaklyConnectedComponents`, `isolatedNodes`) and a cached variant layer for performance.

## What Changes

- Add pure `Domain.Graph` surface functions:
  - `cond(G)` — Condensation DAG of SCCs
  - `isAcyclicDirected`
  - `connectedComponents`
  - `weaklyConnectedComponents`
  - `isolatedNodes`
  - `hasCycleUndirected`
  - `*WithCached` variants of all above functions (14 exports in `Graphos.Domain.Graph.Query`, re-exported from `Graphos.Domain.Graph`).
- Provide property tests for INV-7 uniqueness / condensation-is-DAG.
- Golden fixture tests following AVI-523 pattern (cond, isAcyclicDirected, isolatedNodes, connectedComponents, weaklyConnectedComponents, INV-C6 coarsening).
- Integration with existing test suite (`tests/Graphos/Domain/Graph/SCCSpec.hs`, §2.3–2.6).

## Capabilities

### New Capabilities
- `cond(G)` — Condensation DAG of SCCs.
- `isAcyclicDirected` — Check if directed graph is acyclic.
- `connectedComponents` — Get connected components (undirected).
- `weaklyConnectedComponents` — Get weakly connected components (directed).
- `isolatedNodes` — Find isolated nodes in graph.
- `hasCycleUndirected` — Detect cycles in undirected graphs.
- `*WithCached` variants of all functions for caching intermediate results.

### Modified Capabilities
- Existing `stronglyConnectedComponents` and `stronglyConnectedComponentsWithCached` remain unchanged; now integrated with new condensation-DAG output.

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

- Committed to `AVI-512-add-math-in-graphos-project` (per AVI-1348 heartbeat: G5 lives under the math-adoption branch, not `feature/AVI-1381-release-hygiene`).
- PR deferred: no managed GitHub identity available this run (commit-only). Push + open PR on next run with identity.
