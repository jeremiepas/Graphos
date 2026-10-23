## Why

`buildGraph` drops dangling edges (source/target not in the node set) but leaves
**self-edges** (`edgeSource e == edgeTarget e`) in the graph. A self-edge puts a node
in its own neighbour set, which violates the Phase 1 `SupportGraph` contract
`adj v v == False` for the undirected support graph `G_s`, and breaks the Lean seam
`NoSelfLoops`, which assumes the self-loop count `ℓ = 0`.

Downstream, a self-edge artificially inflates a node's degree, turns a single node
into its own strongly-connected component / a directed cycle (invoking the
`isAcyclicDirected` self-loop clause, INV-C5), and surfaces as an `X --rel--> X` line
in rendered query output (`query-noise-control`).

## What Changes

- `buildGraph` (`Domain/Graph/Core.hs`) SHALL drop a self-edge from its valid-edge set
  alongside the existing dangling-edge drop, in both the undirected and directed
  (`buildGraph True`) views, so `G_s` stays irreflexive.
- A node whose only edge was a self-edge is **preserved** as a degree-0 node (dropping
  the edge does not drop the node).
- Regression tests in `GraphSpec` cover the lone self-edge, a self-edge coexisting
  with a real edge on the same node, and the directed-mode case.

## Capabilities

### Modified Capabilities
- `graph-build` (Workflow 01, Stage 3 Build): `buildGraph` drops self-edges so `G_s`
  is irreflexive.

### New Capabilities
- none

## Impact

- **Domain**: `Graphos.Domain.Graph.Core.buildGraph` (pure, no IO) — a single predicate
  added to the `validEdges` filter.
- **Tests**: `tests/Graphos/Domain/GraphSpec.hs` regression.
- **Downstream**: SCC/acyclicity (INV-C5) and query output no longer rely on their own
  self-edge guard; degrees and adjacency are clean at the source.
- Low risk: additive filter, no signature change, no behaviour outside Build.
