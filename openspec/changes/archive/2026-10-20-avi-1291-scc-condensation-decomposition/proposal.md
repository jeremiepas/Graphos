## Why

[AVI-512](/AVI/issues/AVI-512) rev-3 closed the requirements layer for element **G5 — SCC / condensation** (`docs/math-requirements/AVI-518-shortest-path-scc.md` §1.3, Thm 2.2–2.3, INV-7…INV-10; `docs/math-requirements/AVI-523-cycle-detection-connectivity.md` INV-C1…INV-C6), but the register has **no spec and only a partial implementation**: `Domain.Graph.Query` already provides `stronglyConnectedComponents(WithCached)` and `shortestPathReachable` (AVI-530, PR #30), while the condensation, `isAcyclicDirected`, and the connectivity decompositions (`connectedComponents`, `weaklyConnectedComponents`, `isolatedNodes`, `hasCycleUndirected`) do not exist (AVI-518 §7 GAP-1 remainder). This change adopts the SCC/condensation register as SHALL requirements, spec-first: the existing surface is classified `requirement-creation`, the missing surface `new-feature`; implementing the remainder is separate `head-of-dev` work after the spec lands.

## What Changes

- **New capability `scc-decomposition`** adopting the G5 mathematical register as SHALL requirements, per requirement with checkable acceptance property/invariant/complexity and a per-requirement classification (existing surface = `requirement-creation`; missing surface = `new-feature`):
  - SCC partition invariant (INV-7: classes partition `V`, unique up to relabeling, deterministic labeling by ascending seed order) and refinement under single-edge deltas (INV-10).
  - Condensation is a DAG (Thm 2.3) with well-defined topological order.
  - Reachability / acyclicity predicates: `⤳` decided by hop-minimal `shortestPath` over the directed relation `E` (Thm 2.4, INV-8/INV-9); DAG ⟺ all SCCs singletons ∧ no self-loop (Thm 2.5, INV-C5); SCC is NOT biconnected components (§1.4 disambiguation).
  - Connectivity decomposition coarsening: connected components / weakly connected components partition `V` (INV-C1) and coarsen the SCC partition (Thm 2.2, INV-C6); isolated nodes are degree-0 singletons (Thm 2.6, INV-C3).
  - Complexity + scale guard: SCC/condensation/reachability `O(N + E)`; all-pairs reachability `O(N·(N+E))` forbidden at repository scale.
- **Golden fixtures** F1–F3, F5, F7 of AVI-523 §8 are embedded as spec scenarios (exact expected partitions), giving archivable acceptance for the future implementation.

## Capabilities

### New Capabilities
- `scc-decomposition`: Strongly connected components as the unique partition of the node set under mutual directed reachability, its condensation as a DAG, directed reachability/acyclicity predicates, connectivity decompositions that coarsen the SCC partition, and the linear `O(N + E)` complexity budget for all of them.

### Modified Capabilities
- none — `05-path` (shortest path, "Disconnected nodes return 'no path found'") is cross-referenced as the reachability decision procedure; `fgl-adapter` gives the bijective key-order indexing that makes seed-ordered labeling deterministic. Neither is modified.

## Impact

- **Specs**: new `openspec/specs/scc-decomposition/spec.md` (after archive). Sources: `docs/math-requirements/AVI-518-shortest-path-scc.md` (§1.3, Thm 2.1–2.4, §3, §4, §5 INV-7…INV-10, §8, §9), `docs/math-requirements/AVI-523-cycle-detection-connectivity.md` (§1.4, Thm 2.1–2.2, §2.5–2.6, INV-C1/C3/C5/C6, §8 F1–F7), consolidated GT-W4 scale guard.
- **Domain (constrained surface, no edits in this change)**: `Graphos.Domain.Graph.Core` (`Graph`, `gEdges` keyed by `(NodeId,NodeId)`, `addEdges`, `mergeGraphs` — INV-10 constrains both), `Graphos.Domain.Graph.Query` (`stronglyConnectedComponents:131`, `stronglyConnectedComponentsWithCached:136`, `shortestPathReachable:114`, `shortestPath:92`, `neighbors:49`, `degree:56`), `Graphos.Domain.Graph.Analysis` (`toCachedFGL`, existing undirected decompositions to keep distinct).
- **Haskell**: none in this change (spec-first). The GAP-1 remainder (`isAcyclicDirected`, `connectedComponents`, `weaklyConnectedComponents`, `isolatedNodes`, `hasCycleUndirected`, condensation build, `*WithCached` variants — the existing `stronglyConnectedComponents(WithCached)` and `shortestPathReachable` need property/golden tests only) routes to `head-of-dev` after the feasibility gate.

## Feasibility gate

`head-of-dev` SHALL confirm the Haskell impact (expected: new pure `Domain.Graph` functions + `Tests/` property/golden coverage only) via a comment on [AVI-1291](/AVI/issues/AVI-1291) before this change is archived.