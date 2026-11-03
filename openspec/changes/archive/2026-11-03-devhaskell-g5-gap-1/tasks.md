# Tasks for devhaskell-g5-gap-1 Implementation

## Implementation Tasks
- [x] **Implement G5 GAP-1 Surface Functions**
   - [x] `cond(G)` condensation DAG builder (`cond`, `condWithCached`)
   - [x] `isAcyclicDirected` - Check if directed graph is acyclic (`isAcyclicDirected`, `isAcyclicDirectedWithCached`)
   - [x] `connectedComponents` - Get connected components (`connectedComponents`, `connectedComponentsWithCached`)
   - [x] `weaklyConnectedComponents` - Get weakly connected components (`weaklyConnectedComponents`, `weaklyConnectedComponentsWithCached`)
   - [x] `isolatedNodes` - Find isolated nodes (`isolatedNodes`, `isolatedNodesWithCached`)
   - [x] `hasCycleUndirected` - Detect cycles in undirected graphs (`hasCycleUndirected`, `hasCycleUndirectedWithCached`)
   - [x] `*WithCached` variants of all functions (14 exports in `Graphos.Domain.Graph.Query`; re-exported from `Graphos.Domain.Graph`)

2. **Acceptance Tests**
   - [x] Property tests for INV-7 uniqueness / condensation-is-DAG (`cond (property)` x3: every SCC is a condensation vertex, no self-arc, condensation is acyclic)
   - [x] Golden fixture tests following AVI-523 pattern (cond / isAcyclicDirected / isolatedNodes / connectedComponents golden graphs)
   - [x] Integration with existing test suite (`tests/Graphos/Domain/Graph/SCCSpec.hs`, §2.3-2.6)
   - [x] Fixed `undirectedComponentsWithCached` asymmetric adjacency bug (INV-C6): record each arc in both orientations

## Test status
- `cabal build --enable-tests graphos-test`: green (compiles + links on GHC 9.10.3).
- Full suite: **1380 examples, 0 failures, 3 pending**.
- G5 surface (`-m "SCC"`): 35 examples, 0 failures — `cond`, `isAcyclicDirected`, `isolatedNodes`, `connectedComponents`, `weaklyConnectedComponents`, and INV-C6 coarsening all pass.

## Commit / PR
- Committed to `AVI-512-add-math-in-graphos-project` (per AVI-1348 heartbeat: G5 lives under the math-adoption branch, not `feature/AVI-1381-release-hygiene`).
- PR deferred: no managed GitHub identity available this run (commit-only). Push + open PR on next run with identity.
