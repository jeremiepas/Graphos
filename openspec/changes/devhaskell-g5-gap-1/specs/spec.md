# G5 GAP-1 Surface Specification

## Overview
Implement pure `Domain.Graph` surface functions for condensation DAG and related operations.

## Functions to Implement
1. `cond(G)`: Condensation DAG of `C_c` SCCs
2. `isAcyclicDirected`: Check if directed graph is acyclic
3. `connectedComponents`: Get connected components of undirected graph
4. `weaklyConnectedComponents`: Get weakly connected components of directed graph
5. `isolatedNodes`: Find isolated nodes in graph
6. `hasCycleUndirected`: Detect cycles in undirected graphs
7. `*WithCached` variants of all above functions

## Requirements
- Functions must be deterministic
- Must maintain existing graph invariants
- Property tests required for INV-7 uniqueness and condensation-is-DAG
- Follow existing code patterns in Graphos codebase
- No breaking changes to existing APIs

## References
- Builds on existing `stronglyConnectedComponents` function
- Must integrate with `stronglyConnectedComponentsWithCached` 
- Should follow patterns established in `Graphos.Domain.Graph.Query`