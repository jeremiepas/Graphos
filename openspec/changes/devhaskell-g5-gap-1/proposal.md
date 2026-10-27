# devhaskell-g5-gap-1: Implement G5 GAP-1 Surface Functions

This change implements the missing pure Domain.Graph surface functions required for AVI-1291 math-requirements adoption.

## Specifications
- **Route**: G5 GAP-1 surface (~new-feature)
- **Functions to implement**: 
  - `cond(G)`: Condensation DAG of SCCs
  - `isAcyclicDirected`
  - `connectedComponents`
  - `weaklyConnectedComponents`
  - `isolatedNodes` 
  - `hasCycleUndirected`
  - `*WithCached` variants of all above functions
- **Reference**: `openspec/specs/scc-decomposition/spec.md` (not directly accessible, derived from task description)
- **Property Tests**: Required per INV-7 uniqueness / condensation-is-DAG

## Acceptance Criteria
- All functions must work correctly with existing Graph structure
- Integration with existing `stronglyConnectedComponents` function
- Property tests must pass
- Follow existing code patterns and style
- No pipeline/wiring changes beyond specification