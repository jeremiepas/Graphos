# devhaskell-g6-prop-eqv: Implement G6 PROP-EQV Reconciliation

## Summary
Implement churn reconciliation for `runIncrementalPipeline` to handle `Δ = (R,A)` reconciliation as specified in AVI-1291 adoption.

## Specification
- **Route**: G6 PROP-EQV reconciliation (~new-feature)
- **Function to implement**: 
  - `runIncrementalPipeline` reconciliation with `mergeGraphs` semantics
  - Attach removed nodes as `new` `Domain.Graph.Mutation.deleteNodeIncident`
  - Re-cluster only churn-affected communities with bounded-delta INV
- **Property Tests**: Required in `Tests/` directory
- **Performance**: O(deg) incident-edge removal

## Implementation Details
- Merge churn into retained graph using `mergeGraphs` semantics
- Detach removed nodes with O(deg) incident-edge removal
- Re-cluster only affected communities (delta update should equal full rebuild on `G_{t+1}`)
- Property tests must be included in `Tests/`

## Acceptance Criteria
- No pipeline/wiring changes beyond incremental update semantics
- Correct handling of node removal and community re-clustering
- Property tests verify bounded-delta INV semantics
- Integration with existing pipeline infrastructure