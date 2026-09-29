## MODIFIED Requirements

### Requirement: In-place graph edge enrichment

The `inferEdges` function and the pipeline SHALL add inferred edges directly to the existing Graph's adjacency structures using a new `addEdges` function in `Domain.Graph.Core`, instead of creating a new Graph via `buildGraphFromExtractions` with the full node+edge list.

Previously: Edge inference created a full `Extraction` from all existing nodes + all existing edges + inferred edges, then called `buildGraphFromExtractions` to create an entirely new Graph. This held the old Graph, the Extraction, and the new Graph simultaneously (3× peak memory).

- **Plan**: Eliminate the 3× duplication window during edge inference by adding edges in-place.
- **Do**: Add `addEdges :: Graph -> [Edge] -> Graph` to `Domain.Graph.Core` that inserts edges into existing Maps. Use this in the pipeline instead of `buildGraphFromExtractions`.
- **Check**: Memory profiling shows no 3× spike during the infer→cluster transition. `cabal test` passes.
- **Act**: If `addEdges` is too slow for very large edge lists, consider batch `Map.union` instead of individual `Map.insert`.

#### Scenario: Adding inferred edges to existing graph
- **WHEN** `inferEdges` produces a list of inferred edges
- **THEN** the pipeline calls `addEdges graph inferredEdges` which returns a new Graph with the inferred edges added to `gEdges`, `gAdjFwd`, and `gAdjBack`
- **AND** the original Graph is eligible for GC once the new Graph is constructed (Haskell immutability means old Maps are GC'd when no longer referenced)

#### Scenario: No intermediate Extraction for enrichment
- **WHEN** the pipeline enriches the graph with inferred edges
- **THEN** no `Extraction` value is created containing all nodes and all edges
- **AND** only the inferred edges are passed to `addEdges`

#### Scenario: Backward compatibility
- **WHEN** `addEdges` is called on an empty edge list
- **THEN** the returned Graph is identical to the input Graph (no changes)