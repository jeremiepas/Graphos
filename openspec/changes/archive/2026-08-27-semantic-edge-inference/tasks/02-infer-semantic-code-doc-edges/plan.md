# Task 2 — inferSemanticCodeDocEdges — PLAN

**Task slug**: `02-infer-semantic-code-doc-edges`
**Attempt**: 1
**Status**: pending

## Summary

Implement `inferSemanticCodeDocEdges :: Graph -> Map NodeId [Double] -> [Edge]` in `UseCase/Infer.hs` and add comprehensive Hspec tests. This function computes cosine similarity between `DocFile` and `CodeFile` node embeddings, emitting `References` edges above the configured threshold. Covers subtasks 2.1 (implementation) and 2.2 (tests).

## Detail

### Scope

- **Function implementation** (`src/Graphos/UseCase/Infer.hs`):
  - Collect `DocFile` nodes with non-empty embeddings from the graph
  - Collect `CodeFile` nodes with non-empty embeddings from the graph
  - For each doc node: compute `cosineSimilarity docVec codeVec` against all code nodes
  - Filter pairs where cosine >= threshold (default 0.5)
  - Sort descending by similarity, take top `maxSemanticFanOut` (default 50)
  - Emit `References` edges with confidence = cosine score
  - Use existing `makeInferredEdge` helper with `References` edge type
  - Dedup on `(source, target)` via existing `dedupOn`
  - Export `inferSemanticCodeDocEdges` from module

- **Key algorithm** (brute-force cosine, O(D × C × d)):
  - Acceptable for ≤ 10K `CodeFile` nodes (build-time, run once)
  - Capped by `maxSemanticFanOut` per doc node to limit edge count
  - Skips doc/code nodes without embeddings or with empty vectors

### Check Criteria

**Tests to run**:
- `cabal test` — all Hspec cases in updated `InferSpec.hs`
- `cabal build --flag dev` with `-Wall -Werror` — clean, no warnings

**Spec scenarios satisfied**:
- `Scenario: Doc node matches code node by embedding` (spec `semantic-edge-inference` § "Doc node matches code node by embedding")
- `Scenario: Below-threshold match is dropped` (spec `semantic-edge-inference` § "Below-threshold match is dropped")
- `Scenario: Fan-out cap respected` (spec `semantic-edge-inference` § "Fan-out cap respected", spec `bounded-edge-inference` § "Cap respected on high-fan-out doc node")
- `Scenario: Fan-out cap configurable` (spec `bounded-edge-inference` § "Cap configurable")
- `Scenario: Missing embedding skips doc node` (spec `semantic-edge-inference` § "Missing embedding skips doc node")

**PASS conditions**:
- Function signature: `inferSemanticCodeDocEdges :: Graph -> Map NodeId [Double] -> [Edge]`
- A doc node with cosine 0.82 to a code node emits a `References` edge with `confidence = 0.82`
- A doc node with max cosine 0.4 (below threshold 0.5) emits zero edges
- A doc node with cosine > threshold to 80 code nodes emits exactly 50 edges when `maxSemanticFanOut = 50`
- A doc node with no embedding entry emits zero edges (no error, no crash)
- A doc node with an empty-vector embedding emits zero edges (no error, no crash)
- Edges are deduplicated on `(source, target)` — duplicate pairs produce exactly one edge
- Result edges use `makeInferredEdge` with `References` type and cosine as confidence
- `maxSemanticFanOut` is configurable and applied before sorting (top-k after filtering)
- The function is exported from `Infer` module

**FAIL boundaries**:
- If the function emits edges for doc nodes without embeddings (or with empty vectors), the test fails — these should be silently skipped
- If the function ignores `maxSemanticFanOut` and emits all matching pairs above threshold, the test fails
- If edges have confidence ≠ the actual cosine similarity score, the test fails
- If cosine similarity is not properly normalized (e.g., dot product instead of cosine), the test fails
- If the function crashes on empty input graphs, the test fails

### Affected modules

- **Modified**: `src/Graphos/UseCase/Infer.hs` — add `inferSemanticCodeDocEdges`, export it
- **Modified**: `test/Graphos/UseCase/InferSpec.hs` — add Hspec test cases
- **Imports from**: `src/Graphos/Domain/Graph/Core.hs` (Graph, NodeId, Node, Edge, FileType), `src/Graphos/UseCase/Infer.hs` existing helpers (`makeInferredEdge`, `dedupOn`, `cosineSimilarity`)

### Prerequisites

- `cosineSimilarity :: [Double] -> [Double] -> Double` exists and is tested (from `IngestIndex` or shared utility)
- `makeInferredEdge :: NodeId -> NodeId -> EdgeType -> Double -> Edge` exists in `Infer`
- `dedupOn :: (Edge -> Key) -> [Edge] -> [Edge]` exists in `Infer`
- `NodeType` includes `DocFile` and `CodeFile` constructors in `Domain/Graph/Core.hs`
- `nodeFileType :: Node -> FileType` accessor exists

### Risks

- **Medium**: Performance — O(D × C × d) is acceptable for ≤ 10K code nodes but will be too slow at larger scales. This is explicitly scoped as a temporary solution with a scale guard planned in Task 4.
- **Medium**: Embedding vector normalization — cosine similarity requires normalized vectors or proper normalization in the function. Must verify existing `cosineSimilarity` handles unnormalized vectors correctly.
- **Low**: Reusing `makeInferredEdge` and `dedupOn` follows existing patterns; low risk of integration issues
- **Low**: Function is pure (no IO) — fits cleanly in the UseCase layer
