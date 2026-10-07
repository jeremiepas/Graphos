## MODIFIED Requirements

### Requirement: Semantic code-doc edge inference

The system SHALL provide semantic code↔doc edge inference which, for each `DocFile` node with an embedding, finds the top-k `CodeFile` nodes by cosine similarity above a threshold (default 0.5) and emits `References` edges with confidence equal to the cosine score. The inference SHALL respect `maxSemanticFanOut` (default 50) as the maximum number of code nodes matched per doc node and SHALL skip doc nodes whose embedding is absent or empty. Vectors SHALL be consumed as compact unboxed vectors, and doc vectors SHALL be streamed one doc at a time so peak live vectors are bounded by one doc row plus the code table (bounded-embedding-memory); the emitted edges MUST equal the all-pairs formulation's for the same inputs.

#### Scenario: Doc node matches code node by embedding
- **WHEN** a `DocFile` node labeled "JWT validation" has an embedding with cosine similarity
  0.82 to a `CodeFile` node `fn_verifyToken`
- **THEN** the inference emits a `References` edge from `fn_verifyToken` to the
  doc node with confidence 0.82

#### Scenario: Below-threshold match is dropped
- **WHEN** the highest cosine similarity between a doc node and any code node is 0.4
  (threshold 0.5)
- **THEN** no semantic edge is emitted for that doc node

#### Scenario: Fan-out cap respected
- **WHEN** a doc node has cosine similarity > 0.5 with 80 code nodes and
  `maxSemanticFanOut = 50`
- **THEN** only the top-50 code nodes by similarity receive `References` edges

#### Scenario: Missing embedding skips doc node
- **WHEN** a `DocFile` node has no entry in the embeddings map (or an empty vector)
- **THEN** no semantic edge is emitted for that doc node (no error)

#### Scenario: Streaming formulation preserves edge output
- **WHEN** the streaming doc-at-a-time inference runs on a mixed doc/code graph with embeddings
- **THEN** the emitted edges equal the all-pairs formulation's for the same inputs (up to the existing dedup and sort key)