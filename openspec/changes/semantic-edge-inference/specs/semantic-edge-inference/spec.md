# semantic-edge-inference

Build-time inference of `References` edges between `DocFile` and `CodeFile` nodes using
embedding cosine similarity, gated by embedding availability and `semantic_edges.enabled`,
auto-skipped on single-corpus graphs, capped by `maxSemanticFanOut`.

## ADDED Requirements

### Requirement: Embeddings persisted to graph output sidecar

The pipeline SHALL persist per-node embedding vectors to an `embeddings.json` sidecar in the
output directory (alongside `graph.json`) and SHALL record the sidecar path in `graph.json`
via an optional `embeddings_path` field. The graph loader SHALL follow the pointer and load
embeddings when the sidecar exists, returning `gEmbeddings = Nothing` when the pointer is
absent or the file is missing. Legacy `graph.json` files without the pointer SHALL load
unchanged. The existing `index.json` ingest sidecar SHALL continue to be written for backward
compatibility with the ingest-index capability.

#### Scenario: Pipeline writes embeddings sidecar
- **WHEN** the pipeline runs with `--embed` on a graph with 1,000 nodes
- **THEN** the output directory contains `graph.json` with `"embeddings_path": "embeddings.json"`
  and `embeddings.json` with 1,000 entries keyed by `NodeId`

#### Scenario: Legacy graph loads without embeddings
- **WHEN** a `graph.json` without `embeddings_path` is loaded
- **THEN** the loaded graph has `gEmbeddings = Nothing` and queries succeed without error

#### Scenario: Sidecar missing but pointer present
- **WHEN** `graph.json` has `"embeddings_path": "embeddings.json"` but the file is missing
- **THEN** the loader logs a warning and returns `gEmbeddings = Nothing` (not an error)

### Requirement: Semantic code-doc edge inference

The system SHALL provide `inferSemanticCodeDocEdges :: Graph -> Map NodeId [Double] -> [Edge]`
which, for each `DocFile` node with an embedding, finds the top-k `CodeFile` nodes by cosine
similarity above a threshold (default 0.5) and emits `References` edges with confidence equal
to the cosine score. The function SHALL respect `maxSemanticFanOut` (default 50) as the
maximum number of code nodes matched per doc node and SHALL skip doc nodes whose embedding is
absent or empty.

#### Scenario: Doc node matches code node by embedding
- **WHEN** a `DocFile` node labeled "JWT validation" has an embedding with cosine similarity
  0.82 to a `CodeFile` node `fn_verifyToken`
- **THEN** `inferSemanticCodeDocEdges` emits a `References` edge from `fn_verifyToken` to the
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

### Requirement: Single-corpus auto-skip

The pipeline SHALL detect single-corpus graphs (all nodes share one `FileType`) and SHALL
skip the semantic inference pass automatically, logging a single info line. The
`--force-semantic-edges` flag SHALL override the auto-skip and run the pass regardless.

#### Scenario: Pure-code graph skips semantic inference
- **WHEN** the graph contains only `CodeFile` nodes and `--force-semantic-edges` is not set
- **THEN** the pipeline logs "single-corpus graph detected, skipping semantic edge inference"
  and emits zero semantic edges

#### Scenario: Force flag overrides auto-skip
- **WHEN** the graph contains only `CodeFile` nodes and `--force-semantic-edges` is set
- **THEN** the semantic inference pass runs (and produces zero edges, since there are no doc
  nodes to match — but the pass is not skipped)

### Requirement: Config gating

The `semantic_edges.enabled` config field (default: `true`) SHALL gate the semantic inference
pass. The pass runs only when `semantic_edges.enabled == true` AND embeddings are available
(`gEmbeddings` is `Just` non-empty). The `--no-semantic-edges` CLI flag SHALL override the
config to `false` for one run.

#### Scenario: Embeddings disabled skips semantic pass
- **WHEN** `gEmbeddings == Nothing` (no embeddings were generated)
- **THEN** no semantic edges are inferred, regardless of `semantic_edges.enabled`

#### Scenario: Config disabled skips semantic pass
- **WHEN** `semantic_edges.enabled == false` in `graphos.yaml`
- **THEN** no semantic edges are inferred, even if embeddings are available

#### Scenario: No-semantic-edges flag overrides config
- **WHEN** `graphos.yaml` has `semantic_edges.enabled: true` but the user passes
  `--no-semantic-edges`
- **THEN** no semantic edges are inferred for that run

### Requirement: Scale guard at 10K code nodes

When the number of `CodeFile` nodes exceeds 10,000, the pipeline SHALL log a warning that
semantic inference may be slow and SHALL fall back to literal-name inference only (today's
`inferCodeDocEdges`), unless `--force-semantic-edges` is set. This is a temporary guard
pending a follow-up ANN-index change.

#### Scenario: Large code graph falls back
- **WHEN** the graph has 15,000 `CodeFile` nodes and `--force-semantic-edges` is not set
- **THEN** the pipeline logs "semantic inference capped at 10K code nodes, falling back to
  literal-name inference" and runs only `inferCodeDocEdges`

#### Scenario: Force flag overrides the cap
- **WHEN** the graph has 15,000 `CodeFile` nodes and `--force-semantic-edges` is set
- **THEN** the semantic inference pass runs (slowly) without falling back