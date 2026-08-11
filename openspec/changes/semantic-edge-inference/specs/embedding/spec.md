# embedding

Delta — embeddings persisted to graph output sidecar for cluster + query use.

## MODIFIED Requirements

### Requirement: Embeddings persisted to graph output sidecar

The system SHALL persist per-node embedding vectors to an `embeddings.json` sidecar in the
output directory (alongside `graph.json`) and SHALL record the sidecar path in `graph.json`
via an optional `embeddings_path` field. The graph loader SHALL follow the pointer and load
embeddings when the sidecar exists, returning `gEmbeddings = Nothing` when the pointer is
absent or the file is missing. The existing `index.json` ingest sidecar SHALL continue to be
written for backward compatibility with the ingest-index capability.

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