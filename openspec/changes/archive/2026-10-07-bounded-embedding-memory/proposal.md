## Why

The embedding pass crashes with OOM on large codebases (100k+ nodes). The vector
tables held live simultaneously in `UseCase.Pipeline.Core.generateGraphEmbeddings`
are each a full copy of the graph's vectors: `cachedTable`, `freshTable`,
`vecTable`, the final `assignment`, and `gEmbeddings` held resident through
clustering — at 768 dims in boxed `[Double]` (~40-48 bytes/dim with cons cells),
one copy of a 100k-node graph is ~3-4 GB, so the pass peaks at 15-20 GB plus the
sidecar JSON. `fix-runtime-ram-crash` bounded extraction, observability, and the
Node representation, but never the embedding path — this change closes that gap.

## What Changes

- **Unboxed vector representation** — embedding vectors switch from boxed
  `[Double]` lists to unboxed vectors, dropping per-dimension cost from ~40-48 B
  to 8 B (~5× less memory) with no change to the persisted JSON format.
- **Single-copy streaming assignment** — the write-at-end path is replaced: the
  streaming sidecar path becomes the only path, and the node-to-vector
  assignment is assembled incrementally per batch so no intermediate
  `freshTable`/`vecTable` copy of all vectors is ever materialized.
- **Post-pass embedding release** — the graph's `gEmbeddings` table is detached
  after the semantic-edge inference pass consumes it, so clustering and export
  no longer hold the full vector table live (vectors remain on disk in the
  sidecar for later loads).
- **Bounded semantic inference** — the O(docs × code) all-pairs cosine pass
  streams doc vectors one at a time and caps peak live vectors at one doc row
  plus the code table reference, instead of forcing the whole embedding map.

## Capabilities

### New Capabilities

- `bounded-embedding-memory`: Memory-bounded embedding representation and
  lifecycle — unboxed vectors, single-copy streaming assignment, post-pass
  release, and peak-memory accounting for the `--embed` pipeline path.

### Modified Capabilities

- `embedding`: The batched API surface requirement is extended — vectors
  returned and stored by the client/assignment are unboxed vectors rather than
  boxed lists; the write-at-end sidecar mode is superseded by always-streaming.
- `embedding-throughput`: The "streaming sidecar write" default and the
  sequential-equivalence requirement are updated — streaming is the only sidecar
  write mode and equivalence now covers the single-copy assignment assembly.
- `semantic-edge-inference`: The inference entry point now operates over the
  unboxed vector representation with a bounded peak-vector accounting.

## Impact

- **Code**: `UseCase.Pipeline.Core` (`generateGraphEmbeddings`,
  `embedChunk`, sidecar write helpers), `UseCase.Ingest`
  (`generateEmbeddingsForNodes`), `UseCase.Infer`
  (`inferSemanticCodeDocEdges`), `UseCase.Port.LLMPort` (`cosineSimilarity`
  signature), `Domain.Types` / `Domain.Graph` (`gEmbeddings` type),
  `Infrastructure.LLM.Embedding`, `Infrastructure.FileSystem.EmbeddingCache`,
  export/Neo4j/MCP readers of `gEmbeddings`.
- **API**: `Map NodeId [Double]` → `Map NodeId (VU.Vector Double)` for
  in-memory embeddings; JSON sidecar, cache files, and `graph.json` wire format
  unchanged (arrays of numbers).
- **Performance**: Peak `--embed` memory at 100k nodes × 768 dims drops from
  ~15-20 GB to under ~2 GB; sidecar and cache behavior unchanged.
- **Compatibility**: No wire-format changes; legacy `[Double]` API callers get
  type errors at compile time (internal API, not a public CLI break).