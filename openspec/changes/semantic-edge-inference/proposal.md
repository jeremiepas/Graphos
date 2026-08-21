## Why

`inferCodeDocEdges` in `UseCase/Infer.hs` links `DocFile` nodes to `CodeFile` nodes by **exact
string match** on labels and file basenames. This works for `auth.md` ↔ `Auth.hs` but never
for `docs/auth.md` ↔ `login.ts` or for a heading "JWT validation" ↔ `verifyToken()` — the
common case where docs and code describe the same concept with different vocabulary. On real
mixed corpora, Leiden therefore clusters code-islands and doc-islands side by side; the
"auth code and auth docs in the same cluster" outcome only happens on lucky name collisions.

The infrastructure to fix this already exists and is paid for:
`Infrastructure/LLM/Embedding.hs` generates per-node embeddings, `IngestIndex.searchSimilar`
does cosine search, and the ingest pipeline already stores vectors in `index.json`. But
embeddings are **never persisted to `graph.json`** and `inferEdges` **never consults them** —
the embedding pass is dead weight at clustering time. This change wires them in.

This is the foundation of mixed-corpus exploration: it makes the *graph* cluster code+docs
together. Subsequent changes (`cluster-composition`, `explorer-queries`) make the cluster
*composition visible* and the *explorer navigable*, but without this change those operate on
a graph that still separates code and docs.

## What Changes

- **Persist embeddings to `graph.json`** via a pointer + sidecar pattern: `graph.json` gains
  an optional `embeddings_path` field pointing to `embeddings.json` in the same output
  directory. The loader follows the pointer and populates `gEmbeddings`; legacy graphs
  without the pointer load as `Nothing`. Keeps `graph.json` small (158K × 768 × 8B ≈ 966MB
  stays in the sidecar, not the main file).
- **New `inferSemanticCodeDocEdges :: Graph -> Map NodeId [Double] -> [Edge]`** in
  `UseCase/Infer.hs`: for each `DocFile` node with an embedding, find top-k `CodeFile` nodes
  by cosine similarity above threshold 0.5, emit `References` edges with confidence = cosine.
  Respects `maxSemanticFanOut` (default 50). Reuses existing `cosineSimilarity`.
- **Wire into pipeline**: `inferEdges` calls `inferSemanticCodeDocEdges` when embeddings
  present AND `semantic_edges.enabled` AND code-node count ≤ 10K (unless `--force-semantic-
  edges`). Merges with existing `inferCodeDocEdges` (literal-name) — additive, not
  replacement.
- **Single-corpus auto-skip**: `isSingleCorpus :: Graph -> Bool` detects homogeneous graphs
  (all nodes one `FileType`); skips the semantic pass automatically. Override with
  `--force-semantic-edges`.
- **Scale guard**: at >10K `CodeFile` nodes, log a warning and fall back to literal-name only
  unless `--force-semantic-edges`. Brute-force cosine is O(D × C × d) — too slow at 100K. A
  follow-up change brings ANN (HNSW/IVF) for the >10K case.
- **Config**: new `semantic_edges.enabled` (default `true`) and `semantic_edges.max_fan_out`
  (default 50) in `graphos.yaml`; new `--no-semantic-edges` / `--force-semantic-edges` CLI
  flags.
- **Documentation**: new section in project docs listing alternative embedding models
  (`nomic-embed-text`, `all-minilm`, `bge-m3`, `voyage-code-2`, `text-embedding-3-small`)
  with a trade-offs table — the user picks via `embedding.model`. Semantic code↔doc edges
  need a model that embeds code identifiers AND prose into a shared space.

## Capabilities

### New Capabilities
- `semantic-edge-inference`: Build-time inference of `References` edges between `DocFile`
  and `CodeFile` nodes using embedding cosine similarity, gated by embedding availability
  and `semantic_edges.enabled`, auto-skipped on single-corpus graphs, capped by
  `maxSemanticFanOut`.

### Modified Capabilities
- `embedding`: Embeddings persisted to `embeddings.json` sidecar pointed to by `graph.json`'s
  `embeddings_path` field, in addition to the existing `index.json` ingest sidecar.
- `bounded-edge-inference`: New `maxSemanticFanOut` cap (default 50) bounds cosine-search
  fan-out per doc node, extending the existing `maxCommunityBridges` / `maxLabelFanOut` family.

## Impact

- **Code**:
  - `src/Graphos/Domain/Graph/Core.hs` — add `gEmbeddings :: Maybe (Map NodeId [Double])` and
    `gEmbeddingsPath :: Maybe FilePath` (additive, `Nothing` defaults)
  - `src/Graphos/Domain/Graph/Core.hs` — `ToJSON`/`FromJSON` write/read `embeddings_path`
  - `src/Graphos/UseCase/Infer.hs` — new `inferSemanticCodeDocEdges`, `isSingleCorpus`
  - `src/Graphos/UseCase/Pipeline/Core.hs` — write `embeddings.json` sidecar; wire
    `inferSemanticCodeDocEdges` into `inferEdges` with gating
  - `src/Graphos/UseCase/Load.hs` — follow `embeddings_path` pointer; warn + `Nothing` if missing
  - `src/Graphos/Domain/Config/*` — `semantic_edges.enabled`, `semantic_edges.max_fan_out`
  - `src/Graphos/CLI/Parser.hs` — `--no-semantic-edges`, `--force-semantic-edges`
  - `app/Main.hs` — dispatch new flags; write sidecar
  - `docs/embedding-models.md` (new) — alternative embedding model trade-offs table
- **APIs**: `graph.json` gains optional `embeddings_path` field (additive; legacy loads
  unchanged). New CLI flags (additive). New `semantic_edges` config section.
- **Dependencies**: No new Haskell libraries. Reuses `aeson`, `containers`, existing
  `Infrastructure.LLM.Embedding` + `cosineSimilarity`.
- **Tests**: Hspec for sidecar load/store, `inferSemanticCodeDocEdges` (match, below-
  threshold drop, fan-out cap, missing embedding skip), single-corpus auto-skip,
  scale-guard fallback, legacy graph load, config gating. `-Wall -Werror` clean.
- **Build**: New module sections + config fields; no new dependency.

## Relationship to other changes

- **`cluster-composition`** (planned): independent — that change computes composition
  metadata that's more useful when this change makes clusters mixed. Merge order: this first
  makes the other more valuable, but either can ship alone.
- **`explorer-queries`** (planned): independent — that change adds `around`/`cluster`
  commands + filter flags. `--mixed-only` there benefits from this change's mixed clusters
  but doesn't depend on it at the code level.
- **`fix-mcp-query-perf-and-correctness`** (in progress): independent — that change threads
  `GraphIndex` + `CachedFGL` through MCP; this change adds `gEmbeddings` to `Graph`. No file
  overlap except `Domain/Graph/Core.hs` (additive field, no conflict).