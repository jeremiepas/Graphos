# Tasks — Semantic Edge Inference

## 1. Embeddings persisted to graph output

### 1.1 Add embedding fields to Graph
- [x] Add `gEmbeddings :: Maybe (Map NodeId [Double])` and `gEmbeddingsPath :: Maybe FilePath` to `Graph` in `src/Graphos/Domain/Graph/Core.hs` (additive; `Nothing` defaults)
- [x] Update `Graph` `ToJSON` to write `embeddings_path` field (omit when `Nothing`)
- [x] Update `Graph` `FromJSON` to read `embeddings_path` (default `Nothing` when absent)
- [x] Hspec: `Graph` round-trips with and without `embeddings_path`; legacy JSON without the field loads as `Nothing`

### 1.2 Write embeddings sidecar in pipeline
- [x] In `src/Graphos/UseCase/Pipeline/Core.hs`: after building the graph, if embeddings were generated, write `embeddings.json` to the output dir (JSON object: `Map NodeId [Double]`)
- [x] Set `gEmbeddingsPath = Just "embeddings.json"` on the output graph
- [x] If no embeddings, leave `gEmbeddingsPath = Nothing`
- [x] Hspec: pipeline with `--embed` produces both `graph.json` (with pointer) and `embeddings.json`; without `--embed` produces only `graph.json` (no pointer)

### 1.3 Load embeddings in loadGraphFromFile
- [x] In `src/Graphos/UseCase/Load.hs` `loadGraphFromFile`: after loading `graph.json`, if `gEmbeddingsPath = Just path`, read the sidecar file and populate `gEmbeddings = Just ...`
- [x] If sidecar file missing, log warning and set `gEmbeddings = Nothing` (not an error)
- [x] Hspec: graph with sidecar loads embeddings; missing sidecar warns + `Nothing`; legacy graph without pointer loads `Nothing`

## 2. inferSemanticCodeDocEdges

### 2.1 Implement the function
- [ ] Add `inferSemanticCodeDocEdges :: Graph -> Map NodeId [Double] -> [Edge]` in `src/Graphos/UseCase/Infer.hs`
- [ ] Collect `DocFile` nodes with non-empty embeddings
- [ ] Collect `CodeFile` nodes with non-empty embeddings
- [ ] For each doc node: compute `cosineSimilarity docVec codeVec` against all code nodes, filter >= 0.5, sort desc, take top `maxSemanticFanOut`, emit `References` edges with confidence = cosine
- [ ] Use existing `makeInferredEdge` helper with `References` edge type
- [ ] Dedup on `(source, target)` via existing `dedupOn`
- [ ] Export `inferSemanticCodeDocEdges` from module

### 2.2 Test the function
- [ ] Hspec: doc node with cosine 0.82 to code node emits `References` edge with confidence 0.82
- [ ] Hspec: below-threshold (0.4) match emits no edge
- [ ] Hspec: 80 code nodes above threshold with `maxSemanticFanOut = 50` emits only top-50
- [ ] Hspec: doc node with no embedding emits no edge (no error)
- [ ] Hspec: doc node with empty-vector embedding emits no edge (no error)

## 3. Single-corpus auto-skip

### 3.1 Implement isSingleCorpus
- [ ] Add `isSingleCorpus :: Graph -> Bool` in `src/Graphos/UseCase/Infer.hs` (or `Domain/Graph/Core.hs`)
- [ ] Returns `True` when all nodes share one `FileType`
- [ ] Hspec: all-`CodeFile` → `True`; mixed → `False`; all-`DocFile` → `True`

## 4. Pipeline wiring + gating

### 4.1 Config fields
- [ ] Add `SemanticEdgesConfig` (or fields on existing config) in `src/Graphos/Domain/Config/`: `seEnabled :: Bool` (default `True`), `seMaxFanOut :: Int` (default 50), `seThreshold :: Double` (default 0.5)
- [ ] `FromJSON` parses `semantic_edges:` section from `graphos.yaml` with defaults
- [ ] Hspec: config round-trips; missing section uses defaults

### 4.2 CLI flags
- [ ] Add `--no-semantic-edges` and `--force-semantic-edges` switches in `src/Graphos/CLI/Parser.hs` (pipeline command)
- [ ] Wire to override config: `--no-semantic-edges` → `seEnabled = False`; `--force-semantic-edges` → bypass scale cap + auto-skip
- [ ] Hspec: parser accepts both flags; `--help` lists them

### 4.3 Wire into inferEdges
- [ ] In `src/Graphos/UseCase/Infer.hs` `inferEdges`: after existing inferences, if `gEmbeddings = Just embs` AND `seEnabled` AND (`codeNodeCount <= 10000` OR `force`) AND NOT (`isSingleCorpus` AND NOT `force`):
  - Call `inferSemanticCodeDocEdges g embs`
  - Merge result into the edge list
- [ ] Log: `"semantic edges: inferred N (cap=M, threshold=0.5, mode=auto-skip|forced|fallback|disabled)"`
- [ ] When `codeNodeCount > 10000` AND NOT `force`: log `"semantic inference capped at 10K code nodes, falling back to literal-name inference"` and skip semantic pass
- [ ] When `isSingleCorpus` AND NOT `force`: log `"single-corpus graph detected, skipping semantic edge inference"` and skip
- [ ] Hspec: embeddings present + enabled → semantic edges in output; `seEnabled = False` → no semantic edges; single-corpus → skip log + 0 edges; `--force-semantic-edges` on single-corpus → runs pass (0 edges); 15K code nodes without force → fallback log + literal-only

## 5. Documentation

### 5.1 Embedding models doc
- [x] Create `docs/embedding-models.md` with trade-offs table: model name, local/hosted, dimension, code-prose quality, latency, cost
- [x] Models: `nomic-embed-text` (default, local, 768-dim), `all-minilm` (local, 384-dim, faster, lower quality), `bge-m3` (local, 1024-dim, multilingual, better code+prose), `voyage-code-2` (hosted, 1536-dim, code-specialized), `text-embedding-3-small` (OpenAI, 1536-dim, hosted)
- [x] Note: for semantic code↔doc edges, a model that embeds code identifiers AND prose into a shared space is required — `nomic-embed-text` works but `bge-m3` or `voyage-code-2` recommended for mixed corpora
- [x] Show how to set via `embedding.model` in `graphos.yaml`

## 6. Build + cross-cutting

### 6.1 Legacy graph compatibility
- [ ] Verify: `graph.json` without `embeddings_path` loads; `gEmbeddings = Nothing`; all existing query-family commands work
- [ ] Hspec: legacy graph fixture (from `graphos-out/` or test data) loads and queries without error

### 6.2 Build + warnings
- [ ] `cabal build` with `-Wall -Werror` clean
- [ ] `cabal test` green (existing tests + new Hspec cases)

### 6.3 Manual mixed-corpus verification
- [ ] Build a mixed corpus: this repo (code) + `docs/` (markdown) with `--embed`
- [ ] Confirm: `embeddings.json` sidecar exists; `graph.json` has `embeddings_path`
- [ ] Confirm: semantic `References` edges appear in the graph between docs and code with different names
- [ ] Confirm: `--no-semantic-edges` reproduces today's clustering (literal-name only)
- [ ] Confirm: single-corpus (code-only) run skips semantic pass automatically