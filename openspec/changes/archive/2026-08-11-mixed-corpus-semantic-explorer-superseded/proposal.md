## Why

Graphos already extracts code (LSP) and knowledge artifacts (PDF/MD/office/vision) into one
unified `graph.json`, and `inferCodeDocEdges` already emits literal-name `References` edges
between `DocFile` and `CodeFile` nodes. But the linking is **exact-string only**: `auth.md`
matches `Auth.hs`, but never matches `login.ts` or a heading "JWT validation" against
`verifyToken()`. On real mixed corpora, Leiden therefore clusters code-islands and doc-islands
side by side — the "auth code and auth docs in the same cluster" outcome the product promises
only happens on lucky name collisions.

Three independent gaps compound this:

1. **No semantic edge inference.** `Infrastructure/LLM/Embedding.hs` and `IngestIndex` already
   generate and persist per-node embeddings — but only to `index.json` (the ingest sidecar),
   never into `graph.json`, and the pipeline's `inferEdges` never consults them. The embedding
   infrastructure is paid for and unused at clustering time.
2. **No cluster composition surface.** `Node.fileType` and `Node.kind` are first-class fields,
   but after Leiden runs nothing records "community 483 is 60% code + 40% docs" or "this cluster's
   docs reference these code modules." The explorer has no way to tell a code-dominant cluster
   from a mixed one.
3. **Explorer queries don't see the mix.** `query`/`path`/`explain`/`symbols`/`neighbors`
   operate on node IDs and labels; there is no `--filetype`, `--kind`, `--mixed-only`, or
   `--code-only` filter. And the single most useful explorer operation — *"I'm at node N,
   show me what I need to understand N"* — requires stitching five separate commands today.

We close all three now because the pieces already exist: embeddings are generated, `FileType`
is on every `Node`, `inferCodeDocEdges` shows the pattern, and the query family already has a
uniform JSON contract (`query-cli-contract`). Doing it now means the HTML viewer (the human
explorer surface) and the CLI/MCP (the agent surface) both get a real mixed-corpus explorer
without architectural change.

## What Changes

### Layer 1 — Semantic edge inference (build time)

- **Persist embeddings into `graph.json`** so the query/cluster path can use them, not just the
  ingest sidecar. New optional `embeddings :: Maybe (Map NodeId [Double])` field on the graph
  container (additive; absent on existing graphs — loads as `Nothing`).
- **New `inferSemanticCodeDocEdges`** in `UseCase/Infer.hs`: for each `DocFile` node, embed its
  label, find top-k similar `CodeFile` nodes by cosine similarity, emit `References` edges with
  confidence = cosine score, gated by `maxLabelFanOut` and a new `maxSemanticFanOut` cap. Reuses
  existing `generateEmbedding` + `cosineSimilarity` infrastructure.
- **Auto-detect single-corpus mode**: if the graph contains only `CodeFile` or only `DocFile`
  nodes, skip the semantic inference pass entirely (no work to do). Override with
  `--force-semantic-edges` for pipelines that want the pass even on homogeneous corpora (rare).
- **New config flag `--no-semantic-edges`** and `semantic_edges.enabled` in `graphos.yaml`
  (default: `true` when embeddings are available, i.e. `--embed` was used or `embedding.enabled`
  is set). Gate is AND-ed: embeddings must be on AND semantic edges must be on.
- **Scale guard**: at >10K nodes, brute-force O(D × C × d) cosine becomes real cost. Use the
  existing `IngestIndex.searchSimilar` (already sorted, top-N) as the ANN substitute. At >100K,
  document the recommendation to use a smaller embedding model or raise the similarity threshold.

### Layer 2 — Cluster composition metadata (post-clustering)

- **New `CommunityComposition` record** in `Domain/Community`:
  ```
  CommunityComposition
    { ccCodeCount    :: Int      -- CodeFile nodes
    , ccDocCount     :: Int      -- DocFile + PaperFile + OfficeFile nodes
    , ccOtherCount   :: Int      -- Image/Video/Audio
    , ccDominantKind :: Maybe Text  -- most frequent nodeKind ("function", "section", ...)
    , ccMixedRatio   :: Double   -- min(code,doc) / max(code,doc); 0 = pure, 1 = balanced
    , ccCodeDocEdges :: Int      -- References edges inside community crossing code↔doc
    }
  ```
- **Compute during `analyzeGraph`** (post-Leiden, alongside `nodeDegree`/`nodeIsBridge`).
  Store in a new `Map CommunityId CommunityComposition` field on the analysis result, persisted
  to `graph.json` alongside `communities`.
- **Surface in HTML viewer**: cluster badge `🔧 12 / 📄 4 / 🌉 3` (code count / doc count /
  cross-type edges). Composable with the in-flight `add-profondeur-view-selector` depth control.
- **Composition-aware LLM labeling**: extend `labelPrompt` in `Domain/Labeling.hs` to pass the
  cluster's composition to the LLM — *"Community 483 (cohesion: 0.72, mixed: 60% code + 40%
  docs): Top code nodes: verifyToken, AuthMiddleware. Top doc nodes: 'JWT validation', 'Auth
  flow'. Name the concept that unifies these."* The prompt today only sees labels; the new
  prompt tags each top node with `(code)` or `(doc)` so the LLM can name the shared concept.

### Layer 3 — Explorer queries (query-time)

- **New filter flags on the query family** (`query`, `symbols`, `neighbors`, and the new
  `around`/`cluster` commands):
  - `--filetype code|doc|paper|image|...` — filter results to a `FileType`
  - `--kind function|module|section|paragraph|...` — filter results to a `nodeKind`
  - `--mixed-only` — only nodes in communities with `ccMixedRatio > 0`
  - `--code-only` / `--doc-only` — shorthand for `--filetype code` / `--filetype doc`
- **New `graphos around <node>` subcommand**: the "what do I need to understand N" bundle in
  one JSON document:
  ```
  AroundResponse
    { arNode         :: Node
    , arInEdges      :: [(Edge, Node)]   -- dependencies of N (imports, calls)
    , arOutEdges     :: [(Edge, Node)]   -- dependents of N (who breaks if N changes)
    , arCommunity    :: Maybe (CommunityId, Text, CommunityComposition)
    , arBridges      :: [NodeId]         -- articulation points in N's community
    , arDepth        :: Int              -- neighborhood depth used
    }
  ```
  Uses the existing `neighbors`, `communityOfNode`, `articulationPoints`, `explainNode` —
  one orchestration call, one JSON. Fits the `query-cli-contract` JSON shape (single doc,
  no interleaved logs, `--json` honored).
- **New `graphos cluster <id>` subcommand**: full community composition + members grouped by
  `FileType` then `nodeKind`. Returns `ClusterResponse { composition, membersByKind, bridges,
  crossTypeEdges }`. Also honors `--json`.
- **HTML viewer**: search results and cluster drill-down render the composition badge; the
  depth selector (`add-profondeur-view-selector`) gains a "Mixed only" filter toggle in the
  header that calls `GET /api/query?q=...&filetype=any&mixed-only=1` (composes with the
  in-flight `add-query-api-port-and-view` HTTP port).

### Documentation

- New section in project docs listing **alternative embedding models** beyond the default
  `nomic-embed-text`: `all-minilm` (faster, lower quality), `bge-m3` (multilingual, better
  code+prose), `voyage-code-2` (code-specialized, hosted), `text-embedding-3-small` (OpenAI,
  hosted). Trade-offs table: local vs hosted, dimension, code-prose quality, latency.

## Capabilities

### New Capabilities

- `semantic-edge-inference`: Build-time inference of `References` edges between `DocFile` and
  `CodeFile` nodes using embedding cosine similarity, gated by `--embed` + `semantic_edges.enabled`,
  auto-skipped on single-corpus graphs, capped by `maxSemanticFanOut`.
- `cluster-composition`: Post-clustering computation of per-community `CommunityComposition`
  (code/doc/other counts, dominant kind, mixed ratio, cross-type edge count) persisted to
  `graph.json` and surfaced in the HTML viewer as a cluster badge.
- `explorer-queries`: New `around` and `cluster` subcommands + `--filetype`/`--kind`/`--mixed-only`
  filter flags on the query family, returning the existing `QueryResponse`/`AroundResponse`/
  `ClusterResponse` JSON contract.

### Modified Capabilities

- `embedding`: Embeddings are now persisted into `graph.json` (optional field, absent on
  legacy graphs) in addition to the existing `index.json` sidecar, so the cluster + query
  paths can use them.
- `llm-labeling`: `labelPrompt` now tags each top node with `(code)` or `(doc)` and includes
  the composition summary, so the LLM names the unifying concept of mixed clusters rather
  than the most frequent word.
- `query-cli-contract`: The uniform flag surface gains `--filetype`, `--kind`, `--mixed-only`,
  `--code-only`, `--doc-only` across the query family; new `around` and `cluster` subcommands
  honor `--json`, `--budget`, `--label-width`, `--edges`.
- `bounded-edge-inference`: The new `inferSemanticCodeDocEdges` reuses the `maxLabelFanOut` cap
  pattern and adds `maxSemanticFanOut` (default 50) to bound cosine-search fan-out.

## Impact

- **Code**:
  - `src/Graphos/Domain/Graph/Core.hs` — optional `gEmbeddings :: Maybe (Map NodeId [Double])`
    on `Graph` (additive).
  - `src/Graphos/UseCase/Infer.hs` — new `inferSemanticCodeDocEdges :: Graph -> Map NodeId
    [Double] -> [Edge]`, wired into `inferEdges` when embeddings present and density >= Normal.
  - `src/Graphos/UseCase/Pipeline/Core.hs` — pass embeddings from `IngestResult` into the graph
    build step; detect single-corpus mode; gate semantic inference.
  - `src/Graphos/Domain/Community.hs` (or new `Composition.hs`) — `CommunityComposition` record
    + `computeCompositions :: Graph -> CommunityMap -> Map CommunityId CommunityComposition`.
  - `src/Graphos/UseCase/Analysis.hs` (or equivalent) — call `computeCompositions` post-Leiden,
    persist to `graph.json`.
  - `src/Graphos/Domain/Labeling.hs` — `labelPrompt` tags nodes with `(code)`/`(doc)`, includes
    composition summary.
  - `src/Graphos/UseCase/Query.hs` — new `aroundNode` and `clusterDetail` orchestration functions;
    new `AroundResponse` / `ClusterResponse` types with `ToJSON`.
  - `src/Graphos/CLI/Parser.hs` — new `around`/`cluster` subcommands; `--filetype`/`--kind`/
    `--mixed-only`/`--code-only`/`--doc-only` flags on query family; `--no-semantic-edges` on
    pipeline; `--force-semantic-edges` override.
  - `src/Graphos/Infrastructure/Export/HTML.hs` — render composition badge on community dots and
    drill-downs; "Mixed only" toggle in header.
  - `app/Main.hs` — dispatch new subcommands and flags; embed composition map into `graph.json`.
- **APIs**: New CLI subcommands (`around`, `cluster`) and flags (additive, no breaking change).
  `graph.json` gains optional `embeddings` and `compositions` fields (additive; legacy graphs
  load as `Nothing`/empty). HTTP `/api/around`, `/api/cluster` added to the in-flight
  `add-query-api-port-and-view` HTTP port family.
- **Dependencies**: No new Haskell libraries. Reuses `aeson`, `containers`, existing
  `Infrastructure.LLM.Embedding`. ANN is delegated to the existing `IngestIndex.searchSimilar`
  (sorted top-N) — no HNSW dependency in this change.
- **Tests**:
  - `inferSemanticCodeDocEdges` emits edges only between `DocFile` and `CodeFile`, respects
    `maxSemanticFanOut`, confidence = cosine score.
  - Single-corpus auto-detect: pure-code graph produces zero semantic edges even when
    embeddings present.
  - `computeCompositions`: `ccMixedRatio` = 0 on pure corpus, > 0 on mixed, `ccCodeDocEdges`
    counts only `References` edges crossing `CodeFile`↔`DocFile`.
  - `labelPrompt` includes `(code)`/`(doc)` tags and composition line.
  - `aroundNode` returns the expected fields; `--json` emits a single document.
  - `--filetype`/`--kind`/`--mixed-only` filters narrow results correctly.
  - Legacy `graph.json` without `embeddings`/`compositions` loads and queries without error.
  - `-Wall -Werror` clean; `cabal test` green.
- **Build**: New modules + extended flags; no new dependency.

## PDCA Cycle

- **Plan**: Hypothesis — semantic embedding-based edge inference makes mixed code+docs
  clustering actually work in the common case (different names, same concept), and cluster
  composition + explorer filters + `around`/`cluster` primitives make the result navigable by
  humans (HTML) and agents (CLI/MCP) on 1K–10K and 158K-node graphs. Success measured by:
  (a) on a mixed corpus where literal-name inference produces N code↔doc edges, semantic
  inference produces ≥ 3N such edges with cosine ≥ 0.5;
  (b) Leiden communities on that corpus have `ccMixedRatio > 0` for at least 60% of non-trivial
  communities (size ≥ 5);
  (c) `graphos around <node> --json` returns in < 500ms on a 10K-node graph (PRD §16.1);
  (d) HTML viewer shows the composition badge on every community and the "Mixed only" toggle
  filters correctly;
  (e) `--no-semantic-edges` reproduces today's behavior; single-corpus graphs skip the pass
  automatically.

- **Do**: Implement the three layers in order (semantic edges → composition → explorer
  queries), keeping all IO in Infrastructure (embedding fetch) and all pure logic in Domain
  (composition, prompt) and UseCase (orchestration, filters). Persist embeddings + compositions
  into `graph.json`. Extend `labelPrompt`. Add `around`/`cluster` subcommands and the filter
  flags. Document alternative embedding models in project docs.

- **Check**: (1) `cabal test` passes with the new Hspec cases above; (2) build a mixed corpus
  (e.g. this repo + its `docs/` folder) and confirm Leiden produces mixed communities; (3) time
  `graphos around` on a 10K-node graph (< 500ms); (4) HTML viewer shows badges and the "Mixed
  only" toggle; (5) `--no-semantic-edges` and single-corpus auto-skip both reproduce today's
  behavior; (6) `-Wall -Werror` clean.

- **Act**: If semantic inference at >10K nodes is too slow with `IngestIndex.searchSimilar`,
  feed that into a follow-up change for a real ANN index (HNSW/IVF via a `vector-search`
  dependency). If `ccMixedRatio` is still 0 on real corpora, the threshold (0.5 cosine) is too
  high — lower it and re-measure. If the LLM labels don't improve with composition tags,
  iterate the prompt (the signal is there; the prompt may need to name the concept explicitly).
  If `around` is widely used by agents, standardize it as the primary entry point of the
  `graphos-query` skill (replacing `query` as the recommended first call).

## Relationship to in-flight changes

- **`add-query-api-port-and-view`** (in progress): the HTTP `/api/*` port family gains
  `/api/around` and `/api/cluster` as new endpoints in the same family. No conflict — this
  change adds endpoints to the same Warp app once that change lands.
- **`add-profondeur-view-selector`** (in progress): the depth selector and the composition
  badge compose — the badge renders at every depth level (overview dots, community drill-down,
  full graph). No conflict; the badge is an additional overlay, not a depth-mode change.
- **`fix-mcp-query-perf-and-correctness`** (in progress): threads `GraphIndex` + `CachedFGL`
  through the MCP server. The new `around`/`cluster` MCP tools reuse the same threaded
  infrastructure. The embedding map would also be threaded if MCP serves semantic queries —
  deferred to a follow-up unless this change and that one merge concurrently.
- **`optimise-community-detection-large-graph`** (complete): the Leiden scalability work is
  the foundation — semantic edges add E to the Leiden input, but the scalable Leiden path
  already handles large E. No rework.