# Tasks — Mixed-Corpus Semantic Explorer

## Layer 1: Semantic edge inference (build time)

### 1.1 Persist embeddings to graph output sidecar
- [ ] Add `gEmbeddings :: Maybe (Map NodeId [Double])` and `gEmbeddingsPath :: Maybe FilePath` to `Graph` in `Domain/Graph/Core.hs` (additive; `Nothing` defaults)
- [ ] Update `Graph` `ToJSON`/`FromJSON` to write `embeddings_path` pointer and load sidecar when present
- [ ] Modify `UseCase/Pipeline/Core.hs` to write `embeddings.json` sidecar alongside `graph.json` when embeddings were generated; set `gEmbeddingsPath` in the output graph
- [ ] Update `UseCase/Load.hs` `loadGraphFromFile` to follow `embeddings_path` pointer and populate `gEmbeddings`; warn + `Nothing` if file missing
- [ ] Hspec: graph with sidecar loads embeddings; legacy graph loads `Nothing`; missing sidecar warns and returns `Nothing`

### 1.2 Implement inferSemanticCodeDocEdges
- [ ] Add `inferSemanticCodeDocEdges :: Graph -> Map NodeId [Double] -> [Edge]` in `UseCase/Infer.hs`
- [ ] For each `DocFile` node with non-empty embedding, use `IngestIndex.searchSimilarThreshold` (or inline cosine over code-node embeddings) to find top-k `CodeFile` nodes above threshold 0.5, emit `References` edges with confidence = cosine
- [ ] Respect `maxSemanticFanOut` (default 50); skip doc nodes with no/empty embedding
- [ ] Dedup with existing `inferCodeDocEdges` output (literal-name edges) — semantic edges add to, don't replace, literal ones
- [ ] Hspec: doc→code match emits edge with correct confidence; below-threshold dropped; fan-out cap respected; missing embedding skips doc node

### 1.3 Wire semantic inference into pipeline
- [ ] Add `semantic_edges.enabled` (default true) and `semantic_edges.max_fan_out` (default 50) to config in `Domain/Config` + YAML parser
- [ ] Add `--no-semantic-edges` and `--force-semantic-edges` CLI flags in `CLI/Parser.hs`
- [ ] In `UseCase/Pipeline/Core.hs`: when `gEmbeddings` present AND `semantic_edges.enabled` AND code-node count <= 10K (unless `--force-semantic-edges`), call `inferSemanticCodeDocEdges` and merge into edge set; else log and skip
- [ ] Implement `isSingleCorpus :: Graph -> Bool`; auto-skip when true and `--force-semantic-edges` not set
- [ ] Log: "semantic edges: inferred N (cap=M, threshold=0.5, mode=auto-skip|forced|fallback)"
- [ ] Hspec: single-corpus auto-skip logs and emits 0; `--force-semantic-edges` runs pass; 15K code nodes without force logs fallback warning and runs literal-only

### 1.4 Document alternative embedding models
- [ ] Add a section to project docs (likely `docs/` or a new `docs/embedding-models.md`) listing: `nomic-embed-text` (default, local), `all-minilm` (faster, lower quality), `bge-m3` (multilingual, better code+prose), `voyage-code-2` (code-specialized, hosted), `text-embedding-3-small` (OpenAI, hosted)
- [ ] Trade-offs table: local vs hosted, dimension, code-prose quality, latency, cost
- [ ] Note: semantic code↔doc edges need a model that embeds code identifiers AND prose headings into a shared space — `nomic-embed-text` works but `bge-m3` or `voyage-code-2` recommended for mixed corpora

## Layer 2: Cluster composition (post-clustering)

### 2.1 CommunityComposition record + computation
- [ ] Add `CommunityComposition` record in `Domain/Community.hs` (or new `Domain/Community/Composition.hs`): `ccCodeCount`, `ccDocCount`, `ccOtherCount`, `ccDominantKind`, `ccMixedRatio`, `ccCodeDocEdges`
- [ ] Implement `computeCompositions :: Graph -> CommunityMap -> Map CommunityId CommunityComposition`
- [ ] `ccMixedRatio = if max(code,doc) == 0 then 0 else min(code,doc) / max(code,doc)`
- [ ] `ccCodeDocEdges` = count of `References` edges inside the community with one endpoint `CodeFile` and the other `DocFile`
- [ ] `ToJSON`/`FromJSON` instances
- [ ] Hspec: pure-code community → ratio 0; balanced → 1; mixed with 3 cross edges → 3

### 2.2 Persist compositions to graph.json
- [ ] Add `compositions :: Maybe (Map CommunityId CommunityComposition)` to `Graph` (additive)
- [ ] `UseCase/Pipeline/Core.hs`: call `computeCompositions` post-Leiden, attach to graph, persist under `compositions` key in `graph.json`
- [ ] `UseCase/Load.hs`: parse `compositions` (empty/`Nothing` on legacy)
- [ ] Hspec: graph.json round-trips compositions; legacy graph loads `Nothing`

### 2.3 HTML viewer composition badge
- [ ] In `Infrastructure/Export/HTML.hs`: render `🔧 N / 📄 M / 🌉 K` badge on community dots (overview mode tooltip) and drill-down headers
- [ ] Badge reads from embedded `compositions` JSON in the HTML payload
- [ ] Manual verification: `graphos serve` shows badges on communities

### 2.4 Composition-aware labeling prompt
- [ ] In `Domain/Labeling.hs` `labelPrompt`: tag each top node with `(code)`/`(doc)`; split "Top code nodes:" / "Top doc nodes:" lines
- [ ] Add composition line to community header: `composition: N code + M docs, K code↔doc links`
- [ ] Update preamble: "code-and-knowledge architecture analyst", "name the CONCEPT that unifies"
- [ ] Hspec: mixed-cluster prompt contains both "Top code nodes:" and "Top doc nodes:"; pure-code prompt has only "Top code nodes:"; preamble contains "concept" or "unifies"

## Layer 3: Explorer queries (query-time)

### 3.1 ExplorerFilter + applyExplorerFilter
- [ ] Add `ExplorerFilter` record (`efFiletype`, `efKind`, `efMixedOnly`) in `UseCase/Query/Refine.hs`
- [ ] Implement `applyExplorerFilter :: ExplorerFilter -> GraphIndex -> Map CommunityId CommunityComposition -> QueryResponse -> QueryResponse`
- [ ] `efMixedOnly` drops nodes whose community has `ccMixedRatio == 0`
- [ ] `efFiletype` / `efKind` drop nodes not matching
- [ ] Hspec: filetype filter narrows; kind filter narrows; mixed-only drops pure communities; no filter = identity

### 3.2 CLI parser: filter flags + new subcommands
- [ ] In `CLI/Parser.hs`: add `--filetype`, `--kind`, `--mixed-only`, `--code-only`, `--doc-only` to `CommonQueryOpts` (or a new `ExplorerFilterOpts` mixed in)
- [ ] Add `aroundOpts` with `<node>` arg + `--depth N` + common flags
- [ ] Add `clusterOpts` with `<id>` arg + common flags
- [ ] Wire `--code-only` → `--filetype code`, `--doc-only` → `--filetype doc`
- [ ] Hspec: parser accepts all flags on all query-family subcommands; `--help` lists them; invalid `--filetype` value errors clearly

### 3.3 aroundNode orchestration
- [ ] In `UseCase/Query.hs`: add `aroundNode :: Graph -> GraphIndex -> Map CommunityId CommunityComposition -> NodeId -> Int -> Maybe ExplorerFilter -> AroundResponse`
- [ ] Resolve node arg via existing `resolveNodeArg` (id-first, label fallback)
- [ ] Compute in-edges (dependencies) and out-edges (dependents) via `neighbors` + edge direction
- [ ] Look up community via `communityOfNode`; attach composition if present
- [ ] Get community bridges via `articulationPoints` filtered to the node's community
- [ ] Apply `ExplorerFilter` to returned edges/members
- [ ] Hspec: around returns expected fields; unknown node → clear error; label resolution works; filter narrows edges

### 3.4 clusterDetail orchestration
- [ ] In `UseCase/Query.hs`: add `clusterDetail :: Graph -> GraphIndex -> Map CommunityId CommunityComposition -> CommunityId -> Maybe ExplorerFilter -> ClusterResponse`
- [ ] Group members by `nodeKind` into `membersByKind :: Map Text [NodeId]`
- [ ] Collect `cross_type_edges` = `References` edges inside community crossing `CodeFile`↔`DocFile`
- [ ] Apply `ExplorerFilter` to members
- [ ] Hspec: cluster returns composition + members grouped + cross-type edges; unknown community → error

### 3.5 AroundResponse / ClusterResponse JSON
- [ ] Define `AroundResponse` and `ClusterResponse` with `ToJSON` instances in `Domain/Graph/Score.hs` or a new `Domain/Query/Explorer.hs`
- [ ] `renderAroundJSON`, `renderClusterJSON` (or reuse generic `toJSON`)
- [ ] Hspec: `--json` emits single document; no interleaved logs; text rendering agrees on counts

### 3.6 app/Main.hs dispatch + text rendering
- [ ] Dispatch `around` and `cluster` subcommands; route to `aroundNode` / `clusterDetail`; render JSON or text
- [ ] Pass `ExplorerFilter` from parsed flags to `query`/`symbols`/`neighbors`/`around`/`cluster`
- [ ] Hspec (integration): `graphos around mod_X --json` against `graphos-out/graph.json` returns valid JSON

### 3.7 HTTP port endpoints (deferred until add-query-api-port-and-view lands)
- [ ] Add `GET /api/around?node=<id>&depth=<n>&filetype=...&kind=...` and `GET /api/cluster?id=<n>&filetype=...&kind=...` to `Infrastructure/Server/QueryAPI`
- [ ] Reuse the same orchestration functions; ensure byte-for-byte parity with CLI `--json`
- [ ] Hspec: HTTP response equals CLI `--json` for same inputs
- [ ] **Dependency**: waits for `add-query-api-port-and-view` to merge

## Cross-cutting

### 4.1 Legacy graph compatibility
- [ ] Verify: `graph.json` without `embeddings_path` and without `compositions` loads and all query-family commands work (compositions treated as empty, embeddings as `Nothing`)
- [ ] Hspec: legacy graph fixture loads; `query`/`around`/`cluster` run without error

### 4.2 Build + warnings
- [ ] `cabal build` with `-Wall -Werror` clean
- [ ] `cabal test` green (existing tests + new Hspec cases)

### 4.3 Manual mixed-corpus verification
- [ ] Build a mixed corpus: this repo (code) + `docs/` (markdown) → `graph.json`
- [ ] Confirm: Leiden communities have `ccMixedRatio > 0` for ≥ 60% of size-≥5 communities
- [ ] Confirm: `graphos around mod_Graphos --json` returns < 500ms on a 10K-node graph
- [ ] Confirm: HTML viewer shows composition badges + "Mixed only" toggle works
- [ ] Confirm: `--no-semantic-edges` reproduces today's clustering (code-islands + doc-islands)