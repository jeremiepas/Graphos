# Design — Mixed-Corpus Semantic Explorer

## Context

The proposal asks for semantic code↔doc edge inference + cluster composition + explorer
queries. The two hardest design decisions are: (1) where embeddings live and how they scale,
and (2) how `around` fits the existing query-family JSON contract. This design captures both.

## Decision 1 — Embedding storage: `graph.json` vs sidecar

### Options considered

| Option | Where | Pros | Cons |
|--------|-------|------|------|
| A. Sidecar only (`graph.json` + `embeddings.json`) | New file | `graph.json` unchanged | Explorer must load 2 files; atomicity harder |
| B. Embeddings in `graph.json` (optional field) | `gEmbeddings :: Maybe (Map NodeId [Double])` | One file, atomic load | `graph.json` grows by ~N × dim × 8B (158K × 768 × 8 ≈ 966 MB) |
| C. Embeddings in `Node.nodeExtra` | Per-node `extra` field | No new field | Awkward to query; vectors aren't first-class |
| D. Embeddings in a separate `GraphEmbeddings` map loaded alongside | Separate but coupled | Keeps `graph.json` small | Two-file load, like A |

### Choice: **B with a twist**

- `gEmbeddings :: Maybe (Map NodeId [Double])` on `Graph` — additive, `Nothing` on legacy graphs.
- **But**: when serializing, embeddings are written to a *separate* `embeddings.json` file in
  the same output directory, and `graph.json` carries only a pointer field
  `gEmbeddingsPath :: Maybe FilePath`. The loader (`loadGraphFromFile`) follows the pointer and
  loads embeddings if the file exists, else `Nothing`.

This keeps `graph.json` small (the 158K × 768 × 8B problem goes away), preserves atomicity (the
pointer is in `graph.json`; the vectors load lazily), and legacy graphs load unchanged
(`gEmbeddingsPath = Nothing`).

```
   graph.json
   ├─ nodes: [...]
   ├─ edges: [...]
   ├─ communities: {...}
   ├─ compositions: {...}     ← Layer 2 (small, inline)
   └─ embeddings_path: "embeddings.json"   ← pointer (Layer 1)
                                    │
                                    ▼
                        embeddings.json (large, sidecar)
```

### Why not ANN (HNSW/IVF) in this change

- The existing `IngestIndex.searchSimilar` is already a sorted top-N cosine search — it's
  O(N) per query but acceptable up to ~50K nodes for a *build-time* pass (run once).
- At 158K nodes, brute-force cosine against all code nodes for each doc node is O(D × C × d).
  With D=10K docs, C=100K code, d=768, that's ~7.7 × 10^11 ops — too slow.
- **Decision**: cap semantic inference at 10K code nodes for now. Above that, log a warning
  and fall back to literal-name inference only. A follow-up change (`ann-index-for-semantic-
  inference`) brings in `vector-search` or a local ANN for the >10K case. Documented in the
  proposal's "Act" step.

### Single-corpus auto-detect

```haskell
-- Pure: only one FileType present in the graph
isSingleCorpus :: Graph -> Bool
isSingleCorpus g =
  case Set.toList (Set.map nodeFileType (Map.elems (gNodes g))) of
    [_] -> True
    _   -> False
```

When `isSingleCorpus g == True` AND `--force-semantic-edges` is not set, skip
`inferSemanticCodeDocEdges` entirely (no doc↔code pairs to find). Saves the embedding fetch
cost on pure-code and pure-doc runs.

## Decision 2 — `around` and the query-cli-contract

### The contract today

`query-cli-contract` requires every query-family subcommand to:
- accept `--graph`, `--budget`, `--json`, `--label-width`, `--edges`
- emit a single JSON document on `--json` with `verdict`/`bestScore`/`hash`/`nodes`/`edges`/
  `suggestions` where applicable
- no interleaved log lines on stdout

### `around` response shape

`around` is **not a search** — it doesn't have a `verdict` or `bestScore` (no query text to
score against). It's a structural bundle. Forcing it into `QueryResponse` would mean
populating `verdict = "none"` and `bestScore = 0` — technically conformant but semantically
wrong.

**Decision**: `around` and `cluster` get their own response types, but **honor the same flags**
(`--json`, `--budget`, `--label-width`, `--edges`) and the same "single JSON document, no
interleaved logs" rule. The `query-cli-contract` spec is extended to say: *"subcommands
without a search query (`around`, `cluster`) emit their own response type with `--json`, but
honor the uniform flag surface and the single-document rule."*

```json
// graphos around mod_Auth --json
{
  "node": { "id": "mod_Auth", "label": "Auth", "file_type": "code", ... },
  "in_edges":  [ { "edge": { "source": "...", "target": "mod_Auth", "type": "imports", "confidence": 0.9 },
                   "node": { "id": "mod_Config", "label": "Config", ... } }, ... ],
  "out_edges": [ { "edge": {...}, "node": {...} }, ... ],
  "community": { "id": 483, "label": "Authentication", "composition": { "code": 12, "doc": 4, "mixed_ratio": 0.33 } },
  "bridges":    [ "mod_Auth", "fn_verifyToken" ],
  "depth":      1
}
```

```json
// graphos cluster 483 --json
{
  "id": 483,
  "label": "Authentication",
  "composition": { "code": 12, "doc": 4, "other": 0, "dominant_kind": "function", "mixed_ratio": 0.33, "code_doc_edges": 3 },
  "members_by_kind": {
    "function":  [ "fn_verifyToken", "fn_refreshToken", ... ],
    "section":   [ "sec_JWT_validation", "sec_Auth_flow", ... ],
    "module":    [ "mod_Auth" ]
  },
  "bridges":      [ "mod_Auth" ],
  "cross_type_edges": [ { "source": "sec_JWT_validation", "target": "fn_verifyToken", "type": "references", "confidence": 0.82 }, ... ]
}
```

### Filter flag semantics

The new flags (`--filetype`, `--kind`, `--mixed-only`, `--code-only`, `--doc-only`) are
**post-filters** on the result set — they don't change the query algorithm, only narrow what
is returned. This keeps `queryGraphWithIndexScored` unchanged; the filter applies in
`refineResponse` (already the place where `EdgeMode` and label-width truncation happen).

```haskell
-- In UseCase/Query/Refine.hs
data ExplorerFilter = ExplorerFilter
  { efFiletype   :: Maybe FileType
  , efKind       :: Maybe Text
  , efMixedOnly  :: Bool
  }

applyExplorerFilter :: ExplorerFilter -> GraphIndex -> QueryResponse -> QueryResponse
```

`--mixed-only` consults the new `CommunityComposition` map (Layer 2) — drops nodes whose
community has `ccMixedRatio == 0`.

## Decision 3 — Composition-aware labeling prompt

The current `labelPrompt`:

```
You are a code architecture analyst. Given these communities of related code nodes,
assign a concise 2-4 word label that describes each community's purpose.

Community 483 (cohesion: 0.72, size: 16):
  Top nodes: verifyToken, AuthMiddleware, refreshToken, sec_JWT_validation, sec_Auth_flow
```

The LLM sees a flat list of labels and can't tell code from docs — it'll name the cluster
after the most frequent token, which on a mixed cluster is often a code identifier.

### New prompt

```
You are a code-and-knowledge architecture analyst. Given these communities of related
nodes (code and documentation), assign a concise 2-4 word label that names the CONCEPT
that unifies each community — not the most frequent word.

Community 483 (cohesion: 0.72, size: 16, composition: 12 code + 4 docs, 3 code↔doc links):
  Top code nodes: verifyToken, AuthMiddleware, refreshToken
  Top doc nodes:  'JWT validation', 'Auth flow'
```

Three changes:
1. "code-and-knowledge" frames the task as mixed-corpus.
2. The composition line (`12 code + 4 docs, 3 code↔doc links`) tells the LLM this is a mixed
   cluster and the concept should bridge both sides.
3. Top nodes split by `(code)`/`(doc)` so the LLM sees doc headings as natural-language
   anchors for the concept name.

## Scale summary

| Graph size | Semantic edges | Composition | `around` | `cluster` | HTML viewer |
|------------|----------------|-------------|----------|-----------|-------------|
| <1K        | brute-force cosine | O(N) | O(1) lookup + O(k) neighbors | O(N) in community | full render |
| 1K–10K     | brute-force cosine | O(N) | <500ms | <100ms | full render |
| 10K–100K   | **capped** (warning + fallback to literal) | O(N) | <500ms (uses index) | <100ms | LOD default |
| >100K      | not supported (follow-up ANN change) | O(N) | <500ms | <100ms | LOD only |

## What this design deliberately does NOT decide

- **ANN library choice** — deferred to follow-up `ann-index-for-semantic-inference` change.
- **Cross-language embedding model** — documented in project docs, user chooses via
  `embedding.model` config; this change doesn't mandate a specific model.
- **HTTP `/api/around` / `/api/cluster` endpoint shape** — inherits from
  `add-query-api-port-and-view` (in flight); this change adds them once that lands.
- **Obsability / tracing for the semantic inference pass** — standard `opSetGauge` /
  `opDebugTraceSpan` calls in the pipeline step, no new observability capability.