# Design — Semantic Edge Inference

## Context

This change makes mixed code+docs clustering actually work by adding embedding-based
`References` edges between `DocFile` and `CodeFile` nodes at build time. Two design decisions
matter: where embeddings are stored, and how to scale brute-force cosine search.

## Decision 1 — Embedding storage: pointer + sidecar

### Options

| Option | Where | `graph.json` size at 158K × 768-dim | Atomicity |
|--------|-------|-------------------------------------|-----------|
| A. Embeddings in `graph.json` (inline) | `gEmbeddings :: Maybe (Map NodeId [Double])` | +966MB | Single file |
| B. Sidecar `embeddings.json` + pointer | `gEmbeddingsPath :: Maybe FilePath` in `graph.json` | +~80 bytes | Two files, pointer-coupled |
| C. Sidecar only, no pointer | Loader looks for `embeddings.json` by convention | +0 | Two files, implicit coupling |

### Choice: **B** (pointer + sidecar)

- `graph.json` stays small — the 158K × 768 × 8B vector payload lives in `embeddings.json`.
- The pointer makes the coupling explicit and self-describing: the loader knows where to
  look without a convention.
- Legacy graphs: `embeddings_path` absent → `gEmbeddings = Nothing`, loads unchanged.
- Atomicity: write `embeddings.json` first, then `graph.json` with the pointer. A reader
  seeing the pointer but a missing sidecar logs a warning and returns `Nothing` (graceful).

```
   graph.json
   ├─ nodes: [...]
   ├─ edges: [...]
   ├─ communities: {...}
   └─ embeddings_path: "embeddings.json"   ← pointer (this change)
                                    │
                                    ▼
                        embeddings.json (sidecar, large)
                        { "mod_Auth": [0.12, -0.03, ...], ... }
```

### Why not ANN (HNSW/IVF) in this change

- The existing `IngestIndex.searchSimilar` is a sorted top-N cosine search — O(N) per query,
  acceptable up to ~10K code nodes for a **build-time** pass (run once).
- At 158K nodes, O(D × C × d) with D=10K docs, C=100K code, d=768 ≈ 7.7×10^11 ops — too slow.
- **Decision**: cap at 10K `CodeFile` nodes for now. Above that, log a warning and fall back
  to literal-name `inferCodeDocEdges` only (today's behavior). A follow-up change
  (`ann-index-for-semantic-inference`) brings HNSW/IVF via `vector-search` for the >10K case.
- `--force-semantic-edges` overrides the cap for users who want to wait.

## Decision 2 — `inferSemanticCodeDocEdges` algorithm

```haskell
inferSemanticCodeDocEdges :: Graph -> Map NodeId [Double] -> [Edge]
inferSemanticCodeDocEdges g embeddings =
  let docNodes  = [(nid, n) | (nid, n) <- Map.toList (gNodes g)
                            , nodeFileType n == DocFile
                            , Just vec <- [Map.lookup nid embeddings]
                            , not (null vec)]
      codeNodes = [(nid, n) | (nid, n) <- Map.toList (gNodes g)
                            , nodeFileType n == CodeFile
                            , Just vec <- [Map.lookup nid embeddings]
                            , not (null vec)]
      -- Build code-node vectors once
      codeVecs = [(nid, vec) | (nid, _) <- codeNodes, Just vec <- [Map.lookup nid embeddings]]

      -- For each doc, find top-k code by cosine >= threshold
      edges = [ makeInferredEdge codeNid docNid References cosine
              | (docNid, _) <- docNodes
              , let codeNid = Map.lookup docNid embeddings  -- (sketch)
              -- ... actual: for each (codeNid, codeVec) in codeVecs,
              --             compute cosineSimilarity docVec codeVec,
              --             filter >= 0.5, take top maxSemanticFanOut
              ]
  in dedupOn (\e -> (edgeSource e, edgeTarget e)) edges
```

Key parameters:
- **threshold** = 0.5 (config: `semantic_edges.threshold`, default 0.5)
- **maxSemanticFanOut** = 50 (config: `semantic_edges.max_fan_out`, default 50)
- **edge type** = `References`, confidence = cosine score (0.5–1.0)

Merges with `inferCodeDocEdges` (literal-name) — additive. The literal pass catches
`auth.md`↔`Auth.hs`; the semantic pass catches `docs/auth.md`↔`login.ts`.

## Decision 3 — Single-corpus auto-skip

```haskell
isSingleCorpus :: Graph -> Bool
isSingleCorpus g =
  case Set.toList (Set.map nodeFileType (Map.elems (gNodes g))) of
    [_] -> True
    _   -> False
```

When `isSingleCorpus g == True` AND `--force-semantic-edges` is not set:
- Log: `"single-corpus graph detected (all CodeFile), skipping semantic edge inference"`
- Emit 0 semantic edges (no doc↔code pairs to find anyway — this is a perf guard + clarity)

`--force-semantic-edges` overrides: the pass runs and produces 0 edges (no docs to match), but
isn't skipped. Useful for benchmarking or verifying the pass works on a pure-code graph.

## Decision 4 — Pipeline gating order

```
   inferEdges density g cm
     │
     ├─ always: inferCodeDocEdges g              ← literal-name (today)
     │
     ├─ if density >= Normal: inferCommunityBridges, inferTransitiveDeps
     │
     └─ NEW: if gEmbeddings present
              AND semantic_edges.enabled
              AND (codeNodeCount <= 10000 OR --force-semantic-edges)
              AND not (isSingleCorpus AND not --force-semantic-edges):
                inferSemanticCodeDocEdges g embeddings
```

The new pass is **additive** — it runs alongside the existing inferences, never replacing
them. A graph with embeddings disabled behaves exactly as today.

## What this design does NOT decide

- **ANN library** — deferred to follow-up `ann-index-for-semantic-inference` change.
- **Embedding model** — user picks via `embedding.model` config; documented in
  `docs/embedding-models.md`. This change doesn't mandate a specific model.
- **Threshold tuning** — 0.5 is a reasonable default; the "Act" step says lower to 0.4 if
  real corpora produce too few edges. Config-exposed so users can tune.