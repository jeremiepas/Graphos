# Embedding Models

Graphos supports node-level embeddings for semantic edge inference. When enabled,
the pipeline generates a vector embedding for every node and writes them to an
`embeddings.json` sidecar file alongside `graph.json`.

## Model Selection

For semantic code↔doc edges, the model must embed code identifiers AND prose
into a shared vector space. `nomic-embed-text` works out of the box; for mixed
corpora, `bge-m3` or `voyage-code-2` are recommended.

| Model                   | Local/Hosted | Dim  | Code+Prose Quality | Latency | Cost         |
|-------------------------|--------------|------|--------------------|---------|--------------|
| `nomic-embed-text`      | local        | 768  | Good               | Medium  | Free         |
| `all-minilm`            | local        | 384  | Lower              | Fast    | Free         |
| `bge-m3`                | local        | 1024 | Better (multilingual) | Slow  | Free         |
| `voyage-code-2`         | hosted       | 1536 | Best (code-specialized) | Medium | Paid       |
| `text-embedding-3-small`| hosted (OpenAI) | 1536 | Good           | Medium  | Paid         |

### LFM2.5-Embedding-350M (default local model)

`hf.co/LiquidAI/LFM2.5-Embedding-350M-GGUF:Q4_K_M` is the default local
model since `lfm-embedding-optimization` (GGUF via a standalone llama-server
or any OpenAI-compatible server):

| Property        | Value                                             |
|-----------------|---------------------------------------------------|
| Local/Hosted    | local (llama-server)                              |
| Dim             | 1024                                              |
| Doc token limit | 512                                               |
| Prefixes        | asymmetric — `document: ` (docs) / `query: ` (queries) |
| Throughput      | ~77 texts/s measured on the target hardware (see the recorded benchmark below); llama-server burst on short inputs ~343 req/s |

Set the model via `embedding.model` in `graphos.yaml`:

```yaml
embedding:
  enabled: true
  model: "hf.co/LiquidAI/LFM2.5-Embedding-350M-GGUF:Q4_K_M"
  dimension: 1024        # or 0 = auto-detect
```

Existing configs naming `nomic-embed-text` explicitly continue to work
unchanged (same cache keys as previous runs).

## Configuration

Embeddings are configured under the `embedding` key in the Graphos config file:

```json
{
  "embedding": {
    "enabled": true,
    "model": "nomic-embed-text",
    "dimension": 768
  }
}
```

| Field       | Type    | Default              | Description                          |
|-------------|---------|----------------------|--------------------------------------|
| `enabled`   | `bool`  | `false`              | Enable embedding generation          |
| `model`     | `string`| `"hf.co/LiquidAI/LFM2.5-Embedding-350M-GGUF:Q4_K_M"` | Embedding model name |
| `dimension` | `int`   | `0` (auto-detect)    | Vector dimension                     |

## Token limits and preparation

Before any request is submitted, each embed text is **prepared** to fit the
model's token limit: the default limit comes from the model's documented
context (`nomic-embed-text` 8192, LFM2.5 512, `all-minilm` 256), overridden by
`embedding.maxTokens` (`0` or absent = model default). Preparation applies
truncation under a conservative characters÷4 token estimate — a prepared text
never exceeds the effective limit, and the original unprepared text is never
sent to the API. Two raw texts that prepare identically (e.g. differing only
past the truncation point) share one cache entry.

Optional asymmetric prefixes for bi-encoders like LFM2.5:
`embedding.docPrefix` (e.g. `"document: "`) is prepended to every embedded
text; `embedding.queryPrefix` (e.g. `"query: "`) applies to query-time text.
Empty defaults preserve unprefixed behavior, and both prefixes participate in
the cache key and persisted source-hash.

Throughput measurements per model/server/concurrency (and the oversized-input
regression probe) are recorded in the
`lfm-embedding-optimization` change's
[`bench/BENCHMARK.md`](../openspec/changes/lfm-embedding-optimization/bench/BENCHMARK.md);
the pinned default concurrency (1) cites its recorded sweep.

## Semantic Edge Inference

When embeddings are present, the pipeline infers `References` edges between
`DocFile` and `CodeFile` nodes using cosine similarity. This complements the
literal-name matching in `inferCodeDocEdges`.

### Gating

Semantic inference is gated by the `semanticMode` function:

1. **Explicit disable** (`--no-semantic-edges` or `seEnabled = false`) → skipped.
2. **Force** (`--force-semantic-edges`) → always runs.
3. **Single-corpus auto-skip** → skipped when all nodes share one `FileType`.
4. **Scale cap** → skipped when >10 000 code nodes (falls back to literal matching).
5. **Enabled** → runs for mixed-corpus graphs under the scale cap.

### Configuration

Semantic edges are configured under the `semantic_edges` key:

```json
{
  "semantic_edges": {
    "enabled": true,
    "maxFanOut": 50,
    "threshold": 0.5
  }
}
```

| Field       | Type    | Default | Description                              |
|-------------|---------|---------|------------------------------------------|
| `enabled`   | `bool`  | `true`  | Enable semantic edge inference           |
| `maxFanOut` | `int`   | `50`    | Max semantic edges per doc node          |
| `threshold` | `float` | `0.5`   | Min cosine similarity for an edge        |

### CLI Flags

| Flag                        | Effect                              |
|-----------------------------|-------------------------------------|
| `--no-semantic-edges`       | Disable semantic inference          |
| `--force-semantic-edges`    | Force semantic inference (bypass caps) |

## Storage

Embeddings are stored in an `embeddings.json` sidecar file:

```json
{
  "nodeId1": [0.1, 0.2, 0.3],
  "nodeId2": [0.4, 0.5, 0.6]
}
```

The `graph.json` file references the sidecar via the optional `embeddings_path`
field (relative to the graph file's directory). Legacy graphs without this field
load with `gEmbeddings = Nothing`.
