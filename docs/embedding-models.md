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

Set the model via `embedding.model` in `graphos.yaml`:

```yaml
embedding:
  enabled: true
  model: bge-m3
  dimension: 1024
```

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
| `model`     | `string`| `"nomic-embed-text"` | Embedding model name                 |
| `dimension` | `int`   | `768`                | Vector dimension                     |

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
