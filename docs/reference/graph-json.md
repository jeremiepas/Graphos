# graph.json — the JGF contract

`graphos-out/graph.json` is the canonical output. It is a
[JSON Graph Format (JGF)](https://jsongraphformat.info) document
(`application/vnd.jgf+json`), not a Graphos-specific blob: any JGF reader can
open it, and Graphos-specific data lives under `graph.metadata.graphos`.

## Envelope

```json
{
  "graph": {
    "directed": true,
    "type": "graphos.code-knowledge-graph",
    "metadata": {
      "graphos": {
        "schemaVersion": "1.0",
        "graph_hash": "…",
        "communities": [ … ],
        "cohesion": { … },
        "god_nodes": [ … ],
        "community_labels": { … },
        "community_aggregates": { … },
        "compositions": [ … ],
        "embeddings_path": "embeddings.json",
        "null_model": { … },
        "input_source": "…"
      }
    },
    "nodes": {
      "<node-id>": { "id": "…", "label": "…", "metadata": { … } }
    },
    "edges": [
      { "source": "…", "relation": "imports", "target": "…", "directed": true, "metadata": { … } }
    ]
  }
}
```

- **Nodes** — `id` and `label` stay top-level; `file_type`, `source_file`,
  `community_id`, `line_start`, `line_end`, `signature`, `kind`, `degree`,
  `is_bridge`, and `extra` live under node `metadata`. Absent optional fields
  are omitted.
- **Edges** — `source`, `relation`, `target`, and the `directed` override stay
  top-level; `id`, `weight`, `confidence`, and `extra` live under edge
  `metadata`.
- **Graph** — `directed` and `type` are fixed directives. The optional JGF
  `graph.label` field is unused. `schemaVersion` is `major.minor`; readers
  reject unknown major versions and accept additive minor bumps.

The full normative contract is
[`openspec/specs/graph-json-contract/spec.md`](../../openspec/specs/graph-json-contract/spec.md).

## Migrating a legacy graph

Pre-JGF `graph.json` files keep loading during the deprecation window. Upgrade
one in place (or to a new path):

```bash
graphos migrate-graph graphos-out/graph.json
graphos migrate-graph old.json --output new.json
```

## Related

- [CLI reference](cli.md) — `migrate-graph` and the pipeline flags that write
  `graph.json`.
- [Workflow 01 — full pipeline](../workflows/01-full-pipeline.md) — when the
  file is produced.
