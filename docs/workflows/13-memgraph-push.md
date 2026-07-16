# 13 — Memgraph Push

> `graphos <path> --memgraph --memgraph-push <uri>`

Push the knowledge graph to a Memgraph in-memory graph database via the Bolt protocol.

---

## Flow

Identical to [12 — Neo4j Push](12-neo4j-push.md) but targeting Memgraph instead of Neo4j. Same three push modes (FullPush, SubgraphPush, CommunityPush), same representative node selection, same Cypher statement generation.

---

## Why Memgraph Instead of Neo4j

| Aspect | Neo4j | Memgraph |
|--------|-------|----------|
| Storage | Disk-based | In-memory |
| Speed | Good | Faster (no disk I/O) |
| Persistence | Yes | Ephemeral (lost on restart) |
| GDS algorithms | Built-in | Limited |
| APOC | Built-in | Limited |
| Best for | Persistent graph storage | Fast interactive exploration |

Memgraph is better for real-time interactive exploration of the graph. Neo4j is better for persistent storage and advanced graph algorithms.

---

## Configuration

| Flag | Default | Description |
|------|---------|-------------|
| `--memgraph` | off | Enable Memgraph push |
| `--memgraph-push <uri>` | required | Bolt URI (e.g. `bolt://localhost:7688`) |
| `--memgraph-push-mode` | auto | full/subgraph/community |
| `--memgraph-subgraph-size N` | 7 | Representatives per community |

YAML:

```yaml
memgraph:
  uri: "bolt://localhost:7688"
  user: ""              # No auth by default for local dev
  password: ""
  push_mode: "subgraph"
  subgraph_size: 7
```

---

## Prerequisite

- A running Memgraph instance
- Full pipeline completed (graph with communities)