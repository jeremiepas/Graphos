# 12 — Neo4j Push

> `graphos <path> --neo4j --neo4j-push <uri>`

Push the knowledge graph to a Neo4j graph database for interactive exploration via Cypher queries.

---

## Three Push Modes

```
┌──────────────────────────────────────────────────────────────┐
│                  NEO4J PUSH MODES                            │
│                                                              │
│  ┌──────────────┐  ┌──────────────┐  ┌────────────────┐   │
│  │  FullPush    │  │SubgraphPush  │  │ CommunityPush  │   │
│  │              │  │ (default     │  │                │   │
│  │  All nodes   │  │  >10k nodes)│  │  Communities   │   │
│  │  All edges   │  │              │  │  + inter-comm  │   │
│  │  + comm.     │  │ Comm + reps  │  │  edges only   │   │
│  │              │  │ + bridges    │  │                │   │
│  │  ~990k stmt  │  │ ~64k stmt   │  │  ~8k stmt      │   │
│  │  2–4 hours   │  │ ~30 sec     │  │  ~5 sec        │   │
│  └──────────────┘  └──────────────┘  └────────────────┘   │
│                                                              │
│  Auto-selection:                                            │
│    nodes < 10k  → FullPush (small graph, no need to cut)  │
│    nodes ≥ 10k  → SubgraphPush (recommended)                │
│                                                              │
│  Override: --neo4j-push-mode full|subgraph|community        │
└──────────────────────────────────────────────────────────────┘
```

---

## Why This Workflow Exists

The JSON/HTML exports are great for local exploration, but Neo4j enables:
- **Cypher queries**: "Find all paths from Auth to Database"
- **Graph algorithms**: PageRank, betweenness centrality via GDS
- **Visualization**: Neo4j Bloom, custom dashboards
- **Integration**: Other tools can query the graph database

---

## SubgraphPush: Representative Node Selection

For large graphs, SubgraphPush selects structurally important nodes per community:

| Criterion | What It Captures | Example |
|-----------|-------------------|---------|
| **Centroid** (highest degree) | Main concept of community | `parseConfig` in Config community |
| **Top-N by degree** | Most-referenced functions | `loadYAML`, `validateSettings` |
| **Bridge nodes** (articulation points) | Cross-community connectors | `defaultConfig` used by Config + Pipeline |
| **Entry points** (file nodes) | Where to start reading | `src/Config/Parser.hs` |

Default: 7 representatives per community (`--neo4j-subgraph-size 7`).

---

## What Gets Written to Neo4j

### All Modes

```
Community nodes (id, label, size, cohesion)
```

### SubgraphPush (adds)

```
Representative nodes (id, label, file_type, source_location, representative=true)
Bridge nodes (id, label, file_type, source_location, bridge=true)
BELONGS_TO edges (representative/bridge → community)
Intra-community edges (between representatives)
CONNECTED_TO edges (inter-community, with edge_count + bridge_nodes)
```

### FullPush (adds)

```
All nodes with all properties
All edges with all properties
All BELONGS_TO memberships
```

---

## Streaming Neo4j Push

When `--neo4j` is enabled during the full pipeline, nodes are pushed to Neo4j **during extraction** (node-by-node) instead of waiting for the export stage. After extraction completes, an edge repair pass re-pushes all edges to ensure cross-file connections are correct.

---

## Configuration

| Flag | Default | Description |
|------|---------|-------------|
| `--neo4j` | off | Enable Neo4j push |
| `--neo4j-push <uri>` | required | Neo4j HTTP URI |
| `--neo4j-push-mode` | auto | full/subgraph/community |
| `--neo4j-subgraph-size N` | 7 | Representatives per community |

YAML:

```yaml
neo4j:
  uri: "http://localhost:7474"
  user: "neo4j"
  password: "graphos_dev"
  push_mode: "subgraph"    # full | subgraph | community
  subgraph_size: 7
```

---

## Prerequisite

- A running Neo4j instance (local or remote)
- Full pipeline completed (graph with communities)