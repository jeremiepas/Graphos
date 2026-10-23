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
│  │  ~140 UNWIND │  │ ~64k stmt    │  │  ~8k stmt      │   │
│  │  statements  │  │ seconds      │  │  seconds       │   │
│  │  ≈35s–7 min* │  │ ~30 sec      │  │  ~5 sec        │   │
│  └──────────────┘  └──────────────┘  └────────────────┘   │
│                                                              │
│  Auto-selection:                                            │
│    nodes < 10k  → FullPush (small graph, no need to cut)  │
│    nodes ≥ 10k  → SubgraphPush (recommended)                │
│                                                              │
│  Override: --neo4j-push-mode full|subgraph|community        │
└──────────────────────────────────────────────────────────────┘
```

\* FullPush timings on localhost Neo4j 5 (UNWIND batching + indexed MERGE,
since optimize-neo4j-full-push): synthetic 100k nodes / 200k edges pushed in
**~7 s**; a real-world 119k-node / 128k-edge graph (labels carrying source
text) measured **~35 s**. The historical path (one statement per node/edge,
curl subprocess per batch, no index) took 2–4 hours for the same graphs and
is no longer shipped.

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

When `--neo4j` is enabled during the full pipeline, nodes are pushed to Neo4j **during extraction** (per-file UNWIND batches instead of waiting for the export stage). After extraction completes, an edge repair pass re-pushes all edges (grouped by relationship type, UNWIND batches) to ensure cross-file connections are correct.

---

## The id_hash Key Scheme

Pushed nodes are keyed on `id_hash = sha1(id)` — lowercase hex over the id's UTF-8 bytes — never on raw `id`:

```
(:Node {id_hash: "a9993e36...", id: "original raw id", label: ..., ...})
```

Why not raw `id`: some ids (e.g. `External` nodes embedding whole source snippets) exceed Neo4j's ~8 KB RANGE-index key limit. A uniqueness constraint on `id` fails to populate (state FAILED) and every lookup silently degrades to a label scan. `id_hash` is always 40 chars, always indexable.

Before the first data statement the push:

1. **Backfills** `id_hash` on any pre-existing hashless `:Node` rows (client-side hashing in batched read/write windows — vanilla Neo4j has no `sha1()` function, and APOC is not a dependency).
2. **Creates** uniqueness constraints `node_id_hash_unique` on `:Node(id_hash)` and `community_id_unique` on `:Community(id)` with `IF NOT EXISTS` — each schema statement in its own transaction.
3. Only then sends data: community nodes, `BELONGS_TO` memberships, graph nodes (chunks of ≤1,000 rows or ≤4 MB payload), then edges grouped by relationship type (chunks of ≤5,000 rows), all as parameterized `UNWIND $rows` statements over one persistent HTTP connection.

If constraint creation fails (pre-existing duplicate `id_hash` rows from past unconstrained pushes), the push aborts with an error naming the constraint — deduplicate (e.g. `apoc.refactor.mergeNodes`) and re-run. No data is sent on schema failure.

**Migration note for consumers**: queries that `MERGE`d nodes by raw `id` still work (`id` remains a regular property), but they label-scan. Migrate such queries to key on `id_hash`:

```cypher
-- before
MERGE (n:Node {id: $id})
-- after
MERGE (n:Node {id_hash: $id_hash}) ON CREATE SET n.id = $id
-- with $id_hash = sha1($id) computed client-side (lowercase hex, UTF-8)
```

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
- Neo4j 5.x (community edition is sufficient — no APOC, no plugins)

Quick start (disposable local instance):

```bash
docker run -d --name graphos-neo4j -p 7474:7474 -p 7687:7687 \
  -e NEO4J_AUTH=neo4j/graphos_dev neo4j:5-community
```