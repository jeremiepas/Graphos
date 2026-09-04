# 18 — Cypher Write

> `graphos cypher "<statement>" --write [--graph PATH] [--budget N] [--json]`

openCypher write clause subset over the in-memory property graph
(openspec change `opencypher-write-mutations`). Mutations apply in memory
by default; `--write` also persists the mutated graph back to the loaded
`graph.json` with a timestamped backup.

Surfaces:

| Surface | Entry | Read-only default |
|---------|-------|-------------------|
| CLI | `graphos cypher QUERY --write` | yes (write clauses rejected without `--write`) |
| MCP | `cypher_mutate` tool (`persist` param) | yes (`cypher_query` rejects write clauses) |
| HTTP | `POST /api/cypher/mutate` body `{"query": ..., "persist": false}` | yes (read-only statements evaluate normally) |

---

## Supported write subset

| Clause | Semantics |
|--------|-----------|
| `CREATE (a:Kind {..}), (a)-[:Rel {..}]->(b)` | Creates nodes and relationships; pattern variables bind left-to-right; a node without `id` gets a generated `gen-<var>-<n>` id; a duplicate id is an error |
| `MERGE (n:Kind {id: x}) [ON CREATE SET ..] [ON MATCH SET ..]` | Matches on `(label, id)` first, then the full property map; creates when nothing matches; also works on relationship patterns (upsert) |
| `SET n.prop = expr` | Model fields (`label`, `source_file`, `line_start/end`, `signature`, `community`, `degree`, `is_bridge`, edge `weight`/`confidence`) are written in place; any other property goes to the node/edge `extra` object |
| `SET n:Label` | Adds a label: the primary kind if unset, otherwise the `extra_labels` list |
| `REMOVE n.prop` / `REMOVE n:Label` | Deletes an extra property; removes an extra label; removing the primary label leaves an unlabelled node |
| `DELETE a, r` / `DETACH DELETE a` | `DELETE` errors when a node retains relationships (naming the node); `DETACH DELETE` removes incident edges too |
| `RETURN` (optional) | Any read expression against the post-mutation graph, plus the implicit summary counters (`nodes_created`, `rels_created`, `rels_upserted`, `properties_set`, `properties_removed`, `nodes_deleted`, `rels_deleted`) |

`MATCH ... WHERE` prefixes work as in the read subset (mutate what you matched).

Not supported (parse errors naming the construct): `WITH`, `UNWIND`,
subqueries, parameters, `SET n += {map}`, relationship types outside the
closed vocabulary (`calls`, `imports`, `extends`, `implements`,
`references`, `contains`, `depends_on`, `inferred`).

---

## Model reconciliation rules

- **Single primary label** — `nodeKind` is the Cypher label; extra labels
  (`SET n:X` on a labelled node) live in `nodeExtra.extra_labels` and are
  visible to subsequent `MATCH` patterns.
- **No parallel edges** — the graph keys edges by (source, target) pair.
  `CREATE`/`MERGE` of an existing pair **upserts** the edge (properties
  overwritten, `rels_upserted` instead of `rels_created`).
- **Closed relationship vocabulary** — write patterns accept only the
  eight `Relation` enum types; anything else is a parse error naming the
  type and the vocabulary.
- **Extra properties** — non-model `SET` properties are stored in
  `nodeExtra` / `edgeExtra` and are queryable (`WHERE`, `RETURN`,
  `SET`-round-trip).

---

## Persistence

`--write` (CLI) / `persist: true` (MCP, HTTP):

1. Copies the loaded `graph.json` to `graph.json.bak-<timestamp>`.
2. Writes the mutated graph in the same versioned schema
   (`schema_version` preserved; `communities`, `cohesion`, `god_nodes`,
   `community_labels`, `community_aggregates`, `compositions` carried
   over; degrees/adjacency/hash recomputed).
3. Prints the backup path and the caveat below.

> **Caveat:** `graph.json` is a derived artifact. The next extraction run
> (`graphos <path>`) overwrites it and **discards persisted mutations**.
> For durable curation use the Neo4j/Memgraph push paths.

---

## Examples

### Match then set

```
$ graphos cypher "MATCH (n:Function) WHERE n.source_file =~ 'src/old/.*' SET n.source_file = 'src/new/main.hs' RETURN n.id" --graph graphos-out/graph.json
properties set: 3

n.id
"old_fn_1"
...
```

### Merge with ON clauses + persist

```
$ graphos cypher "MERGE (m:Module {id: 'auth'}) ON CREATE SET m.label = 'Auth'" --write
nodes created: 1
Persisted to graphos-out/graph.json (backup: graphos-out/graph.json.bak-20260904T120000Z)
Note: the next extraction run overwrites graph.json and discards mutations.
```

### JSON summary

```
$ graphos cypher "MATCH (n:Deprecated) DETACH DELETE n" --write --json
{"columns":[],"rows":[],"summary":{"nodes_created":0,"nodes_deleted":2,...},"truncated":false}
```