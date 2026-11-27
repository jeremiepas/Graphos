# Pushing a Graphos graph to Neo4j

Connection settings come from the `neo4j:` block of `graphos.yaml` (`uri`, `user`, `password`, `pushMode`, `subgraphSize`). Never hard-code credentials in scripts or prompts. Read them from the config or the environment.

## Built-in push

```bash
# push an existing graph.json, no extraction
graphos push --graph <graph.json> --uri <http-uri> --user <user> --password "$NEO4J_PASSWORD" --mode FullPush
# push during a build
graphos <path> --neo4j --neo4j-push <uri> --neo4j-push-mode FullPush
```

- `graphos push` defaults to `--mode SubgraphPush` (a few representatives per community), not the full graph. Its `--uri/--user/--password` flags have built-in defaults; always pass your own.
- `--neo4j-push <uri>` does nothing unless `--neo4j` is also set.
- Push-mode values are parsed as constructor names (`FullPush`, `SubgraphPush`, `CommunityPush`). Lower-case `full` is rejected even though the build help text shows it.
- The push sends one MERGE per node and per edge in small batches and creates no index, so it slows down more and more as the graph grows. Fine for a few thousand nodes; on 100k+ nodes it takes hours. Unconstrained MERGE can also leave duplicate nodes.
- Pushed properties: `id`, `label`, `file_type`, `line_start`/`line_end`. `kind` and `source_file` are **not** pushed; add them back from graph.json if you need them.

## Faster route for large graphs (~10k rows/s)

1. **Don't index `id` directly**: `External` node ids can embed source snippets larger than Neo4j's RANGE-index key limit (~8 KB), and the index silently ends in state `FAILED`. A TEXT index skips oversized values, so MERGE then misses and duplicates.
2. Key nodes on `id_hash = sha1(id)` (lowercase hex of the UTF-8 id; APOC's `apoc.util.sha1([id])` gives the same value as Python's `hashlib.sha1`).
3. If several projects share one database, prefix the hashed value with a project namespace (e.g. `sha1("<project>\0" + id)`) and set `n.project`. Graphos ids are not unique across builds.
4. Create a uniqueness constraint on `:Node(id_hash)` **in its own transaction**: Neo4j rejects schema changes in a transaction that also writes data.
5. Load with `UNWIND $rows AS row MERGE …` over the HTTP transaction API: nodes in batches of about 1,000 rows (stay under ~4 MB per request), edges grouped by `relation` in batches of about 5,000. Include `kind` and `source_file` from graph.json.
6. Merge leftover duplicates with `apoc.refactor.mergeNodes(nodes, {properties: "discard", mergeRels: true})`.

For saved queries stored as Neo4j properties, write the payload with a JSON library rather than shell-quoted curl: single quotes in Cypher get mangled.

## Styling in Neo4j's web UI

Recent Neo4j web UIs ("Query") ignore GRASS stylesheets and colour a node by its highest-priority label, which is usually the oldest label (`Node`). Reorder in the UI: results overview → node labels → styling priority, and drag your view label first. The classic Browser, which applies GRASS, is reachable at `/browser/?prefersOldBrowser=true`.
