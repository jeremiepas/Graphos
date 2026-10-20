# Optimize Neo4j FullPush: constraint-first schema, UNWIND batching, persistent HTTP

## Why

FullPush of a real-world graph (119k nodes / 128k edges, solario-core) takes hours — measured at ~1,000 nodes/min — and the specs even codify "~990k statements, 2–4 hours for 100k graph" as accepted behavior. The slowness is architectural, not inherent: the push never creates an index or uniqueness constraint on `:Node(id)`, so every `MERGE (n:Node {id: $id})` and every edge's two `MATCH`es do a full label scan over a growing node set (O(N²) in total); each node and edge is its own Cypher statement; and every 50-statement batch forks a fresh `curl` subprocess with a temp-file payload and its own HTTP transaction (~4,900 forks for a 247k-statement push). A FullPush of a 100k-node graph should take minutes, not hours.

## What Changes

- **Schema setup before data**: the push SHALL store `id_hash = sha1(id)` on every node and create a uniqueness constraint on `:Node(id_hash)` (and `:Community(id)`) with `IF NOT EXISTS` before the first data statement, turning every `MERGE`/`MATCH` lookup into an indexed seek instead of a label scan. A constraint on raw `id` is NOT viable: `External` node ids embed whole source snippets and exceed Neo4j's RANGE-index key limit (~8 KB), which leaves the index in state FAILED and silently degrades every lookup back to a label scan (observed in production: the index reported FAILED/0% and batches ran 30+ s).
- **UNWIND row batching**: replace one-statement-per-entity with one parameterized `UNWIND $rows AS row` statement per entity kind per chunk (nodes, then edges grouped by relationship type), with chunks of thousands of rows instead of 50 statements. Eliminates the O(statements) overhead and lets Neo4j plan once per chunk.
- **Persistent HTTP connection**: replace the per-batch `curl` subprocess + `/tmp` payload file with an in-process HTTP client (`http-client` Manager, already a transitive dependency) reusing one connection across the whole push.
- **Ordering guarantee kept**: nodes are fully pushed before edges, so edge `MATCH`es always find their endpoints (the current streaming per-file interleave plus edge-repair pass is preserved for `--neo4j` during pipeline, but each per-file batch also uses UNWIND).
- **Spec timing expectations updated**: FullPush of a 100k-node graph targets minutes (< 5 min on localhost), not 2–4 hours; batch-size requirement changes from "≤50 statements per request" to "UNWIND chunks sized in rows (default 5,000)".
- No CLI changes; `--neo4j`, `--neo4j-push`, `--neo4j-push-mode`, `--neo4j-subgraph-size` keep their meaning. Not breaking.

## Capabilities

### New Capabilities

(none)

### Modified Capabilities

- `12-neo4j-push`: FullPush performance requirement (minutes, not 2–4 h), schema setup (constraint-first) added to the push contract, batching requirement changes from ≤50 statements/request to UNWIND row chunks. (PRD §9, workflow 12)
- `neo4j-integration`: `Infrastructure.Export.Neo4j` statement-generation requirement changes — parameterized UNWIND per entity kind, uniqueness constraints created before data, persistent HTTP transport instead of curl subprocesses. (PRD §9.1)

## Impact

- **Code**: `src/Graphos/Infrastructure/Export/Neo4j.hs` (statement generation, batching, transport — the bulk of the change); call sites in `UseCase/Export.hs` and `UseCase/Pipeline/Incremental.hs` keep their signatures. Memgraph export (`13-memgraph-push`) is intentionally untouched in this change; the same pattern can be ported later.
- **Dependencies**: uses `http-client`/`http-client-tls` already in the build plan (LLM client); removes the runtime dependency on a `curl` binary for Neo4j push. Adds `cryptohash-sha1` for `id_hash` (sha1 lowercase-hex).
- **Docs**: `docs/workflows/12-neo4j-push.md` mode-comparison timings.
- **Operational**: the constraint is created with `IF NOT EXISTS`, so pushing into a database that already has one (e.g. created manually) is a no-op; pushing into a database with duplicate `:Node(id_hash)` rows will fail constraint creation — surfaced as a clear error instead of silent slowness (duplicates were observed in practice after unconstrained MERGE pushes). Databases populated by older graphos versions lack `id_hash`; the push backfills it — client-side (vanilla Neo4j has no sha1 function), batched read/hash/write windows — before creating the constraint.
