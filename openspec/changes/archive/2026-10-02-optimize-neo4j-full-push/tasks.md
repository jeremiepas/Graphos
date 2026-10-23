# Tasks — Optimize Neo4j FullPush

## 1. Test harness and check criteria first

- [x] 1.1 Add Hspec specs for the hash helper: sha1 lowercase-hex over UTF-8 bytes, parity vectors shared with the backfill Cypher (e.g. `sha1("abc") = a9993e…`), property test that any Text round-trips to 40 hex chars
- [x] 1.2 Add specs for batch planning: node batches split at 1,000 rows OR ~4 MB payload (a single oversized node ships alone); edge rows grouped by relationship type with 5,000-row chunks; nodes always flushed before edges
- [x] 1.3 Add specs for statement generation: node UNWIND MERGE keys on `id_hash` and `ON CREATE SET` includes raw `id`; edge UNWIND matches both endpoints by `id_hash`; relationship type backtick-escaped, never parameterized into strings

## 2. Hash and statement generation (Domain/UseCase-pure)

- [x] 2.1 Add `nodeIdHash :: NodeId -> Text` (sha1 hex, UTF-8) in a pure module; wire it into node row construction
- [x] 2.2 Replace `generateParameterizedNodeStatement`/`generateParameterizedEdgeStatement` per-entity statements with UNWIND row-batch builders (one statement per entity kind per chunk), including community and BELONGS_TO rows
- [x] 2.3 Implement the batch planner (row-count + payload-byte caps) as a pure function returning ordered chunks: schema phase, node chunks, edge chunks by type

## 3. Transport (Infrastructure)

- [x] 3.1 Replace the `curl` subprocess + `/tmp` payload file in `pushBatch` with `http-client`: one shared `Manager`, keep-alive, basic auth, body in memory; surface Neo4j's error JSON in failures
- [x] 3.2 Schema setup phase: backfill `id_hash` on hashless `:Node` rows (batched `SKIP/LIMIT` loop, no APOC dependency), then `CREATE CONSTRAINT node_id_hash_unique IF NOT EXISTS …` and `:Community(id)` — each schema statement in its own transaction (Neo4j forbids schema+write mixing)
- [x] 3.3 Abort the push with a named, actionable error when constraint creation fails (pre-existing duplicate `id_hash` rows); no data statements may be sent after a schema failure

## 4. Pipeline integration

- [x] 4.1 Wire the new push into `UseCase/Export.hs` (post-pipeline push) keeping `pushToNeo4j`'s signature and the three push modes; FullPush uses the new path end to end
- [x] 4.2 Wire the streaming path in `UseCase/Pipeline/Incremental.hs`: per-file batches use UNWIND + hash rows; the edge-repair pass reuses the edge chunk builder
- [x] 4.3 Remove the curl runtime dependency from the Neo4j path; verify `cabal build` has no new warnings (`-Werror` clean)

## 5. Validation and docs

- [x] 5.1 Integration test against a disposable Neo4j (docker): push a synthetic 10k-node graph twice; assert exact node/edge count parity with the input and zero growth on the second push (idempotence)
- [x] 5.2 Integration test: a node whose id exceeds 8 KB pushes successfully and is findable via `id_hash`; a database seeded with duplicate `id_hash` rows makes the push abort during schema setup with the constraint name in the error
- [x] 5.3 Benchmark on a 100k-node graph against localhost Neo4j; record the time in the workflow doc (< 5 min required, ~1 min expected)
- [x] 5.4 Update `docs/workflows/12-neo4j-push.md`: mode-comparison timings, the `id_hash` key scheme, migration note for consumers that MERGE by raw `id`
