# Design — Optimize Neo4j FullPush

## Context

`Graphos.Infrastructure.Export.Neo4j` pushes graphs to Neo4j by generating one parameterized Cypher statement per node and per edge, chunking them 50 statements per request, writing each request payload to a temp file and shelling out to `curl` against the transactional HTTP endpoint. Nothing creates an index, so every `MERGE (n:Node {id: $id})` and every edge's two `MATCH`es label-scan the whole `:Node` set.

Measured on a real graph (solario-core main: 119,097 nodes / 127,728 edges, localhost Neo4j 5.26 community):

- graphos streaming FullPush: ~1,000 nodes/min — multi-hour push; the specs codified "2–4 hours for 100k graph" as accepted.
- A naive `CREATE INDEX FOR (n:Node) ON (n.id)` ends in state **FAILED (0% populated)**: `External` node ids embed whole source snippets, exceeding the RANGE-index key size limit (~8 KB). The failure is silent — queries keep running, unindexed.
- With a sha1 `id_hash` property, a uniqueness-capable RANGE index, and `UNWIND` row batches over a persistent HTTP connection, the same full graph (nodes + edges) pushed in **~35 seconds** (~12,000 rows/s), validated end-to-end with exact count parity against graph.json.

## Goals / Non-Goals

**Goals:**

- FullPush of a 100k-node graph in minutes (< 5 min target; ~1 min measured) on localhost.
- Idempotent re-push: running the push twice creates no duplicate nodes or relationships.
- Clear, early failure instead of silent degradation (constraint violations abort before data flows).
- Works against databases populated by older graphos versions (hashless nodes get backfilled).

**Non-Goals:**

- Memgraph push (workflow 13) — same pattern applies but is ported separately.
- SubgraphPush / CommunityPush statement generation (they inherit the constraint + transport improvements for free but keep their selection logic).
- Switching to the Bolt protocol — the HTTP transactional API is sufficient at these volumes.
- Changing what is pushed (properties, labels, relationship types stay as today).

## Decisions

### D1 — Key nodes by `id_hash = sha1(id)`, not raw `id`

A uniqueness constraint (or index) on raw `id` is not viable: RANGE index keys cap at ~8 KB and `External` ids embed source text far beyond it; population fails silently (state FAILED) and every lookup label-scans. Alternatives considered:

- **TEXT index on `id`** — Lucene-backed, also has a term-size cap (~32 KB) and oversized values are simply not indexed, which would make `MERGE` seeks *miss* those nodes and create duplicates. Rejected: correctness risk.
- **Shorten `External` ids at extraction** — the right long-term fix but a breaking change to node identity across every consumer (graph.json, MCP, HTML viewer). Out of scope here.
- **`id_hash` property** — sha1 hex is 40 chars, always indexable, computable identically client-side (pusher) and server-side (backfill). Chosen.

The raw `id` stays on the node as a regular property; only the lookup key changes. Hash parity rule: lowercase hex sha1 over the id's UTF-8 bytes.

### D2 — Schema setup phase before any data

Push order: (1) backfill `id_hash` on hashless `:Node` rows, (2) `CREATE CONSTRAINT node_id_hash_unique IF NOT EXISTS FOR (n:Node) REQUIRE n.id_hash IS UNIQUE` (+ `:Community(id)`), (3) data. Schema statements go in their own transactions — Neo4j forbids mixing schema and write statements in one transaction. The uniqueness constraint (not a plain index) is deliberate: unconstrained `MERGE` pushes were observed to leave duplicate node rows; the constraint turns that corruption into an immediate, named error.

**Implementation deviation (approved during apply):** the original plan computed hashes
server-side (`SET n.id_hash = sha1(n.id)`), but vanilla Neo4j — community edition,
no APOC — has no `sha1()` function and its Cypher PL cannot express one sanely (no
bitwise operations). Verified against Neo4j 5.26 community (`Unknown function 'sha1'`).
The backfill therefore hashes **client-side**: batched windows of `MATCH … WHERE
n.id_hash IS NULL … RETURN n.id` (5,000 rows, ordered by `elementId`), hashed with
the same pure `Graphos.Domain.CypherHash.nodeIdHash` the pusher uses (parity by
construction), written back with one `UNWIND … MATCH (n:Node {id}) SET n.id_hash`
transaction per window. No APOC dependency is retained, satisfying the original
intent; the cost is reading ids back (one batched read per 5,000 nodes, once per
database).

### D3 — UNWIND row batches, one statement per entity kind

Nodes: `UNWIND $rows AS row MERGE (n:Node {id_hash: row.id_hash}) ON CREATE SET n.id = row.id, …` with batches capped at 1,000 rows **or** ~4 MB of payload, whichever comes first (node labels carry full source text; byte-capping keeps requests bounded). Edges: grouped by relationship type (the type cannot be a parameter), `UNWIND` batches of 5,000 rows, `MATCH` both endpoints by `id_hash`, `MERGE` the relationship. All nodes are flushed before the first edge batch so endpoint `MATCH`es always resolve.

### D4 — In-process persistent HTTP client

Replace per-batch `curl` + temp file with `http-client` (already a transitive dependency via the LLM client): one `Manager`, keep-alive connection reuse, request body built in memory. Removes ~5,000 process forks and `/tmp` writes per push, and lets errors carry the full Neo4j response.

## Risks / Trade-offs

- [Older databases lack `id_hash`] → backfill phase handles it; a database with *duplicate ids* (from past unconstrained pushes) makes constraint creation fail → the error names the constraint; operator deduplicates (e.g. `apoc.refactor.mergeNodes`) and re-runs. Failing loudly is the intended behavior.
- [sha1 collision] → 2⁻⁸⁰ birthday bound at these cardinalities; negligible. Raw `id` remains on the node for verification.
- [4 MB/1,000-row node batches with huge labels] → worst-case single node label larger than the cap still ships alone in its own batch; the transactional API handles multi-MB payloads.
- [Existing consumers querying by `n.id`] → unchanged; `id` is still stored. Only pushes key on `id_hash`. Consumers doing their own `MERGE` by `id` should migrate, documented in workflow 12.
- [Streaming mode (`--neo4j` during pipeline) interleaves per-file batches] → each per-file batch uses the same UNWIND + hash scheme; the edge-repair pass at the end guarantees edge completeness.

## Migration Plan

1. Implement behind the existing CLI — no flag changes; first push to an existing database runs the backfill + constraint path automatically.
2. Rollback: revert the module; data pushed by the new path remains valid for the old path (ids intact). The constraint can be dropped manually if the old unconstrained path must write again.
3. Update `docs/workflows/12-neo4j-push.md` timings and the mode-comparison table.

## Open Questions

- Should `SubgraphPush`/`CommunityPush` adopt UNWIND batching in the same change or a follow-up? (Low volume — they are already fast; leaning follow-up.)
- Backfill implementation: plain batched Cypher loop vs `apoc.periodic.iterate` — APOC is present on the dev stack but SHALL NOT be a hard dependency; a `SKIP/LIMIT` loop over hashless nodes works on vanilla Neo4j.
