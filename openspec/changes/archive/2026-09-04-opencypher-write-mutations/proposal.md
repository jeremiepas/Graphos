## Why

Graphos ships a read-only openCypher/GQL subset engine (`MATCH`/`WHERE`/`RETURN`,
`src/Graphos/Domain/Query/Cypher/*`) that is proven and tested, but write clauses
(`CREATE`, `MERGE`, `SET`, `REMOVE`, `DELETE`) are parse errors by design. Agents and
users therefore cannot manipulate the graph with the same standard query language they
already use to query it — graph curation today requires hand-editing `graph.json` or
round-tripping through Neo4j/Memgraph.

No maintained Haskell openCypher write library exists on Hackage (verified during the
`opencypher-gql-query` change; `hasbolt` is a client, not an evaluator). The idiomatic
path is to extend the existing hand-rolled megaparsec engine with the openCypher write
clause subset — the same dialect the repo's Neo4j/Memgraph export paths already emit —
so one query language covers read and manipulate, locally, without a graph database.

## What Changes

- Extend the Cypher parser/AST to accept the openCypher write clause subset on top of
  the existing read subset: `CREATE` (nodes/relationships), `MERGE` (with
  `ON CREATE SET` / `ON MATCH SET`), `SET` (properties), `REMOVE` (properties),
  `DELETE` / `DETACH DELETE`, and an optional trailing `RETURN` — combined with
  `MATCH`/`WHERE` (mutate what you matched).
- Add a pure mutation evaluator that applies the matched bindings to the in-memory
  graph and returns a mutation summary (nodes created, relationships created,
  properties set/removed, nodes/relationships deleted) in the same row/JSON shape as
  query results.
- Reconcile the write subset with the graph model: single-label nodes with extra
  labels stored in `nodeExtra.extra_labels`, arbitrary `SET` properties stored in
  `nodeExtra`, relationship types restricted to the existing closed `Relation` enum,
  and no-parallel-edges `CREATE` upserting the existing pair edge's properties.
- Mutations apply in memory by default; an explicit `--write` (CLI) / `persist`
  (MCP, HTTP) opt-in persists the mutated graph back to `graph.json` with a
  timestamped backup. Documented caveat: the next re-extraction overwrites mutations.
- Wire the mutation surface end to end: `graphos cypher QUERY --write` (CLI),
  a new `cypher_mutate` MCP tool, and `POST /api/cypher/mutate` on `graphos serve`.
  Read-only enforcement stays the default everywhere: write clauses are rejected
  unless the mutation surface is explicitly selected.

## Capabilities

### New Capabilities

- `cypher-mutation`: The openCypher write clause subset evaluated against the
  in-memory property graph — accepted syntax, mutation semantics, model
  reconciliation rules (labels, properties, relationship types, upsert), mutation
  summary contract, persistence opt-in, and read-only-by-default gating.

### Modified Capabilities

- `cypher-query`: The read-only enforcement requirement changes — write clauses
  become *recognized* syntax that is gated at the command layer instead of rejected
  as unknown grammar; the `cypher` CLI command gains `--write` and mutation
  evaluation, while plain `graphos cypher` remains strictly read-only.
- `query-http-port`: Adds `POST /api/cypher/mutate` (explicit write context) served
  from the single in-memory graph, with an optional `persist` body flag to write
  `graph.json` back; `GET /api/cypher` stays read-only.
- `mcp-server`: Adds the `cypher_mutate` tool with an explicit `persist` parameter;
  the existing `cypher_query` tool remains strictly read-only.
- `graph-json-contract`: Adds a requirement for mutation persistence — `--write`
  saves the mutated graph under the same versioned schema (schema version
  unchanged), with a timestamped backup of the previous file and derived sections
  (communities, cohesion, aggregates) recomputed or preserved.

## Impact

- **Code (additive):**
  - `src/Graphos/Domain/Query/Cypher/AST.hs` — mutation statement AST (`MutStatement`, `MutOp`)
  - `src/Graphos/Domain/Query/Cypher/Parser.hs` — write clause grammar (megaparsec, additive)
  - `src/Graphos/Domain/Query/Cypher/Eval.hs` — mutation evaluator reusing MATCH binding enumeration
  - `src/Graphos/Domain/Query/Cypher/Mapping.hs` — expose `extra_labels` / `nodeExtra` properties to reads
  - `src/Graphos/CLI/Parser.hs` + `app/Main.hs` — `--write` flag on `cypher`, mutation render path
  - `src/Graphos/Infrastructure/Server/MCP.hs` — `cypher_mutate` tool
  - `src/Graphos/Infrastructure/Server/HTTP.hs` (serve) — `POST /api/cypher/mutate` route
  - persistence helper (Infrastructure) — backup + write-back of `graph.json`
- **Specs:** new `cypher-mutation`; deltas to `cypher-query`, `query-http-port`,
  `mcp-server`, `graph-json-contract`
- **No new dependencies** — megaparsec, aeson, containers already in use
- **Risks:** persisted mutations are overwritten by the next re-extraction
  (documented); broad `MATCH` before mutation can enumerate large bindings (bounded
  by the existing budget cap); read-only surfaces must remain byte-identical