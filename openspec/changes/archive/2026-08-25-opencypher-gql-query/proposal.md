## Why

`graphos query TERM` is full-text only: it ranks nodes by label similarity and
expands a BFS neighbourhood. It cannot express **structural** questions — "every
`Function` that `Calls` a node whose `source_file` matches `src/services/.*`",
"documents that `References` a `Type` two hops from `RunReconciler`", "nodes with
no incoming `Imports` edge". Agents work around this by running several text
queries and stitching results by hand (the same pain that motivated
`research-view`).

The graph is already a **property graph** — nodes carry a kind (`Function`,
`Type`, `Module`…), a `source_file`, a community and more; edges carry a typed
relation (`Calls`, `Imports`, `Contains`, `References`, `documents`, `inferred`).
That is exactly the model openCypher / GQL (ISO/IEC 39075) is designed to query.
graphos also already speaks Cypher on the **export** side (Neo4j / Memgraph
push), so a Cypher-family surface is idiomatic for its ecosystem.

There is no ready-made openCypher engine on Hackage, so this is a bounded,
in-process **read-only subset** built on a Haskell parser-combinator library.

## What Changes

- Add a read-only **openCypher / GQL subset** query surface over the in-memory
  graph: `MATCH` node/relationship patterns, `WHERE` predicates on node/edge
  properties, variable-length paths (`-[:REL*1..3]->`), `RETURN` projection with
  `DISTINCT`/`LIMIT`/`ORDER BY`, and `count(...)`.
- Map graphos's model to the property-graph model deterministically: a node's
  **Cypher label** = its `kind`; a relationship's **type** = its `relation`;
  node/edge fields (`source_file`, `community`, `is_bridge`, `weight`,
  `confidence`, …) are queryable **properties**; the source snippet is the
  `label`/`text` property.
- Parse the grammar with **megaparsec** (new dependency); evaluate purely over
  the existing `Graph` + `GraphIndex`, reusing `CachedFGL` for variable-length
  path expansion. No Neo4j server, no mutation.
- Expose it as a CLI command `graphos cypher "MATCH … RETURN …"` (with `--json`)
  and an MCP tool `cypher_query`, both bounded by the existing query result
  budget.

## Capabilities

### New Capabilities
- `cypher-query`: a read-only openCypher / GQL (ISO/IEC 39075) subset that
  evaluates structural pattern queries in-process over the property graph.

### Modified Capabilities
<!-- Reuses GraphIndex and the query result budget; does not change the existing
     `query`/`path`/`symbols` commands. -->

## Impact

- **New parser module** (`Infrastructure/Query/Cypher` or `Domain/Query/Cypher`)
  and evaluator; **megaparsec** added to `graphos.cabal`.
- **CLI/Parser**: new `cypher` subcommand; **MCP**: new `cypher_query` tool.
- Reuses `GraphIndex`, `CachedFGL`, and the token/result budget — no change to
  `query`, `path`, `symbols`, `research`.
- Read-only: no write/mutation clauses; no user-facing breaking change.
