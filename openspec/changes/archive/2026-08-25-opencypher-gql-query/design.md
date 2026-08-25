## Context

The graph is a directed property graph held in memory (`Graph` = `Map NodeId
Node` + `Map EdgeId Edge`), with a `GraphIndex` (label/id lookups) and a
`CachedFGL` used by the analysis algorithms. Queries today are full-text
(`queryGraph`) — there is no way to match on structure. openCypher / GQL
(ISO/IEC 39075) is the standard language for exactly this, and graphos already
emits Cypher for Neo4j/Memgraph, so the concepts already live in the codebase.

## Goals / Non-Goals

**Goals:**
- A read-only, in-process openCypher/GQL **subset** sufficient for structural
  code-graph questions.
- Deterministic mapping from graphos's model to the property-graph model.
- Bounded, index-backed evaluation (no full scans when a pattern is anchored).

**Non-Goals:**
- Write/mutation clauses (`CREATE`, `MERGE`, `SET`, `DELETE`).
- Full ISO GQL / openCypher coverage (subqueries, `WITH` pipelines, list
  comprehensions, temporal types) — additive later.
- Delegating to a Neo4j server (that is the existing *push* path, not local query).

## Decisions

- **Parse with megaparsec** (new dep). No maintained openCypher parser exists on
  Hackage, so a subset grammar is authored on a modern combinator library.
  - *Alternative considered:* `hasbolt` — rejected, it is a Bolt **client** to a
    running Neo4j, not a local parser/evaluator.
- **Property-graph mapping is fixed and documented:** Cypher node label =
  `nodeKind`; relationship type = `edgeRelation`; properties = the remaining
  node/edge fields (`source_file`, `community`, `is_bridge`, `degree`, `weight`,
  `confidence`, plus `label`/`text` for the snippet).
  - *Alternative considered:* a single synthetic `:Node` label — rejected, loses
    the natural `(:Function)`/`(:Type)` selectivity.
- **Evaluate over `GraphIndex` + `CachedFGL`.** Anchored patterns start from an
  index lookup; variable-length paths use the cached FGL (shared with analysis).
  - *Alternative considered:* naive per-pattern scans — rejected, O(N) per query.
- **Read-only subset, budget-bounded.** Results honour the existing query result
  budget so a `MATCH` cannot dump the whole graph.

## Supported subset (initial)

- `MATCH` with node patterns `(a:Kind {prop: value})` and relationship patterns
  `-[:REL]->`, `-[r:REL*1..3]->`, undirected `-[:REL]-`.
- `WHERE` with `=`, `<>`, `<`, `>`, `<=`, `>=`, `IN`, `STARTS WITH`, `CONTAINS`,
  `=~` (regex), `AND`/`OR`/`NOT`, `IS NULL`.
- `RETURN` projection (`a`, `a.source_file`, `count(*)`), `DISTINCT`, `ORDER BY`,
  `LIMIT`, `SKIP`.

## Risks / Trade-offs

- [Grammar scope creep] → freeze the subset in the spec; anything outside it is a
  parse error with a clear message, not silent partial behaviour.
- [Cypher semantics subtleties: nulls, cartesian products] → document evaluation
  order; require at least one anchored pattern element or a `LIMIT`.
- [Large-graph performance] → planner must start from the most selective indexed
  element; enforce the result budget; `EXPLAIN`-style node/row cap.

## Migration Plan

- Additive: a new `cypher` command and `cypher_query` MCP tool; no change to
  existing commands. Rollback = remove the subcommand/tool registration.
- Verify with `cabal build --flag dev -Werror`, `cabal test` (parser + evaluator
  suites), and golden queries over a fixture graph.
