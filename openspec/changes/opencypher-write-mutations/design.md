## Context

The read-only openCypher/GQL subset engine (change `opencypher-gql-query`, archived
2026-08-25) froze a clean split: `AST.hs` (frozen subset types) → `Parser.hs`
(megaparsec combinator grammar) → `Eval.hs` (pure evaluator over `Graph` +
`GraphIndex`) → `Mapping.hs` (model → property-graph mapping). Write clauses are
today reserved words that produce parse errors; the `cypher-query` spec requires
"write clause rejected … the loaded graph is left unchanged".

The graph model constrains what mutations can mean:

- `Graph` = `gNodes :: Map NodeId Node` + `gEdges :: Map (NodeId, NodeId) Edge`
  (**keyed by endpoint pair — no parallel edges**), adjacency maps maintained by
  `buildGraph`; `gDirected` toggles directionality.
- `Node.kind` is the single Cypher label; `nodeExtra :: Maybe Value` (Aeson) is an
  existing extensibility field. Same for `edgeExtra`.
- `Relation` is a **closed enum** (`Calls, Imports, Extends, Implements, References,
  Contains, DependsOn, Inferred`); `textToRelation` degrades unknowns to `inferred`.
- `graph.json` is a **derived artifact** (extraction pipeline output) with a
  versioned schema (`graph-json-contract`) and an existing checkpoint/backup
  infrastructure; the `merge` command already persists modified graphs, so
  write-back has precedent.

Surfaces already in place: `graphos cypher QUERY` (CLI), MCP `cypher_query` tool,
`graphos serve` HTTP API (`/api/query|path|explain|symbols|neighbors`), renderers
(`Render.hs`: text + `{"columns","rows","truncated"}` JSON), shared budget cap.

Open coordination point: `cypher-eval-graphindex` (open change) makes the evaluator
use `GraphIndex`/`CachedFGL`. Mutations reuse MATCH binding enumeration, so they
inherit that work — independent, but landing order matters for perf.

## Goals / Non-Goals

**Goals:**

- An openCypher **write clause subset** evaluated in-process, pure (Domain), with a
  mutation summary — the same dialect the Neo4j/Memgraph export paths emit.
- Read-only stays the **default** everywhere; mutation requires an explicit
  surface opt-in (`--write`, `cypher_mutate`, `POST /api/cypher/mutate`).
- In-memory mutation by default; explicit persistence of `graph.json` with backup.
- Deterministic, documented semantics for the model conflicts (labels, parallel
  edges, closed relation enum).

**Non-Goals:**

- Full ISO GQL / openCypher coverage: no `WITH`/`UNWIND` pipelines, no subqueries,
  no parameters, no list comprehensions, no temporal types (additive later, as the
  read engine did).
- Custom relationship types beyond the closed `Relation` enum.
- Mutation replay/overlay that survives re-extraction (mutations are overwritten by
  the next pipeline run; documented).
- Transactionality: a mutation statement applies its operations sequentially in
  memory; no all-or-nothing rollback for partial failures (budget-bounded and
  pure, so a crash loses only unsaved state — `--write` is atomic per file).

## Decisions

- **Parser: recognize write clauses in the grammar; gate at the command layer.**
  The megaparsec grammar grows mutation statement forms; the parser itself stays
  mode-free. The `cypher-query` read-only requirement changes from "unknown
  grammar" to "recognized but not permitted": plain `graphos cypher` (and
  `cypher_query` MCP, `GET` HTTP) return a clear error naming the clause when a
  mutation statement is submitted, without a write surface. This keeps one parser
  and makes the read surface's error actionable ("requires --write").
  - *Alternative considered:* two parsers / a parser flag — rejected: duplicated
    grammar, drift risk between read and write acceptance.
- **AST shape: `CypherStatement = ReadStatement CypherQuery | MutStatement Mut`.**
  `Mut { mMatch :: Maybe (patterns, Maybe Predicate), mOps :: [MutOp],
  mReturn :: Maybe ReturnClause }`; `MutOp = MCreate [PatternElem] |
  MMerge PatternElem [OnClause] | MSet [SetItem] | MRemove [RemoveItem] |
  MDelete Bool [Text]` (Bool = detach). Existing `CypherQuery` type and all read
  specs stay untouched.
- **Evaluator: reuse MATCH enumeration, then fold ops over bindings.** A mutation
  runs: bind paths (existing `decomposePaths`/`walk` machinery, same budget cap),
  fold `mOps` left-to-right over `Graph` (pure `State Graph`-style threading),
  then evaluate the optional `RETURN` against the final graph with a `MutationSummary`
  exposed as implicit columns (`nodes_created`, `rels_created`, `properties_set`,
  `properties_removed`, `nodes_deleted`, `rels_deleted`). Ops apply to matched
  bindings; `CREATE`/`MERGE` patterns may reference bound variables or create
  fresh anonymous nodes.
- **Labels: single label remains `nodeKind`; extra labels go to
  `nodeExtra.extra_labels` (JSON array).** `SET n:Label` appends if absent;
  `REMOVE n:Label` removes from `extra_labels` if present, and clears `kind` when
  removing the primary label (resulting labelless nodes match `()`-only patterns,
  matching today's read semantics). `Mapping.hs` learns to expose `extra_labels`
  so multi-label `MATCH` patterns consult them.
  - *Alternative considered:* adding an `extraLabels` field to `Node` — rejected
    for now: touches the versioned `graph.json` schema for marginal benefit;
    `nodeExtra` already round-trips.
- **Properties: known fields stay primary; `SET` of non-model properties lands in
  `nodeExtra`/`edgeExtra`.** `SET n.label = x`, `n.source_file`, edge `weight`/
  `confidence` write the model fields; anything else writes the extra object.
  `Mapping.hs` resolves reads from the extra object so `WHERE`/`RETURN`/`SET`
  round-trip. `REMOVE n.prop` deletes from the extra object; removing a model
  field removes it from the JSON write-back (restored to defaults on next
  extraction).
- **Relationship types stay inside the closed `Relation` enum.** `CREATE (a)-[:Calls]->(b)`
  is fine; `[:Collabs]` is a parse-time error naming the enum. This preserves
  graph validation, Leiden/analysis invariants, and the JSON contract.
- **No parallel edges: `CREATE` of a relationship whose (source, target, type)
  pair already exists upserts** — existing edge's properties are merged/overwritten
  by the new pattern's properties, `rels_created` does not increment, and the
  summary counts `rels_upserted` instead. `MERGE` on relationships naturally uses
  the same upsert semantics (that is `MERGE`'s whole point).
- **Node identity on `CREATE`:** `id` property, if provided and unused, becomes the
  `NodeId`; a duplicate id is an eval error naming the colliding id. Without `id`,
  a deterministic generated id `gen-<hash>` (hash of pattern text + current node
  count) is used. `MERGE` matches on `(label, id)` first, then on the full
  property map of the pattern; if nothing matches, a node is created (id required
  or generated as above).
- **Persistence: `--write` (CLI) / `persist: true` (MCP/HTTP) writes the mutated
  graph back to the loaded `graph.json` path** through the existing graph writer,
  after copying the original to `graph.json.bak-<timestamp>`. Schema version,
  `communities`, `cohesion`, `god_nodes`, and community aggregates are recomputed
  where cheap (degree/adjacency) or preserved as-is from the loaded file (full
  re-clustering is out of scope). Documented caveat, surfaced in the summary
  output: the next re-extraction (`graphos <path>`) overwrites the file and
  discards mutations.
  - *Alternative considered:* writing to a new output file only — rejected for the
    default; a `--output` escape hatch remains available via the existing `merge`
    command for users who want that.
- **Summary contract (JSON):** `{"summary": {"nodes_created": n, "rels_created": n,
  "rels_upserted": n, "properties_set": n, "properties_removed": n,
  "nodes_deleted": n, "rels_deleted": n}, "rows": ..., "columns": ...,
  "truncated": bool}` — the same renderer family as query results, with `summary`
  added. Text rendering prints the summary line plus any returned rows.
- **HTTP shape: `POST /api/cypher/mutate` with body `{"query": "...",
  "persist": false}`** returning the mutation summary JSON; response for a
  read-only statement in the mutate route is a 400 naming the restriction (and
  vice versa, `GET /api/cypher` stays out of scope — the read path remains the
  CLI/MCP surface). Persisting rewrites `graph.json` once per request when
  `persist: true`.
- **MCP: new `cypher_mutate` tool** (`query :: String`, `persist :: Bool` default
  false) — separate from `cypher_query` so tool listings make the capability
  explicit to agents; `cypher_query` keeps its read-only contract verbatim.

## Supported write subset (initial)

- `CREATE (p1), (p2), (a)-[r:REL {..}]->(b), ...` — nodes and relationships,
  inline properties, bound or fresh variables.
- `MERGE (n:Kind {id: x}) [ON CREATE SET ... | ON MATCH SET ...]` and
  `MERGE (a)-[:REL]->(b)` with the same ON clauses.
- `SET n.prop = <expr>`, `SET n.prop += <map-expr>` is **not** in the subset
  (plain assignment and `SET n:Label` only), multiple items comma-separated.
- `REMOVE n.prop, n:Label`.
- `DELETE a, r` / `DETACH DELETE a` — `DELETE` of a node with remaining
  relationships is an eval error (naming the node) unless `DETACH`.
- Optional `RETURN` with the existing read expression subset (plus the summary
  counters as implicit bindings).

## Risks / Trade-offs

- [Mutations lost on re-extraction] → documented in spec + summary output; users
  needing durable curation should push to Neo4j/Memgraph (existing paths) or
  accept the caveat.
- [Broad MATCH before mutation enumerates large bindings] → the existing budget
  cap bounds enumeration; DELETE/DETACH over a huge binding set is a user choice
  gated behind the explicit write surface.
- [Schema drift via nodeExtra/edgeExtra] → extras already round-trip today
  (unknown-tolerant reader); mutation writes use the same field, so no new schema
  risk beyond existing behavior.
- [Read surfaces regress] → `cypher_query` MCP and plain CLI `cypher` keep
  byte-identical outputs for read queries; existing ParserSpec/EvalSpec goldens
  must stay green.
- [Write-back clobbers concurrent edits] → backup file + atomic write; `serve` is
  the only concurrent surface and mutations there are explicitly opted into.

## Migration Plan

- Purely additive: new AST constructor, new grammar rules, new evaluator entry
  point `evaluateMutation`, new CLI flag, new MCP tool, new HTTP route. Existing
  commands/specs unchanged except the `cypher-query` read-only requirement (error
  message wording for write clauses in non-write contexts).
- Rollback = drop the `--write` flag registration, MCP tool, HTTP route; the
  parser changes are inert without the mutation entry point.
- Verify with `cabal build --flag dev -Werror`, `cabal test` (new Parser/Eval
  mutation suites + existing goldens), and an end-to-end mutation + `--write`
  round-trip over a fixture graph.

## Open Questions

- Should `SET n += {map}` (map merge) land with this change or follow-up?
  Proposed: follow-up (keeps expression evaluator small).
- Should the HTTP route also expose a dry-run (`"dry_run": true`) that returns the
  would-be summary without applying? Proposed: cheap to add, defer unless needed.