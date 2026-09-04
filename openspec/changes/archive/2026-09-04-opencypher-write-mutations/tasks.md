## 1. AST + parser: recognize the write subset

- [x] 1.1 Extend `src/Graphos/Domain/Query/Cypher/AST.hs` with `CypherStatement = ReadStatement CypherQuery | MutStatement Mut` and mutation types (`Mut { mMatch, mOps, mReturn }`, `MutOp = MCreate [PatternElem] | MMerge PatternElem [OnClause] | MSet [SetItem] | MRemove [RemoveItem] | MDelete Bool [Text]`, `OnClause = OnCreate [SetItem] | OnMatch [SetItem]`, `SetItem` incl. `SetProp Text Expr` / `SetLabel Text`, `RemoveItem` incl. `RemoveProp Text` / `RemoveLabel Text`); keep the existing `CypherQuery` type untouched
- [x] 1.2 In `Parser.hs`, add grammar for `CREATE`, `MERGE ... ON CREATE SET / ON MATCH SET`, `SET`, `REMOVE`, `DELETE` / `DETACH DELETE` and the optional trailing `RETURN`; make `parseQuery`/new entry point return `CypherStatement`; unknown relationship types in write patterns are parse errors naming the type and the supported `Relation` vocabulary
- [x] 1.3 Parser tests: accept each write clause form (create node/rel, merge with ON clauses, set prop/label, remove prop/label, delete/detach, match+set, return after set); reject out-of-subset constructs (`WITH`, `UNWIND`, `SET n += {...}`) with position; existing read-golden parse results unchanged

## 2. Read-only gating at the command layer

- [x] 2.1 Thread an explicit write-permission flag through the command layer so plain `graphos cypher` (CLI), MCP `cypher_query`, and read-oriented HTTP paths reject `MutStatement` with an error naming the clause and pointing at the mutation surface (`--write` / `cypher_mutate` / `POST /api/cypher/mutate`)
- [x] 2.2 Tests: read surface rejects each write clause with the actionable message; read query behavior/byte-output unchanged (existing CLI + MCP specs stay green)

## 3. Mutation evaluator (Domain, pure)

- [x] 3.1 Add `evaluateMutation` to `Eval.hs`: enumerate MATCH bindings via the existing machinery (same budget cap), fold `mOps` left-to-right over `Graph`, then evaluate optional `RETURN` against the final graph; pure, no IO
- [x] 3.2 Implement op semantics: `CREATE` (fresh/bound variables, generated `gen-<hash>` ids, duplicate-id error), `MERGE` (match by `(label, id)`, then full pattern property map; `ON CREATE SET` / `ON MATCH SET`), `SET` (model fields in place, non-model into `nodeExtra`/`edgeExtra`, label add to `extra_labels`), `REMOVE` (delete extra property, remove extra label, clear primary label), `DELETE` (edge-only or error if node retains relationships) / `DETACH DELETE`
- [x] 3.3 Upsert semantics: `CREATE`/`MERGE` relationship on an existing (source, target, type) pair merges properties and counts `rels_upserted` instead of creating a parallel edge (model has none)
- [x] 3.4 Extend `Mapping.hs` so reads see `extra_labels` and `nodeExtra`/`edgeExtra` properties (multi-label `MATCH` patterns consult extras; unknown-property null behavior unchanged)
- [x] 3.5 Tests: `EvalSpec`-style mutation suites per op (create/merge/set/remove/delete/detach/upsert/duplicate-id/label round-trip); existing read goldens unchanged

## 4. Mutation summary contract

- [x] 4.1 Define `MutationSummary` (nodes_created, rels_created, rels_upserted, properties_set, properties_removed, nodes_deleted, rels_deleted) and thread it through the evaluator result alongside rows/columns/truncated
- [x] 4.2 Extend `Render.hs` renderers: text summary line (+ any returned rows) and JSON `{"summary": {...}, "rows": [...], "columns": [...], "truncated": bool}`; all-zero summary for read-only statements on the mutation surface
- [x] 4.3 Tests: renderer specs for summary JSON/text shape; budget cap still bounds rows

## 5. Persistence (`--write`)

- [x] 5.1 Add an Infrastructure persistence helper: copy loaded `graph.json` to `graph.json.bak-<timestamp>`, recompute node degrees/adjacency, carry over `communities`/`cohesion`/`god_nodes`/`community_aggregates` from the loaded file, write the mutated graph via the existing graph writer (schema version preserved)
- [x] 5.2 Wire CLI `--write` on `cypher` (app/Main.hs path): after evaluation, persist when flagged; output includes the backup path and the re-extraction-overwrites caveat
- [x] 5.3 Tests: persistence round-trip (persist → reload → mutations visible; extra labels/properties round-trip; backup file exists; schema_version unchanged)

## 6. CLI surfacing

- [x] 6.1 Add `--write` to `cypher` in `src/Graphos/CLI/Parser.hs` (`help "Permit openCypher write clauses (CREATE/MERGE/SET/REMOVE/DELETE); persist graph.json when set"`)
- [x] 6.2 `app/Main.hs` `CypherCmd` dispatch: parse → gate → evaluate (read or mutation) → render; keep `--json` contract consistent
- [x] 6.3 Tests: CLI `ParserSpec` covers `--write`; end-to-end CLI mutation over the fixture graph

## 7. MCP tool `cypher_mutate`

- [x] 7.1 Register the tool in `src/Graphos/Infrastructure/Server/MCP.hs` (`query :: String`, `persist :: Bool` default false), reusing the warm graph/index; `persist=true` triggers the persistence helper
- [x] 7.2 Keep `cypher_query` strictly read-only (error naming `cypher_mutate` for write clauses)
- [x] 7.3 Tests: MCP specs for mutate tool (summary returned, persist behavior, read-only rejection on `cypher_query`)

## 8. HTTP route `POST /api/cypher/mutate`

- [x] 8.1 Add the route to the `serve` server: body `{"query", "persist"}` → evaluate against the shared in-memory graph → mutation summary JSON; parse errors → 400 with message; `persist=true` → backup + write-back
- [x] 8.2 Reads via existing `/api/*` routes reflect the mutated in-memory state
- [x] 8.3 Tests: route specs (mutate, persist, 400 parse error, subsequent read reflects mutation)

## 9. Docs + verification

- [x] 9.1 Update `docs/workflows/17-cypher.md` (or add a sibling doc) documenting the write subset, model reconciliation rules, `--write`/persist semantics, and the re-extraction caveat
- [x] 9.2 `cabal build --flag dev` green (`-Werror`)
- [x] 9.3 `cabal test --flag dev` green (existing 558 examples remain, new suites added)
- [x] 9.4 E2E: fixture-graph mutation round-trip through CLI (`--write`), MCP, and HTTP; confirm read surfaces unchanged for read-only queries