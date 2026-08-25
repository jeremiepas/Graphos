## 1. Property-graph mapping

- [x] 1.1 Define the deterministic mapping: Cypher label ← `nodeKind`, relationship type ← `edgeRelation`, properties ← node/edge fields (+ `label`/`text` snippet)
- [x] 1.2 Expose a pure property accessor for nodes and edges used by the evaluator
- [x] 1.3 Tests: property accessor returns declared fields; unknown property → null

## 2. Grammar (megaparsec)

- [x] 2.1 Add `megaparsec` to `graphos.cabal`
- [x] 2.2 AST types for the supported subset (`MATCH`/`WHERE`/`RETURN`, patterns, expressions)
- [x] 2.3 Parser for node/relationship patterns incl. variable-length `*m..n` and direction
- [x] 2.4 Parser for `WHERE` predicates (`=`,`<>`,`<`,`>`,`IN`,`STARTS WITH`,`CONTAINS`,`=~`,`AND`/`OR`/`NOT`,`IS NULL`) and `RETURN`/`DISTINCT`/`ORDER BY`/`LIMIT`/`SKIP`/`count`
- [x] 2.5 Tests: parse accepted queries; reject out-of-subset with a clear error + position

## 3. Evaluation

- [x] 3.1 Anchored-pattern planner: begin from the most selective indexed element via `GraphIndex`
- [x] 3.2 Relationship expansion over `Graph`; variable-length via `CachedFGL`
- [x] 3.3 Apply `WHERE`, projection, `DISTINCT`, `ORDER BY`, `SKIP`, `LIMIT`; enforce the result budget
- [x] 3.4 Tests: pattern + predicate + projection golden results over a fixture graph; budget cap honoured

## 4. Surfaces

- [x] 4.1 CLI `graphos cypher "…" --graph ARG [--json] [--budget ARG]`
- [x] 4.2 MCP tool `cypher_query` (reuse the warm graph + index, no per-call rebuild)
- [x] 4.3 Docs: supported subset + property/label/relation mapping + examples
- [x] 4.4 Tests: CLI text + `--json` output shape; MCP tool response shape

## 5. Verification

- [x] 5.1 `cabal build --flag dev` with `-Werror`
- [x] 5.2 `cabal test` green including parser + evaluator suites
- [x] 5.3 Run representative queries (label filter, relation hop, var-length path, regex `WHERE`, `count`) and confirm results + budget bound
