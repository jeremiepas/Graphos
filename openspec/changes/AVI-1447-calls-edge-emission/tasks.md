## 1. Delta spec on `openspec/specs/extraction/spec.md`

Add an ADDED requirement (with scenarios) documenting that the tree-sitter extractor
emits `calls` edges for call/new expressions matching LSP `parseIncomingCall` shape.

## 2. Emit `calls` edges in `tsNodeToGraphEdges`

In `src/Graphos/Infrastructure/Extract/TreeSitter/Convert.hs`:

- When processing a node, if it is `call_expression`/`new_expression`, extract the
  callee name via the first `identifier` in the callee subtree (DFS).
- Resolve the callee best-effort: same-file → in-file `makeNodeId`; imported →
  `resolveImport`/`extractSpecifier` `External` node; unresolvable → drop.
- Emit one edge: `srcId = makeNodeId filePath <parentLabel>`, `tgtId = makeNodeId
  filePath <calleeName>`, `Calls`, `Confidence 0.9`, weight `0.9`, `edgeExtra = Nothing`.
- Suppress nested calls (only the top-level call of an enclosing function is emitted).
- Emit edges only; never nodes (these types are not whitelisted).

## 3. Tests

In `tests/Graphos/Infrastructure/Extract/TreeSitter/`:

- Unit tests proving a `calls` edge for a same-file callee and an imported callee.
- Test that `new Foo()` targets `Foo` and nested calls are excluded.
- Test that `contains`/`imports`/`depends_on` are unchanged by the new emission.

## 4. Verification

- Baseline: `example/ts-lsp-test/graph.json` has `calls = 0`.
- Run graphos on the fixture → expect `calls > 0` and no regression to other relations.
- `devenv shell -- cabal test graphos-test` is green.
