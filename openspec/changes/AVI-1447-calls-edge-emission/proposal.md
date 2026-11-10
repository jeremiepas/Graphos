# Tree-Sitter: emit `calls` edges for call/new expressions

This change extends the tree-sitter extractor so that every `call_expression` and
`new_expression` in a program emits a Graphos `calls` edge from the enclosing
definition (the nearest ancestor definition label) to the resolved callee, mirroring
the edge shape produced by the LSP extractor's `parseIncomingCall`.

## Problem

The tree-sitter extractor currently produces `contains`, `imports`, and inferred edges
but never emits `calls` edges. As a result, call relationships between functions,
methods, classes, and imported modules are absent from the graph even at fine
granularity. The LSP extractor already produces these edges via `parseIncomingCall`,
so the tree-sitter path should match that shape to keep the two extractors consistent.

## Solution

Emit `calls` edges in `tsNodeToGraphEdges` while iterating node children. For each
`call_expression`/`new_expression`, extract the callee name (the first `identifier`
in the callee subtree, which yields the class for `new Foo()` and the receiver for
`receiver.method()`), resolve it best-effort by name, and emit one edge:

```
edgeId   = srcId <> "->call:" <> tgtId
srcId    = makeNodeId filePath <enclosingDefLabel>
tgtId    = makeNodeId filePath <calleeName>
```

with `edgeRelation = Calls`, `edgeConfidence = Confidence 0.9`, `edgeWeight = 0.9`,
`edgeExtra = Nothing` (the exact shape of LSP `parseIncomingCall`).

- **Same-file callee** → resolve to the in-file node `makeNodeId filePath <calleeName>`.
- **Imported callee** → resolve via `resolveImport`/`extractSpecifier` to the matching
  `External` node.
- **Unresolvable callee** (e.g. globals like `Date`) → drop the edge; never crash.
- **Nested calls excluded**: only the top-level call of an enclosing function is
  emitted; calls nested inside a call's arguments are suppressed.
- `call_expression`/`new_expression` are NOT whitelisted definition types, so this
  change emits edges only — never nodes.

## Acceptance Criteria

- [ ] The tree-sitter path emits `calls` edges for `call_expression`/`new_expression`.
- [ ] `new Foo()` emits a `calls` edge targeting `Foo`.
- [ ] Same-file and imported callees resolve to the correct in-file and `External`
      nodes respectively.
- [ ] No regression to `contains`, `imports`, or `depends_on` edges on the
      `example/ts-lsp-test` fixture.
- [ ] `graphos-test` stays green.

## Scope

- Edit only `openspec/specs/**` inside this change directory.
- Modify `src/Graphos/Infrastructure/Extract/TreeSitter/Convert.hs` for edge emission.
- Extend `tests/Graphos/Infrastructure/Extract/TreeSitter/` for unit coverage.
