# Extraction
## ADDED Requirements

### Requirement: Tree-Sitter emits `calls` edges for call/new expressions

The tree-sitter extractor SHALL emit a Graphos `calls` edge for every
`call_expression` and `new_expression` node encountered while converting a
`TSNodeInfo` tree into edges, matching the edge shape of the LSP extractor's
`parseIncomingCall`. For each such call node the extractor SHALL emit exactly one
edge:

- **Source** = `makeNodeId filePath <enclosingDefLabel>`, where the enclosing
  definition label is the nearest ancestor definition (`parentLabel`) — i.e. the
  enclosing function/method/constructor name, or `module` for module-level calls.
- **Target** = `makeNodeId filePath <calleeName>`, where `<calleeName>` is the
  first `identifier` in the callee subtree (the class name for `new Foo()`, the
  receiver for `receiver.method()`).
- **Edge shape**: `edgeId = srcId <> "->call:" <> tgtId`,
  `edgeRelation = Calls`, `edgeConfidence = Confidence 0.9`, `edgeWeight = 0.9`,
  `edgeExtra = Nothing`.

Resolution of the callee is best-effort and name-based:

- A **same-file** callee resolves to the in-file node
  `makeNodeId filePath <calleeName>`.
- An **imported** callee resolves to the matching `External` node produced by
  `resolveImport`/`extractSpecifier`.
- An **unresolvable** callee (e.g. a global such as `Date`) drops the edge; the
  extractor never crashes on such a call.

The extractor SHALL emit these edges only — `call_expression`/`new_expression` are
NOT whitelisted definition types and SHALL never be emitted as nodes. Only the
top-level call of an enclosing function SHALL be emitted; calls nested inside a
call's arguments are excluded. This edge addition SHALL NOT alter the behavior of
`contains`, `imports`, or `depends_on` edges.

#### Scenario: Same-file call emits a calls edge

- **WHEN** a module contains `function a() {}` and `function b() { a(); }` at fine
  granularity
- **THEN** the extractor emits a `calls` edge whose source is `makeNodeId filePath
  "b"` and whose target is `makeNodeId filePath "a"`.

#### Scenario: `new Foo()` targets the class name

- **WHEN** a module contains `new Foo()` where `Foo` is imported from another file
- **THEN** the extractor emits a `calls` edge targeting the `External` node for
  `Foo`.

#### Scenario: Nested calls are excluded

- **WHEN** a call expression contains a nested call in its arguments
- **THEN** only the top-level call emits a `calls` edge; the nested call does not.

#### Scenario: Calls addition does not regress other edges

- **WHEN** the extractor processes a TypeScript file with imports and calls
- **THEN** the counts and endpoints of `contains`, `imports`, and `depends_on` edges
  are unchanged relative to extraction without `calls`.
