# Extraction

## MODIFIED Requirements

### Requirement: Tree-Sitter emits `calls` edges for call/new expressions

The tree-sitter extractor SHALL emit a Graphos `calls` edge for every
call-denoting node of the file's grammar — not only `call_expression` /
`new_expression` — matching the edge shape of the LSP extractor's
`parseIncomingCall`. The set of call-denoting node types SHALL be resolved per
grammar name (the one `grammarForFile` assigns), covering at minimum:

- typescript / tsx / javascript: `call_expression`, `new_expression`
- python: `call`
- ruby: `method_call` (the real invocation node; its `method` field may itself
  be a `call` subtree for the dotted form `recv.name(...)`) plus bare `call`
  (a dotted reference without parentheses, which Ruby's grammar does not
  distinguish from a zero-argument method invocation)
- go, c, cpp: `call_expression`
- rust: `call_expression`
- java (including `.kt` parsed with the java grammar): `method_invocation`,
  `object_creation_expression`
- c-sharp: `invocation_expression`, `object_creation_expression`
- php: `function_call_expression`, `member_call_expression`,
  `scoped_call_expression`, `object_creation_expression`

For each such call node the extractor SHALL emit exactly one edge:

- **Source** = `makeNodeId filePath <enclosingDefLabel>`, where the enclosing
  definition label is the nearest ancestor definition (`parentLabel`) — i.e.
  the enclosing function/method/constructor name, or `module` for
  module-level calls.
- **Target** = `makeNodeId filePath <calleeName>`, where `<calleeName>` is
  extracted from the call node's grammar *field* (`function`, `name`, `type`,
  `method`, per grammar) where the grammar exposes one; for a member or
  qualified callee the rightmost name-like leaf is the callee
  (`svc.save()` ⇒ `save`, `new a.b.C()` ⇒ `C`). The first-identifier-in-preorder
  heuristic (the class name for `new Foo()`, the receiver for
  `receiver.method()`) remains only as fallback for field-less grammars or
  field-less node types (confirmed field-less: PHP's
  `object_creation_expression`; unconfirmed for C# pending independent
  field-id verification).
- **Edge shape**: `edgeId = srcId <> "->call:" <> tgtId`,
  `edgeRelation = Calls`, `edgeConfidence = Confidence 0.9`, `edgeWeight = 0.9`,
  `edgeExtra = Nothing`.

Resolution of the callee is best-effort and name-based, consulting, in order:
the file's import bindings, then an index of **all** definitions emitted for
the file — including definitions nested inside classes, namespaces, impls and
modules — not only top-level ones:

- A **same-file** callee (top-level or nested) resolves to the in-file node
  `makeNodeId filePath <calleeName>`.
- An **imported** callee resolves to the matching `External` node produced by
  `resolveImport`/`extractSpecifier`.
- An **unresolvable** callee (e.g. a global such as `Date`) drops the edge; the
  extractor never crashes on such a call.

The extractor SHALL emit these edges only — call-denoting node types are NOT
whitelisted definition types and SHALL never be emitted as nodes. Only the
top-level call of an enclosing function SHALL be emitted; calls nested inside
a call's arguments are excluded. This edge addition SHALL NOT alter the
behavior of `contains`, `imports`, or `depends_on` edges. Haskell function
application emits no `calls` edges (no call node in the grammar).

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

#### Scenario: Java intra-class call resolves to the sibling method

- **WHEN** a Java class declares `retrieveClient` whose body contains
  `helper(id)`, and a sibling method `helper`
- **THEN** a `calls` edge is emitted from the `retrieveClient` method node to
  the `helper` method node

#### Scenario: receiver call targets the method, not the receiver

- **WHEN** a Java method body contains `clientRepository.save(client)` and no
  local or imported binding matches `save`
- **THEN** no `calls` edge with target label `clientRepository` is emitted

#### Scenario: object creation targets the class

- **WHEN** a Java method body contains `new ClientData(key, id)` and
  `ClientData` is imported
- **THEN** a `calls` edge is emitted targeting the `ClientData` import node
