# Tree-Sitter semantic enrichment: real names, calls, annotations, notable strings — all languages

Qualification on Apache Fineract (Java, ~4k files) showed the tree-sitter path
produces a structurally correct but semantically hollow graph. Four gaps, all
reproduced on a 20-line annotated Java fixture with the current binary:

1. **Declaration names are wrong.** A Java/Kotlin/C# method node is labeled with
   its *return type* (`ClientData` for `retrieveClientData`): `extractIdentifier`
   picks the first child in `identifierTypes`, and `type_identifier` (the return
   type) precedes the name `identifier`. Every method of a class returning the
   same type collides on one base id; the `contains` remap (`Map.fromList` keeps
   the last duplicate) then silently drops containment edges for all but one.
2. **Zero `calls` edges outside TS/JS.** `callTypes = ["call_expression",
   "new_expression"]` matches only the TS/JS/Go/Rust/C grammars. Java
   (`method_invocation`, `object_creation_expression`), C#
   (`invocation_expression`), PHP (`function_call_expression`, …), Python/Ruby
   (`call`) never match. Additionally `collectCallsCtx` indexes only *top-level*
   definitions, so class members (the normal case in Java/C#/PHP/Kotlin) are
   unresolvable even when the call node type matches; and the callee heuristic
   (first `identifier` in the subtree) returns the *receiver* of
   `receiver.method()`, not the method.
3. **Annotations are unstructured.** `@Path("/v1/clients")`, `@GET`,
   `@Scheduled(...)`, `@KafkaListener("topic")`, `@Value("${key}")` survive only
   as the head of a 200-char flattened signature that also swallows the method
   body. Nothing queryable: no route, verb, cron, topic, or config key.
4. **Notable strings invisible.** `System.getenv("CLIENTS_TABLE")`,
   `process.env.X`, `${config.key}` placeholders, table names — at function
   granularity bodies are opaque, so these never reach the graph; at fine
   granularity they drown in statement-node text.

A fifth, discovered during diagnosis: bare `type_identifier` *references* leak as
nodes with empty ids (whitelisted in `apiSurfaceTypes` despite the AVI-1440
decision that references produce no node), polluting every Java/TS graph.

## What Changes

- **Field-aware AST (foundation).** Extend `TSNodeInfo` with the tree-sitter
  field name of each child (`tsnFieldName`). The Hackage binding already
  populates `nodeFieldName` in the child buffer (`bridge.c` /
  `ts_node_copy_child_nodes`); we only surface it. Every stream below keys off
  grammar fields instead of positional child scans.
- **Correct declaration names (all languages).** `extractIdentifier` prefers the
  child whose field is `name`; current type-list scan becomes the fallback.
  Remove `type_identifier` from `apiSurfaceTypes` (reference, not declaration).
- **`calls` edges for every grammar.** Per-grammar call-node table + field-based
  callee extraction (`name`/`function`/`constructor` fields); call-resolution
  context extended to definitions nested in classes/namespaces/impls so
  intra-class calls resolve. Haskell function application is explicitly out of
  scope (no call node in the grammar).
- **Structured annotations/decorators/attributes.** Per-grammar annotation nodes
  (Java `annotation`/`marker_annotation`, Python/TS `decorator`, C#
  `attribute_list`, Rust `attribute_item`) land in the annotated node's
  `nodeExtra.annotations` as `[{name, args}]`, plus a `references` edge to the
  imported annotation's node when it resolves. Well-known annotations are
  distilled (route, HTTP verb, cron, topic, config key) into `nodeExtra`.
  Signatures become declaration *headers* (annotations + modifiers + name +
  params + return type), never the body. **PHP is excluded**: the pinned
  `tree-sitter-php` 0.5.0.2 grammar has zero attribute-syntax support (verified
  by reading its `node-types.json` — no node type contains "attr"), so there
  is nothing to extract until the pinned grammar version is bumped.
- **Notable-string extraction.** Context-gated string literals inside bodies
  (getenv-family calls, `@Value`/placeholder `${…}` patterns, listener topics)
  emit canonical `Config`/`Env` nodes (`env:CLIENTS_TABLE`,
  `config:spring.datasource.url`) with `references` edges from the enclosing
  definition — at every granularity, with hard noise caps.

## Capabilities

### New Capabilities
- annotation-extraction: structured annotation/decorator/attribute capture and
  well-known distillation (IF/EVT/CFG surfacing).
- notable-string-extraction: env vars, config keys and placeholder strings as
  first-class nodes with `references` edges.

### Modified Capabilities
- extraction: per-language `calls` emission (supersedes the TS-only AVI-1447
  shape), field-aware callee extraction, nested-definition call context.
- extraction-quality: declaration names from grammar `name` fields; method label
  is never the return type; reference-only AST types emit no node; signatures
  are headers.

## Impact

- `Infrastructure/Extract/TreeSitter/Core.hs` (TSNodeInfo + field names,
  extractIdentifier), `Convert.hs` (callTypes table, callee/ctx resolution,
  annotation + string extraction, signature header, apiSurfaceTypes),
  `Grammar.hs` (per-grammar capability table).
- `tests/Graphos/Infrastructure/Extract/TreeSitter/` + a per-language annotated
  fixture corpus.
- No graph.json schema change: `nodeExtra` is already free-form JSON;
  `References` already exists in `Relation`.

## Non-goals / known limits

- **Kotlin**: `.kt` currently parses with the *Java* grammar (imports-only
  approximation) — investigated and confirmed genuinely blocked, not merely
  deferred. The only `fwcd/tree-sitter-kotlin` revision that generates at the
  required `LANGUAGE_VERSION` ≤ 11 (tag `0.2.11`, 2021) has a real parsing
  bug — `return foo(x)` never produces a `call_expression` — unchanged for
  every release through at least 2026-04; the upstream fix requires ABI 14,
  which the Hackage `tree-sitter` package (latest: `0.9.0.3`, checked
  directly) cannot load. See tasks.md task 6.1 for the full investigation.
  Closing this needs either a correct ABI-≤-11 Kotlin grammar that doesn't
  currently exist, or upgrading past the Haskell binding's ABI ceiling — a
  cross-cutting change affecting every vendored grammar, not a Kotlin-only
  fix, and out of scope here.
- **Haskell calls**: function application has no call node; deferred.
- **Type-level resolution**: callee resolution stays name-based best-effort
  (confidence 0.9), consistent with AVI-1447; no symbol tables.
- **PHP annotations**: out of scope this change — the pinned grammar has no
  attribute syntax at all (see design.md §3). PHP still gets streams 1, 2
  (names, calls) and 4 (notable strings).
- **C# field names are unconfirmed.** No `node-types.json` is vendored for
  C# in this repo; only type-name *presence* in `parser.c` was grep-verified
  (the AVI-1765 bar). Field-based callee/name extraction for C# falls back to
  positional scan until task 2.1 independently confirms field ids.
- **Cross-file resolution for Java/Kotlin/C#/PHP/Perl is same-file only,
  discovered during implementation.** `collectCallsCtx`'s import-binding map
  (shared by `calls` and the new annotation `references` edges) is built from
  `extractSpecifier`, which only understands the *quoted* import-specifier
  shape TS/JS/Python expose. These five grammars write an *unquoted* import
  path (`import a.b.C;`) that `extractSpecifier` deliberately returns
  `Nothing` for — their real resolution lives entirely in the separate,
  repository-aware `Imports.hs` pipeline, which this change does not wire
  into calls/references resolution. Concretely: `new ClientData()` where
  `ClientData` is imported from another file, or `@GET` where `GET` is
  imported from `javax.ws.rs`, never resolves to an edge for these five
  grammars today — only intra-file resolution (sibling methods, same-file
  annotation targets) does. This is a pre-existing limitation inherited from
  AVI-1447's original TS/JS-only scope, not something this change introduces,
  but it was silently present until this implementation pass surfaced it.
  Wiring `Imports.hs`'s resolved bindings into `collectCallsCtx` is
  unimplemented — tracked as follow-up, not a blocker for what ships here
  (same-file calls/annotations, which is most of the Fineract-style value).

## Dependency

- This change's `extraction` spec delta is written as **MODIFIED** against
  the `calls`-edges requirement from `AVI-1447-calls-edge-emission`, whose
  code already shipped to `main` but whose change folder was never archived
  (its requirement text was therefore never merged into
  `openspec/specs/extraction/spec.md`). **AVI-1447 must be archived before —
  or as part of — this change**, otherwise the MODIFIED delta has no base
  requirement to modify. See design.md §4a.
