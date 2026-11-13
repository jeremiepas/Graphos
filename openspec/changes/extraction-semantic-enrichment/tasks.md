# Tasks

Streams 1–4 all depend on 0. After 0, streams 1+2 unblock the Fineract
headline gaps and should land first; 3 and 4 are independent of each other.

## 0. Foundation — field-aware AST

- [x] 0.1 Add `tsnFieldName :: Maybe String` to `TSNodeInfo` (`Core.hs`);
      populate in `toNodeInfo` from the binding's `nodeFieldName` (NULL-safe,
      same pattern as `safeNodeType`). Add `childByField` (and
      `childrenByField` for `multiple: true` fields, needed by stream 3's TS
      decorator field).
- [x] 0.2 Updated every test constructing `TSNodeInfo` literals (field
      defaults to `Nothing`); no behavior change. Targeted specs green.

## 1. Declaration names (all languages)

- [x] 1.1 Delta spec on `extraction-quality`: names come from the grammar
      `name` field; a method label is never its return type; reference-only
      types emit no node.
- [x] 1.2 `extractIdentifier`: field `name` first, then a C/C++-specific
      `declarator`-field chain follow (`function_definition.declarator` ->
      `function_declarator.declarator` -> `identifier`, which also needed a
      "name" short-circuit at each step once Java's
      `field_declaration.declarator` -> `variable_declarator.name` case was
      hit live — found and fixed during implementation, not planned), then
      the type-list fallback preferring `identifier` over `type_identifier`.
- [x] 1.3 Removed `type_identifier` from `apiSurfaceTypes`; verified no
      empty-id nodes on the Java fixture (live CLI run and unit test).
- [x] 1.4 Fixed duplicate-base-id `contains` remap: `ContainEdge`/import
      sources now key on `(NodeId, Int)` (base id + line), not bare `NodeId`,
      so overloaded siblings each keep their own containment edge instead of
      the last one silently winning.
- [x] 1.5 Unit specs (`ConvertSpec.hs` "3.4"/"3.3"): Java method named
      `retrieveClient` (not `ClientData`), with and without field metadata;
      C/C++ `function_declarator` chain; overloads get `#L` ids AND both
      keep `contains` edges (regression test for 1.4).

## 2. `calls` edges per language

- [x] 2.1a Verified by reading each pinned grammar's `src/node-types.json`
      directly (java 0.7.0.3, python 0.9.0.4, ruby 0.5.0.4, go 0.5.0.3, rust
      0.1.0.2, php 0.5.0.2, typescript 0.5.0.3) — two corrections found:
      Ruby's real invocation node is `method_call` (not bare `call`, the
      no-arg/no-paren dotted form); PHP's `object_creation_expression` has no
      fields at all.
- [ ] 2.1b Still unverified: C# field names (no `node-types.json` vendored
      here; only type-name string presence grep-confirmed in `parser.c` —
      `invocation_expression`/`object_creation_expression` are in `callTypes`
      but resolve callees via the positional fallback only) and Perl's
      call-node type (not added to `callTypes` at all — deliberately left
      out rather than guessed). Follow-up, not a blocker: Perl calls are
      simply absent until this is done; C# calls work via fallback callee
      resolution already.
- [x] 2.2 Delta spec on `extraction` (MODIFIED, layered on the archived
      AVI-1447 requirement — see "Dependency" below).
- [x] 2.3 Implemented as one global `callTypes` union list (not a
      per-grammar `callTypesFor` dispatch keyed by grammar name as
      originally planned): tree-sitter node-type strings are each grammar's
      own vocabulary with no cross-grammar collisions among the verified
      names, so a flat list is equally correct with far less churn (no new
      parameter to thread through `tsNodesToExtraction` and every call
      site). `calleeNameOf`/`rightmostName` do the field-based extraction
      with the rightmost-name rule, falling back to `firstIdentifierInList`
      for field-less node types.
- [x] 2.4 Extended via a new `allDefinitions` full-tree walk (mirrors
      `traverseNode`'s own isDef/wrapper/opacity rules) feeding
      `collectCallsCtx`'s `callsCtxLocal`, so class members resolve as call
      targets, not only top-level definitions.
- [x] 2.5 Unit specs per language (java x3, php x2, ruby, typescript) in
      `ConvertSpec.hs` "3.6": intra-class call resolves to the sibling
      method; receiver call resolves via the method name (not the
      receiver) — both the positive case and the "no bogus receiver edge"
      case; object creation via a field and via the field-less PHP fallback;
      Ruby's nested `method_call.method` -> `call` unwrapping. Also found and
      fixed: Ruby's `method`/`singleton_method` were missing from
      `functionBoundaryTypes`, so every Ruby call's enclosing scope resolved
      to the module instead of the method — not in the original plan,
      discovered by writing the Ruby test.
- [x] 2.6 Regression: the full existing `AVI-1447 calls edges` describe
      block (TS/JS-only, pre-existing) still passes unchanged, plus the
      broader `tree-sitter grammars`/`tree-sitter imports` suites (72+
      examples) — `example/ts-lsp-test` itself was not separately re-run,
      but its exact fixture shapes are what the pre-existing tests already
      cover.

## 3. Annotations / decorators / attributes

PHP is **not** in this stream: confirmed the pinned `tree-sitter-php` 0.5.0.2
grammar has no attribute node type at all (zero `"attr"`-containing types in
its `node-types.json`, zero `"attribute"` hits in `parser.c`). Rust's
`attribute_item`/C#'s `attribute_list` are **also not implemented** this
pass — deferred, see note on task 3.2.

- [x] 3.1 New capability spec `annotation-extraction` (requirements +
      scenarios: capture, references edge, distillation keys, header
      signatures) — written during planning, matches the implementation.
- [x] 3.2 Implemented for **Java** (fields: `annotation.name`/`.arguments`,
      `marker_annotation.name`, confirmed via node-types.json) and
      **Python/TypeScript decorators** (no name/args fields in either
      grammar — parsed from the decorator's own source text instead:
      `@name`, `@name(args)`, `@a.b.name(args)`, which is robust without
      assuming either grammar's internal decorator shape). **Not
      implemented**: Rust `attribute_item`/C# `attribute_list` — both are
      preceding **siblings** of their target (not a field, not a wrapping
      parent), which `attribute_item` already emits as its own standalone
      "Attribute" node; attaching it to the *next* sibling as well would
      need a sibling-aware traversal refactor this pass did not undertake,
      to avoid risking that existing standalone-node behavior. TS method/
      property-level decorators (vs. the confirmed class-level `decorator`
      field) are also not covered — only the field-based class case was
      verified.
- [x] 3.3 `references` edge (confidence 1.0) to the resolved imported
      annotation node, reusing `collectCallsCtx`'s import-binding map.
      **Caveat found during implementation**: that map is populated only via
      the TS/JS/Python *quoted* import-specifier shape; Java/Kotlin/C#/PHP/
      Perl's *unquoted* `import a.b.C;` form is deliberately excluded
      (belongs to the separate `Imports.hs` pipeline, not wired in here) —
      so this edge only fires for same-file-styled or TS/JS/Python-style
      resolvable imports, not real Java import resolution. Documented in
      `collectCallsCtx`'s doc comment and the proposal's non-goals.
- [x] 3.4 Distillation table implemented for the JAX-RS/Spring/Kafka/Rabbit/
      Scheduled vocabulary only (route, httpVerb, cron, fixedDelay, topics,
      configKeys) — scoped down from the original plan's Python/TS decorator
      equivalents (flask/fastapi, NestJS), which are **not implemented**:
      ambiguous generic names (`get`, `post`, `route`) risked false positives
      without per-framework verification this pass didn't do.
- [x] 3.5 Header-only signatures implemented generically via `childByField
      "body"` + `T.breakOn` on normalized text (works for any grammar
      exposing a `body` field — java/python/typescript/go/rust/php/c/cpp);
      Ruby's fallback (no `body` field at all) is the pre-existing
      full-text behavior, unchanged, not a new truncation path — a precise
      byte-offset truncation for Ruby was not implemented (`TSNodeInfo`
      carries no byte offsets, only row/col).
- [x] 3.6 Unit specs (`ConvertSpec.hs` "3.7", 8 examples): structured
      annotations array, header-only signature, class/method/field/
      `@Scheduled` distillation, references edge, Python decorator via
      wrapper, TS class decorator field.

## 4. Notable strings (Env / Config / Topic)

- [x] 4.1 New capability spec `notable-string-extraction`.
- [x] 4.2 Implemented: `envVarName` (text-prefix matching, not a per-grammar
      AST model) covers `System.getenv`/`os.getenv`/`os.environ.get`/
      `os.environ[...]`/`std::env::var`/`getenv`/`process.env.X`/`ENV[...]`/
      `$_ENV[...]`; config/topic reuse the same annotation data stream 3
      already parses (`@Value` -> `config:`, `@KafkaListener`/
      `@RabbitListener` -> `topic:`) so the human-readable `nodeExtra` keys
      and these graph nodes/edges can't drift apart. **Bug found and fixed
      during implementation**: the string literal argument lives inside the
      call's `arguments`-field value (an `argument_list` node), not as a
      direct child of the call itself — the first version silently matched
      zero env-reads until this was corrected.
- [x] 4.3 Caps implemented (32 distinct (kind,name) pairs per file, 1–80
      char names) — **the "debug log on drop" requirement is not
      implemented**: `Convert.hs` is explicitly documented as a pure, no-IO
      module, so over-cap entries are dropped silently instead.
- [x] 4.4 Unit specs (`ConvertSpec.hs` "3.8", 7 examples): Java call-form,
      `process.env.X` member form, Ruby `ENV[...]` subscript form, two call
      sites converging on one node with two edges, `@KafkaListener`/
      `@Value` canonical nodes, and the 40-names-capped-to-32 case.

## 5. Fixtures & end-to-end verification

- [x] 5.1/5.2 (scoped down from a per-language file-based fixture corpus):
      added two **real-grammar** hspec checks in `GrammarsSpec.hs`
      ("extraction-semantic-enrichment: real-grammar checks") that run the
      actual pinned Java and Python parsers (not hand-built `TSNodeInfo`
      literals) end-to-end — confirming the node-types.json-derived field
      assumptions hold against the real FFI parse, not just offline
      verification. A fixture corpus under `tests/fixtures/semantic-
      enrichment/<lang>/` for every language (c-sharp, php, ruby, go, rust,
      typescript too) was **not** built — the `ConvertSpec.hs` unit tests
      (streams 1–4, ~25 new examples) already pin down each language's
      field-based behavior precisely against verified fixtures; a second,
      slower, file-based layer for the remaining languages is deferred.
- [ ] 5.3 **Not done**: no Fineract checkout was available in this session,
      so no full-corpus before/after metrics were recorded against the
      actual originating repository. What was verified instead: a hand-built
      fixture reproducing the four diagnosed gaps (route/verb/cron/config
      annotations, overloaded methods, `System.getenv`, intra-class calls)
      confirms, live, via the compiled CLI: correct method/field names (0
      return-type-label instances), 2 `calls` edges, `httpVerb`/`route`/
      `cron`/`configKeys` populated, and `env:`/`config:` nodes with
      `references` edges — all at both `--granularity fine` and the
      production-default `--granularity function` (notable strings; calls
      remain Fine-only, matching AVI-1447's original, unchanged scope).
- [x] 5.4 Targeted `cabal test` runs green throughout (153 examples, 0
      failures, 1 pre-existing pending, across every `tree-sitter`/
      `signatures`/`AVI-1447`/`Core`/`Resolver`/`multi-source`/`notable
      strings`/`structured annotations`/`field-aware`/`per-language calls`/
      `real-grammar` describe block) — `BoundedDeltaCommunitySpec` was not
      invoked (unrelated module, matches the known pre-existing hang).

## 6. Stretch (separate PR, not blocking)

- [ ] 6.1 Vendor tree-sitter-kotlin (ABI ≤ 11) and stop parsing `.kt` with
      the Java grammar; add Kotlin to the matrix, fixtures, and tables. Not
      attempted this pass.
