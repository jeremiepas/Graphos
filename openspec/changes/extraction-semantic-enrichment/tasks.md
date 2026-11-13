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
- [x] 5.3 Done: full-corpus before/after rerun against a shallow clone of
      apache/fineract (7250 `.java` files), comparing a `main`-built binary
      (separate worktree) against this change's binary on the identical
      checkout. See `FINERACT_VERIFICATION.md` for the complete table.
      Headline, at real-world scale: `calls` edges 0 -> 20,709;
      return-type-mislabeled methods 5,803/40,059 (14.5%) -> 0;
      `type_identifier` leak nodes 2,824 -> 0; 33,920 nodes gained a
      structured `annotations` array (829 `httpVerb`, 782 `route`, 37
      `configKeys`, 1 `topics`); 2 `Env` + 36 `Config` + 1 `Topic` canonical
      nodes appeared. Also surfaced (and documented, not fixed — pre-existing,
      out of scope): identifier-less Tier-3 nodes (`if_statement` etc.) share
      a bare, kind-agnostic, cross-file-collidable id space
      (`baseId = maybe "" (...)`, unchanged by this change); and `@Scheduled`
      usages in Fineract use `fixedRate`/`initialDelay`, which `namedArg`
      doesn't extract (only `cron`/`fixedDelay` are implemented).
- [x] 5.4 Targeted `cabal test` runs green throughout (153 examples, 0
      failures, 1 pre-existing pending, across every `tree-sitter`/
      `signatures`/`AVI-1447`/`Core`/`Resolver`/`multi-source`/`notable
      strings`/`structured annotations`/`field-aware`/`per-language calls`/
      `real-grammar` describe block) — `BoundedDeltaCommunitySpec` was not
      invoked (unrelated module, matches the known pre-existing hang).

## 6. Stretch (separate PR, not blocking)

- [ ] 6.1 **Investigated, blocked — not completable within the ABI ≤ 11
      constraint.** `.kt` keeps parsing with the Java grammar, unchanged.
      Attempted:
      - `fwcd/tree-sitter-kotlin` tag `0.2.11` (2021-10-12, the last release
        whose `grammar.js` predates all of the Unicode-identifier and
        parser-conflict changes below) generates cleanly at
        `LANGUAGE_VERSION 11` with the CLI 0.16.9 process this repo's other
        vendored grammars use. But it has a confirmed, serious parsing bug,
        not a cosmetic gap: `return foo(x)` never parses as a
        `call_expression` (an `infix_expression`/`jump_expression` rule wins
        the ambiguity instead), and `return Foo(x, y)` (two-plus args)
        produces an outright `ERROR` node. Verified directly: `tree-sitter
        parse` on `fun f() { helper(id) }` (bare statement) and
        `fun f() { val x = helper(id) }` (RHS of a property) both correctly
        produce `call_expression`; the identical call wrapped in `return
        helper(id)` does not. `return expr(...)` is an extremely common
        Kotlin shape — shipping this would make `calls` edges systematically
        and silently incomplete for Kotlin, which is worse than the current
        honest Java-alias approximation.
      - Confirmed, via `git log -S`, that `grammar.js` had **zero commits**
        between 0.3.0 (2023-10-02) and 2026-04-11 — so every tagged/dated
        release through mid-2026 carries the identical bug.
      - The current upstream HEAD (`d6af96d`, 2026-10-31) does fix it
        (verified: `return helper(id)` correctly nests a `call_expression`
        inside `jump_expression`), but requires `LANGUAGE_VERSION 14`: its
        `grammar.js` uses a `\p{L}` Unicode regex class the 0.16.9 generator
        rejects outright. Patching just that one rule to an ASCII-only
        `[a-zA-Z_][a-zA-Z0-9_]*` equivalent gets past the regex error but
        hits a second, deeper wall: an unresolved grammar conflict (`'!'
        _expression  •  '@'  …`, between `call_expression`/
        `check_expression`/`prefix_expression`) that the 0.16.9 generator's
        older conflict-resolution logic cannot auto-resolve — the kind of
        gap `vendor/README.md` already documents for SQL
        (`DerekStride/tree-sitter-sql` "does not generate with it"). Fixing
        this would mean hand-authoring explicit `conflicts:` entries against
        a grammar under heavy, unfamiliar active development (40+ commits of
        parser fixes between 2026-08 and 2026-10-31 alone) — an open-ended
        task, not a bounded patch.
      - Checked `tree-sitter-grammars/tree-sitter-kotlin` (the other
        actively-maintained fork, 8 stars) as an alternative: also ships
        only `LANGUAGE_VERSION 14`, same wall.
      - Confirmed the **actual root constraint**: the Haskell `tree-sitter`
        package is at `0.9.0.3` on Hackage (checked directly) — the latest
        release, and still the ABI 9–11 ceiling `vendor/README.md` already
        documents for `cpp`/`nix` being similarly blocked. There is no
        newer Haskell binding to move to; the ceiling is not
        grammar-specific.
      - **Conclusion**: closing this gap needs either (a) a correct,
        ABI-≤-11-generatable Kotlin grammar that doesn't currently exist
        upstream, or (b) upgrading past the `tree-sitter-0.9` Haskell
        binding's ABI ceiling — the latter is a cross-cutting change
        affecting every vendored grammar (cpp, nix, yaml, sql, c-sharp,
        perl), far beyond "add Kotlin," and belongs in its own proposal.
