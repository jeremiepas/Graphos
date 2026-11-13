# Extraction

## Purpose

Extract code/structure from files into graph nodes using tree-sitter parsers with configurable granularity.
## Requirements
### Requirement: Default extractors use tree-sitter for all code languages

The system SHALL default `defaultExtractors` to `ExtractTreeSitter` for all code languages, with an appropriate `grammar` field and `language_id` field. LSP extraction remains available via config override in `graphos.yaml`. The default extractors SHALL be:

| Extension | Mode | Grammar | Language ID |
|-----------|------|---------|-------------|
| `.ts` | tree-sitter | typescript | typescript |
| `.tsx` | tree-sitter | tsx | typescriptreact |
| `.js` | tree-sitter | javascript | javascript |
| `.jsx` | tree-sitter | javascript | javascriptreact |
| `.hs` | tree-sitter | haskell | haskell |
| `.lhs` | tree-sitter | haskell | haskell |
| `.go` | tree-sitter | go | go |
| `.rs` | tree-sitter | rust | rust |
| `.py` | tree-sitter | python | python |
| `.pyw` | tree-sitter | python | python |
| `.c` | tree-sitter | c | c |
| `.cpp` | tree-sitter | cpp | cpp |
| `.h` | tree-sitter | c | c |
| `.hpp` | tree-sitter | cpp | cpp |
| `.nix` | tree-sitter | nix | nix |
| `.rb` | tree-sitter | ruby | ruby |
| `.java` | tree-sitter | java | java |
| `.json` | tree-sitter | json | json (granularity: file) |
| `.md` | tree-sitter | markdown | markdown |
| `.rst` | tree-sitter | markdown | rest |
| `.adoc` | tree-sitter | markdown | asciidoc |

#### Scenario: Default extraction for Haskell uses tree-sitter
- **WHEN** no `extractors` section is provided in graphos.yaml
- **THEN** `.hs` files are extracted with `mode: tree-sitter, grammar: haskell, language_id: haskell`

#### Scenario: LSP override via config still works
- **WHEN** graphos.yaml contains `extractors: {".hs": {mode: lsp, language_id: haskell}}`
- **THEN** `.hs` files are extracted using the haskell-language-server

### Requirement: Extraction granularity defaults to function level

The system SHALL default `defaultGranularity` to `GranularityFunction`. This extracts module/structure nodes, API-surface definitions (functions, classes, types, fields, imports/exports), and module-level constants. Extraction stops at function bodies — no statement-level nodes are produced.

#### Scenario: Default granularity is function
- **WHEN** no `granularity` is specified in config or CLI
- **THEN** extraction produces function-level nodes (functions, classes, types, module constants) and does not descend into function bodies

#### Scenario: Fine granularity override
- **WHEN** `granularity: fine` is set in config or `--granularity fine` on CLI
- **THEN** extraction produces statement-level nodes including all AST detail

### Requirement: Tree-Sitter Grammar FFI Bindings for Configured Languages

`Infrastructure.Wiring.getGrammarPtr` SHALL return a valid `Ptr Language` for every grammar name that appears in the default `graphos.yaml` extractors configuration. Specifically, the following bindings SHALL be present when the corresponding Haskell package is available:

- `"ruby"` → `TSRuby.tree_sitter_ruby`
- `"java"` → `TSJava.tree_sitter_java`
- `"cpp"` → `TSCpp.tree_sitter_cpp`
- `"nix"` → `TSNix.tree_sitter_nix`
- `"markdown"` → `TSMarkdown.tree_sitter_markdown` (or the appropriate module name from the `tree-sitter-markdown` package)

The corresponding Haskell packages SHALL be added to `graphos.cabal` build-depends.

#### Known Limitation: cpp and nix bindings unavailable

`getGrammarPtr "cpp"` and `getGrammarPtr "nix"` currently return `Nothing` because no compatible Haskell package exists on Hackage and the upstream C grammar parsers require a newer `tree-sitter` C library (`LANGUAGE_VERSION` 13–15) than the version bundled with the `tree-sitter` Haskell package used by this project (`LANGUAGE_VERSION` 11). Until the base `tree-sitter` package is upgraded or the grammars are vendored with a compatible version, `.cpp`/`.hpp`/`.c` and `.nix` files fall back to stub extraction. A startup warning lists these missing bindings.

`getGrammarPtr "markdown"` also returns `Nothing`; however, `.md`, `.rst`, and `.adoc` files are handled by the existing native markdown extractor and therefore do produce file/header/tag/wikilink nodes, not stubs.

#### Scenario: Ruby file parsed with tree-sitter
- **WHEN** a `.rb` file is processed and `grammar: ruby` is configured
- **THEN** `getGrammarPtr "ruby"` returns `Just` with a valid language pointer, and the file produces function/class/method nodes (not a stub)

#### Scenario: Nix file parsed with tree-sitter
- **WHEN** `devenv.nix` is processed and `grammar: nix` is configured
- **THEN** the startup warning mentions nix as missing, and the file is stub-extracted (pending upstream grammar compatibility)

#### Scenario: Java file parsed with tree-sitter
- **WHEN** a `.java` file is processed and `grammar: java` is configured
- **THEN** `getGrammarPtr "java"` returns `Just` and the file produces class/method/field nodes

#### Scenario: C++ header parsed with tree-sitter
- **WHEN** a `.hpp` file is processed and `grammar: cpp` is configured
- **THEN** the startup warning mentions cpp as missing, and the file is stub-extracted (pending upstream grammar compatibility)

#### Scenario: Markdown file parsed with tree-sitter
- **WHEN** a `.md` file is processed and `grammar: markdown` is configured
- **THEN** the native markdown extractor handles the file and produces file/header/tag/wikilink nodes

### Requirement: Init template shows tree-sitter as default with LSP as commented alternatives

The `graphos init` command SHALL generate a YAML template where the `extractors` section lists tree-sitter as the active default for each language, with LSP mode as a commented-out alternative. Each language entry SHALL show the grammar and language_id fields.

#### Scenario: Init template for Haskell extractor
- **WHEN** `graphos init` is run
- **THEN** the generated YAML contains:
  ```yaml
  # Haskell (default: tree-sitter; uncomment for LSP)
  ".hs":
    mode: tree-sitter
    grammar: haskell
    language_id: haskell
    # mode: lsp
    # language_id: haskell
  ```

#### Scenario: Init template for Python extractor
- **WHEN** `graphos init` is run
- **THEN** the generated YAML contains:
  ```yaml
  # Python (default: tree-sitter; uncomment for LSP)
  ".py":
    mode: tree-sitter
    grammar: python
    language_id: python
    # mode: lsp
    # language_id: python
  ```

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

### Requirement: Tree-sitter grammar bindings for PHP

The system SHALL provide a tree-sitter language binding for the `php` grammar usable by `Infrastructure.Wiring.getGrammarPtr`, returning `Just` with a valid `Ptr Language` so `.php` files produce real symbol nodes in default tree-sitter mode. The binding SHALL be provided either via a compatible Hackage `tree-sitter-*` package or via a vendored source compiled at ABI 9–11 under `vendor/`, consistent with the existing cpp/yaml/sql grammars.

#### Scenario: PHP file parsed with tree-sitter
- **WHEN** a `.php` file is processed and `grammar: php` is configured
- **THEN** `getGrammarPtr "php"` returns `Just` with a valid language pointer, and the file produces function/class/method/module nodes rather than a single stub node

### Requirement: Tree-sitter grammar bindings for Kotlin

The system SHALL provide a tree-sitter language binding for the `kotlin` grammar usable by `Infrastructure.Wiring.getGrammarPtr`, returning `Just` with a valid `Ptr Language` so `.kt` and `.kts` files produce real symbol nodes in default tree-sitter mode.

#### Scenario: Kotlin file parsed with tree-sitter
- **WHEN** a `.kt` or `.kts` file is processed and `grammar: kotlin` is configured
- **THEN** `getGrammarPtr "kotlin"` returns `Just` with a valid language pointer, and the file produces function/class/method/module nodes rather than a single stub node

### Requirement: Tree-sitter grammar bindings for Perl

The system SHALL provide a tree-sitter language binding for the `perl` grammar usable by `Infrastructure.Wiring.getGrammarPtr`, returning `Just` with a valid `Ptr Language` so `.pl`, `.pm`, and `.psgi` files produce real symbol nodes in default tree-sitter mode.

#### Scenario: Perl file parsed with tree-sitter
- **WHEN** a `.pl`, `.pm`, or `.psgi` file is processed and `grammar: perl` is configured
- **THEN** `getGrammarPtr "perl"` returns `Just` with a valid language pointer, and the file produces subroutine/package/module nodes rather than a single stub node

### Requirement: Tree-sitter grammar bindings for C Sharp

The system SHALL provide a tree-sitter language binding for the `csharp` grammar usable by `Infrastructure.Wiring.getGrammarPtr`, returning `Just` with a valid `Ptr Language` so `.cs` files produce real symbol nodes in default tree-sitter mode.

#### Scenario: C Sharp file parsed with tree-sitter
- **WHEN** a `.cs` file is processed and `grammar: csharp` is configured
- **THEN** `getGrammarPtr "csharp"` returns `Just` with a valid language pointer, and the file produces class/method/namespace nodes rather than a single stub node

### Requirement: Default extractors for new language extensions

The system SHALL default `defaultExtractors` to `ExtractTreeSitter` for the new language extensions with the following grammar and language_id values:

| Extension | Mode | Grammar | Language ID |
|-----------|------|---------|-------------|
| `.php` | tree-sitter | php | php |
| `.kt` | tree-sitter | kotlin | kotlin |
| `.kts` | tree-sitter | kotlin | kotlin |
| `.pl` | tree-sitter | perl | perl |
| `.pm` | tree-sitter | perl | perl |
| `.psgi` | tree-sitter | perl | perl |
| `.cs` | tree-sitter | csharp | csharp |

#### Scenario: Newly supported extension uses tree-sitter by default
- **WHEN** no `extractors` section is provided in `graphos.yaml`
- **THEN** `.php`, `.kt`, `.pl`, and `.cs` files are extracted with `mode: tree-sitter` and the corresponding grammar shown above

### Requirement: Grammar extension registry includes new extensions

`Grammar.knownExtensions` SHALL contain entries for `.php`, `.kt`, `.kts`, `.pl`, `.pm`, `.psgi`, and `.cs`, each mapping to a `LanguageInfo` whose `liGrammarName` equals the configured grammar (`php`, `kotlin`, `perl`, `csharp` respectively) and whose `liFileType` is `CodeFile`.

#### Scenario: Extension lookup succeeds
- **WHEN** `languageForExt` is called with any of `.php`, `.kt`, `.pl`, or `.cs`
- **THEN** it returns `Just` with the matching `LanguageInfo`
"

