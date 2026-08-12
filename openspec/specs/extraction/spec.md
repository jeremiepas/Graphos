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
