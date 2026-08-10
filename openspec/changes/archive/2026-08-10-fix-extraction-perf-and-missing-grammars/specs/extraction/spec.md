## MODIFIED Requirements

### Requirement: Default Extraction Thread Count

The default value for `cfgThreads` in `PipelineConfig` SHALL be `numCapabilities` (from `GHC.Conc`), not 1. This enables parallel extraction out-of-the-box on multi-core machines. Users can override with `--threads N`.

#### Scenario: Default thread count on 4-core machine
- **WHEN** `graphos .` is run without `--threads` on a machine with 4 capabilities
- **THEN** extraction runs with 4 threads (tree-sitter files processed in parallel batches of 4)

#### Scenario: Explicit single thread
- **WHEN** `graphos . --threads 1` is run
- **THEN** extraction runs sequentially (same as current behavior)

#### Scenario: Explicit thread override
- **WHEN** `graphos . --threads 8` is run
- **THEN** extraction runs with 8 threads regardless of `numCapabilities`

### Requirement: Tree-Sitter Grammar FFI Bindings for Configured Languages

`Infrastructure.Wiring.getGrammarPtr` SHALL return a valid `Ptr Language` for every grammar name that appears in the default `graphos.yaml` extractors configuration. Specifically, the following bindings SHALL be added:

- `"ruby"` → `TSRuby.tree_sitter_ruby`
- `"java"` → `TSJava.tree_sitter_java`
- `"cpp"` → `TSCpp.tree_sitter_cpp`
- `"nix"` → `TSNix.tree_sitter_nix`
- `"markdown"` → `TSMarkdown.tree_sitter_markdown` (or the appropriate module name from the `tree-sitter-markdown` package)

The corresponding Haskell packages SHALL be added to `graphos.cabal` build-depends.

### Known Limitation: cpp and nix bindings unavailable

`getGrammarPtr "cpp"` and `getGrammarPtr "nix"` currently return `Nothing` because no compatible Haskell package exists on Hackage and the upstream C grammar parsers require a newer `tree-sitter` C library (`LANGUAGE_VERSION` 13–15) than the version bundled with the `tree-sitter` Haskell package used by this project (`LANGUAGE_VERSION` 11). Until the base `tree-sitter` package is upgraded or the grammars are vendored with a compatible version, `.cpp`/`.hpp`/`.c` and `.nix` files fall back to stub extraction. A startup warning lists these missing bindings.

`getGrammarPtr "markdown"` also returns `Nothing`; however, `.md`, `.rst`, and `.adoc` files are handled by the existing native markdown extractor and therefore do produce header/tag/wikilink nodes, not stubs.

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