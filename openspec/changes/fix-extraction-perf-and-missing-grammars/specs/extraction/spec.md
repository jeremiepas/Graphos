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

#### Scenario: Ruby file parsed with tree-sitter
- **WHEN** a `.rb` file is processed and `grammar: ruby` is configured
- **THEN** `getGrammarPtr "ruby"` returns `Just` with a valid language pointer, and the file produces function/class/method nodes (not a stub)

#### Scenario: Nix file parsed with tree-sitter
- **WHEN** `devenv.nix` is processed and `grammar: nix` is configured
- **THEN** `getGrammarPtr "nix"` returns `Just` with a valid language pointer, and the file produces nodes for its declarations (not a stub and not a warning)

#### Scenario: Java file parsed with tree-sitter
- **WHEN** a `.java` file is processed and `grammar: java` is configured
- **THEN** `getGrammarPtr "java"` returns `Just` and the file produces class/method/field nodes

#### Scenario: C++ header parsed with tree-sitter
- **WHEN** a `.hpp` file is processed and `grammar: cpp` is configured
- **THEN** `getGrammarPtr "cpp"` returns `Just` and the file produces class/function/namespace nodes

#### Scenario: Markdown file parsed with tree-sitter
- **WHEN** a `.md` file is processed and `grammar: markdown` is configured
- **THEN** `getGrammarPtr "markdown"` returns `Just` and the file produces heading/section nodes