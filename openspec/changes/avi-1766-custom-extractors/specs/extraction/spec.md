# extraction (delta)

## ADDED Requirements

### Requirement: Command extractor dispatch

Extract/Core.hs SHALL partition files into command extractors in addition to
tree-sitter, LSP and stub, dispatching with same streaming accumulation,
progress logging and cache write-through.

#### Scenario: Command files dispatched separately

- **WHEN** repository contains files configured mode command
- **THEN** they are not stubbed and processed by command runner

### Requirement: Runtime grammar counts as tree-sitter

A tree-sitter entry with grammar_path SHALL be treated as ExtractTreeSitter for
dispatch/fingerprint/cache but use runtime loader; incompatible grammars fall
back to stub without aborting.

#### Scenario: Runtime grammar files cached like tree-sitter

- **WHEN** runtime-grammar file unchanged
- **THEN** cached extraction reused without loading library

### Requirement: Config schema validation

Config loader SHALL reject inconsistent extractor entries and missing required
fields (command for command; grammar for tree-sitter) before build.

#### Scenario: Inconsistent config fails fast at load

- **WHEN** ".x": { mode: command } has no command
- **THEN** config loading fails naming extension and missing field
