# runtime-tree-sitter-grammar

## ADDED Requirements

### Requirement: Runtime grammar shared library loading

Graphos SHALL allow a tree-sitter entry to specify grammar_path to a shared
library, load it with FFI dynamic loader, obtain Ptr Language, and reuse existing
vendored runtime. Paths resolve relative to config directory when not absolute.

#### Scenario: Runtime grammar produces nodes

- **WHEN** graphos.yaml maps ".php": { mode: tree-sitter, grammar_path: "~/.local/lib/tree-sitter-php.so", queries: "~/.config/graphos/queries/php/" }
- **THEN** Graphos loads library and extracts nodes plus query definitions/imports/calls

### Requirement: ABI compatibility guard

Graphos SHALL refuse grammars whose ABI is outside 9-11, log path and ABI,
and fall back to stub without aborting.

#### Scenario: Incompatible ABI falls back to stub

- **WHEN** a library reports ABI 13
- **THEN** file stub-extracted with warning, build continues

### Requirement: Runtime queries

Graphos SHALL load definitions.scm/imports.scm/calls.scm from queries if present,
apply each; a missing kind disables only that kind with a warning.

#### Scenario: Missing calls query disables calls only

- **WHEN** only definitions.scm and imports.scm exist
- **THEN** definitions and imports extracted, calls omitted, warning emitted

### Requirement: Grammar cache and invalidation

Runtime-grammar extraction SHALL use SHA256 cache and include grammar path
metadata + query mtimes in fingerprint.

#### Scenario: Grammar change invalidates cache

- **WHEN** shared library replaced
- **THEN** second run does not reuse cached extraction

### Requirement: Security guard for runtime grammars

Graphos SHALL validate grammar_path readability and sensitivity policy and load
only via FFI helper.

#### Scenario: Unreadable grammar path falls back to stub

- **WHEN** grammar_path missing
- **THEN** stub extraction with per-extension warning
