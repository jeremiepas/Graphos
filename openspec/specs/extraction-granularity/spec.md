# extraction-granularity Specification

## Purpose
TBD - created by archiving change configurable-extraction-granularity. Update Purpose after archive.
## Requirements
### Requirement: Three-level granularity enum

The system MUST support a `Granularity` configuration value with exactly three levels — `fine`, `function`, and `file` — controlling which tree-sitter AST nodes become graph nodes (PRD §3.2 extract stage, §14 configuration). The value SHALL serialize to/from the strings `fine`, `function`, and `file` in config files.

#### Scenario: Enum round-trips through config

- **WHEN** a config file sets granularity to any of `fine`, `function`, or `file`
- **THEN** parsing yields the corresponding level and re-serializing yields the same string, verifiable by `cabal test`

#### Scenario: Unknown level is rejected

- **WHEN** a config file sets granularity to an unrecognized string
- **THEN** config parsing fails with an error naming the allowed values

### Requirement: Granularity resolution order

The effective granularity for a file MUST be resolved as: CLI `--granularity` flag, else per-extension `ExtractorConfig` override, else global `extraction.granularity`, else the built-in default `function` (PRD §14). Resolution SHALL be a pure function of configuration values.

#### Scenario: CLI flag wins over all config

- **WHEN** the CLI passes `--granularity fine` and config sets global `function` with a `.json` override of `file`
- **THEN** every file is extracted at `fine` level

#### Scenario: Per-extension override wins over global

- **WHEN** no CLI flag is given, global granularity is `function`, and `.json` has a `file` override
- **THEN** `.ts` files extract at `function` and `.json` files extract at `file`

#### Scenario: Built-in default applies

- **WHEN** neither CLI flag nor any config granularity is set
- **THEN** files extract at `function` level

### Requirement: Function-level extraction stops at function bodies

At `function` granularity, tree-sitter extraction MUST emit module/structure nodes, API-surface definitions (functions, methods, constructors, classes, interfaces, types, enums, traits, structs, fields, properties, imports, exports), and module-level variable/constant declarations — and MUST NOT emit any node located inside a function, method, or constructor body (PRD §3.2). Statement, parameter, and local-variable AST nodes SHALL NOT produce graph nodes at this level.

#### Scenario: Function bodies are opaque

- **WHEN** a source file containing a class with a method whose body has assignments, conditionals, returns, and local variables is extracted at `function` level
- **THEN** nodes exist for the module, class, and method, and no node exists for any construct inside the method body, verifiable by `cabal test`

#### Scenario: Module-level constants survive

- **WHEN** a source file declares a top-level constant outside any function and is extracted at `function` level
- **THEN** a node for that constant exists

#### Scenario: Node volume target on a real repository

- **WHEN** the full pipeline runs at `function` granularity over a code-dominant repository
- **THEN** the average emitted nodes per code file is at most ~25, an order of magnitude below `fine` level on the same input

### Requirement: File-level extraction emits one node per file

At `file` granularity, tree-sitter extraction MUST emit exactly one module node per file and no children.

#### Scenario: JSON file collapses to a single node

- **WHEN** a JSON file with many nested objects, arrays, and pairs is extracted at `file` level
- **THEN** the extraction contains exactly one node (the file/module node) and zero contains-edges, verifiable by `cabal test`

### Requirement: Fine level preserves current behavior

At `fine` granularity, tree-sitter extraction MUST emit the same node set as the pre-change implementation (all whitelisted definition types with full tree recursion), providing a backward-compatible escape hatch.

#### Scenario: Fine level includes statement nodes

- **WHEN** a source file with statements inside function bodies is extracted at `fine` level
- **THEN** statement-level nodes are emitted as before the change

### Requirement: JSON defaults to file granularity

The built-in default extractor configuration MUST assign `file` granularity to the `.json` extension so that data files do not inflate the graph (PRD §11 multi-format input).

#### Scenario: Default run keeps JSON small

- **WHEN** the pipeline runs with default configuration on a repository containing a large lock file
- **THEN** that JSON file contributes exactly one node to the graph

