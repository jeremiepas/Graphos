# import-resolution Specification

## Purpose
TBD - created by archiving change fix-graph-quality-and-tracing. Update Purpose after archive.

## Requirements

### Requirement: Canonical module node identity

Haskell stub extraction (PRD §3.2 extract stage) MUST assign module nodes and import-target nodes a canonical node ID derived from the module name, such that an `import X` edge in any file resolves to the same node as the module node produced for the file that declares `module X`. The `Main` module SHALL be exempt and keep a directory-scoped ID to avoid collisions between distinct executables.

#### Scenario: Import edge reaches the imported module node

- **WHEN** file A declares `module Graphos.Domain.Config` and file B in a different directory contains `import Graphos.Domain.Config`
- **THEN** the built graph contains exactly one node for `Graphos.Domain.Config`, and B's module node has an `imports` edge whose target is that node

#### Scenario: Main modules do not merge

- **WHEN** two files in different directories both declare `module Main`
- **THEN** the built graph contains two distinct `Main` nodes

### Requirement: Cross-file connectivity

The built graph for a multi-module codebase MUST contain `imports` edges whose source and target nodes originate from different source files (PRD §3.3 build stage), **for every extraction path that produces `Import` nodes — the Haskell stub extractor and all tree-sitter grammars alike**. The graph SHALL NOT consist solely of per-file islands. An extraction path that emits a node of kind `Import` without also emitting a corresponding `imports` edge is non-conforming.

#### Scenario: Repository-scale connectivity (Haskell)

- **WHEN** the full pipeline runs on the Graphos repository itself
- **THEN** `graph.json` contains at least one `imports` edge whose source and target have different `source_file` values, and the number of connected components is strictly less than the number of source files

#### Scenario: Repository-scale connectivity (TypeScript)

- **WHEN** the full pipeline runs on a TypeScript repository of at least 1,000 source files
  (e.g. `typescipt-repository`, 1,291 files under `./src`)
- **THEN** `graph.json` contains `imports` edges whose source and target have different
  `source_file` values, and the number of connected components is strictly less than the
  number of source files

#### Scenario: No orphan import nodes

- **WHEN** the built graph contains a node whose `kind` is `Import`
- **THEN** the graph also contains an `imports` edge originating from that node's enclosing
  file/module node

### Requirement: Relation semantics for stub edges

Stub extraction MUST emit `imports` edges only for targets parsed from import declarations, and `contains` edges for module-to-declaration relationships. Declarations SHALL NOT appear as targets of `imports` edges.

#### Scenario: Declarations are contained, not imported

- **WHEN** a Haskell file with one import and one top-level function is stub-extracted
- **THEN** the extraction has exactly one `imports` edge (module → imported module) and one `contains` edge (module → function), verifiable by `cabal test`

### Requirement: Import specifier extraction for tree-sitter grammars

Tree-sitter extraction SHALL parse the module specifier from every import-like declaration
(`import_declaration`, `import_statement`, `import_from_statement`, `use_declaration`) from the
declaration's string-literal child and retain it verbatim, independently of any signature truncation
applied for display. The specifier SHALL be available to edge construction as structured data,
not recovered by searching the node text for `from ` (PRD §3.2). A node that is not an import-like
declaration SHALL never yield a specifier, whatever its text contains.

#### Scenario: Single-line TypeScript import

- **WHEN** a `.ts` file contains `import { ok } from '../../types/result.js';`
- **THEN** extraction yields an import declaration whose specifier is exactly
  `../../types/result.js`

#### Scenario: Multi-line import longer than the truncation budget

- **WHEN** a `.ts` file contains a multi-line `import { a, b, c, … } from '../x.js';` whose
  flattened text exceeds the extraction truncation budget
- **THEN** the extracted specifier is still exactly `../x.js`, even though the signature is
  truncated

#### Scenario: Package and builtin specifiers

- **WHEN** a file contains `import path from 'node:path';`, `import { parse } from 'yaml';` and
  `import x from '@scope/pkg/sub';`
- **THEN** the extracted specifiers are `node:path`, `yaml` and `@scope/pkg/sub` respectively

#### Scenario: Template literal containing "from " yields nothing

- **WHEN** a `.ts` file contains a template literal whose text includes
  `## Examples by kind (derived deterministically). … from ` inside a function body
- **THEN** no import edge and no external node is created for it, verifiable by `cabal test`
  on the converter

### Requirement: Canonical import target identity for path-based module systems

A relative specifier SHALL resolve to a canonical target identity shared with the node emitted
for the target file. For grammars whose module system is path-based (TypeScript, JavaScript,
Python relative imports), the specifier is resolved against the importing file's directory.
Resolution SHALL try, in order: the specifier with its `.js`/`.mjs`/`.cjs` extension rewritten to
the source extension, the specifier as-is, and `<specifier>/index.<ext>`. Because module node ids
are derived from the path alone (`extraction-quality`, "Node identity is derived from path, kind
and identifier"), the target id computed from the resolved path SHALL equal the id of the module
node emitted when that file is extracted, with no placeholder node left behind. Package and
builtin specifiers SHALL resolve to a canonical external module node identity. The target node
SHALL be materialized in the extraction, because `buildGraph` drops edges with unknown endpoints
(PRD §3.3 build stage).

#### Scenario: Relative import resolves to the imported file's node

- **WHEN** `./src/lib/project/config-loader.ts` contains
  `import type { ProjectConfig } from '../../domain/init/project-config.js';` and
  `./src/domain/init/project-config.ts` is also extracted
- **THEN** the graph contains an `imports` edge from the `config-loader.ts` module node to the
  `project-config.ts` module node

#### Scenario: Directory import resolves through index

- **WHEN** a file contains `import { x } from '../templates/index.js';` and
  `./src/templates/index.ts` exists in the extraction
- **THEN** the edge target is the node for `./src/templates/index.ts`

#### Scenario: Package import resolves to a single external node

- **WHEN** twelve different files import `zod`
- **THEN** the graph contains exactly one external module node for `zod` and twelve `imports`
  edges pointing to it

#### Scenario: Unresolvable specifier is counted, not fabricated

- **WHEN** a specifier cannot be resolved to any extracted file or package
- **THEN** no edge is emitted, and the extraction report records the unresolved specifier count

#### Scenario: Shortest path crosses files through imports

- **WHEN** the full pipeline runs on `example/ts-lsp-test` and `graphos path index.ts db.ts` is
  run
- **THEN** a path of `imports` hops is returned whose first node is the `index.ts` module node
  and whose last node is the `db.ts` module node, verifiable by `cabal test` as an integration
  scenario

#### Scenario: Placeholder targets do not survive the build

- **WHEN** the built graph for `example/ts-lsp-test` is inspected
- **THEN** no node has kind `External` with a `source_file` that is also the `source_file` of a
  `Module` node

### Requirement: Re-export declarations produce import edges

A re-export declaration SHALL produce an `imports` edge using the same resolution rules as an
import declaration. This covers `export { x } from './y.js'`, `export * from './y.js'` and
`export type { T } from './y.js'`. The edge SHALL carry a marker in `edgeExtra` distinguishing it
from a plain import, so consumers can filter barrel files.

#### Scenario: Barrel file connects to its members

- **WHEN** `./src/lib/project/index.ts` contains `export { loadProjectConfig } from
  './config-loader.js';` and two further re-exports from `./errors.js` and `./types.js`
- **THEN** the graph contains three `imports` edges from `index.ts` to those three files, each
  marked as a re-export in `edgeExtra`

#### Scenario: Plain imports are not marked as re-exports

- **WHEN** a file contains both `import { a } from './x.js'` and `export { b } from './y.js'`
- **THEN** only the edge to `./y.js` carries the re-export marker

### Requirement: Go, Rust and Ruby imports edges

Tree-sitter extraction of Go, Rust and Ruby files SHALL emit one `imports` edge per imported unit. The edge SHALL start at the file's module node and point at a placeholder node of kind `External`. The placeholder's `source_file` SHALL be the imported repository file when the import resolves inside the repository, and `external:<name>` otherwise. Specifiers that are not literal SHALL NOT produce a target, and neither SHALL specifiers that contain whitespace or interpolation.

#### Scenario: Go internal package import
- **WHEN** a Go file in module `example.com/shop` imports `m "example.com/shop/internal/model"`, and that package directory holds two non-test `.go` files
- **THEN** the file has one `imports` edge to each of the two files, each with weight 0.5, `extra.package_files = 2` and `extra.alias = "m"`, and no edge to a `_test.go` file

#### Scenario: Go stdlib import
- **WHEN** a Go file imports `"fmt"`
- **THEN** it has an `imports` edge to a placeholder whose `source_file` is `external:fmt`

#### Scenario: Rust paths across modules and crates
- **WHEN** a Rust file uses `super::Kind`, `crate::util::helper`, `crate::util::{self, fmt::*}` and `other_crate::model::Item`, where `other_crate` is a workspace path dependency
- **THEN** each path has an `imports` edge to the file of the deepest module it names: the parent module's file, `util.rs`, `util/fmt.rs`, and the other crate's `model` file

#### Scenario: Rust local item
- **WHEN** a Rust file uses `Color::*` and `Color` is an enum declared in the same file
- **THEN** no `imports` edge is emitted for that path

#### Scenario: Ruby requires
- **WHEN** a Ruby file calls `require_relative "../helpers/money"`, `require "shop/pricing"` (present as `lib/shop/pricing.rb`) and `require "json"`
- **THEN** it has `imports` edges to `app/helpers/money.rb`, `lib/shop/pricing.rb` and `external:json`, and a `require` with an interpolated string produces no edge

### Requirement: One Rust Import node per use occurrence

Every Rust `use_declaration` SHALL produce its own `Import` node. This applies when two declarations have the same text, and when the declaration sits inside a function body at `function` granularity.

#### Scenario: Repeated use
- **WHEN** `use crate::util::helper;` appears at module level and again inside `mod tests`
- **THEN** the extraction holds two `Import` nodes, one for each start line

#### Scenario: Use in a function body
- **WHEN** a function body contains `use crate::util::fmt::render;`
- **THEN** the extraction holds an `Import` node for it, and the function `contains` it
