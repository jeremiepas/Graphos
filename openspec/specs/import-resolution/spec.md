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
(`import_declaration`, `import_statement`, `import_from_statement`, `use_declaration` —
`Convert.hs:234–238`) and retain it verbatim, independently of any label truncation applied for
display. The specifier SHALL be available to edge construction as structured data, not
recovered by re-parsing the node label.

#### Scenario: Single-line TypeScript import

- **WHEN** a `.ts` file contains `import { ok } from '../../types/result.js';`
- **THEN** extraction yields an import declaration whose specifier is exactly
  `../../types/result.js`

#### Scenario: Multi-line import longer than the truncation budget

- **WHEN** a `.ts` file contains a multi-line `import { a, b, c, … } from '../x.js';` whose
  flattened text exceeds the extraction truncation budget
- **THEN** the extracted specifier is still exactly `../x.js`, even though the node label is
  truncated

#### Scenario: Package and builtin specifiers

- **WHEN** a file contains `import path from 'node:path';`, `import { parse } from 'yaml';` and
  `import x from '@scope/pkg/sub';`
- **THEN** the extracted specifiers are `node:path`, `yaml` and `@scope/pkg/sub` respectively

### Requirement: Canonical import target identity for path-based module systems

A relative specifier SHALL resolve to a canonical target identity shared with the node emitted
for the target file. For grammars whose module system is path-based (TypeScript, JavaScript,
Python relative imports), the specifier is resolved against the importing file's directory.
Resolution SHALL try, in order: the specifier with its `.js`/`.mjs`/`.cjs` extension rewritten to
the source extension, the specifier as-is, and `<specifier>/index.<ext>`. Package and builtin
specifiers SHALL resolve to a canonical external module node identity. The target node SHALL be
materialized in the extraction, because `buildGraph` drops edges with unknown endpoints
(`Domain/Graph/Core.hs:70–72`).

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

