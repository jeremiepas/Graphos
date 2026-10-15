## MODIFIED Requirements

### Requirement: Import specifier extraction for tree-sitter grammars

Tree-sitter extraction SHALL parse the module specifier from every import-like declaration
(`import_declaration`, `import_statement`, `import_from_statement`, `use_declaration`) from the
declaration's string-literal child and retain it verbatim, independently of any label truncation
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
