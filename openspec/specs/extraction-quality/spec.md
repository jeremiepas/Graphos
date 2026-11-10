# extraction-quality Specification

## Purpose
TBD - created by archiving change fix-graph-quality-and-tracing. Update Purpose after archive.
## Requirements
### Requirement: No truncated fragment nodes

The Haskell stub extractor (PRD §3.2, node-schema spec) MUST NOT emit nodes whose label is a truncated prefix of a source line. When no valid identifier can be extracted from a candidate declaration line, the extractor SHALL skip the line instead of emitting a fallback label.

#### Scenario: Unparseable line is skipped

- **WHEN** a line such as `"  - from: " ++ T.unpack x` or `( NodeId, CommunityId )` is processed by declaration extraction
- **THEN** no node is emitted for that line

#### Scenario: No 20-character truncation artifacts

- **WHEN** the full pipeline runs on the Graphos repository
- **THEN** `graph.json` contains zero nodes whose label is exactly the first 20 characters of a non-identifier source line

### Requirement: Top-level declarations start at column zero

The stub extractor MUST treat only column-0 lines beginning with a letter or `(` as top-level declaration candidates (Haskell layout rule). Indented lines, guards (`|`), braces, and string/expression fragments SHALL NOT produce nodes.

#### Scenario: Guard and brace lines excluded

- **WHEN** lines `  | otherwise -> x`, `}` and `    where` are processed
- **THEN** no nodes are emitted for any of them, verifiable by `cabal test`

### Requirement: Declaration nodes carry a kind

Every declaration node emitted by the stub extractor MUST have a non-empty `nodeKind` (PRD §4 node schema): `data`/`newtype`/`type` → Type, `class` → Class, `instance` → Instance, other identifier declarations → Function.

#### Scenario: Kinds assigned by declaration form

- **WHEN** a file containing `data Foo`, `class Bar`, and `baz :: Int -> Int` is stub-extracted
- **THEN** the resulting nodes have kinds Type, Class, and Function respectively, and no emitted declaration node has an absent kind

### Requirement: Named truncation budget for extracted text

The maximum length applied to extracted source text SHALL be an exported, documented named
constant rather than a literal at the call site, and SHALL be applied in exactly one place. The
constant SHALL be referenced by tests so a change of value is a visible, intentional edit.

#### Scenario: Truncation budget is a referenced constant

- **WHEN** the extraction module is inspected for the truncation call
- **THEN** the call site uses the exported named constant, and no numeric literal length is
  passed to `truncateText` anywhere in the tree-sitter extraction path

### Requirement: Import and export declarations are normalized before truncation

Import-like and re-export declarations SHALL be normalized before truncation so that the
`from '<specifier>'` clause is preserved in the retained text: interior newlines and runs of
whitespace are collapsed to single spaces, and when the normalized declaration still exceeds the
truncation budget, the retained signature SHALL keep both the leading form and the trailing
specifier clause (eliding the middle) rather than cutting the tail. The node label is the
resolved specifier (see "Labels are identifiers and declaration text is the signature").

#### Scenario: Multi-line import keeps its specifier in the signature

- **WHEN** a `.ts` file contains a multi-line
  `import { loadAndProcessTemplate, loadTemplate, buildConfigVariables, … } from '../../templates/index.js';`
  whose flattened text exceeds the truncation budget
- **THEN** the emitted node signature ends with `from '../../templates/index.js';` and contains an
  elision marker in the middle, and its label is `../../templates/index.js`

#### Scenario: Short declarations are unchanged

- **WHEN** a declaration's normalized text is shorter than the truncation budget
- **THEN** the label is the normalized text with no elision marker

#### Scenario: No import node loses its specifier

- **WHEN** the full pipeline runs on a TypeScript repository of at least 1,000 source files
- **THEN** the number of `Import` nodes whose signature contains no `from '<specifier>'` clause
  (for declarations that have one in source) is zero

### Requirement: Node identity is derived from path, kind and identifier

The node id computed for a tree-sitter declaration SHALL be
`<dirHash>_<stem>_<kind>:<identifier>` where `<dirHash>` and `<stem>` are the existing
directory hash and file stem, `<kind>` is the node's `nodeKind`, and `<identifier>` is the
declared name (PRD §3.2 extract stage). When two declarations in the same file share kind and
identifier, every one after the first SHALL carry a `#L<startLine>` suffix. Module nodes SHALL
use the file name as identifier. The id SHALL be a pure function of (path, kind, identifier,
collision rank), so two runs over unchanged source produce identical ids, an id never contains
declaration text, and an id is at most 512 bytes.

#### Scenario: Function id names the function, not its body
- **WHEN** `./src/db.ts` contains `export function dbGetById(id: UserId): User | undefined { return users.get(id); }`
- **THEN** the emitted node has id `<dirHash>_db_Function:dbGetById`, verifiable by `cabal test`
  on the tree-sitter converter

#### Scenario: Module id is bounded and content-free
- **WHEN** a 5,000-line file is extracted
- **THEN** its module node id is `<dirHash>_<stem>_Module:<file name>` and contains no source
  text

#### Scenario: Overloads stay distinct
- **WHEN** a file declares two functions named `handle` on lines 10 and 40
- **THEN** the ids are `…_Function:handle` and `…_Function:handle#L40`, and both runs over the
  unchanged file produce the same two ids

### Requirement: Labels are identifiers and declaration text is the signature

The label of a tree-sitter node SHALL be its identifier (declared name; file name for module
nodes; resolved specifier for import and re-export nodes), and the normalized declaration text,
bounded by the named truncation budget, SHALL be stored in `nodeSignature` (PRD §3.2, §4.1
node fields). A wrapper node that carries no identifier of its own (for example an
`export_statement` around a `function_declaration`) SHALL NOT be emitted as a separate node; its
inner declaration is emitted once, marked exported in `nodeExtra`.

#### Scenario: Exported function is one node with a short label
- **WHEN** a `.ts` file contains `export async function executeSharedRunCommand(toolCall: ToolCall, …): Promise<…> { … }`
- **THEN** exactly one node is emitted for it, with label `executeSharedRunCommand`, kind
  `Function`, `nodeSignature` starting with `export async function executeSharedRunCommand(` and
  `nodeExtra` marking it exported, verifiable by `cabal test`

#### Scenario: Module label is the file name
- **WHEN** `./src/workers/workflow-task-worker.ts` is extracted
- **THEN** its module node label is `workflow-task-worker.ts` and `nodeSignature` is `Nothing`

