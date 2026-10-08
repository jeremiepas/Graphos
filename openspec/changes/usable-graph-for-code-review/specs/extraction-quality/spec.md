## REMOVED Requirements

### Requirement: Node identity is derived from normalized declaration text
**Reason**: Deriving identity from the declaration text makes a module node's id its entire file
and a function node's id its whole body, which produced a 2.7 GB `graph.json` for 129,839 nodes,
ids that cannot be passed as CLI arguments, and labels that defeat identifier lookup (bug report
2026-10-08).
**Migration**: Identity is now derived from file path, kind and identifier (see the ADDED
requirement below). Existing graphs must be rebuilt with `--fresh`; the extraction cache
fingerprint changes so stale entries miss.

## ADDED Requirements

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

## MODIFIED Requirements

### Requirement: Import and export declarations are normalized before truncation

Import-like and re-export declarations SHALL be normalized before truncation so that the
`from '<specifier>'` clause is preserved in the retained text: interior newlines and runs of
whitespace are collapsed to single spaces, and when the normalized declaration still exceeds the
truncation budget, the retained text SHALL keep both the leading form and the trailing
specifier clause (eliding the middle) rather than cutting the tail. The retained text is stored
in `nodeSignature`; the node label is the resolved specifier (see "Labels are identifiers and
declaration text is the signature").

#### Scenario: Multi-line import keeps its specifier in the signature

- **WHEN** a `.ts` file contains a multi-line
  `import { loadAndProcessTemplate, loadTemplate, buildConfigVariables, … } from '../../templates/index.js';`
  whose flattened text exceeds the truncation budget
- **THEN** the emitted node's `nodeSignature` ends with `from '../../templates/index.js';` and
  contains an elision marker in the middle, and its label is `../../templates/index.js`

#### Scenario: Short declarations are unchanged

- **WHEN** a declaration's normalized text is shorter than the truncation budget
- **THEN** `nodeSignature` is the normalized text with no elision marker

#### Scenario: No import node loses its specifier

- **WHEN** the full pipeline runs on a TypeScript repository of at least 1,000 source files
- **THEN** the number of `Import` nodes whose `nodeSignature` contains no `from '<specifier>'`
  clause (for declarations that have one in source) is zero
