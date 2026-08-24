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
truncation budget, the retained label SHALL keep both the leading form and the trailing
specifier clause (eliding the middle) rather than cutting the tail.

#### Scenario: Multi-line import keeps its specifier in the label

- **WHEN** a `.ts` file contains a multi-line
  `import { loadAndProcessTemplate, loadTemplate, buildConfigVariables, … } from '../../templates/index.js';`
  whose flattened text exceeds the truncation budget
- **THEN** the emitted node label ends with `from '../../templates/index.js';` and contains an
  elision marker in the middle

#### Scenario: Short declarations are unchanged

- **WHEN** a declaration's normalized text is shorter than the truncation budget
- **THEN** the label is the normalized text with no elision marker

#### Scenario: No import node loses its specifier

- **WHEN** the full pipeline runs on a TypeScript repository of at least 1,000 source files
- **THEN** the number of `Import` nodes whose label contains no `from '<specifier>'` clause
  (for declarations that have one in source) is zero

### Requirement: Node identity is derived from normalized declaration text

The node identity computed for a declaration SHALL be derived from its normalized text, so two
runs over unchanged source produce identical IDs and an ID never encodes a truncation artifact
such as a trailing ellipsis.

#### Scenario: IDs are stable and artifact-free

- **WHEN** the pipeline is run twice over the same unchanged TypeScript file containing a
  multi-line import
- **THEN** the import node has the same ID in both runs, and that ID contains no ellipsis
  marker

