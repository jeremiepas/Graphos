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

