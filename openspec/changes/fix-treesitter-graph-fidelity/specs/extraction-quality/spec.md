# extraction-quality Capability — Delta

## Purpose

Make text truncation a named, documented policy that never destroys the semantic tail of a
declaration. Today `Infrastructure/Extract/TreeSitter/Core.hs:134` applies a bare literal
`truncateText 200` to raw source text, and `Convert.hs:184` strips newlines instead of
normalizing the declaration, so multi-line import/export declarations lose the trailing
`from '<specifier>'` clause — 307 of 12,164 `Import` nodes (2.5%) on a real TypeScript corpus.

## ADDED Requirements

### Requirement: Named truncation budget for extracted text

The maximum length applied to extracted source text SHALL be an exported, documented named
constant rather than a literal at the call site, and SHALL be applied in exactly one place. The
constant SHALL be referenced by tests so a change of value is a visible, intentional edit.

- **Plan**: `Core.hs:134` hardcodes `200` and `truncateText` (`Core.hs:145–148`) is generic;
  nothing documents the budget or ties it to the node-ID derivation that consumes it.
- **Do**: Introduce the named constant, export it, and use it at the single truncation site.
- **Check**: Scenario below plus a unit test asserting the constant is the value used.
- **Act**: If the budget proves too small for other declaration kinds, change the constant once
  and record the impact on graph size.

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

- **Plan**: The specifier is the join key for `imports` edges (see `import-resolution`); a label
  that ends mid-identifier is unusable to any consumer and also poisons `makeNodeId`
  (`Convert.hs:166`, `:286–293`).
- **Do**: Normalize the declaration text in the label path; elide the middle, never the tail.
- **Check**: Scenarios below, verifiable by `cabal test`.
- **Act**: If middle-elision harms readability in the HTML export, adjust the display layer
  rather than reintroducing tail truncation.

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
