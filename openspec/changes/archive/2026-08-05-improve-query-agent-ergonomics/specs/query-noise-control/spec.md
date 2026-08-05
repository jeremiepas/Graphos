# query-noise-control

Signal-per-token controls for query-family output (`query`, `path`, `explain`,
`neighbors`; PRD §13.1, §7.3 compact context format). Removes structural trivia,
duplicate declarations, and oversized labels that drown semantic content.

## ADDED Requirements

### Requirement: Semantic edge filtering
Query-family commands SHALL accept `--edges semantic|all` with default `semantic`. In
`semantic` mode the output MUST exclude `contains` edges whose target label is a trivia
token (`undefined`, `unknown`, `null`, bare primitive or wrapper types such as `Promise`
and `Result`, and single-token type parameters). `--edges all` SHALL restore the
unfiltered edge set.

#### Scenario: Trivia edges dropped by default
- **WHEN** a query result subgraph contains a `contains` edge targeting a node labeled `undefined`
- **THEN** that edge is absent from default output

#### Scenario: All-edges mode preserves everything
- **WHEN** the same query is run with `--edges all`
- **THEN** the trivia-targeting edge is present in the output

### Requirement: Self-referential edge collapse
Query-family output MUST NOT contain edges whose source and target resolve to the same
node id.

#### Scenario: Self-edge suppressed
- **WHEN** the selected subgraph contains an edge from a node to itself
- **THEN** the rendered output contains no `X --rel--> X` line for it

### Requirement: Duplicate declaration deduplication
Query-family output SHALL merge nodes that represent the same declaration — labels
differing only by declaration prefix (such as `export const X`, `const X`, `X`) with an
identical source file and identical start line — into a single rendered node using the
shortest label, with edges from all merged variants attached to it.

#### Scenario: Declaration triple collapses to one node
- **WHEN** the result set contains `export const foo = …`, `const foo = …`, and `foo` all pointing at the same file and line
- **THEN** exactly one node for `foo` is rendered, carrying the union of their edges

#### Scenario: Same-name symbols at different locations stay separate
- **WHEN** two nodes share a label but have different source files or lines
- **THEN** both nodes are rendered separately

### Requirement: Label elision
Query-family commands SHALL accept `--label-width N` (default 120). Labels longer than N
characters MUST be elided at a word boundary with an ellipsis, and the full node id MUST
still be printed so the complete node remains retrievable via `explain`.

#### Scenario: Long label elided
- **WHEN** a matched node's label is 400 characters and `--label-width` is left at default
- **THEN** the rendered label is at most 120 characters plus an ellipsis, and the node id is printed unelided

#### Scenario: Custom width honored
- **WHEN** `--label-width 40` is passed
- **THEN** no rendered label exceeds 40 characters plus the ellipsis marker
