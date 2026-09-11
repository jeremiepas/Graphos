# canvas-export

Focused Obsidian `.canvas` files from sub-graphs and diffs — not the full-vault
`--obsidian` export. Replaces the hand-written Python canvas generator.

## ADDED Requirements

### Requirement: Canvas from focused results

Graphos SHALL emit an Obsidian `.canvas` document from a sub-graph extraction
(`subgraph --canvas`) or a diff (`diff --canvas OUT`): one node per **file** (nodes
aggregated by `source_file`), colored by category — source, test, doc, config — with
edges from the aggregated dependency relations between files, and a deterministic layout
(same input → same coordinates) so canvases diff cleanly in git. Diff canvases SHALL
color by change status (added/removed/changed) instead of category. The canvas writer
SHALL be the one already generating the vault `graph.canvas` (`Export/Obsidian.hs`),
extracted to a shared module — not a second implementation.

- **Plan**: File-level aggregation and category coloring are what the feedback's Python
  generator did; the vault exporter already speaks `.canvas`, it just lacks a focused
  entry point.
- **Do**: Extract the canvas writer to `Infrastructure/Export/Canvas.hs`; add file
  aggregation, category classification (test/doc/config by path convention, source
  otherwise) and a deterministic grid/hierarchical layout.
- **Check**: Scenarios below.
- **Act**: If Obsidian's canvas schema evolves, pin the emitted schema version in one
  constant and cover it with a golden-file test.

#### Scenario: Sub-graph canvas opens in Obsidian

- **WHEN** `graphos subgraph --path "src/domain/design/*" --canvas plan.canvas` runs and
  plan.canvas is opened in Obsidian
- **THEN** it renders one node per extracted file, colored by category, with edges where
  the sub-graph has cross-file relations

#### Scenario: Deterministic layout

- **WHEN** the same extraction is run twice
- **THEN** the two `.canvas` files are byte-identical

#### Scenario: Diff canvas colors by change status

- **WHEN** `graphos diff old.json new.json --canvas changes.canvas` runs
- **THEN** the canvas contains only files with differences, colored by
  added/removed/changed status
