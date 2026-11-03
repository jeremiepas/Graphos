## ADDED Requirements

### Requirement: Stub extraction is a visible, reported outcome

When any detected code file is reduced to stub extraction (a single node with no `kind` and no line range, because no tree-sitter or LSP extractor produced structure), the system SHALL make this visible without the user enabling debug logging. The extract stage SHALL emit a warning at INFO level naming the count of stubbed code files and their counts by extension. This warning SHALL be emitted whenever the stubbed-code-file count is greater than zero.

#### Scenario: Stubbed files warn at INFO

- **WHEN** a run detects 3 `.scala` files for which no extractor produces structure
- **THEN** an INFO-level warning is logged naming `3` stubbed code files and the breakdown `.scala (3)`

#### Scenario: Fully-extracted corpus is silent

- **WHEN** every detected code file produces at least one non-stub node
- **THEN** no stub warning is logged

### Requirement: Extraction coverage is reported in GRAPH_REPORT.md

`GRAPH_REPORT.md` SHALL include an "Extraction coverage" line set reporting the number of code files detected, the number that produced at least one structural (non-stub) node, and the stubbed count broken down by extension. This SHALL appear regardless of language so that a near-empty extraction is self-evident from the report alone.

#### Scenario: Coverage reported

- **WHEN** a run detects 42 code files of which 8 produce structural nodes and 34 are stubbed (all `.scala`)
- **THEN** `GRAPH_REPORT.md` contains `Files detected: 42`, `Files with structure: 8`, and `Stubbed: 34 (.scala 34)`

### Requirement: Dominated extraction is flagged

When the ratio of stubbed code files to detected code files exceeds the named exported threshold `stubDominanceThreshold` (default `0.5`), the stub warning SHALL additionally state that the graph likely under-represents the corpus and name the dominant stubbed extension.

#### Scenario: Majority-stubbed corpus is flagged

- **WHEN** 34 of 42 detected code files are stubbed and the dominant stubbed extension is `.scala`
- **THEN** the INFO warning states the graph likely under-represents the corpus and names `.scala` as the dominant stubbed type

#### Scenario: Minority-stubbed corpus is not flagged

- **WHEN** 1 of 42 detected code files is stubbed
- **THEN** the dominated-extraction clause is not appended to the warning
