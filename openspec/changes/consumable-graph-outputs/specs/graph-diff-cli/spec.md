# graph-diff-cli

`graphos diff OLD NEW`: branch-vs-main comparison as one command, replacing the
two-stream Python comparator. Built on `Graphos.Domain.Graph.Diff.graphDiff`, extended
with changed-symbol detection.

## ADDED Requirements

### Requirement: Two-graph comparison

The `graphos diff OLD.json NEW.json` command SHALL load two contract-shaped graphs and
compute added, removed and **changed** symbols — changed meaning the node id exists in
both with a different label, kind, signature or source span — plus added/removed edges,
reusing and extending the Domain `graphDiff` (which today computes only added/removed).
`--scope PATTERN` (repeatable glob over `source_file`) SHALL restrict the comparison to
matching files. The command SHALL work on any two graphs of the contract shape — different
branches, different runs, or a graph and its own sub-graph.

- **Plan**: `graphDiff` exists with no CLI; changed-node detection is an id-intersection
  it currently misses (pure `Map.intersectionWith` extension, Domain layer).
- **Do**: Extend `Domain.Graph.Diff` with changed detection; new `UseCase.Diff` for
  per-file rollups and scope filtering; CLI wiring.
- **Check**: Scenarios below.
- **Act**: If loading two multi-GB graphs in one process hits memory limits, document the
  measured ceiling and fall back to comparing per-file node sets streamed from JSON — the
  report format must not change.

#### Scenario: Added, removed and changed symbols

- **WHEN** NEW differs from OLD by one added function, one deleted function, and one
  function whose signature changed
- **THEN** the diff reports exactly those three, each classified correctly and attributed
  to its source file

#### Scenario: Scope restricts the comparison

- **WHEN** `graphos diff old.json new.json --scope "src/domain/design/*"` runs on graphs
  that also differ outside that subtree
- **THEN** the report contains only differences whose source_file matches the scope

#### Scenario: Unchanged graphs

- **WHEN** the two inputs are identical
- **THEN** the report states zero differences and the command exits zero

### Requirement: Diff report formats

The diff SHALL emit a markdown report on stdout by default — summary counts, then a
per-file table of added/removed/changed symbols grouped by file — and SHALL support
`--json` (one structured document: per-file entries with symbol lists and the
added/removed edge sets) and `--canvas OUT` (a changed-files canvas per `canvas-export`).
Errors — unreadable input, non-contract shape — SHALL go to stderr with a non-zero exit,
never mixed into the report.

- **Plan**: The feedback asked for exactly this triple: human report, scriptable JSON,
  visual canvas.
- **Do**: One rollup structure rendered three ways.
- **Check**: Scenarios below.
- **Act**: If per-file tables explode on large diffs, cap rows per file with an explicit
  "and N more" line — never silently.

#### Scenario: Markdown per-file rollup

- **WHEN** a diff has changes in three files
- **THEN** stdout is markdown with a summary block and one section or table row per file
  listing its added/removed/changed symbols by name

#### Scenario: JSON is a single document

- **WHEN** `graphos diff old.json new.json --json` runs
- **THEN** stdout is exactly one JSON document (no interleaved log lines) whose top level
  carries summary counts and per-file entries

#### Scenario: Bad input goes to stderr

- **WHEN** OLD.json is not a contract-shaped graph
- **THEN** the command exits non-zero with the parse error on stderr and emits no report
  on stdout
