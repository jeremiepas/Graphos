# subgraph-extraction-cli

Seed-driven sub-graph extraction: the one command that replaces the 300-line ijson
workaround. Existing `--config` subsystem mode is preserved; seed mode is added.

## ADDED Requirements

### Requirement: Seed-driven extraction

The `graphos subgraph` command SHALL accept seeds in three forms — repeatable
`--path PATTERN` (matched against node `source_file`, glob semantics), `--seeds FILE` (one
pattern per line, `#` comments ignored), and patterns on stdin when neither is given and
stdin is not a TTY — and SHALL expand the seed set by `--hops N` (default 1) undirected
hops over the graph's edges before emitting the sub-graph. At least one seed form or
`--config` SHALL be required; the error for a seedless invocation SHALL name all accepted
forms.

- **Plan**: The feedback's exact attempted invocations (`--path`, seed lists, stdin)
  become the supported surface; `--config` subsystem mode stays for taxonomy use.
- **Do**: Extend `subgraphOpts`; seed resolution and hop expansion in the existing
  subgraph use-case path.
- **Check**: Scenarios below.
- **Act**: If glob-vs-substring matching surprises users, follow the gitignore-parsing
  capability's semantics and say so in `--help`.

#### Scenario: Path seeds with hop expansion

- **WHEN** `graphos subgraph --graph graphos-out/graph.json --path "src/domain/design/plan-runner*" --hops 1 -o plan-sub.json` runs
- **THEN** plan-sub.json contains every node whose source_file matches the pattern, plus
  all nodes within 1 undirected hop, plus the edges among them — and nothing else

#### Scenario: Seed file

- **WHEN** `graphos subgraph --seeds seeds.txt --hops 1` runs with a file of one pattern
  per line including a `#` comment line
- **THEN** the union of the patterns' matches seeds the extraction and the comment line is
  ignored

#### Scenario: Stdin seeds

- **WHEN** `echo "src/domain/design/plan-runner.ts" | graphos subgraph --hops 1` runs
- **THEN** the piped pattern seeds the extraction, equivalent to the same value via
  `--path`

#### Scenario: Seedless invocation fails with guidance

- **WHEN** `graphos subgraph` runs with no `--path`, `--seeds`, stdin patterns or
  `--config`
- **THEN** the command exits non-zero with an error naming all four accepted seed forms

### Requirement: Compact consumable output

The sub-graph output SHALL be a contract-shaped `graph.json` (the `exportSubgraphJSON`
layout — directly loadable via `--graph` by every graphos command and by the studio),
containing only the extracted nodes and their induced edges, and SHALL print a one-line
summary (node count, edge count, output path, source-graph coverage percentage) so the
user can immediately judge the extraction. `--graphml` SHALL additionally emit GraphML of
the same sub-graph and `--canvas` a `.canvas` per the `canvas-export` capability.

- **Plan**: "Compact or filtered output" from the feedback: the small JSON is the compact
  format — a 4.3 GB graph becomes a consumable file in one command.
- **Do**: Reuse `exportSubgraphJSON`; route GraphML/canvas through the existing exporters.
- **Check**: Scenarios below.
- **Act**: If extractions routinely produce >100 MB sub-graphs, add a node-count guard
  with a `--force` override rather than silently emitting another unusable file.

#### Scenario: Output round-trips into graphos

- **WHEN** a sub-graph is extracted to plan-sub.json and `graphos cypher "MATCH (n) RETURN n.name" --graph plan-sub.json` runs
- **THEN** the query executes against the sub-graph without any format conversion

#### Scenario: Summary line

- **WHEN** any extraction completes
- **THEN** stdout's final line states node count, edge count, output path and the
  percentage of source-graph nodes retained
