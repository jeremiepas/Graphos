# query-cli-contract

Uniform, machine-readable CLI contract across the query family (`query`, `path`,
`explain`, `symbols`, `neighbors`) (PRD §13.1 commands, §13.2 flags). Eliminates the
inconsistent flag surface that forced agents to discover the contract by triggering
errors.

## ADDED Requirements

### Requirement: Uniform flag acceptance
Every query-family subcommand SHALL accept `--graph <path>`, `--budget <n>`, `--json`,
`--label-width <n>`, and `--edges semantic|all`, and MUST support `--help` without
error. No query-family subcommand may reject a flag that another accepts.

#### Scenario: Budget accepted by explain
- **WHEN** `graphos explain <node> --budget 5000` is run
- **THEN** the command executes normally instead of failing with an invalid-option error

#### Scenario: Help on every subcommand
- **WHEN** `--help` is passed to any of `query`, `path`, `explain`, `symbols`, `neighbors`
- **THEN** usage text listing the common flags is printed and the exit code is zero

### Requirement: JSON output mode
With `--json`, every query-family subcommand SHALL emit a single JSON document on stdout
containing the same information as the text rendering — including verdict, best score,
result-set hash, scored nodes (id, label, score, source file, community), edges, and
suggestions where applicable — with stable field names and no interleaved log lines.

#### Scenario: Query JSON contains verdict and hash
- **WHEN** `graphos query "term" --json` is run
- **THEN** stdout parses as JSON with fields for verdict, best score, result-set hash, nodes, edges, and suggestions

#### Scenario: Text and JSON agree
- **WHEN** the same query is run with and without `--json`
- **THEN** both renderings report the same verdict, hash, and node id set

### Requirement: Budget honored uniformly
The `--budget` value SHALL bound the token estimate of the rendered output on every
query-family subcommand, truncating at the tail per the query-legibility ordering rules.

#### Scenario: Neighbors respects budget
- **WHEN** `graphos neighbors <id> --depth 3 --budget 500` would produce more output than the budget allows
- **THEN** output stops within the budget estimate and a footer reports the omitted counts
