# cli-subcommand-help

Every subcommand answers `--help` with its own surface. Root cause of the feedback's
"no help for subcommands": several `info` registrations in `CLI/Parser.hs:290-308` lack
`<**> helper` (`cypher`, `push`, `push-memgraph`, `merge`, `ingest`, `subgraph`, `serve`,
`init`, `install-skill`), so `--help` on those falls through to the top-level parser.

## ADDED Requirements

### Requirement: Subcommand help everywhere

Every `graphos` subcommand SHALL respond to `--help`/`-h` with its own help text — its
flags with their defaults, and at least one worked usage example — never the top-level
command list. A CI-covered test SHALL enumerate the subparser registrations and assert
each carries the helper, so a newly added subcommand cannot regress this.

- **Plan**: One-line fix per registration (`<**> helper`), plus examples in the parser
  `info` footers; the regression test makes the property structural.
- **Do**: Fix all registrations; add examples for the commands the feedback tried blind
  (`subgraph`, `cypher`, `diff`); Hspec test greps/along the parser structure.
- **Check**: Scenarios below.
- **Act**: If optparse-applicative's generated help is too terse for a command, extend its
  footer — never a separate doc page that can drift.

#### Scenario: subgraph help shows its own surface

- **WHEN** `graphos subgraph --help` runs
- **THEN** the output lists the subgraph flags (seeds, paths, hops, config, out, canvas)
  with defaults and shows a worked example — not the top-level command list

#### Scenario: cypher help reveals output formats

- **WHEN** `graphos cypher --help` runs
- **THEN** the output documents `--json`/`--format` alongside `--write` and the common
  query flags

#### Scenario: No registration without helper

- **WHEN** the parser test suite runs
- **THEN** it fails if any registered subcommand lacks subcommand-level help
