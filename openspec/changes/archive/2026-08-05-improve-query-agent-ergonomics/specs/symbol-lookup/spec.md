# symbol-lookup

Exact identifier lookup via a new `graphos symbols <name>` subcommand (extends PRD §13.1
command table; workflow doc 14-symbols). Bypasses fuzzy scoring entirely for the common
agent case of "I know the exact name".

## ADDED Requirements

### Requirement: Exact symbol lookup command
The CLI SHALL provide `graphos symbols <name>` which returns nodes whose indexed
identifier token or full label equals `<name>`, matching case-sensitively first and
falling back to case-insensitive matching when no case-sensitive hit exists. The command
MUST NOT apply fuzzy scoring or graph traversal.

#### Scenario: Exact hit returns the declaration
- **WHEN** `graphos symbols CliCommand` is run and a node with identifier `CliCommand` exists
- **THEN** the output lists that node with its id, source file, line, kind, degree, and community

#### Scenario: Case-insensitive fallback
- **WHEN** `graphos symbols clicommand` is run and only `CliCommand` exists
- **THEN** the `CliCommand` node is returned via the case-insensitive fallback

#### Scenario: Miss is explicit
- **WHEN** the name matches no identifier token or full label
- **THEN** the command reports no symbol found (with did-you-mean suggestions) and exits without fabricating results

### Requirement: Multiple definitions all listed
When several nodes share the exact name, `graphos symbols` SHALL list every match with
its distinct source location so the caller can disambiguate.

#### Scenario: Same name in two files
- **WHEN** two nodes named `parse` exist in different source files
- **THEN** both are listed, each with its own file and line
