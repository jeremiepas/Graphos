## ADDED Requirements

### Requirement: Haskell declaration nodes carry line range and signature

Every node the Haskell extractor emits for a top-level declaration SHALL carry
`nodeLineStart`, `nodeLineEnd` and `nodeSignature`. `nodeLineStart` SHALL be the
1-based line of the declaration's first line: the type signature line when one
directly precedes the definition, otherwise the first definition line. `nodeLineEnd`
SHALL be the last non-blank, non-comment line before the next top-level declaration or
the end of the file. `nodeSignature` SHALL be the type signature with its continuation
lines joined by single spaces when one exists, otherwise the first definition line,
trimmed, and at most 200 characters. Node ids and labels SHALL NOT change.

#### Scenario: Function with a type signature

- **WHEN** a file contains `verdict :: Int -> Int` on line 3, `verdict x = helper x + 42` on line 4, a blank line 5 and another declaration on line 6
- **THEN** the node of `verdict` SHALL have `nodeLineStart = 3`, `nodeLineEnd = 4` and `nodeSignature = "verdict :: Int -> Int"`

#### Scenario: Multi-line type signature

- **WHEN** the signature of `connect` spans lines 10 to 12 and its definition lines 13 to 20
- **THEN** the node SHALL have `nodeLineStart = 10`, `nodeLineEnd = 20`, and a `nodeSignature` made of the three signature lines joined by single spaces

#### Scenario: Declaration without a type signature

- **WHEN** `helper x = x * 2` on line 7 has no type signature
- **THEN** the node SHALL have `nodeLineStart = 7`, `nodeLineEnd = 7` and `nodeSignature = "helper x = x * 2"`

#### Scenario: Data type declaration

- **WHEN** `data Mode = Auto | On | Off` spans lines 30 to 33
- **THEN** the node of `Mode` SHALL have `nodeLineStart = 30` and `nodeLineEnd = 33`

#### Scenario: Ids are stable

- **WHEN** a Haskell file is extracted before and after this requirement
- **THEN** the set of node ids and the set of edges SHALL be identical

### Requirement: Source command prints a node's lines from disk

The CLI SHALL provide `graphos source <node>`. It SHALL turn the argument into a node
through `resolveNodeArg`, and SHALL render `NotFound` and `Ambiguous` as the other
node-argument commands do. It SHALL read the node's source file, resolved against
`--root <dir>` (default: the current directory), and print lines `nodeLineStart` to
`nodeLineEnd`, each prefixed with its line number, under a header line with the node
id and `<source file>:<start>-<end>`. `--context <n>` (default 0) SHALL add `n` lines
before and after. `--json` SHALL print one object with `id`, `label`, `kind`,
`source_file`, `line_start`, `line_end` and `text`. The command SHALL accept `--graph`
and SHALL NOT modify the graph.

#### Scenario: Print a function body

- **WHEN** `graphos source verdict --root /work/p` is run and the node of `verdict` is in `src/a.ts`, lines 1 to 3
- **THEN** the output SHALL be the header `… src/a.ts:1-3` followed by lines 1 to 3 of `/work/p/src/a.ts`, numbered

#### Scenario: Context lines

- **WHEN** `--context 2` is added and the node spans lines 10 to 12 of a 40-line file
- **THEN** lines 8 to 14 SHALL be printed

#### Scenario: Context is clipped to the file

- **WHEN** `--context 5` is used on a node spanning lines 2 to 3
- **THEN** printing SHALL start at line 1 and no error SHALL be raised

#### Scenario: JSON output

- **WHEN** `graphos source verdict --json` is run
- **THEN** the output SHALL be one JSON object whose `text` is the node's lines joined by newlines and whose `line_start` and `line_end` match the node

#### Scenario: Ambiguous name is not guessed

- **WHEN** two files each define `main` and `graphos source main` is run
- **THEN** both candidates SHALL be listed with id, kind and source file, and the exit status SHALL be non-zero

### Requirement: Source command reports what it cannot show

`graphos source` SHALL exit with a non-zero status and a message that names the cause
when it cannot print the node's own lines: the node has no `nodeLineStart` (message
names the node and its source file), the file does not exist under the root (message
names the resolved path and the `--root` flag), or the file has fewer lines than
`nodeLineEnd`. A node with `nodeLineStart` and no `nodeLineEnd` SHALL be printed as the
single line `nodeLineStart`. When the node carries a `body_hash` and the hash of the
lines read differs, the command SHALL print the lines, add a warning that the file
changed since the graph was built, and exit with status zero. With `--json`, each of
these outcomes SHALL be expressed in the object (`error` code, or `stale: true`).

#### Scenario: Node without a line range

- **WHEN** `graphos source` is run on a node that has no `nodeLineStart`
- **THEN** the command SHALL print that the node has no line range, name its source file, and exit with a non-zero status

#### Scenario: Wrong root

- **WHEN** the node's source file is `./src/a.ts` and no such file exists under the current directory
- **THEN** the message SHALL name the resolved path and mention `--root`, and the exit status SHALL be non-zero

#### Scenario: File changed since the build

- **WHEN** the node has a `body_hash` and its lines on disk hash to a different value
- **THEN** the lines SHALL be printed with a warning that the file changed, and with `--json` the object SHALL have `stale: true`

#### Scenario: Start line only

- **WHEN** the node has `nodeLineStart = 12` and no `nodeLineEnd`
- **THEN** line 12 alone SHALL be printed
