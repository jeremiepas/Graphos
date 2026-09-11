# cypher-query (delta)

Structured output for `graphos cypher`. Today `--json` parses (it rides
`commonQueryOptsP`) and `renderCypherResultJSON` exists with unit tests — but the CLI
success path never calls it: the flag is silently ignored and output is always TSV. All
existing requirements of this capability (pattern subset, property mapping, read-only
enforcement, bounded results) are unchanged.

## ADDED Requirements

### Requirement: Structured result output

The `cypher` command SHALL honor `--json` by emitting the result as a single JSON
document on stdout (the existing `renderCypherResultJSON` shape: columns, rows,
truncation flag), and SHALL support `--format json|csv|tsv` as the general form (`--json`
≡ `--format json`; default remains `tsv` for backward compatibility). In every format the
result SHALL be the only stdout content — errors and progress go to stderr — and CSV
SHALL quote fields containing separators per RFC 4180. Conflicting `--json` and
`--format` values SHALL be rejected with a clear error.

- **Plan**: The renderer exists and is tested; this wires it to the flag users can
  finally discover once `cypher --help` works (`cli-subcommand-help`).
- **Do**: Dispatch on the format in the cypher success path; add the csv renderer next to
  the existing text/JSON renderers in `UseCase/Query/Render.hs`.
- **Check**: Scenarios below.
- **Act**: If scripts need streaming for very large results, add `--format jsonl` later —
  the dispatch point is the same.

#### Scenario: JSON output is one parseable document

- **WHEN** `graphos cypher "MATCH (n) RETURN n.name" --json` runs against a graph
- **THEN** stdout parses as a single JSON document containing the column names, the rows,
  and the truncation flag — with no interleaved log lines

#### Scenario: Default stays TSV

- **WHEN** `graphos cypher "MATCH (n) RETURN n.name"` runs with no format flag
- **THEN** the output is the existing TSV rendering, unchanged

#### Scenario: CSV quotes separators

- **WHEN** a returned label contains a comma and `--format csv` is used
- **THEN** that field is quoted per RFC 4180 and a standard CSV parser reads the row
  correctly

#### Scenario: Errors never pollute stdout

- **WHEN** a cypher parse error occurs under `--json`
- **THEN** stdout is empty, the error is on stderr, and the exit code is non-zero
