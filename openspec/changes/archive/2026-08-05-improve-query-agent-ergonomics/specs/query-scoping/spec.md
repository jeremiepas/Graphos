# query-scoping

Source-path scoping for `graphos query` (PRD §13.1). Makes file paths first-class,
searchable query inputs so agents can restrict results to production code or a specific
subtree.

## ADDED Requirements

### Requirement: Path glob filter
`graphos query` SHALL accept `--path <glob>` and MUST restrict candidate matches to
nodes whose `nodeSourceFile` matches the glob before traversal, so the token budget is
spent only on in-scope nodes.

#### Scenario: Filter includes only matching subtree
- **WHEN** `graphos query --path 'src/cli/**' "observability"` is run
- **THEN** every rendered node's source file is under `src/cli/`

#### Scenario: Filter with no in-scope match reports none
- **WHEN** the query term matches nodes only outside the given glob
- **THEN** the response reports verdict `none` for the scoped query rather than returning out-of-scope nodes

### Requirement: Source paths are indexed and searchable
The graph index SHALL index source-file path segments, and `graphos query` MUST match
path-like query terms (terms containing `/`) against that path index in addition to node
labels.

#### Scenario: Bare path query matches
- **WHEN** `graphos query "src/cli/commands"` is run against a graph containing nodes from that directory
- **THEN** the response contains nodes whose source files are under `src/cli/commands` (previously this returned no matches)
