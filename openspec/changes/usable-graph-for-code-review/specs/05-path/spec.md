## MODIFIED Requirements

### Requirement: Workflow 05 — shortest path between two nodes
CLI `graphos path <from> <to>` SHALL: (1) load graph.json via `UseCase.Load`, (2) resolve source and target through `Graphos.UseCase.Query.resolveNodeArg` (exact id, then exact identifier, then case-insensitive identifier — see `identifier-resolution`), (3) compute shortest path via `Domain.Graph.Query.shortestPath` (BFS via FGL `esp`), (4) return ordered list with node labels, edge relations, confidence per hop. For directed graphs (`--directed`), paths follow edge direction. Disconnected nodes return "no path found". An endpoint that resolves to `NotFound` or `Ambiguous` SHALL be reported as such (with suggestions or candidates) before any traversal, and the command SHALL NOT fall back to a tokenized best match. Flag: `--graph PATH` (default `graphos-out/graph.json`). (PRD §13, workflow 05)

#### Scenario: Shortest path between connected nodes
- **WHEN** `graphos path "AuthModule" "Database"` is called on a connected graph
- **THEN** result SHALL be ordered list like `[Auth, AuthMiddleware, DBPool, Database]` with relation and confidence per hop

#### Scenario: No path exists
- **WHEN** two nodes are disconnected
- **THEN** result SHALL indicate "no path found"

#### Scenario: Path between module nodes by file name
- **WHEN** `graphos path index.ts db.ts` is called on the graph built from `example/ts-lsp-test`
- **THEN** both arguments resolve to their module nodes by identifier and the returned path consists of `imports` hops, verifiable by `cabal test`

#### Scenario: Ambiguous endpoint lists candidates
- **WHEN** `graphos path main db.ts` is called and two nodes are labelled `main`
- **THEN** the output lists both `main` candidates with ids and source files, prints no path, and with `--json` emits the ambiguous document instead of `{"path": null}`
