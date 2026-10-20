## Context

Query responses embed full source text twice: once in the node `id` and once in
the `label`. For code with very long lines or large snippets this explodes the
response (observed 106 KB–1.4 MB against a nominal `budget` of 800–2000),
exceeding client and provider limits. The `budget` currently influences traversal
but not serialized output size.

## Goals / Non-Goals

**Goals:**
- Make `budget` a hard, predictable cap on serialized bytes.
- Compact, useful list responses (short id, truncated label, key fields).
- Short stable node IDs with a separate preview field.

**Non-Goals:**
- Changing traversal algorithms (BFS/DFS) or scoring.
- Streaming/paginated responses (possible future work).

## Decisions

- **Two-phase response build**: rank → serialize with a running byte counter,
  stop when the budget is reached, record `omitted`.
  - *Alternative considered:* estimate size upfront — rejected as inaccurate for
    variable-length labels.
- **Short NodeId = `relativePath#startLine` with a content-hash suffix on
  collision.**
  - *Alternative considered:* pure content hash — rejected, less human-readable.
  - *Alternative considered:* keep text-blob IDs — rejected, root cause of bloat.
- **Truncation in Domain/Context formatting**, keeping IO out of the cut logic.
  - *Alternative considered:* truncate in MCP server — rejected, duplicates logic
    across CLI and MCP.
- **`preview` field carries truncated snippet**; full text stays in graph.json
  node records, not in query responses.

## Risks / Trade-offs

- [BREAKING: node ID format change] → provide a migration note; bump graph.json
  schema version; document that cached IDs must be regenerated.
- [Budget accounting overhead] → O(results) byte counting, negligible vs current
  serialization cost.
- [Truncated labels hide detail] → `explain`/`get_node` still return full detail
  on demand.

## Migration Plan

- Bump graph.json schema version; regenerate graphs (IDs change).
- Consumers must stop persisting old text-blob IDs.
- Verify with `cabal test` (budget + serialization suites) and MCP smoke tests
  confirming responses stay under budget.
