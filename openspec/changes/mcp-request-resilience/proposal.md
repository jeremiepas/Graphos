## Why

MCP tools hang or fail on large graphs. This session saw `get_community` and
`select_context` return `-32001 Request timed out`, and a `get_neighbors` call
abort, whenever they touched a hub or mega-community. A single pathological node
should degrade gracefully — return a bounded, truncated result — never hang the
whole MCP session.

## What Changes

- Add a **per-request node/expansion cap** to MCP tools that expand neighborhoods
  or communities (`get_community`, `get_neighbors`, `select_context`).
- Add a **per-request timeout** with a partial, well-formed result on expiry
  rather than a hard `-32001`.
- When a request hits a cap or timeout, return results plus a `truncated: true`
  flag and an `omitted` count instead of failing.
- Make caps and timeout configurable (config + MCP tool params).

## Capabilities

### New Capabilities
- `mcp-request-limits`: bound node expansion and wall-clock time per MCP request,
  returning partial results with truncation metadata instead of errors.

### Modified Capabilities
<!-- Confirm during specs phase whether existing MCP tool specs change their
     response contract; the truncation fields are additive. -->

## Impact

- **Infrastructure/Server (MCP)**: enforce caps/timeouts, add truncation metadata.
- **UseCase/SelectContext**: honor a node budget during expansion.
- **Domain/Graph query**: bounded neighborhood expansion helper.
- Complements the community-size cap and query-budget changes.
