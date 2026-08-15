## Context

The MCP server (stdio JSON-RPC, 11 tools) expands neighborhoods and communities
without bounds. On a graph with a 32k–43k-node community, expansion-based tools
timed out (`-32001`) or aborted, taking down the interactive session. The failure
is unbounded work per request, not a specific bug.

## Goals / Non-Goals

**Goals:**
- Never hang: every request returns bounded, well-formed output.
- Partial results with clear truncation metadata over hard errors.
- Configurable caps and timeout.

**Non-Goals:**
- Fixing the mega-community itself (separate changes).
- Streaming/pagination (possible future work).

## Decisions

- **Bound expansion in the pure query helper** (Domain/Graph) by node count,
  passed down from the MCP layer.
  - *Alternative considered:* cap only in the server — rejected, duplicates logic
    and leaves CLI query unbounded.
- **Wrap request handling in a timeout in Infrastructure/Server** using
  STM/async, harvesting whatever the pure computation produced.
  - *Alternative considered:* cooperative deadline checks inside pure code —
    rejected, complicates pure functions with clock concerns.
- **Additive truncation metadata** (`truncated`, `omitted`) on responses.
  - *Alternative considered:* new error code — rejected, callers prefer partial
    data to failure for exploratory tools.

## Risks / Trade-offs

- [Partial results may mislead if truncation is ignored] → always set explicit
  `truncated` flag and `omitted` counts; document in tool descriptions.
- [Timeout harvest race] → use STM to snapshot accumulated results safely.
- [Default caps too low for power users] → configurable per request.

## Migration Plan

- Additive; defaults chosen to comfortably exceed typical requests.
- Rollback: set caps very high and timeout to a large value.
- Verify with `cabal test` (bounded-expansion properties) and MCP smoke tests
  against a graph with a known mega-community confirming no `-32001`.
