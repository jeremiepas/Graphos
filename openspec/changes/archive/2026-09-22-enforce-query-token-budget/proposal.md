## Why

MCP query tools do not bound their output. `query_graph` called with
`budget=2000` returned ~106 KB of JSON; other queries returned 200 KB–1.4 MB,
all truncated by the client and one triggering a provider size-limit error. The
main cause is that each node/edge carries its full source-text `label` (often the
entire snippet), and node IDs embed the whole snippet too. Callers cannot get a
compact, usable answer.

## What Changes

- Treat `budget` as a **hard cap on serialized response size**, not just a
  traversal hint.
- **Slim list responses**: return `id`, `source_file`, `score`, `kind`, and a
  short truncated `label` (default 120 chars) instead of full source text.
- Add `--max-nodes` and `--max-label-chars` controls (and MCP tool params).
- Introduce **short stable node IDs** (hash or `file#line`) exposed alongside a
  separate `preview` field, so IDs stop embedding entire snippets. **BREAKING**
  for consumers relying on the old text-blob IDs.
- Deterministically rank and cut results to fit the budget, reporting
  `omitted` counts (already partially present).

## Capabilities

### New Capabilities
- `query-response-budget`: enforce a serialized-size budget and compact
  serialization for MCP/query outputs, including short node IDs and truncated labels.

### Modified Capabilities
<!-- If an existing 'mcp-tools' or 'query' spec exists in openspec/specs/, this
     modifies its response-shape requirements. Confirm during specs phase. -->

## Impact

- **Domain/Context** formatting: compact node/edge serialization, label truncation.
- **Domain/Graph**: short stable NodeId derivation (hash/`file#line`).
- **Infrastructure/Server (MCP)**: enforce byte budget, new tool params.
- **BREAKING**: node ID format changes; graph.json consumers and cached IDs affected.
