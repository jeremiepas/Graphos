## Why

Interrupted or concurrent runs leave the output directory in a broken state. This
session found `graph.json` written incrementally and left **truncated/corrupt**
(`Unterminated string`) after a labeling run, and a mid-session rebuild left
`graphos-out/graph.json` **missing**, which broke the MCP server. Because
`graphos-out/` is the single source of truth, a half-written or absent graph
silently breaks every downstream consumer.

## What Changes

- Write all primary outputs (`graph.json`, `graph.checkpoint.json`,
  `GRAPH_REPORT.md`, etc.) **atomically**: write to a temp file, `fsync`, then
  rename into place.
- Perform destructive rebuilds via a **staging directory** that is swapped into
  `graphos-out/` only on success, so an existing good graph is never removed
  before a new one is ready.
- On startup, **validate** an existing `graph.json`; if corrupt, fail with a
  clear message pointing to the checkpoint rather than producing confusing errors.

## Capabilities

### New Capabilities
- `atomic-output-writes`: crash-safe, atomic writing and swapping of graph output
  artifacts so the output directory is always valid or untouched.

### Modified Capabilities
<!-- Confirm during specs phase whether existing export specs' requirements change;
     the atomic-write guarantee is additive to current behavior. -->

## Impact

- **Infrastructure/Export (JSON, Report, ...)**: temp-write + rename.
- **Infrastructure/FileSystem**: staging-dir swap for full rebuilds; startup
  validation of existing graph.json.
- **UseCase/Pipeline / Load**: surface a clear error on corrupt graph.
- No schema change; behavior becomes crash-safe.
