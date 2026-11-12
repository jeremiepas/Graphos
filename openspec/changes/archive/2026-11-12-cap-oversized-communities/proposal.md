## Why

Even with generated-code detection, a densely-connected subgraph can still
collapse into one giant community. This session saw a single community holding
32k–43k nodes (24–33% of a 132k-node graph), which no `--resolution` value could
split (0.4 → larger, 2.0 → no change). Such mega-communities destroy clustering
value, dominate centrality (`god_nodes`), and are the direct cause of MCP
timeouts. A structural safety net is needed independent of the input.

## What Changes

- After Leiden clustering, run a **size-cap post-pass** that splits any community
  exceeding a configurable fraction of total nodes (default 5%) into smaller
  sub-communities.
- Emit a **WARNING** whenever a community exceeds the cap before splitting.
- **Exclude generated/vendored nodes from centrality** (god-node/bridge)
  computation so metrics reflect real code even if such nodes remain.
- Add config keys and CLI flags for the cap fraction and splitting on/off.

## Capabilities

### New Capabilities
- `community-size-cap`: bound maximum community size via a post-clustering split
  pass and exclude noise nodes from centrality analysis.

### Modified Capabilities
<!-- Confirm during specs phase whether an existing clustering/analysis spec's
     requirements change; otherwise this is purely additive. -->

## Impact

- **Domain/Community**: add a size-cap splitter operating on cluster output.
- **Domain/Analysis**: exclude flagged nodes from god-node/bridge computation.
- **Domain/Config + CLI**: cap fraction, enable/disable flags.
- **UseCase/Cluster + Analyze**: wire the post-pass; no IO added.
