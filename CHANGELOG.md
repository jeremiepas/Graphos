# Change Log

## Unreleased

### Changed
- **Tree-sitter extraction granularity is now configurable and defaults to `function` level.**
  Statement-level nodes (assignments, returns, conditionals, parameters, local
  variables, JSON key-value pairs) are no longer extracted by default. This
  reduces node counts ~5-10x on statement-dense codebases and proportionally
  speeds up clustering, export, and queries.
  - New levels: `fine` (previous behavior), `function` (default), `file` (one node per file).
  - Resolution order: CLI `--granularity` flag → per-extension `granularity:` in
    `extractors:` config → global `granularity:` in `graphos.yaml` → built-in default.
  - `.json` files default to `file` granularity (one node per file; lock files
    no longer inflate the graph).
  - **Rollback**: add `granularity: fine` to `graphos.yaml` to restore the previous output.
- Leiden community detection now scales to 100k+ node graphs (16x faster at
  100k nodes: 169s → 10.5s, compiled): in-place assignment updates, batched
  refinement, incremental merge indexing.
- **MCP query path now caches `GraphIndex` and `CachedFGL` at load time** (was
  rebuilt per request). `query_graph` and `shortest_path` latency drops from
  O(N) to O(k) on the second and subsequent calls. `handleQueryGraph` now
  makes a single query invocation (was 3). `bridge_nodes` uses the cached FGL
  (was rebuilt per call).
- **MCP `query_graph` response gains fields**: `verdict`, `best_score`, `hash`,
  `suggestions`. `traverse` field kept as `mode` echo for one release.

### Fixed
- `mergeSmallCommunities` no longer silently drops nodes when a community that
  received members from an earlier merge is itself merged.
- Haskell stub extraction: cross-file `imports` edges now resolve via canonical
  module IDs; no more truncated 20-char junk labels; declarations carry kinds.
- `GRAPH_REPORT.md` and `graph.json` are now generated from the same enriched
  graph state (totals always match); duplicate surprising connections removed.
- `span_build`/`span_cluster` debug-trace durations now measure forced work
  instead of thunk creation.
- The debug-trace `traces/` directory is only created when tracing is enabled
  and events were emitted.
- **FGL node indexing is now bijective** (sequential `0..N-1` indices, was
  hash-based `nidToInt`). Two distinct `NodeId`s that collided under the old
  hash no longer silently lose one node — `shortestPath`/`articulationPoints`/
  `biconnectedComponents`/`dominators` now find paths/bridges through
  collision-prone node pairs.

## 0.1.0.0

- Initial release
- LSP-based code extraction (universal language support)
- Knowledge graph construction
- Leiden community detection
- God nodes, surprising connections, suggested questions
- Export: JSON, HTML, Obsidian, Neo4j Cypher, GraphML, SVG
- MCP server (placeholder)
- File watching (placeholder)
- Incremental updates