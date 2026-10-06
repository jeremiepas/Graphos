# Change Log

## Unreleased

### Changed
- **`graph.json` is now a JSON Graph Format (JGF) document** (media type
  `application/vnd.jgf+json`, spec at jsongraphformat.info): the graph lives
  under a top-level `graph` object with `directed: true`,
  `type: "graphos.code-knowledge-graph"`, `nodes` (object keyed by id),
  `edges` (array), and `metadata`.
  - Node `id`/`label` stay top-level; `file_type`, `source_file`,
    `community_id`, `line_start`/`line_end`, `signature`, `kind`, `degree`,
    `is_bridge` and `extra` moved under node `metadata`.
  - Edge `source`/`relation`/`target` stay top-level; `id`, `weight`,
    `confidence` and `extra` moved under edge `metadata`.
  - Graph-level `communities`, `cohesion`, `god_nodes`, `community_labels`,
    `community_aggregates`, `compositions`, `embeddings_path`, `null_model`,
    `graph_hash` and a new `schemaVersion` ("1.0") live under
    `graph.metadata.graphos`.
  - **Compatibility**: the reader still loads legacy top-level `nodes`/`edges`
    files during a deprecation window (detected by the absence of the
    top-level `graph` key). JGF documents whose major `schemaVersion` is
    unsupported are rejected with a clear error.
  - **Migration**: `graphos migrate-graph <file>` rewrites a legacy graph to
    the JGF envelope (in place, or via `--output`); the next full run also
    rewrites files automatically.
  - The checkpoint file (`graph.checkpoint.json`) carries the same envelope.
  - Files are now readable by any standard JGF tooling.
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
- **`--embed` no longer sends arbitrary-length documents to `/v1/embeddings`.**
  Each input is now truncated to `embedding.maxTokens` (default 512) before the
  request, so a long document can no longer overflow a small encoder's context
  window and be rejected (e.g. a 512-token model served by llama-server
  answering HTTP 500 on oversized inputs). Set `embedding.maxTokens: 0` to
  restore the previous unbounded behavior.
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