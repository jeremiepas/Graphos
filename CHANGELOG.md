# Change Log

## Unreleased

### Added
- Tree-sitter grammars for PHP, Kotlin (aliased to the Java parser), Perl, and
  C# are registered, plus vendored Perl/C# parsers and Hackage PHP; Java
  imports of the form `a.b.C` now resolve to `source-root/a/b/C.java`.
- Kotlin import resolution (AVI-1765): `import a.b.C` resolves against
  `src/main/kotlin` / `src/test/kotlin` first, then the Java source roots for
  Kotlin/Java interop, then falls back to an external node.
- **Stub extraction is now a visible, reported outcome.** Any code file that
  falls through to stub extraction (no tree-sitter/LSP structure) is now
  named at INFO level with a per-extension breakdown (was silent debug
  logging), and `GRAPH_REPORT.md` gains an "Extraction Coverage" section
  (`Files detected`, `Files with structure`, `Stubbed: N (.ext n, …)`). A
  corpus where stubbed files exceed 50% additionally states that the graph
  likely under-represents the corpus and names the dominant stubbed
  extension.

### Fixed
- **PHP, Perl, C# and Kotlin now actually produce import edges and labelled
  import nodes (AVI-1765).** Their grammars write the specifier unquoted, so
  `Convert.extractSpecifier` — which only looked for string literals — yielded
  nothing: `Import` nodes carried an empty label and no `imports` edge, which
  the `import-resolution` spec forbids. Alongside that, PHP's import forms
  (`namespace_use_declaration`, `include`/`require` expressions) were missing
  from the emission whitelist, C#'s file root `compilation_unit` was missing
  from `structureTypes` (so `.cs` files had no module node and their `using`
  edges had no source), and three Perl node names in the whitelists did not
  exist in the vendored ABI 11 grammar — `use Foo;` is `use_no_statement` and
  a named `sub f { }` is `function_definition`. Verified node names are
  recorded in `vendor/README.md`.
- **Declarations that name themselves with a non-`identifier` node no longer
  collapse onto one empty node id.** Perl `package Shop::Cart;`
  (`package_name`), PHP `namespace Shop;` (`namespace_name`), C#
  `namespace A.B { }` (`qualified_name`) and C#/Java field declarations (the
  name sits in the `variable_declarator`) all resolved to no identifier, so
  each got the node id `""` — and every such node in a corpus collided on
  that id when the per-file extractions were merged, leaving exactly one
  survivor for the whole graph.
- **`.pl`, `.pm` and `.psgi` are in `fecCode`.** They were registered as
  tree-sitter extensions but absent from the file-category defaults, so Perl
  files were never detected and never reached the extractor.
- **Java imports resolve across source roots.** Resolution walked only the
  importing file's ancestors, so a class under `src/test/java` could not
  resolve an import of a class under `src/main/java`. Candidate roots now
  include each ancestor's `src/main/java` and `src/test/java`. Static imports
  (`import static a.b.C.m;`) resolve to the enclosing type, and a file is
  never its own import target.
- Java wildcard imports deduplicate source files discovered across ancestor
  directories.
- Apply review-driven changes from AVI-10 verdicts (AVI-11).

### Changed
- Node ids now use the `<dirHash>_<stem>_<kind>:<identifier>` layout (Group 3
  node identity, `idScheme=2`), with a `#L<line>` suffix for in-file
  collisions; `makeNode` labels nodes with the identifier and stores the
  normalized declaration text as the bounded signature. Caches/checkpoints
  built with the previous layout miss.

## 0.2.0 - 2026-10-29

### Fixed
- `--cluster-only` now persists its output: `graph.json` (with communities
  filled, written atomically), the report exports and `community_graph.json`
  under `--community-graph`, plus the checkpoint, instead of dropping them.
- Leiden no longer collapses code graphs: doc↔code co-location links are
  capped (one node per doc file to one node per code file; docs spanning more
  than 25 code files are skipped), `--edge-density Sparse` drops the
  co-location heuristic, community detection and cohesion run on the
  undirected projection, and `addEdges` merges adjacency sets instead of
  keeping only the old set.

## 0.1.1 - 2026-10-20

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
- **`--embed` peak memory is now bounded to a single live copy of the vector set**
  instead of scaling as a multiple of it (drops ~15-20 GB → under ~2 GB at
  100k nodes × 768 dims): embedding vectors switched from boxed `[Double]` to
  compact unboxed `Vector Double` (~8 B/dim vs ~40-48 B), the node-to-vector
  assignment is folded incrementally per batch via the streaming sidecar path
  (no full fresh-table/union-table copy), and `gEmbeddings` is released after
  semantic-edge inference so clustering, analysis, and export never hold the
  table live (vectors remain in the `embeddings.json` sidecar and are re-loaded
  on demand). Semantic code↔doc inference streams doc vectors one at a time.
  Wire formats (sidecar, cache files, `graph.json`) are unchanged; the legacy
  `embedding.streaming` config key is ignored (streaming is now the only mode).

### Fixed
- **`--embed` no longer sends arbitrary-length documents to `/v1/embeddings`.**
  Each input is now truncated to `embedding.maxTokens` (default 512) before the
  request, so a long document can no longer overflow a small encoder's context
  window and be rejected (e.g. a 512-token model served by llama-server
  answering HTTP 500 on oversized inputs). Set `embedding.maxTokens: 0` to
  restore the previous unbounded behavior.

## 0.1.0.0

### Initial release
- LSP-based code extraction (universal language support)
- Knowledge graph construction
- Leiden community detection
- God nodes, surprising connections, suggested questions
- Export: JSON, HTML, Obsidian, Neo4j Cypher, GraphML, SVG
- MCP server (placeholder)
- File watching (placeholder)
- Incremental updates

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
- **Go, Rust and Ruby emit `imports` edges in tree-sitter mode** (#83). Each
  edge has the TypeScript shape: it goes from the file's module node to a
  placeholder whose `source_file` is the imported repo file, or
  `external:<name>`. A Go internal package import becomes one edge per
  non-test file of the package, with weight `1/n` and the alias kept in
  `extra.alias`. Rust resolves `crate::`/`self::`/`super::`, workspace crates,
  groups and globs over the module files. Ruby resolves `require_relative` and
  in-repo `require`. Each Rust `use` now gets its own `Import` node per
  occurrence (the id gains `#L<line>`), and a `use` inside a function body is
  no longer dropped.
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
