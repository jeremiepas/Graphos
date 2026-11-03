# AVI-961: intent code-graph projection (classifier + well-formedness)

## Context

Project: Graphos — context graph builder, any input → knowledge graph → clustered communities → HTML + JSON + report. Tagline: "Context graph builder — any input → knowledge graph → clustered communities → HTML + JSON + report". Problem & Solution: LLMs waste tokens re-reading entire codebases; developers lack navigable maps of how code connects. Solution: Use LSP to extract code structure as a graph, cluster with Leiden community detection, produce persistent queryable context that saves tokens per LLM call.

Tech stack (from `openspec/config.yaml`):
- Language: Haskell (GHC 9.10)
- Build: Cabal 3.0
- Dev Shell: Nix (nixpkgs-unstable, shell.nix)
- Graph Library: FGL 5.8+ (functional graph library, pure graph operations)
- Protocol: LSP (lsp-types 2.0+, Language Server Protocol for extraction)
- Serialization: Aeson JSON serialization
- CLI: optparse-applicative 0.17+
- Testing: Hspec 2.10+ / QuickCheck 2.14+
- Concurrency: STM + async (watcher, multi-thread extraction, MCP)
- Observability: hs-opentelemetry-sdk + custom IORef MetricsStore + Prometheus endpoint
- LLM Integration: OpenAI-compatible client for labeling/ingest/extraction
- MCP Protocol: 2024-11-05 stdio JSON-RPC 2.0

## Goals / Non-Goals

**Goals:**
- Implement the six pure `Domain.Graph` surface functions required for G5 GAP-1 (AVI-1291 math-requirements adoption):
  - `cond(G)` — condensation DAG of SCCs.
  - `isAcyclicDirected`
  - `connectedComponents`
  - `weaklyConnectedComponents`
  - `isolatedNodes`
  - `hasCycleUndirected`
  - *WithCached* variants of all above functions (14 exports in `Graphos.Domain.Graph.Query`, re-exported from `Graphos.Domain.Graph`).
- Provide property tests for INV-7 uniqueness / condensation-is-DAG.
- Golden fixture tests following AVI-523 pattern (cond, isAcyclicDirected, isolatedNodes, connectedComponents, weaklyConnectedComponents, INV-C6 coarsening).
- Integration with existing test suite (`tests/Graphos/Domain/Graph/SCCSpec.hs`, §2.3–2.6).

**Non-Goals:**
- No pipeline/wiring changes beyond the specification.
- Do not introduce new Haskell dependencies (keep existing `stronglyConnectedComponents` function; do **not** import `Domain.Graph` to avoid cycles).
- Preserve all existing graph invariants.

## Decisions

### 1. Pure Domain layer
Domain must contain **zero IO**. All side effects (LSP extraction, file-system cache) live in Infrastructure only. This keeps the pure functions deterministic and testable.

### 2. FGL as graph library
Use `FGL` 5.8+ (Patricia Tree) to implement graph operations on raw Maps/Sets; do **not** import `Domain.Graph`. This avoids cycles and matches existing code patterns (`stronglyConnectedComponents`, etc.).

### 3. Condensation DAG via SCC decomposition
Implement `cond(G)` using FGL's SCC decomposition to build the condensation DAG of SCCs. The result is a mapping from each node to its SCC id, and edges between SCCs derived from original arcs.

### 4. Cached variants (`*WithCached`)
Implement `*WithCached` functions that cache intermediate results (e.g., per-node SCC ids) using FGL's caching primitives; integrate with existing `stronglyConnectedComponentsWithCached`.

### 5. Leiden community detection parameters
Respect the project defaults: `resGamma = 1.0`, `resMinSize = 3`, `resMaxIterations = 50` (from context). Use these when computing node/edge budgets and community labels.

### 6. Performance & strictness
Apply StrictData, BangPatterns on performance-critical paths; prefer pure functions; avoid partial functions in production code.

## Risks / Trade-offs

- **Risk:** FGL's Patricia Tree may not expose all needed adjacency lookups for large graphs (>10k nodes). Mitigation: fall back to naive map-based traversal with STM locking.
- **Risk:** Caching introduces memory pressure on the cache/; write-back rewrites cache hits every run. Mitigation: keep cache bounded via `SHA256(model <> text)` key, prune stale entries.
- **Risk:** Changing from vis-network to sigma.js v3 + graphology + @sqlite.org/sqlite-wasm (see design decision 6). Mitigation: validate HTML output size (<200 MB) and community-lod rendering.

## Migration Plan

1. Add the six pure functions (`cond`, `isAcyclicDirected`, `connectedComponents`, `weaklyConnectedComponents`, `isolatedNodes`, `hasCycleUndirected`) to `Domain.Graph` using FGL's pure graph primitives.
2. Implement the seven `*WithCached` variants that wrap the above with caching (FGL cache key = `(model, text)`).
3. Wire integration with existing `stronglyConnectedComponents` and `stronglyConnectedComponentsWithCached`.
4. Add property tests for INV-7 uniqueness / condensation-is-DAG.
5. Run golden fixture tests following AVI-523 pattern on the largest graph (10k+ nodes) to confirm performance.
6. Deploy via `graphos serve`; monitor HTML output size and community-lod rendering.

## Open Questions

- **Community join pass:** Should a dedicated community-join pass be added to produce community-aggregate dataset (`community_aggregates`) in `graph.json`?
- **Cached variants performance on 10K+ node graphs:** Expected >10× speedup for second consecutive `query_graph` and `shortest_path` calls (see proposal INV-7).
- Whether the existing vis-network → sigma.js v3 migration requires updating MCP server toolset (articulationPointsWithCached, bridge_nodes).
