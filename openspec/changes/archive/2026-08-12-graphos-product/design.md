## Context

Graphos is a universal knowledge graph builder that extracts code structure via LSP, clusters it with Leiden community detection, and produces persistent, queryable context for LLM calls. The codebase follows clean architecture with three layers: Domain (pure), UseCase (pure orchestration), and Infrastructure (all IO). The current state is a scaffolded project with module structure defined in `graphos.cabal` but requiring full implementation across all eight capabilities defined in the proposal.

## Goals / Non-Goals

**Goals:**

- Implement the full seven-stage pipeline as pure functions composed in the UseCase layer
- Build LSP-based extraction supporting 30+ languages with graceful fallback chain
- Implement Leiden community detection in pure Haskell with configurable parameters
- Deliver context selection with five strategies mapped to query complexity
- Provide MCP server with 11 tools and non-polluting chat memory
- Support Neo4j/Memgraph push with three modes and auto-selection
- Handle multi-format input (code, docs, papers, images, video/audio)
- Integrate observability (OTLP traces, metrics, logs, debug trace)
- Achieve `cabal build` with zero warnings and `cabal test` passing all specs
- Meet performance targets: 100k nodes < 5 min extraction, Leiden < 30s, queries < 500ms

**Non-Goals:**

- Mutable graph via MCP (future per PRD §18)
- Embedding-based semantic search (future per PRD §18)
- Incremental community update without full re-cluster (future)
- LLM-driven conversation summarization wiring (infrastructure exists, not wired)
- Publishing to Hackage (post-stabilization)
- Bolt protocol native Haskell driver (using HTTP/REST for Neo4j)

## Decisions

### D1: Clean architecture layer boundaries

```
Domain ← UseCase ← Infrastructure
(pure)    (pure)     (all IO)
```

**Choice**: Strict dependency inversion. Domain declares types and pure functions. UseCase composes domain functions into workflows but does NOT define IO. Infrastructure implements all side effects.

**Alternatives considered**:
- *Free monad / effect systems*: More composable but adds complexity, indirection, and GHC extension overhead. Overkill for a CLI tool with known IO points.
- *MTL-style typeclasses*: Flexible but orphan instance risk and hard-to-debug type inference errors at scale.
- *Simple IO everywhere*: Fast to write but untestable domain logic and no layer guarantee.

**Rationale**: Clean architecture gives testable pure domain without mocks, clear module ownership, and compile-time enforcement via import structure. The cost is more files and explicit wiring, but the payoff is reliable reasoning about purity.

### D2: FGL adapter as isolated translation layer

```
Domain Types              FGL Types
┌───────────┐            ┌───────────┐
│ NodeId    │──nidToInt──►│ FGL Int   │
│ (Text)    │  (hash)     │ node ID   │
└───────────┘            └───────────┘
│ Map NodeId Node │◄─fromFGL─│ Gr a b │
│ Map EId   Edge  │─toFGL──►│(Patricia│
└─────────────────┘        │  Tree)  │
                          └───────────┘

FGL module does NOT import Domain.Graph — avoids cyclic deps.
Operates on raw Maps/Sets.
```

**Choice**: Domain works with `Map NodeId Node` / `Map EId Edge`. FGL adapter converts to/from `Gr a b` for algorithmic operations (BFS, DFS, articulation points, dominator trees). Conversion is explicit, not transparent.

**Alternatives considered**:
- *FGL as the primary graph type*: Locks domain to FGL's Int-indexed Patricia Tree. Hard to extend with Text IDs or custom indices.
- *Custom graph type from scratch*: Full control but reimplementing BFS/DFS/articulation points. FGL already provides these correctly.
- *Alga (algebraic graphs)*: Elegant composition but lacks efficient local neighborhood queries needed for Leiden.

**Rationale**: Keeping domain graph as Maps gives flexibility, human-readable IDs, and easy JSON serialization. FGL is used only where its algorithms are needed, with explicit bidirectional conversion.

### D3: Leiden algorithm in pure Haskell

**Choice**: Implement Leiden from scratch in pure Haskell within `Domain.Community`. Three phases: local moving, refinement (what Louvain misses), aggregation. Pure function signature: `leiden :: LabeledGraph -> Resolution -> CommunityMap`.

**Alternatives considered**:
- *Louvain (simpler)*: Faster but produces poorly-connected communities — no refinement phase. Leiden's refinement guarantees well-connected communities (cohesion > 0.5).
- *FFI to C++/Python Leiden*: Faster for very large graphs but breaks purity, adds dependency, complicates Nix build.
- *igraph via FFI*: Battle-tested but C dependency, FFI overhead, and GPL licensing concern.

**Rationale**: Pure Haskell gives testability without mocks, no FFI/Nix complexity, and fast enough for target graphs (< 100k nodes, < 30s). StrictData + unboxed vectors keep performance acceptable.

### D4: LSP extraction with three-tier fallback

```
LSP (primary) → tree-sitter (fallback) → stub (last resort)
```

**Choice**: LSP is always preferred because it provides semantic info (symbols, references, call hierarchy, types). Tree-sitter provides syntax-only extraction when no LSP server exists. Stub creates one node per file when neither is available.

**Alternatives considered**:
- *Tree-sitter only*: No semantic info, no cross-file refs, grammar maintenance burden.
- *LSP only*: Requires users to install LSP servers for every language. Some languages have no LSP.
- *Custom parsers per language*: Maximum control but unsustainable maintenance (30+ parsers).

**Rationale**: LSP gives the richest extraction for zero per-language maintenance. The fallback chain ensures Graphos degrades gracefully.

### D5: MCP server with community 0 chat memory

```
Code communities: 1..N (pure code clusters)
Chat community:   0   (synthetic, added post-Leiden)
Edges: conversation → code (ONE-WAY only)
select_context: EXCLUDES community 0 by default
                 INCLUDES with include_history=true
```

**Choice**: Chat memory lives in community 0 as a synthetic overlay. One-way edges prevent chat from polluting code node degrees or community detection. Chat is opt-in for context selection.

**Alternatives considered**:
- *Separate storage, no graph integration*: Simpler but loses graph-based retrieval of relevant conversations.
- *Bidirectional edges*: Pollutes code node degrees, biases community detection toward chatty modules.
- *Dedicated conversation graph*: Double graph loading, complex merging, no unified query.

**Rationale**: Community 0 provides graph-based retrieval while guaranteeing non-pollution. One-way edges are a simple, enforceable constraint.

### D6: Neo4j auto-selection by graph size

```
nodes < 10k  → FullPush  (all nodes, ~990k statements, 2-4 hours)
nodes >= 10k → SubgraphPush (reps + bridges, ~64k statements, ~30 seconds)
Override:    → CommunityPush (communities only, ~8k statements, ~5 seconds)
```

**Choice**: Auto-select push mode by node count to balance completeness vs time. FullPush is exhaustive but slow for large graphs. SubgraphPush captures structure via representatives. CommunityPush is the fastest overview.

**Alternatives considered**:
- *Always FullPush*: Too slow for 100k+ node graphs (hours).
- *Always CommunityPush*: Loses all node-level detail.
- *User must always choose*: Poor UX for new users.

**Rationale**: Auto-selection with override gives sensible defaults while preserving user control.

### D7: IORef MetricsStore over hs-opentelemetry-sdk metrics

**Choice**: Custom `IORef`-based metrics store for counters, gauges, and histograms. hs-opentelemetry-sdk handles traces only. Metrics exposed via Prometheus `/metrics` HTTP endpoint.

**Alternatives considered**:
- *hs-opentelemetry-sdk for everything*: Metrics API is not yet stable in the Haskell SDK. Risk of breaking changes.
- *EKG + ekg-prometheus*: Mature but adds another dependency. IORef is simpler for our three metric types.
- *No metrics*: Unacceptable for production monitoring and performance debugging.

**Rationale**: hs-opentelemetry-sdk metrics are unstable. IORef gives atomic operations, zero dependencies, and full control. Prometheus exposition format is simple to implement.

### D8: Config resolution cascade

```
Priority (later wins):
  1. Built-in defaults (Domain.Config)
  2. Global config: ~/.config/graphos/graphos.yaml
  3. Project config: <project>/graphos.yaml
  4. CLI flags
```

**Choice**: Four-level cascade with CLI as final authority. Each level overrides the previous.

**Alternatives considered**:
- *CLI flags only*: No persistent configuration. Users must repeat long flag strings.
- *Single config file*: No distinction between global and project settings.
- *Environment variables only*: No structured config for nested settings like LSP mappings.

**Rationale**: Cascade gives sensible defaults, project-specific customization, and CLI override power. Matches conventions from git, curl, and other CLI tools.

## Risks / Trade-offs

| Risk | Mitigation |
|------|-----------|
| LSP server quality varies by language → poor extraction for some languages | Graceful fallback chain (LSP → tree-sitter → stub); report extraction quality per language in `GRAPH_REPORT.md` |
| Leiden stochastic results → different runs may produce different communities | Document this; provide `--resolution` tuning; deterministic mode via fixed seed in future |
| FGL conversion overhead for very large graphs (> 100k nodes) | UsenidToInt hashing for O(1) lookup; batch conversion; measure and optimize if bottleneck exceeds 30s target |
| hs-opentelemetry-sdk API instability | Pin version in cabal; isolate OTel imports behind Infrastructure.Observability wrapper; custom IORef metrics independent of SDK |
| MCP server blocking on single slow tool call | Each tool call is a separate JSON-RPC request; long-running calls should use timeout; STM for concurrent graph access |
| Memory pressure on 1M+ node graphs | StrictData + BangPatterns + NFData; unboxed vectors for numeric arrays; `+RTS -M` for heap limit; lower resolution gamma for large graphs |
| Neo4j FullPush takes hours for large graphs | Auto-select SubgraphPush for ≥ 10k nodes; parallel batch push in future (PRD §18.3) |

## Verification Strategy (Check)

1. **Build verification**: `cabal build` completes with zero warnings (with `--flag dev` enabling `-Werror`)
2. **Test verification**: `cabal test` passes all Hspec unit tests + QuickCheck property tests
3. **Spec scenario verification**: Every `#### Scenario:` in all 8 spec files has a corresponding test case
4. **Performance verification**: Extraction of 100k nodes completes in < 5 minutes; Leiden clustering < 30s; MCP query response < 500ms
5. **Integration verification**: Full pipeline runs end-to-end on a real codebase (this repository) producing valid `graph.json`, `graph.html`, `GRAPH_REPORT.md`
6. **MCP server verification**: All 11 tools respond correctly to JSON-RPC requests over stdio
7. **Neo4j verification**: SubgraphPush produces valid Cypher statements that load correctly in a Neo4j instance
8. **Observability verification**: Traces visible in OTLP collector + Grafana Tempo; metrics scrapeable via Prometheus endpoint

## Iteration & Rollback (Act)

**If Check fails**:

- Build failure → fix warnings/errors, re-run `cabal build`. Do NOT proceed until clean.
- Test failure → analyze failure, fix code or spec discrepancy, re-run `cabal test`. Record finding in task's Attempt history.
- Performance miss → profile with `+RTS -s`, identify bottleneck, consider lowering resolution or adding strictness annotations.
- Integration failure → verify IO wiring between UseCase and Infrastructure. Check LSP server availability.

**Rollback strategy**: Each task is atomic (single PDCA cycle). Failed tasks do NOT advance. The `graphos-out/` directory can be deleted to reset all output. `graph.checkpoint.json` enables resume without re-extraction.

**Standardization**: Successful patterns (e.g., FGL adapter conversion, community 0 convention, IORef metrics) become documented conventions. Failed approaches are recorded in task Attempt history for future reference.

## Migration Plan

This is a greenfield implementation — no migration needed. The module structure is defined in `graphos.cabal` and implementation fills the existing scaffolding.

## Open Questions

1. Should Leiden use a deterministic seed for reproducible community detection across runs? (Currently stochastic.)
2. What is the maximum graph size before FGL conversion becomes a bottleneck? Need profiling data.
3. Should the MCP server support concurrent tool calls via STM, or is sequential processing sufficient initially?
4. Is hs-opentelemetry-sdk stable enough for metrics, or should IORef MetricsStore remain the permanent solution?