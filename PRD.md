# Graphos — Product Requirements Document

**Version**: 1.0  
**Date**: 2026-06-09  
**Status**: Active  

---

## 1. Product Identity

| Field | Value |
|-------|-------|
| **Name** | Graphos |
| **Tagline** | Context graph builder — any input → knowledge graph → clustered communities → HTML + JSON + report |
| **Category** | Developer tooling / AI context optimization |
| **License** | MIT |

### Problem Statement

LLMs waste tokens re-reading entire codebases. Developers lack navigable maps of how their code connects. Every LLM call re-sends the same context because there is no persistent, structured representation of what matters.

### Solution

Graphos uses the Language Server Protocol to extract code structure as a graph, clusters it with community detection, and produces persistent, queryable context that saves tokens per LLM call. The knowledge graph acts as a **context compressor**: instead of sending raw files, send only the relevant nodes, edges, and communities.

---

## 2. System Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          GRAPHOS SYSTEM                                  │
│                                                                          │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │                        INPUT LAYER                                │   │
│  │                                                                  │   │
│  │   Code Files ──────► LSP Servers ──────► AST + Symbols + Refs    │   │
│  │        │                  (auto-detect)        (cross-file)       │   │
│  │        │                                                         │   │
│  │   Docs/Papers ───► LLM / PDF ──► Concepts + Relationships       │   │
│  │        │                                                         │   │
│  │   Images ─────────► LLM Vision ─► Descriptions + Relations     │   │
│  │        │                                                         │   │
│  │   Video/Audio ────► Whisper ────► Transcripts ──► LLM Extract   │   │
│  │        │                                                         │   │
│  │   .json/.nix ─────► LSP / Stub ─► Schema + Config nodes         │   │
│  └────────┼─────────────────────────────────────────────────────────┘   │
│           │                                                              │
│           ▼                                                              │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │                      PROCESSING PIPELINE                          │   │
│  │                                                                  │   │
│  │   detect → extract → build → cluster → infer → analyze → export │   │
│  │                                                                  │   │
│  │   Each stage is a pure function.                                │   │
│  │   No shared state, no side effects outside graphos-out/.         │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│           │                                                              │
│           ▼                                                              │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │                       OUTPUT LAYER                                │   │
│  │                                                                  │   │
│  │   graph.json ────► Persistent, queryable knowledge graph         │   │
│  │   graph.html ────► Interactive vis.js visualization              │   │
│  │   report.md ─────► Plain-language audit report                   │   │
│  │   community_graph.json ► Community-level graph for LLM nav     │   │
│  │   Obsidian vault ─► Knowledge management markdown files         │   │
│  │   Neo4j/Memgraph ► Graph database push (3 modes)                │   │
│  │   SVG/GraphML ───► Static export formats                         │   │
│  │   MCP server ───► AI agent integration (11 tools)              │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## 3. Pipeline Specification

### 3.1 Seven-Stage Pipeline

```
┌──────────┐   ┌──────────┐   ┌──────────┐   ┌──────────┐
│  DETECT  │──►│ EXTRACT  │──►│  BUILD   │──►│ CLUSTER  │
│          │   │          │   │          │   │          │
│ Scan     │   │ LSP/TS/  │   │ Merge    │   │ Leiden   │
│ files by │   │ LLM/Stub │   │ nodes +  │   │ comm.    │
│ ext +    │   │ per lang │   │ edges +  │   │ detect.  │
│ category │   │ (cache)  │   │ dedup    │   │ + merge  │
└──────────┘   └──────────┘   └──────────┘   └────┬─────┘
                                                   │
                                                   ▼
┌──────────┐   ┌──────────┐   ┌──────────┐   ┌──────────┐
│  EXPORT  │◄──│ ANALYZE  │◄──│RE-CLUSTER│◄──│  INFER   │
│          │   │          │   │(enriched)│   │          │
│ 7 formats│   │ God nodes│   │          │   │ Edge     │
│ + Neo4j  │   │ Bridges  │   │          │   │ density  │
│ + MCP    │   │ Surprise │   │          │   │ bridges  │
└──────────┘   └──────────┘   └──────────┘   └──────────┘
```

### 3.2 Stage Definitions

| Stage | Input | Process | Output | Purity |
|-------|-------|---------|--------|--------|
| **Detect** | Filesystem path | Scan directory, categorize by extension, respect .gitignore | `Detection` with file categories and counts | IO (filesystem) |
| **Extract** | Detection + Config | Per-language extraction (LSP, tree-sitter, LLM, stub), SHA256 cache check | `Extraction` with nodes and edges | IO (LSP servers, LLM) |
| **Build** | Extractions + directed flag | Merge all extractions, deduplicate nodes/edges, build adjacency maps | `LabeledGraph` | Pure |
| **Cluster** | Graph + Resolution | Convert to FGL, run Leiden community detection, merge small communities | `CommunityMap` + `CohesionMap` | Pure |
| **Infer** | Graph + EdgeDensity | Add bridge edges, transitive dependencies, shared context based on density setting | Enriched `LabeledGraph` | Pure |
| **Analyze** | Enriched graph + Communities | Compute god nodes, bridge nodes, surprising connections, suggested questions | `Analysis` | Pure |
| **Export** | Graph + Analysis + Config | Generate all output formats (JSON, HTML, report, Obsidian, Neo4j, SVG, GraphML, community graph) | `ExportResult` | IO (filesystem, HTTP) |

### 3.3 Checkpoint Strategy

The pipeline saves intermediate state to `graphos-out/cache/graph.checkpoint.json` after the Build stage. On incremental runs (`--update`), it resumes from checkpoint instead of re-extracting unchanged files. The checkpoint is removed after successful export — `graph.json` is the authoritative artifact.

### 3.4 Incremental Update Flow

When `--update` or `--watch` is active, only changed files are re-extracted:

```
File change detected
  │
  ├─ Detect diff: which files changed since last run?
  │
  ├─ Re-extract changed files only (SHA256 cache hit = skip)
  │
  ├─ Merge new extractions into existing graph
  │
  ├─ Re-cluster entire graph (Leiden is fast enough)
  │
  └─ Incremental export (append to JSON, rebuild HTML)
```

---

## 4. Architecture Specification

### 4.1 Clean Architecture Layers

```
┌──────────────────────────────────────────────────────────┐
│                    DOMAIN (Pure, no IO)                    │
│                                                           │
│  Purpose: Core business logic, zero dependency on       │
│  infrastructure or external libraries (beyond base).     │
│                                                           │
│  Modules:                                                 │
│  ├── Types/     Node, Edge, Graph, Community, Analysis,   │
│  │              Pipeline, Extraction, Ingest, Context    │
│  ├── Graph/     Core, FGL adapter, Query, Diff, Analysis  │
│  ├── Community  Leiden detection, Label, Resolution,     │
│  │              Merge, Cohesion, Representatives          │
│  ├── Analysis   God nodes, surprising connections,       │
│  │              suggested questions                        │
│  ├── Config     GraphosConfig, extractor definitions,   │
│  │              LSP server mappings                       │
│  ├── Context    Context formatting for LLM consumption  │
│  └── Extraction Extraction result validation            │
│                                                           │
│  Hard rule: NO IO, NO imports from Infrastructure,       │
│  NO side effects. Pure functions only.                    │
└──────────────────────────┬───────────────────────────────┘
                           │ depends on
                           ▼
┌──────────────────────────────────────────────────────────┐
│                 USE CASE (Pure orchestration)              │
│                                                           │
│  Purpose: Compose domain functions into workflows.       │
│  No implementation details — delegates IO to Infra.      │
│                                                           │
│  Modules:                                                 │
│  ├── Pipeline    Full 7-stage pipeline orchestration     │
│  ├── Detect      File detection, categorization          │
│  ├── Extract     Orchestrate LSP/TS/LLM/Stub extractors  │
│  ├── Build       Graph construction from extractions    │
│  ├── Cluster     Leiden clustering with resolution       │
│  ├── Infer       Edge density inference                  │
│  ├── Analyze     Analysis orchestration                  │
│  ├── Report      Report generation                       │
│  ├── Export      Export orchestration (7+ formats)        │
│  ├── Query       Graph querying (BFS/DFS + token budget)│
│  ├── Load        Load graph.json from disk               │
│  ├── Ingest      Multi-format file ingestion            │
│  └── SelectContext Context selection for LLM queries     │
│                                                           │
│  Hard rule: No IO implementation. Calls Infrastructure    │
│  functions but does not define them.                      │
└──────────────────────────┬───────────────────────────────┘
                           │ depends on
                           ▼
┌──────────────────────────────────────────────────────────┐
│              INFRASTRUCTURE (IO boundary)                   │
│                                                           │
│  Purpose: All side effects. LSP, filesystem, network,    │
│  export formats, servers, logging, observability.        │
│                                                           │
│  Modules:                                                 │
│  ├── LSP/       Client, Protocol, Transport, Extraction, │
│  │              ServerMap (30+ languages), Capabilities   │
│  ├── Extract/   Tree-sitter CLI integration              │
│  ├── FileSystem/ Cache, Manifest, Watcher, Ignore,      │
│  │              Sensitive, OfficeConvert, Conversation    │
│  ├── Export/    JSON, HTML, Report, Obsidian, Neo4j,    │
│  │              SVG, GraphML, CommunityGraph               │
│  ├── Server/    MCP (stdio protocol, 11 tools),          │
│  │              Static HTTP server                       │
│  ├── Logging    Leveled logging + OTLP log bridge        │
│  ├── Observability/  hs-opentelemetry-sdk wrapper        │
│  ├── Config     Load graphos.yaml, merge with defaults   │
│  └── LLM/      OpenAI-compatible client, Embeddings     │
│                                                           │
│  Hard rule: Implements interfaces defined by Domain.      │
│  All IO happens here and nowhere else.                    │
└──────────────────────────────────────────────────────────┘
```

### 4.2 Key Architectural Principles

| Principle | Meaning | Why |
|-----------|---------|-----|
| **Dependencies point inward** | Domain ← UseCase ← Infrastructure | Domain is stable; Infrastructure varies |
| **Domain is pure** | No IO, no external libraries beyond base/aeson/containers | Testable without mocks |
| **LSP is an adapter** | Domain receives extraction results, not LSP calls | Swap extraction without changing domain |
| **Standard output format** | `graph.json` is the interchange format | Interoperability with all tools |
| **Config overrides cascade** | Defaults → global YAML → project YAML → CLI flags | Flexible without complexity |
| **Checkpoint-resume** | Save state after Build, resume on incremental | Fast re-runs on large codebases |

### 4.3 FGL Adapter Pattern

```
┌──────────────────────────────────────────────────────┐
│               FGL ADAPTER LAYER                       │
│                                                       │
│  Graphos Domain Types          FGL Types              │
│  ┌───────────┐                ┌───────────┐          │
│  │ NodeId    │──nidToInt───►  │ FGL Int   │          │
│  │ (Text)    │   (hash)       │ node ID   │          │
│  └───────────┘                └───────────┘          │
│  ┌───────────┐                ┌───────────┐          │
│  │ Map       │──toFGL──────►  │ Gr a b   │          │
│  │ NodeId    │                │ (Patricia │          │
│  │ Node      │◄──fromFGL──── │  Tree)    │          │
│  │ Map EId   │                └───────────┘          │
│  │ Edge      │                                        │
│  └───────────┘                Used for:              │
│                               ├── BFS / DFS queries  │
│  IMPORTANT: FGL module does   ├── Articulation points │
│  NOT import Domain.Graph     ├── Biconnected comp.   │
│  (avoids cyclic deps).       └── Dominator trees     │
│  Operates on raw Maps/Sets.                          │
└──────────────────────────────────────────────────────┘
```

---

## 5. Community Detection Specification

### 5.1 Leiden Algorithm Implementation

```
┌──────────────────────────────────────────────────────────────┐
│              LEIDEN ALGORITHM (Pure Haskell)                   │
│                                                               │
│  Phase 1: LOCAL MOVING                                        │
│  ┌─────────────────────────────────────────────────────┐     │
│  │ For each node:                                      │     │
│  │   Compute modularity gain for moving to each         │     │
│  │   neighboring community                              │     │
│  │   Move to best community if gain > 0                │     │
│  │ Repeat until no node wants to move (stable)         │     │
│  └─────────────────────────────────────────────────────┘     │
│                        │                                      │
│  Phase 2: REFINEMENT (what Louvain misses — key fix)          │
│  ┌─────────────────────────────────────────────────────┐     │
│  │ For each community:                                 │     │
│  │   Check if internal subsets should split off       │     │
│  │   Only merge if cohesion > 0.5                      │     │
│  │   Guarantees well-connected communities             │     │
│  └─────────────────────────────────────────────────────┘     │
│                        │                                      │
│  Phase 3: AGGREGATION                                        │
│  ┌─────────────────────────────────────────────────────┐     │
│  │ Merge communities into supernodes                   │     │
│  │ Build coarser graph                                 │     │
│  │ Return to Phase 1 on coarser graph                  │     │
│  └─────────────────────────────────────────────────────┘     │
│                        │                                      │
│  Loop until: assignment stabilizes OR max iterations hit     │
└──────────────────────────────────────────────────────────────┘
```

### 5.2 Resolution Configuration

| Parameter | Type | Default | Purpose |
|-----------|------|---------|---------|
| `resGamma` | Double | 1.0 | Resolution parameter (higher = smaller communities) |
| `resMinSize` | Int | 3 | Minimum community size; smaller get merged |
| `resMergeInto` | MergeStrategy | MergeToNeighbor | How small communities merge |
| `resMaxIterations` | Int | 50 | Max Leiden iterations before stopping |

### 5.3 Tuning by Graph Size

| Graph Size | gamma | minSize | maxIterations | Rationale |
|-----------|-------|---------|---------------|-----------|
| < 1k nodes | 1.0 | 3 | 50 | Small graph, default works |
| 1k–10k | 0.8 | 5 | 30 | Moderate, slightly more relaxed |
| 10k–100k | 0.5 | 10 | 20 | Large, keep communities meaningful |
| 100k+ | 0.3–0.5 | 10–20 | 10–20 | Very large, prioritize speed |

### 5.4 Cohesion Scoring

Cohesion measures how internally connected a community is:

```
cohesion(c) = Σ_{i∈c} (neighbors_in_c(i) / total_neighbors(i)) / |c|
```

Range [0,1]. Higher values indicate more internally cohesive communities. Used for:
- Selecting representative nodes (highest cohesion)
- Quality filtering (flag low-cohesion communities)
- Sub-graph push mode to Neo4j

---

## 6. LSP Integration Specification

### 6.1 Auto-Detection Flow

```
┌───────────────────────────────────────────────────────────┐
│             LSP SERVER AUTO-DETECTION                      │
│                                                            │
│  graphos lservers                                         │
│       │                                                    │
│       ├── Scan PATH for known LSP server commands         │
│       │   (30+ language → server mappings in ServerMap)    │
│       │                                                    │
│       ├── For each found server:                           │
│       │   ├── Verify executable exists                    │
│       │   ├── Check capabilities (documentSymbol, refs,   │
│       │   │   workspaceSymbol, callHierarchy)              │
│       │   └── Report what each server supports            │
│       │                                                    │
│       └── Output: table of language, server, capabilities │
│                                                            │
│  During extraction:                                       │
│       ├── Group files by LSP server                       │
│       ├── Spawn ONE server process per language           │
│       │   (shared across all files of that language)       │
│       ├── JSON-RPC handshake (initialize → initialized)   │
│       ├── For each file:                                  │
│       │   ├── textDocument/didOpen                        │
│       │   ├── textDocument/documentSymbol                 │
│       │   ├── workspace/symbol (if supported)            │
│       │   ├── textDocument/references (if supported)      │
│       │   └── callHierarchy/incomingCalls (if supported) │
│       ├── shutdown + exit                                 │
│       └── Clean up process                               │
└───────────────────────────────────────────────────────────┘
```

### 6.2 LSP vs Tree-Sitter Decision

| Aspect | tree-sitter | LSP (Graphos choice) |
|--------|-------------|----------------------|
| Language support | 25 hardcoded grammars | Any language with LSP server |
| New language | Add grammar + recompile | Install LSP server |
| Semantic info | Syntax only (AST) | Symbols, references, call hierarchy, types |
| Cross-file refs | Second-pass inference | Native via LSP methods |
| Hover/docs | Not available | Available |
| Maintenance | Per-grammar, by Graphos | Zero — by language teams |
| Offline | Works without server | Requires LSP server installed |
| Speed | Fast (local parse) | Slower (process spawn + IPC) |

**Decision**: LSP is the primary extraction method. Tree-sitter is a fallback when no LSP server is available. Stub extraction (one node per file) is the last resort.

### 6.3 Supported Language Servers

Graphos ships default mappings for 30+ languages. Users override in `graphos.yaml`:

```yaml
lsp:
  ".hs":
    command: haskell-language-server
    args: ["--lsp"]
    language_id: haskell
  ".py":
    command: pyright-langserver
    args: ["--stdio"]
    language_id: python
  # Set command: "" to explicitly disable LSP for an extension
  ".nix":
    command: ""
    language_id: nix
```

---

## 7. Context Selection Specification

### 7.1 Two-Tier LLM Pipeline

```
┌──────────────────────────────────────────────────────────────┐
│               CONTEXT SELECTION PIPELINE                      │
│                                                               │
│  User Query                                                   │
│      │                                                        │
│      ▼                                                        │
│  ┌─────────────────────────┐                                 │
│  │  COMPLEXITY CLASSIFIER  │                                 │
│  │  Classifies query type: │                                 │
│  │  Focused / Module /     │                                 │
│  │  CrossModule /          │                                 │
│  │  Architectural /        │                                 │
│  │  Exploratory            │                                 │
│  └──────────┬──────────────┘                                 │
│             │                                                  │
│             ▼                                                  │
│  ┌─────────────────────────┐                                 │
│  │  STRATEGY SELECTOR     │                                  │
│  │                         │                                 │
│  │  Focused    → Community-aware (match node → community)  │
│  │  Module     → Community-aware + bridge nodes             │
│  │  CrossMod   → Path-based (shortest path + neighbors)   │
│  │  Architectural → God nodes + bridges + structure        │
│  │  Exploratory → Relevance-weighted BFS                   │
│  └──────────┬──────────────┘                                 │
│             │                                                  │
│             ▼                                                  │
│  ┌─────────────────────────┐                                 │
│  │  BUDGET ALLOCATOR      │                                  │
│  │                         │                                 │
│  │  Assigns token budget  │                                 │
│  │  based on complexity   │                                 │
│  │  (higher = more graph  │                                 │
│  │  context included)     │                                 │
│  └──────────┬──────────────┘                                 │
│             │                                                  │
│             ▼                                                  │
│  ┌─────────────────────────┐                                 │
│  │  COMPACT MARKDOWN      │                                  │
│  │  FORMATTER             │                                  │
│  │                         │                                 │
│  │  Nodes: id, kind,      │                                 │
│  │    signature, location │                                 │
│  │  Edges: from → to,    │                                 │
│  │    relation, weight   │                                 │
│  │  Communities: label,  │                                 │
│  │    stats, bridges     │                                 │
│  └─────────────────────────┘                                 │
│                                                                │
└──────────────────────────────────────────────────────────────┘
```

### 7.2 Token Budget Allocation by Complexity

| Query Type | Graph Context | Source Code | Headroom |
|-----------|--------------|-------------|----------|
| Focused (single function) | 500 tokens | 2000 tokens | 75% |
| Module-level (one community) | 1500 tokens | 4000 tokens | 55% |
| Cross-module (path query) | 2500 tokens | 3000 tokens | 55% |
| Architectural (overview) | 3000 tokens | 1000 tokens | 70% |
| Exploratory (broad) | 2000 tokens | 2000 tokens | 65% |

### 7.3 Context Output Format

Each selected context node includes metadata that allows LLMs to reason without reading source files:

| Metadata | Token Cost | Value | Use Case |
|----------|-----------|-------|----------|
| `kind` | +1 | High | "Function or type?" — changes reasoning |
| `line_start` + `line_end` | +3 | High | Enables `read_file("Auth.hs", 42, 58)` |
| `signature` | +5–10 | High | Type signature — understand without reading |
| `community_id` | +1 | Medium | "This belongs to Parser community" |
| `degree` | +1 | Medium | High degree = important hub |
| `is_bridge` | +1 | Medium | Connects communities = cross-cutting concern |

---

## 8. MCP Server Specification

### 8.1 Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                   MCP SERVER (stdio)                           │
│                                                                │
│  Transport: stdin/stdout JSON-RPC 2.0, one object per line   │
│  Protocol version: 2024-11-05                                 │
│                                                                │
│  Startup:                                                      │
│  1. Load graph.json (nodes, edges, communities, cohesion)   │
│  2. Load chat history from graphos-out/memory/               │
│  3. Enrich community map with chat history (community 0)      │
│  4. Run analysis (god nodes, bridges, suggestions)            │
│  5. Enter JSON-RPC request loop                               │
│                                                                │
│  Shutdown:                                                     │
│  - Flush pending conversations                                 │
│  - No explicit shutdown message required                      │
└────────────────────────────────────────────────────────────────┘
```

### 8.2 Tool Inventory (11 Tools)

| # | Tool | Purpose | Key Params |
|---|------|---------|------------|
| 1 | `query_graph` | BFS/DFS graph traversal | question, mode, budget |
| 2 | `get_node` | Node details by ID | node_id |
| 3 | `get_neighbors` | All neighbors of a node | node_id |
| 4 | `get_community` | Community membership | node_id |
| 5 | `god_nodes` | Highest-degree hub nodes | top_n |
| 6 | `graph_stats` | Graph statistics | (none) |
| 7 | `shortest_path` | Path between two nodes | from, to |
| 8 | `bridge_nodes` | Articulation points | (none) |
| 9 | `select_context` | LLM context selection | question, budget, include_history, verbose |
| 10 | `add_conversation` | Persist exchange memory | question, answer_summary, source_nodes |
| 11 | `conversation_history` | Search past exchanges | query, limit |

### 8.3 Chat History Architecture

```
┌──────────────────────────────────────────────────────────────┐
│           CONVERSATION MEMORY DESIGN                          │
│                                                               │
│  During Leiden:     Communities 1..N (pure code clusters)   │
│  After Leiden:      Community 0 = synthetic chat community  │
│                                                               │
│  Edges are ONE-WAY:                                          │
│  conversation → code node  (chat references code)            │
│  code ─/→ conversation     (degree stays pure)               │
│                                                               │
│  select_context behavior:                                    │
│    Default: EXCLUDES community 0 (no pollution)              │
│    include_history=true: INCLUDES community 0                │
│                                                               │
│  Storage: graphos-out/memory/conv_*.md                       │
│  Format: YAML frontmatter + markdown body                   │
│                                                               │
│  This means:                                                  │
│  - Code node degrees are NEVER affected by conversations    │
│  - Community detection is NEVER influenced by chat history   │
│  - Chat memory is optional and non-polluting                 │
└──────────────────────────────────────────────────────────────┘
```

---

## 9. Neo4j Integration Specification

### 9.1 Three Push Modes

```
┌───────────────────────────────────────────────────────────┐
│               NEO4J PUSH MODES                             │
│                                                            │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐  │
│  │  FullPush  │  │SubgraphPush │  │ CommunityPush   │  │
│  │             │  │ (default    │  │                 │  │
│  │  All nodes │  │  for >10k)  │  │ Communities + │  │
│  │  All edges │  │             │  │ inter-comm     │  │
│  │  + comm.   │  │ Community + │  │ edges only     │  │
│  │             │  │ reps +      │  │                 │  │
│  │ ~990k stmt │  │ bridges     │  │ ~8k stmt        │  │
│  │ 2-4 hours  │  │ ~64k stmt   │  │ ~5 sec          │  │
│  │             │  │ ~30 sec     │  │                 │  │
│  └─────────────┘  └─────────────┘  └─────────────────┘  │
│                                                            │
│  Auto-selection:                                           │
│    nodes < 10k  → FullPush                                 │
│    nodes >= 10k → SubgraphPush                             │
│                                                            │
│  Override: --neo4j-push-mode full|subgraph|community        │
└───────────────────────────────────────────────────────────┘
```

### 9.2 Representative Node Selection

For SubgraphPush mode, each community selects its structurally important nodes:

| Criterion | What It Captures | Example |
|-----------|-------------------|---------|
| **Centroid** (highest degree) | Main concept of community | `parseConfig` in Config community |
| **Top-N by degree** | Most-referenced functions/types | `loadYAML`, `validateSettings` |
| **Bridge nodes** (articulation points) | Cross-community connectors | `defaultConfig` used by Config + Pipeline |

Default: 7 representatives per community (`--neo4j-subgraph-size 7`).

---

## 10. Observability Specification

### 10.1 Observability Stack

```
┌──────────────────────────────────────────────────────────────────┐
│                   OBSERVABILITY STACK                              │
│                                                                    │
│  ┌──────────────────────────────────────────────────────────────┐ │
│  │  APPLICATION                                                │ │
│  │                                                              │ │
│  │  TRACES     METRICS        LOGS         DEBUG TRACE         │ │
│  │  hs-otel    IORef store   OTLP bridge  JSONL local         │ │
│  │  SDK        + Prometheus   → Collector   (graphos-out/)    │ │
│  │  inSpan     /metrics       → Loki       debug-trace/      │ │
│  │             endpoint                                   │ │
│  └──────────┬──────────┬──────────┬───────────────────────────┘ │
│             │          │          │                               │
│  ┌──────────▼──────────▼──────────▼───────────────────────────┐ │
│  │  OTLP COLLECTOR (Docker)                                   │ │
│  │  HTTP: 4318  gRPC: 4317                                   │ │
│  │  Processors: batch (5s, 1024 batch), transform             │ │
│  │                                                              │ │
│  │  Exporters:                                                  │ │
│  │    Traces  → Tempo (distributed trace backend)              │ │
│  │    Metrics → Prometheus (pull from :8889)                  │ │
│  │    Logs    → Loki (push to :3100)                           │ │
│  └──────────────────────────────────────────────────────────────┘ │
│             │          │          │                               │
│  ┌──────────▼──────────▼──────────▼───────────────────────────┐ │
│  │  GRAFANA                                                    │ │
│  │  Dashboards: pipeline health, trace exploration,           │ │
│  │              logs, metrics                                  │ │
│  └──────────────────────────────────────────────────────────────┘ │
│                                                                    │
└──────────────────────────────────────────────────────────────────────┘
```

### 10.2 OpenTelemetry Configuration

| Env Var | Default | Purpose |
|--------|---------|---------|
| `OTEL_EXPORTER_OTLP_ENDPOINT` | `http://localhost:4318` | OTLP collector endpoint |
| `OTEL_EXPORTER_OTLP_HEADERS` | (none) | Auth headers for vendor endpoints |
| `OTEL_SERVICE_NAME` | `graphos` | Service identity |
| `OTEL_RESOURCE_ATTRIBUTES` | (none) | Resource metadata |
| `OTEL_BSP_SCHEDULE_DELAY` | 5000ms | Batch export interval |
| `OTEL_SDK_DISABLED` | false | Kill switch for all telemetry |

CLI flags override env vars: `--otel` enables, `--otel-endpoint` overrides, `--metrics PORT` exposes Prometheus endpoint.

### 10.3 Metrics Store

The custom `MetricsStore` uses atomic `IORef` operations with three metric types:

| Type | Operations | Prometheus Format |
|------|-----------|------------------|
| Counter | `incCounter`, `decCounter` | `# TYPE name counter` |
| Gauge | `setGauge` | `# TYPE name gauge` |
| Histogram | `observeHistogram` | `# TYPE name histogram` with bucket boundaries |

### 10.4 Log-to-Trace Correlation

Logs shipped via OTLP include a `trace_id` attribute when a span is active, enabling click-through from log line to trace in Grafana. The correlation is set via `setLogTraceContext` in the Logging module.

---

## 11. Multi-Format Input Specification

### 11.1 Supported File Types

| Type | Extensions | Extraction Method |
|------|-----------|-------------------|
| Code | 30+ extensions (hs, py, ts, js, go, rs, java, c, cpp, etc.) | LSP → AST + call graph + references **or** tree-sitter fallback **or** stub |
| Docs | .md, .txt, .rst, .adoc, .org | LLM → concepts + relationships + rationale |
| Papers | .pdf | Citation mining + concept extraction |
| Office | .docx, .xlsx | Convert to markdown → LLM extraction |
| Images | .png, .jpg, .webp, .gif | LLM vision → descriptions + relations |
| Video/Audio | .mp4, .mp3, .wav, etc. | Whisper transcription → LLM extraction |

### 11.2 SHA256 Cache

Every file's SHA256 hash is computed on first extraction and stored in `graphos-out/cache/`. On re-runs, only files with changed hashes are re-extracted. This makes incremental updates fast regardless of codebase size.

---

## 12. Export Formats Specification

| Format | Trigger | Output | Use Case |
|--------|---------|--------|----------|
| `graph.json` | Always | Full node/edge/community data | Persistent query source, MCP input |
| `graph.html` | Default (skip with `--no-viz`) | Interactive vis.js + sidebar | Code exploration, community browsing |
| `GRAPH_REPORT.md` | Always | Audit report with stats | Human-readable overview |
| `community_graph.json` | `--community-graph` | Community-level graph only | LLM navigation optimization |
| Obsidian vault | `--obsidian` | Markdown files with wiki-links | Knowledge management |
| Neo4j Cypher | `--neo4j` | 3 push modes (full/subgraph/community) | Graph database exploration |
| Memgraph | `--memgraph` | Bolt protocol push | In-memory graph DB |
| SVG | `--svg` | Static graph visualization | Presentations, documentation |
| GraphML | `--graphml` | XML graph format | Gephi/yEd analysis |

### 12.1 graph.html Viewer Subsection

`graph.html` is a self-contained interactive viewer: all graph data, viewer JavaScript,
stylesheet and the vendored vis-network 10.1.1 renderer are embedded inline. It opens from
`file://` with zero network requests.

**Architecture**: The payload is an interned, style-free view model. Node ids, source files,
kinds and relations are stored once in string tables; nodes and edges reference them by integer
index. No per-node `color`/`group`/`title` and no per-edge `color`/`arrows`/`dashes`/`width`.
Styling is applied once from community palette and relation definitions.

**Depth levels**: `Overview` (one dot per community, default) → `Community` (one community
expanded) → `Full` (all nodes, explicit) → `Custom` (N-hop BFS around a selected node, N 1–6).
Depth, selection and facet state persist in `sessionStorage` under 4 KB.

**Size budget** (authoritative, asserted by test suite):

| Metric | Budget |
|---|---|
| Total `graph.html` (104K nodes / 122K edges) | ≤ 30 MB |
| Payload per node | ≤ 200 B |
| Payload per edge | ≤ 24 B |
| Network requests on `file://` | 0 |

**Measured on Graphos self-graph** (2026-08-12, after payload interning — task 2):
135.4 B/node, 15.3 B/edge, 2,771,213 B total. Reference corpus (104K/122K): not measured
in this environment; budget enforced by automated test.

**Interaction latency targets** (measured, not assumed; see `html-lod-viewer` spec):
overview load < 3 s, drill-down < 500 ms, pan/zoom > 30 fps. Browser-only; not verified
headlessly.

---

## 13. CLI Reference Specification

### 13.1 Commands

| Command | Purpose |
|---------|---------|
| `graphos <path>` | Full pipeline on directory |
| `graphos query <q>` | Query knowledge graph (BFS/DFS) |
| `graphos path <from> <to>` | Shortest path between nodes |
| `graphos explain <node>` | Show node + all connections |
| `graphos lservers` | Discover available LSP servers |
| `graphos serve --dir <d>` | HTTP server for HTML viz |
| `graphos init` | Generate graphos.yaml config |
| `graphos --mcp <graph.json>` | Start MCP server |
| `graphos merge <a> <b> -o <dir>` | Merge two graphs |

### 13.2 Key Flags

| Flag | Default | Purpose |
|------|---------|---------|
| `--directed` | undirected | Preserve edge direction |
| `--no-viz` | viz on | Skip HTML visualization |
| `--update` | full | Incremental: only changed files |
| `--watch` | one-shot | Continuous file watching + re-pipeline |
| `--resolution N` | 1.0 | Leiden gamma parameter |
| `--min-comm-size N` | 3 | Minimum community size |
| `--max-leiden-iterations N` | 50 | Max Leiden iterations |
| `--community-graph` | off | Export community-level graph |
| `--obsidian` | off | Export Obsidian vault |
| `--neo4j` | off | Enable Neo4j push |
| `--neo4j-push-mode` | auto | Neo4j: full/subgraph/community |
| `--neo4j-subgraph-size N` | 7 | Representatives per community |
| `--otel` | off | Enable OpenTelemetry |
| `--otel-endpoint <url>` | env var | Override OTLP endpoint |
| `--metrics <port>` | off | Prometheus metrics server port |
| `--label` | off | LLM-based community labeling |
| `--dfs` | bfs | Use DFS traversal for queries |
| `--budget N` | 2000 | Token budget for query results |

---

## 14. Configuration Specification

### 14.1 Config Resolution Order

```
Priority (later wins):
  1. Built-in defaults (Domain.Config)
  2. Global config: ~/.config/graphos/graphos.yaml
  3. Project config: <project>/graphos.yaml
  4. CLI flags (--otel, --metrics, --resolution, etc.)
```

### 14.2 Config Sections

| Section | Controls |
|---------|----------|
| `lsp` | File extension → LSP server command + args + language ID |
| `language_ids` | Override language IDs for file extensions |
| `file_extensions` | Categorize extensions as code/doc/paper/image/video |
| `observability` | OTel enabled, endpoint, metrics port, service name |
| `neo4j` | URI, auth, push mode, subgraph size |
| `memgraph` | URI, auth, push mode, subgraph size |

---

## 15. Quality & Testing

### 15.1 Build & Test Commands

| Command | Purpose |
|---------|---------|
| `nix-shell shell.nix` | Enter correct GHC 9.10 environment |
| `cabal build` | Build with -Wall -Wcompat -Werror |
| `cabal test` | Run Hspec + QuickCheck tests |
| `cabal repl` | REPL with project loaded |

### 15.2 Compilation Standards

| Setting | Value | Rationale |
|---------|-------|-----------|
| `-Wall` | All warnings | Catch issues early |
| `-Wcompat` | Future-proofing | Prepare for GHC changes |
| `-Wincomplete-uni-patterns` | Exhaustiveness | Prevent runtime pattern match failures |
| `-Werror` | Warnings as errors | Zero tolerance for warnings |
| `-threaded` | Runtime threading | Multi-core execution |
| `-rtsopts` | RTS options | Runtime configuration |
| `-with-rtsopts=-N` | Auto-core count | Use all available cores |

### 15.3 Pure Testing Strategy

All domain logic is pure — testable without mocks, IO, or external services:

- **Domain**: Hspec unit tests + QuickCheck property tests
- **UseCase**: Orchestration tests with stubbed infrastructure
- **Infrastructure**: Integration tests with real LSP servers, filesystem

---

## 16. Non-Functional Requirements

### 16.1 Performance

| Scenario | Target | Strategy |
|----------|--------|----------|
| 100k node extraction | < 5 minutes | Parallel per language, SHA256 cache |
| Leiden clustering (100k nodes) | < 30 seconds | Unboxed vectors, StrictData, low resolution |
| Query response (MCP) | < 500ms | BFS/DFS on pre-built index, O(log N) lookup |
| Incremental update (100 files changed) | < 30 seconds | Only re-extract changed files |
| Neo4j SubgraphPush (100k nodes) | < 30 seconds | Representative-only push, ~64k statements |

### 16.2 Scalability

| Concern | Approach |
|---------|----------|
| Large codebases (1M+ nodes) | Lower resolution gamma, larger min community size, fewer Leiden iterations |
| Memory usage | StrictData, BangPatterns, NFData for deep evaluation, unboxed vectors for numeric arrays |
| Disk footprint | Incremental JSON export, cache reuse, optional format selection |
| Concurrent extraction | cfgThreads parallel extraction, shared LSP servers per language |

### 16.3 Reliability

| Concern | Mitigation |
|---------|-----------|
| LSP server crash | Graceful error handling, fallback to stub extraction |
| OTLP Collector unreachable | Best-effort log shipping, stderr warnings on failure |
| Graph mutation errors | Pure functions return Either/Result types, no runtime exceptions |
| Checkpoint corruption | JSON-based, human-readable, reconstructable from source |
| Community detection instability | Louvain/Leiden are stochastic — document this, provide tuning knobs |

---

## 17. OpenSpec Workflow Specification

### 17.1 PDCA Schema

The project uses the **PDCA** (Plan-Do-Check-Act) OpenSpec schema for spec-driven development:

```
┌──────────────────────────────────────────────────────────┐
│                  PDCA ARTIFACT FLOW                        │
│                                                           │
│  ┌──────────┐                                            │
│  │ proposal │ ← root (no dependencies)                    │
│  └────┬─────┘                                            │
│       ├────────────────────┐                              │
│       ▼                    ▼                              │
│  ┌──────────┐         ┌──────────┐                       │
│  │  specs   │         │  design  │                       │
│  └────┬─────┘         └──────────┘                       │
│       └──────┬───────────┘                               │
│              ▼                                            │
│         ┌──────────┐                                      │
│         │  tasks   │ ← PDCA per task (N.P→N.D→N.C→N.A) │
│         └────┬─────┘                                      │
│       ┌──────┼───────┐───────┐                            │
│       ▼      ▼       ▼       ▼                            │
│  ┌────────┐ ┌────────┐ ┌────────┐ ┌────────┐            │
│  │  plan  │ │   do   │ │ check  │ │  act   │            │
│  │**/plan  │ │ **/do  │ │**/check│ │ **/act │            │
│  └────────┘ └────────┘ └────────┘ └────────┘            │
│                                                           │
│  Apply Phase:                                             │
│    requires: tasks                                        │
│    tracks: tasks.md                                       │
│    For each task: P→D→C→A loop until PASS                 │
│                                                           │
│  Key rule: Plan writes Check criteria BEFORE Do.          │
│  Check executes Plan's criteria — never invents new ones. │
└──────────────────────────────────────────────────────────┘
```

### 17.2 PDCA Per Task Discipline

Every task follows its own micro PDCA cycle:

```
  ┌──────────────┐     ┌─────────┐     ┌──────────────┐     ┌──────────┐
  │     Plan     │────►│   Do   │────►│    Check     │────►│   Act    │
  │              │     │         │     │              │     │          │
  │ Check        │     │ Implement│    │ Execute Check │    │ Pass:    │
  │ criteria     │     │         │    │ criteria     │    │ standard │
  │ defined here │     │         │    │ from Plan    │    │          │
  │ (before code)│     │         │    │ (not invented)│   │ Fail:    │
  │              │     │         │     │              │     │ retry    │
  └──────────────┘     └─────────┘     └──────────────┘     └──────────┘
```

A task PASSES only when Check passes AND Act is OK. If Act is NOT OK, the trace is kept and a new attempt begins.

---

## 18. Future Directions

### 18.1 Memory Agent Evolution

Graphos is approximately 70% of a full memory agent. The key gaps:

| Gap | Status | Priority |
|-----|--------|----------|
| Real-time graph mutation via MCP | Missing | Critical |
| Semantic search (embeddings) | Partial (substring only) | High |
| LLM-driven conversation summarization | Infrastructure exists, not wired | High |
| Temporal relevance (time decay) | Missing | Medium |
| Incremental graph updates via MCP | Missing | Medium |

### 18.2 Vision: Graph as Persistent AI Memory

```
┌──────────┐     ┌──────────┐     ┌──────────┐
│  Small LLM │────►│  Graphos  │◄────│  Big LLM  │
│  (local)   │     │  (graph)  │     │  (cloud)  │
└──────────┘     └────┬─────┘     └──────────┘
                       │
                  ┌────▼─────┐
                  │  Codebase │
                  │  + History│
                  │  + Chats  │
                  └──────────┘

The graph becomes the agent's long-term memory.
Every exchange, decision, insight — persisted as nodes and edges.
Small LLM navigates memory to build minimal, high-signal context.
Big LLM thinks deeply with exactly the right context.
```

### 18.3 Planned Enhancements

| Enhancement | Description | Phase |
|------------|-------------|-------|
| Mutable graph via MCP | TVar-based graph in MCP server | Next |
| Embedding-based search | Vector similarity for semantic matching | Next |
| Parallel batch push | 8x Neo4j push speedup | Soon |
| Bolt protocol | Native Haskell Neo4j driver | Future |
| Incremental community update | Update communities for changed nodes only | Future |
| LLM community labeling | Human-readable names via LLM | Available (`--label`) |
| Adaptive context learning | Track which selections lead to good responses | Future |

---

*This PRD synthesizes all existing project documentation into a single authoritative reference. For implementation details, refer to source modules, context stores, and individual design documents.*