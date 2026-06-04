# Graphos — Flow Diagram

> Updated: 2026-04-24
>
> End-to-end data flow through the Graphos pipeline, secondary flows, and architecture layers.

---

## CLI Entry Point

```
┌─────────────────────────────────────────────────────────────────────────┐
│                            CLI Entry Point                             │
│                           app/Main.hs                                  │
│                                                                        │
│  graphos <path>          → Run Pipeline (full extraction → graph)      │
│  graphos query <q>       → Query existing graph.json                  │
│  graphos path <a> <b>    → Shortest path between nodes                │
│  graphos explain <node>  → Explain a node + connections                │
│  graphos lservers        → Discover available LSP servers              │
│  graphos serve           → HTTP static server for HTML viz             │
│  graphos init            → Generate graphos.yaml config                │
└───────────────────────────┬─────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                     CONFIGURATION LOADING                              │
│                   Infrastructure/Config.hs                             │
│                                                                        │
│  graphos.yaml ──► merge with CLI defaults ──► PipelineConfig          │
│  (LSP servers, extractors, language IDs, file extensions)              │
└───────────────────────────┬─────────────────────────────────────────────┘
                            │
                            ▼
```

---

## Main Pipeline (7 Steps)

```
╔════════════════════════════════════════════════════════════════════════╗
║                    MAIN PIPELINE (7 Steps)                            ║
║                  UseCase/Pipeline.hs                                  ║
║  detect → extract → build → cluster → infer → analyze → export       ║
╚════════════════════════════════════════════════════════════════════════╝
```

### Step 1 — Detect

```
┌──────────────────────────────────────────────────────────────────────┐
│ STEP 1: DETECT                                                       │
│ UseCase/Detect.hs + UseCase/Detect/Paper.hs                         │
│                                                                      │
│  Input: Filesystem path                                              │
│  Process:                                                            │
│    ├── Scan directory for files by extension                         │
│    ├── Categorize: CodeFiles | DocFiles | PaperFiles                │
│    │                 ImageFiles | VideoFiles                         │
│    └── Load .gitignore + sensitive file exclusion                    │
│  Output: Detection { fileCategories, totalFiles, warnings }          │
└──────────────────────────┬───────────────────────────────────────────┘
                           │
                           ▼
```

### Step 2 — Extract

```
┌──────────────────────────────────────────────────────────────────────┐
│ STEP 2: EXTRACT                                                      │
│ UseCase/Extract.hs (orchestrator)                                   │
│                                                                      │
│  Input: Detection + PipelineConfig                                  │
│  Process (parallel per language, cfgThreads):                        │
│    │                                                                 │
│    ├── Code Files ─┬─► LSP Extraction   (Infra/LSP/*.hs)            │
│    │               │   ┌──────────────────────────────────┐          │
│    │               │   │ Spawn LSP server process          │          │
│    │               │   │ Initialize (JSON-RPC handshake)   │          │
│    │               │   │ textDocument/documentSymbol       │          │
│    │               │   │ textDocument/references           │          │
│    │               │   │ Parse capabilities                │          │
│    │               │   └──────────────────────────────────┘          │
│    │               │                                                │
│    │               ├─► TreeSitter Extraction (Infra/Extract/TS/*.hs)│
│    │               │   ┌──────────────────────────────────┐          │
│    │               │   │ Run tree-sitter CLI on file       │          │
│    │               │   │ Parse CST JSON → symbols          │          │
│    │               │   │ Convert to Domain types            │          │
│    │               │   └──────────────────────────────────┘          │
│    │               │                                                │
│    │               └─► Stub Extraction (one node per file)          │
│    │                                                                │
│    ├── .hs Files  ──► UseCase/Extract/Haskell.hs (specialized)     │
│    ├── .md Files  ──► UseCase/Extract/Markdown.hs (headings+links)  │
│    ├── Papers     ──► PDF/image/video extraction                    │
│    │                                                                │
│    └── Cache check: Infra/FileSystem/Cache.hs                       │
│        (skip unchanged files if cfgUpdate=true)                     │
│                                                                      │
│  Output: Extraction { nodes: [Node], edges: [Edge] }                │
└──────────────────────────┬───────────────────────────────────────────┘
                           │
                           ▼
```

### Step 3 — Build

```
┌──────────────────────────────────────────────────────────────────────┐
│ STEP 3: BUILD                                                        │
│ UseCase/Build.hs                                                     │
│                                                                      │
│  Input: [Extraction] + directed flag                                 │
│  Process:                                                            │
│    ├── Merge all extractions into single graph                       │
│    ├── Deduplicate nodes/edges by ID                                 │
│    └── Build LabeledGraph (Domain)                                   │
│                                                                      │
│  💾 Checkpoint: graph.checkpoint.json (save before clustering)       │
│  Output: LabeledGraph { gNodes: Map NodeId Node,                     │
│                         gEdges: Map (NodeId,NodeId) Edge }           │
└──────────────────────────┬───────────────────────────────────────────┘
                           │
                           ▼
```

### Step 4 — Cluster

```
┌──────────────────────────────────────────────────────────────────────┐
│ STEP 4: CLUSTER                                                      │
│ UseCase/Cluster.hs  +  Domain/Community.hs                          │
│                                                                      │
│  Input: LabeledGraph + Resolution { gamma, minSize, mergeStrategy }  │
│  Process:                                                            │
│    ├── Convert to FGL graph (Domain/Graph/FGL.hs)                   │
│    ├── Run Leiden community detection                                │
│    │   ├── Phase 1: Local moving (modularity optimization)           │
│    │   ├── Phase 2: Refinement (avoid random aggregation)            │
│    │   └── Phase 3: Aggregation (build coarser graph)                │
│    ├── Merge small communities (cfgMinCommSize)                       │
│    └── Label communities (Domain/Community/Label.hs)                 │
│  Output: (CommunityMap, CohesionMap)                                 │
└──────────────────────────┬───────────────────────────────────────────┘
                           │
                           ▼
```

### Step 4b — Infer

```
┌──────────────────────────────────────────────────────────────────────┐
│ STEP 4b: INFER                                                       │
│ UseCase/Infer.hs                                                     │
│                                                                      │
│  Input: EdgeDensity + LabeledGraph + CommunityMap                    │
│  Process:                                                            │
│    ├── Sparse  → no inferred edges                                   │
│    ├── Normal  → bridge edges + transitive dependencies              │
│    ├── Dense   → + shared context edges                              │
│    └── Maximum → + lower thresholds for shared context               │
│                                                                      │
│  🔁 Enriched graph = original + inferred edges                       │
│  💾 Checkpoint updated with enriched graph                            │
│  Output: Enriched LabeledGraph                                       │
└──────────────────────────┬───────────────────────────────────────────┘
                           │
                           ▼
```

### Step 5 — Re-Cluster + Analyze

```
┌──────────────────────────────────────────────────────────────────────┐
│ STEP 5: RE-CLUSTER + ANALYZE                                         │
│ UseCase/Cluster.hs + UseCase/Analyze.hs                             │
│                                                                      │
│  Re-cluster enriched graph:                                          │
│    └── Same Leiden algorithm on enriched graph                       │
│                                                                      │
│  Analyze (Domain/Analysis.hs + Domain/Graph/Analysis.hs):           │
│    ├── Community statistics (size, density, cohesion)                │
│    ├── God nodes (highest-degree nodes per community)                 │
│    ├── Surprising connections (edges between distant communities)     │
│    ├── Bridge nodes (connect multiple communities)                   │
│    └── Suggested questions for LLM exploration                       │
│                                                                      │
│  Output: (FinalCommunityMap, FinalCohesion, Analysis)                │
└──────────────────────────┬───────────────────────────────────────────┘
                           │
                           ▼
```

### Step 6 — Report

```
┌──────────────────────────────────────────────────────────────────────┐
│ STEP 6: REPORT                                                       │
│ UseCase/Report.hs                                                    │
│                                                                      │
│  Input: Graph + Analysis + Config + Detection                        │
│  Process:                                                            │
│    ├── Human-readable summary (stats, communities, connections)      │
│    ├── God nodes, surprising connections, suggestions                 │
│    └── Token savings estimate                                        │
│  Output: Report struct (consumed by export step)                     │
└──────────────────────────┬───────────────────────────────────────────┘
                           │
                           ▼
```

### Step 7 — Export

```
┌──────────────────────────────────────────────────────────────────────┐
│ STEP 7: EXPORT                                                       │
│ UseCase/Export.hs + Infrastructure/Export/*.hs                       │
│                                                                      │
│  ┌─────────────────┐  ┌──────────────────┐  ┌────────────────────┐  │
│  │  graph.json     │  │  report.md       │  │  index.html        │  │
│  │  (always)       │  │  (always)        │  │  (vis.js, unless   │  │
│  │  Export/JSON.hs │  │  Export/Report.hs│  │   --no-viz)        │  │
│  └─────────────────┘  └──────────────────┘  │  Export/HTML.hs    │  │
│                                             └────────────────────┘  │
│  ┌─────────────────┐  ┌──────────────────┐  ┌────────────────────┐  │
│  │  Obsidian vault │  │  Neo4j Cypher    │  │  community_graph   │  │
│  │  (--obsidian)    │  │  (--neo4j)       │  │  .json             │  │
│  │  Export/         │  │  Export/         │  │  (--community-     │  │
│  │   Obsidian.hs   │  │   Neo4j.hs       │  │   graph)           │  │
│  └─────────────────┘  └──────────────────┘  │  Export/            │  │
│                                             │   CommunityGraph.hs│  │
│  ┌─────────────────┐  ┌──────────────────┐  └────────────────────┘  │
│  │  SVG            │  │  GraphML         │                          │
│  │  (--svg)        │  │  (--graphml)     │                          │
│  │  Export/SVG.hs  │  │  Export/GraphML  │                          │
│  └─────────────────┘  └──────────────────┘                          │
│                                                                      │
│  🗑️ Remove checkpoint (final graph.json is authoritative)            │
│  Output: ExportResult { json, html, report, obsidian, neo4j, ... }  │
└──────────────────────────────────────────────────────────────────────┘
```

---

## Secondary Flows

### Query Flow

```
┌──────────────────────────────────────────────────────────┐
│                  QUERY FLOW                                │
│  graphos query <question> [--dfs] [--budget N]             │
│                                                           │
│  graph.json ──► Load (UseCase/Load) ──► LabeledGraph     │
│                                           │               │
│                  ┌────────────────────────┘               │
│                  ▼                                        │
│         UseCase/Query.hs + UseCase/Query/Normalize.hs     │
│                  │                                        │
│         Normalize question terms                          │
│         Match nodes by label/id                           │
│                  │                                        │
│         ├─► BFS traversal (default)                      │
│         └─► DFS traversal (--dfs)                        │
│                  │                                        │
│         Respect token budget                              │
│                  │                                        │
│         ▼                                                │
│    QueryResult { nodes: [(NodeId,Label)],                 │
│                   edges: [(From,To,Rel,Conf)],            │
│                   traverse: "bfs"|"dfs" }                 │
└──────────────────────────────────────────────────────────┘
```

### Path Flow

```
┌──────────────────────────────────────────────────────────┐
│                  PATH FLOW                                 │
│  graphos path <from> <to>                                  │
│                                                           │
│  graph.json ──► Load ──► LabeledGraph ──► pathQuery       │
│                                            (BFS shortest) │
│  Output: [NodeId] path with edge relations                │
└──────────────────────────────────────────────────────────┘
```

### MCP Server Flow

```
┌──────────────────────────────────────────────────────────┐
│                  MCP SERVER FLOW                           │
│  graphos --mcp graph.json                                  │
│                                                           │
│  graph.json ──► Load ──► MCP Server (stdio)               │
│  Infrastructure/Server/MCP.hs                             │
│                                                           │
│  AI agents connect via MCP protocol:                      │
│    ├── query_graph   → search nodes/edges                 │
│    ├── get_community → list community members              │
│    └── navigate      → BFS/DFS with token budget          │
└──────────────────────────────────────────────────────────┘
```

### Watcher Mode

```
┌──────────────────────────────────────────────────────────┐
│                  WATCHER MODE                              │
│  graphos . --watch                                         │
│                                                           │
│  Infrastructure/FileSystem/Watcher.hs                      │
│                                                           │
│  File change ──► Detect diff ──► Re-extract changed      │
│                                    files only             │
│                                       │                    │
│                              Re-build + Re-cluster        │
│                                       │                    │
│                              Incremental export            │
└──────────────────────────────────────────────────────────┘
```

---

## Clean Architecture Layer Map

```
┌─────────────────────────────────────────────────────────┐
│                    DOMAIN (Pure, no IO)                   │
│                                                          │
│  Types/    Node, Edge, Relation, Extraction, Graph,      │
│            Community, Analysis, Detection, PipelineConfig│
│                                                          │
│  Graph/    Core, FGL, Query, Diff, Analysis              │
│  Community Community detection, Label, Resolution        │
│  Analysis  God node, surprising connections              │
│  Config    GraphosConfig, extractors, LSP server defs    │
│  Context   Context formatting for LLM consumption       │
│  Extraction Extraction result types                     │
└──────────────────────────┬──────────────────────────────┘
                           │ depends on
                           ▼
┌─────────────────────────────────────────────────────────┐
│                 USE CASE (Pure orchestration)             │
│                                                          │
│  Pipeline    detect → extract → build → cluster → ...   │
│  Detect      File detection + categorization            │
│  Extract     Orchestrate extractors (LSP/TS/MD/HS)      │
│  Build       Construct LabeledGraph from extractions    │
│  Cluster     Leiden clustering with resolution           │
│  Infer       Edge density inference                      │
│  Analyze     Graph analysis (stats, god nodes, etc.)    │
│  Report      Human-readable report generation           │
│  Export      Orchestrate all export formats             │
│  Query       Graph querying (BFS/DFS + budget)          │
│  Load        Load graph.json from disk (calls Infra)     │
│  Conversation Conversation tracking                    │
│  SelectContext Context selection for LLM               │
│  FormatContext Format context for output                │
│  Benchmark   Performance measurement                   │
│  Ingest      Multi-format file ingestion               │
└──────────────────────────┬──────────────────────────────┘
                           │ depends on
                           ▼
┌─────────────────────────────────────────────────────────┐
│              INFRASTRUCTURE (IO boundary)                 │
│                                                          │
│  LSP/        Client, Protocol, Transport, Extraction,    │
│              ServerMap, CapabilityParse, Capabilities    │
│                                                          │
│  Extract/    TreeSitter/Core, Grammar, Convert           │
│  (TreeSitter)                                            │
│                                                          │
│  FileSystem/ Cache, Manifest, Watcher, Ignore,          │
│              Sensitive, OfficeConvert, Conversation      │
│                                                          │
│  Export/     JSON, HTML, Report, Obsidian, Neo4j,       │
│              SVG, GraphML, CommunityGraph                │
│                                                          │
│  Server/     MCP (stdio protocol), Static (HTTP)         │
│  Config      Load graphos.yaml                           │
│  Logging     Leveled logging                            │
│  Security    Path safety, validation                    │
│  Git/        Hook management                            │
│  Tracking/   Cost tracking (token usage)                │
└─────────────────────────────────────────────────────────┘
```

---

## Key Data Types Flow

```
FilePath ──► Detection ──► Extraction ──► LabeledGraph ──► CommunityMap
                                         │                     │
                                         │              CohesionMap
                                         │                     │
                                         └─────────┬───────────┘
                                                   ▼
                                            EnrichedGraph ──► Analysis
                                                   │              │
                                                   │              ▼
                                            (community_graph.json) │
                                                   │         Report
                                                   ▼
                                            ExportResult { json, html, report,
                                                           obsidian?, neo4j?, svg?,
                                                           graphml?, communityGraph? }
```

---

*This document reflects the architecture as of the current codebase. For implementation details, refer to the source modules listed in each step.*