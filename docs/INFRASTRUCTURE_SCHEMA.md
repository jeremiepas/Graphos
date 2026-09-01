# Graphos Infrastructure Schema

## 1. Application Architecture

### 1.1 Module Dependency Graph (Clean Architecture)

```
Domain (pure) ← UseCase (orchestration, pure) ← Infrastructure (IO) ← CLI
```

**Dependency direction**: Infrastructure → UseCase → Domain (never reverse)

### 1.2 Domain Layer (`src/Graphos/Domain/`)

| Module | Purpose |
|--------|---------|
| `Types.hs` | Core types: `Node`, `Edge`, `Extraction`, `Confidence` |
| `Types/Node.hs` | Node type definition with id, label, properties |
| `Types/Edge.hs` | Edge type definition with source, target, relation |
| `Types/Graph.hs` | Graph container (nodes, edges, metadata) |
| `Types/Pipeline.hs` | Pipeline stage types, progress tracking |
| `Types/Analysis.hs` | Analysis result types (god nodes, bridges) |
| `Types/Ingest.hs` | Ingest index schema, file tracking |
| `Types/Writer.hs` | Output writer types |
| `Types/GraphFile.hs` | Graph file format (JSON schema) |
| `Graph.hs` | Graph operations (add, merge, query, shortest path) |
| `Graph/Core.hs` | Core graph algorithms |
| `Graph/Query.hs` | BFS, DFS, shortest path queries |
| `Graph/Analysis.hs` | God nodes, surprising connections, suggested questions |
| `Graph/Diff.hs` | Graph diff operations |
| `Graph/FGL.hs` | fglib-compatible graph representation |
| `Graph/Index.hs` | Node/edge index for fast lookup |
| `Graph/Score.hs` | Node scoring algorithms |
| `Community.hs` | Leiden community detection algorithm |
| `Community/Label.hs` | Community labeling logic |
| `Analysis.hs` | Top-level analysis orchestration |
| `Extraction.hs` | Extraction schema, validation |
| `Labeling.hs` | LLM-based community labeling |
| `Logging.hs` | Structured logging types |
| `Context.hs` | Context building for LLM queries |
| `PdfStructure.hs` | PDF document structure types |
| `Scaffold.hs` | Project scaffold types |
| `Query/Research.hs` | Research query types |
| `Query/Cypher/AST.hs` | Cypher query AST |
| `Query/Cypher/Parser.hs` | Cypher query parser |
| `Query/Cypher/Mapping.hs` | Graph-to-Cypher mapping |
| `Query/Cypher/Eval.hs` | Cypher query evaluation |
| `Config.hs` | Top-level configuration types |
| `Config/Core.hs` | Core config (granularity, extractors) |
| `Config/Extraction.hs` | Extraction config (LSP, tree-sitter) |
| `Config/Export.hs` | Export config (Neo4j, Memgraph, formats) |
| `Config/Ingest.hs` | Ingest config (embed, merge, deduplicate) |
| `Config/Observability.hs` | Observability config (OTLP, metrics) |
| `Config/Vision.hs` | Vision config (multimodal LLM) |
| `Config/MCP.hs` | MCP server config |
| `HexColor.hs` | Color utilities for visualization |

### 1.3 Use Case Layer (`src/Graphos/UseCase/`)

| Module | Purpose | Pipeline Stage |
|--------|---------|----------------|
| `Pipeline.hs` | Full pipeline orchestration | — |
| `Pipeline/Core.hs` | Core pipeline execution | — |
| `Pipeline/Incremental.hs` | Incremental pipeline (file-level) | — |
| `Detect.hs` | File detection (extension → type) | detect() |
| `Extract.hs` | LSP/tree-sitter extraction | extract() |
| `Extract/TreeSitter.hs` | Tree-sitter extraction implementation | — |
| `Extract/LSP.hs` | LSP extraction implementation | — |
| `Extract/Core.hs` | Extraction core logic | — |
| `Extract/Haskell.hs` | Haskell-specific extraction | — |
| `Build.hs` | Graph construction from extractions | build() |
| `Cluster.hs` | Community detection (Leiden) | cluster() |
| `Infer.hs` | Edge inference (bridges, transitive) | infer() |
| `Analyze.hs` | Analysis orchestration | analyze() |
| `Report.hs` | Report generation | — |
| `Export.hs` | Export orchestration | export() |
| `Query.hs` | Graph querying | query() |
| `Query/Refine.hs` | Query refinement | — |
| `Query/Normalize.hs` | Query normalization | — |
| `Query/Research.hs` | Research query execution | — |
| `Query/Render.hs` | Query result rendering | — |
| `Label.hs` | Community labeling via LLM | infer() |
| `Ingest.hs` | Single-file ingestion | — |
| `IngestIndex.hs` | Ingest index management | — |
| `Merge.hs` | Graph merge operations | — |
| `Subgraph.hs` | Subgraph extraction | — |
| `SelectContext.hs` | Context selection for LLM | — |
| `FormatContext.hs` | Context formatting | — |
| `Conversation.hs` | Conversation management | — |
| `Benchmark.hs` | Performance benchmarking | — |
| `Scaffold.hs` | Project scaffolding | — |
| `Port/LLMPort.hs` | LLM port abstraction | — |
| `Port/ExtractionPort.hs` | Extraction port abstraction | — |
| `Port/ExportPort.hs` | Export port abstraction | — |
| `Port/FileSystemPort.hs` | File system port abstraction | — |
| `Port/ObservabilityPort.hs` | Observability port abstraction | — |
| `Port/LoggingPort.hs` | Logging port abstraction | — |
| `AppEnv.hs` | Application environment | — |

### 1.4 Infrastructure Layer (`src/Graphos/Infrastructure/`)

| Module | Purpose |
|--------|---------|
| `LSP/Client.hs` | Language server connection |
| `LSP/Protocol.hs` | LSP JSON-RPC protocol types |
| `LSP/Capabilities.hs` | LSP capability detection |
| `LSP/CapabilityParse.hs` | Capability parsing |
| `LSP/Extraction.hs` | LSP-based extraction |
| `LSP/Transport.hs` | LSP transport (stdio) |
| `LSP/ServerMap.hs` | Language server → extension mapping |
| `FileSystem/Watcher.hs` | File watching for --update |
| `FileSystem/AtomicWrite.hs` | Atomic file writes |
| `FileSystem/Cache.hs` | File cache (SHA256 dedup) |
| `FileSystem/Conversation.hs` | Conversation file I/O |
| `FileSystem/Ignore.hs` | .gitignore pattern matching |
| `FileSystem/Manifest.hs` | Project manifest |
| `FileSystem/OfficeConvert.hs` | Office document conversion |
| `FileSystem/Sensitive.hs` | Sensitive file detection |
| `Git/Hook.hs` | Git hook management |
| `Export/JSON.hs` | JSON graph output |
| `Export/HTML.hs` | Interactive HTML (vis.js) |
| `Export/Obsidian.hs` | Obsidian vault export |
| `Export/Neo4j.hs` | Neo4j Cypher generation |
| `Export/Memgraph.hs` | Memgraph Cypher generation |
| `Export/GraphML.hs` | GraphML export (Gephi/yEd) |
| `Export/SVG.hs` | Static SVG export |
| `Export/Report.hs` | Markdown report generation |
| `Export/CommunityGraph.hs` | Community graph export |
| `Export/IncrementalJSON.hs` | Incremental JSON output |
| `Extract/Image.hs` | Image extraction (vision) |
| `Extract/Markdown.hs` | Markdown extraction |
| `Extract/Office.hs` | Office document extraction |
| `Extract/Pdf.hs` | PDF extraction |
| `Extract/TreeSitter/Core.hs` | Tree-sitter core |
| `Extract/TreeSitter/Grammar.hs` | Grammar resolution |
| `Extract/TreeSitter/Resolver.hs` | Grammar resolver |
| `Extract/TreeSitter/Convert.hs` | AST conversion |
| `LLM/OpenAI.hs` | OpenAI-compatible LLM client |
| `LLM/Embedding.hs` | Embedding generation |
| `LLM/Vision.hs` | Vision/multimodal LLM |
| `Logging.hs` | Structured logging |
| `Observability/SDK.hs` | OpenTelemetry SDK |
| `Server/MCP.hs` | MCP stdio server |
| `Server/QueryAPI.hs` | REST query API server |
| `Server/Static.hs` | Static file server |
| `Scaffold/Writer.hs` | Project scaffold writer |
| `Security.hs` | Security utilities |
| `Tracking/Cost.hs` | LLM cost tracking |
| `Wiring.hs` | Dependency injection wiring |
| `Config.hs` | Configuration loading |
| `IO/Atomic.hs` | Atomic I/O utilities |

### 1.5 CLI (`src/Graphos/CLI/`)

| Module | Purpose |
|--------|---------|
| `Parser.hs` | CLI argument parser (optparse-applicative) |

---

## 2. Data Pipeline

```
detect() → extract() → build() → cluster() → infer() → analyze() → export()
```

Each stage is a pure function. No shared state, no side effects outside `graphos-out/`.

### 2.1 Pipeline Stages

| Stage | Input | Output | Description |
|-------|-------|--------|-------------|
| `detect` | Filesystem paths | File list with types | Scan directory, classify files by extension |
| `extract` | File list | Node/Edge list | Parse files via LSP or tree-sitter |
| `build` | Extractions | Graph | Assemble nodes/edges into graph structure |
| `cluster` | Graph | Communities | Run Leiden algorithm for community detection |
| `infer` | Communities | Inferred edges | Bridge inference, transitive dependencies |
| `analyze` | Graph + communities | Analysis results | God nodes, surprising connections, suggested questions |
| `export` | Graph + analysis | Output files | JSON, HTML, Neo4j, Memgraph, GraphML, SVG, Obsidian |

---

## 3. Docker Infrastructure

### 3.1 Primary Stack (`docker-compose.yml`)

| Service | Image | Ports | Purpose |
|---------|-------|-------|---------|
| `neo4j` | neo4j:5-community | 7474 (HTTP), 7687 (Bolt) | Graph database (persistent) |
| `memgraph` | memgraph/memgraph-platform:latest | 7400 (Lab), 7688 (Bolt) | In-memory graph database |
| `tempo` | grafana/tempo:2.6.1 | 14320 (HTTP) | Distributed tracing (OTLP) |
| `otel-collector` | otel/opentelemetry-collector-contrib:0.96.0 | 14319 (OTLP HTTP), 14316 (OTLP gRPC), 8889 (Prometheus) | Signal routing |
| `prometheus` | prom/prometheus:v2.51.0 | 9091 | Metrics storage |
| `loki` | grafana/loki:2.9.6 | 3100 | Log storage |
| `grafana` | grafana/grafana:10.4.1 | 13000 | Dashboards (anonymous access) |

**Volumes**: neo4j-data, neo4j-logs, neo4j-plugins, neo4j-config, memgraph-data, memgraph-log, tempo-data, prometheus-data, loki-data, grafana-data

### 3.2 OTEL Standalone Stack (`docker-compose.otel.yaml`)

| Service | Image | Ports | Purpose |
|---------|-------|-------|---------|
| `otel-collector` | otel/opentelemetry-collector-contrib:latest | 4318 (OTLP HTTP), 8889 | Signal routing (separate network) |
| `tempo` | grafana/tempo:latest | 3200, 4317 | Tracing |
| `loki` | grafana/loki:latest | 3100 | Logging |
| `prometheus` | prom/prometheus:latest | 9099 | Metrics |
| `grafana` | grafana/grafana:latest | 3000 | Dashboards |

Network: `graphos-otel` (isolated from primary stack)

### 3.3 Service Dependencies

```
grafana → tempo, loki, prometheus
otel-collector → prometheus, loki, tempo
```

---

## 4. Observability Stack

### 4.1 Data Flow

```
Graphos (--otel) → OTLP (HTTP/gRPC) → OTel Collector → [Prometheus, Tempo, Loki]
                                                    ↓
                                              Grafana (query all three)
```

### 4.2 OTel Collector Pipeline

| Pipeline | Receivers | Processors | Exporters |
|----------|-----------|------------|-----------|
| `metrics` | otlp | batch, transform/add_resource | prometheus (localhost:8889) |
| `traces` | otlp | batch, transform/add_resource | otlphttp/tempo (localhost:4318) |
| `logs` | otlp | batch, transform/add_resource | loki (localhost:3100) |

### 4.3 Prometheus Scrape Targets

| Job | Target | Interval |
|-----|--------|----------|
| `graphos` | host IP:9190 | 10s |
| `otel-collector` | otel-collector:8889 | 15s |
| `tempo` | tempo:4320 | 15s |

### 4.4 Grafana Data Sources

| Source | Type | URL | Purpose |
|--------|------|-----|---------|
| Prometheus | prometheus | http://prometheus:9090 | Metrics (default) |
| Loki | loki | http://loki:3100 | Logs |
| Tempo | tempo | http://tempo:4320 | Traces (with tracesToMetrics, tracesToLogs, nodeGraph) |

### 4.5 Tempo Configuration

- Mode: Monolithic (local storage)
- Storage backend: local filesystem (`/var/tempo/traces`)
- WAL: `/var/tempo/wal`
- Retention: 14 days (336h)
- OTLP receivers: gRPC (:4317), HTTP (:4318)

---

## 5. Configuration Schema

### 5.1 Project Config (`graphos.yaml`)

```yaml
ingest:
  embed: boolean          # default: true
  merge: boolean          # default: true
  deduplicate: boolean    # default: true
  resolution: number      # default: 0.8
  min_comm_size: number   # default: 2
  max_leiden_iter: number # default: 20
  index_path: string      # default: graphos-out/index.json
  url:
    timeout: number       # default: 30
    user_agent: string    # default: graphos/0.1.0
    retry: number         # default: 1
  categories:
    image: { embed: false }
    video: { embed: false }

granularity: string       # fine | function | file (default: function)

extractors:               # file extension → extractor config
  ".ts": { mode, grammar, language_id }
  # ... (see graphos.yaml for full list)

pdf_extraction: string    # small | medium | large (default: medium)

labeling:
  provider: string        # ollama | openai | litellm (default: ollama)
  model: string           # default: llama3.2
  base_url: string        # default: http://localhost:11434/v1
  batch_size: number      # default: 20

embedding:
  enabled: boolean        # default: false
  provider: string        # ollama
  model: string           # default: nomic-embed-text
  base_url: string
  dimension: number       # 0 = auto

vision:
  enabled: boolean        # default: false
  model: string           # default: qwen3.6-moe
  api_key: string
  base_url: string
  max_tokens: number      # default: 1000
  batch_size: number      # default: 5

observability:
  enabled: boolean        # default: false
  endpoint: string        # default: http://localhost:4318
  metricsPort: number     # 0 = disabled
  serviceName: string     # default: graphos
  serviceVersion: string  # default: 0.1.0
  exportInterval: number  # default: 15
  debugTraceDir: string   # empty = disabled
```

### 5.2 Config Resolution Order (later wins)

1. Built-in defaults
2. Global config: `~/.config/graphos/graphos.yaml`
3. Project config: `graphos.yaml`
4. CLI flags (`--otel`, `--metrics`, `--embed`, `--no-embed`, etc.)

---

## 6. Graph Data Model

### 6.1 Core Types

```
Node {
  id: String
  label: String          # e.g., "Function", "Class", "Module", "File"
  properties: Map String Value
  community: Int         # assigned after cluster()
}

Edge {
  source: String         # node id
  target: String         # node id
  relation: String       # e.g., "CALLS", "IMPORTS", "DEFINES"
  confidence: Double     # 0.0 - 1.0
}

Graph {
  nodes: [Node]
  edges: [Edge]
  metadata: {
    createdAt: DateTime
    updatedAt: DateTime
    version: String
    config: Config
  }
}
```

### 6.2 File Categories

| Category | Extensions |
|----------|-----------|
| code | `.py`, `.ts`, `.tsx`, `.js`, `.jsx`, `.go`, `.rs`, `.hs`, `.nix`, `.rb`, `.java`, `.c`, `.cpp`, `.h`, `.hpp`, `.cs`, `.kt`, `.kts`, `.scala`, `.php`, `.swift`, `.lua`, `.zig`, `.ps1`, `.ex`, `.exs`, `.m`, `.mm`, `.jl`, `.vue`, `.svelte`, `.dart` |
| doc | `.md`, `.txt`, `.rst`, `.adoc`, `.org` |
| paper | `.pdf` |
| image | `.png`, `.jpg`, `.jpeg`, `.webp`, `.gif` |
| video | `.mp4`, `.mov`, `.mkv`, `.webm`, `.avi`, `.m4v` |
| office | `.docx`, `.xlsx`, `.doc`, `.ppt` |

---

## 7. Development Environment

### 7.1 Haskell Toolchain (devenv)

| Component | Package |
|-----------|---------|
| GHC | 9.10 (via cabal) |
| Cabal | cabal-install |
| LSP | haskell-language-server |
| Build tool | hpack |
| Test runner | hspec-discover |

### 7.2 System Dependencies

| Package | Purpose |
|---------|---------|
| zlib | Compression |
| openssl | TLS |
| poppler-utils | PDF text extraction |

### 7.3 Tooling

| Tool | Purpose |
|------|---------|
| gh | GitHub CLI |
| jq | JSON processing |
| pyright | Python type checking |
| python313Packages.pyyaml | YAML parsing |
| bun | JavaScript runtime |
| uv | Python package manager |
| nixd | Nix language server |
| vscode-langservers-extracted | Language servers |
| openspec | Specification tooling |
| llama-cpp | Local LLM inference |

### 7.4 Local LLM Server

| Model | Port | Config |
|-------|------|--------|
| Qwen 3.6 | 8080 | `llama:server` task |
| Gemma 4 26B | 8081 | `gemma4` process |

---

## 8. Output Formats

| Format | Module | Description |
|--------|--------|-------------|
| JSON | `Export/JSON.hs` | `graph.json` — full graph serialization |
| HTML | `Export/HTML.hs` | Interactive vis.js visualization |
| Neo4j | `Export/Neo4j.hs` | Cypher CREATE/MERGE statements |
| Memgraph | `Export/Memgraph.hs` | Cypher for Memgraph |
| GraphML | `Export/GraphML.hs` | Gephi/yEd compatible |
| SVG | `Export/SVG.hs` | Static SVG graph image |
| Obsidian | `Export/Obsidian.hs` | Obsidian vault markdown |
| Report | `Export/Report.md` | Plain-language audit report |
| Incremental | `Export/IncrementalJSON.hs` | Delta updates to graph.json |

---

## 9. Server Components

| Component | Port | Protocol | Purpose |
|-----------|------|----------|---------|
| MCP Server | stdio | MCP | Model Context Protocol |
| Query API | configurable | HTTP/REST | Graph queries |
| Static Server | configurable | HTTP | Serve assets/viewer |

---

## 10. CI/CD Tasks (devenv)

| Task | Description | Dependencies |
|------|-------------|--------------|
| `ci:build` | `cabal configure --enable-tests --flag dev -j4` | — |
| `ci:test` | `cabal test all` | `ci:build@succeeded` |
| `ci:haddock` | `cabal haddock all` | `ci:build@succeeded` |
| `ci:release-build` | Release build | — |
| `ci:release-test` | Release tests | `ci:release-build` |
| `openspec:apply` | Auto-apply pending OpenSpec changes | — |
| `llama:server` | Start llama.cpp server | `LLAMA_MODEL` env |
