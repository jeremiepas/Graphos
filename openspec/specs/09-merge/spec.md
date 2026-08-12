# 09-merge Specification

## Purpose
TBD - created by archiving change graphos-product. Update Purpose after archive.
## Requirements
### Requirement: Workflow 09 — merge two knowledge graphs
Module `Graphos.UseCase.Merge` SHALL export: `mergeGraphs :: LabeledGraph -> LabeledGraph -> LabeledGraph`. CLI: `graphos merge <path-a> <path-b> -o <output-dir>`. Flow: (1) load graph A and graph B, (2) merge via `Domain.Graph.Core.mergeGraphs` (deduplicate by NodeId, last-write from B wins, union edges, preserve A's directed flag), (3) re-cluster merged graph via Leiden (old community IDs discarded), (4) infer edges, (5) analyze, (6) export to output-dir. (PRD §13, workflow 09)

#### Scenario: Merge deduplicates and re-clusters
- **WHEN** merging two graphs with overlapping NodeIds
- **THEN** duplicate NodeIds merged (B wins), edges unioned, result re-clustered with fresh community IDs

### Requirement: Workflow 10 — ingest single file or URL
Module `Graphos.UseCase.Ingest` SHALL export: `ingestFile :: FilePath -> GraphosConfig -> IO IngestResult`. CLI: `graphos ingest <file>`. Auto-detect type: code → LSP/TS/stub; `.md` → LLM; `.pdf` → citation mining; images → LLM vision; video/audio → Whisper → LLM. URL detection: `twitter.com` → TwitterUrl, `arxiv.org` → ArxivUrl, `.pdf` URL → PdfUrl, images → ImageUrl, `youtube.com` → YoutubeUrl, other → GenericWeb. With `--embed`: generate vector embeddings via Ollama, store in `IngestIndex` at `graphos-out/index.json`. `UseCase.IngestIndex` handles embedding index management. (PRD §11, workflow 10)

#### Scenario: Ingest a PDF paper
- **WHEN** `ingestFile` is called on a `.pdf`
- **THEN** it SHALL extract citations + concepts as nodes with `Cites`/`RelatesTo` edges

#### Scenario: Ingest with embedding generation
- **WHEN** `--embed` flag is set
- **THEN** each extracted node SHALL get a vector embedding stored in `IngestIndex`

### Requirement: Workflow 11 — LLM community labeling
Module `Graphos.Domain.Labeling` SHALL export: `batchCommunities :: Int -> CommunityMap -> [[(CommunityId, [NodeId])]]`, `labelPrompt :: [(CommunityId, [NodeId])] -> LabeledGraph -> Text`. Module `Graphos.UseCase.Label` SHALL export: `labelCommunities :: LabeledGraph -> CommunityMap -> CohesionMap -> LabelingConfig -> IO (Map CommunityId Text)`. Flow: (1) batch communities (N per LLM call), (2) generate prompt per batch listing members + cohesion + stats, (3) call LLM via `Infrastructure.LLM.OpenAI`, (4) parse labels. CLI: `--label` flag. Config: `labeling.model`, `labeling.endpoint`, `labeling.batch_size`, `labeling.temperature`. (PRD §5, workflow 11)

#### Scenario: Label communities with LLM
- **WHEN** `--label` is set with a local Ollama model
- **THEN** each community SHALL receive a human-readable label like "Configuration Parsing & Validation"

### Requirement: Workflow 12 — Neo4j push with three modes
Module `Graphos.Infrastructure.Export.Neo4j` SHALL export: `pushToNeo4j :: Neo4jConfig -> LabeledGraph -> CommunityMap -> CohesionMap -> Analysis -> IO ()`. Three modes: FullPush (all nodes/edges/communities, ~990k stmts), SubgraphPush (representatives + bridges per community, default 7, ~64k stmts, ~30s), CommunityPush (community nodes + inter-community edges, ~8k stmts, ~5s). Auto-select: <10k nodes → FullPush, ≥10k → SubgraphPush. Override: `--neo4j-push-mode`. Representative selection: centroid (highest degree), top-N by degree, bridge nodes, entry points (file nodes). Parameterized Cypher (no string interpolation). Batch ≤50 statements. Streaming: when `--neo4j` during pipeline, push nodes during extraction, edge repair pass after. (PRD §9, workflow 12)

#### Scenario: Auto-select SubgraphPush for large graph
- **WHEN** graph has 50,000 nodes
- **THEN** SubgraphPush SHALL select ≤7 representatives per community + bridge nodes

### Requirement: Workflow 13 — Memgraph push via Bolt protocol
Module `Graphos.Infrastructure.Export.Memgraph` SHALL export: `pushToMemgraph :: MemgraphConfig -> LabeledGraph -> CommunityMap -> CohesionMap -> Analysis -> IO ()`. Same three modes as Neo4j. Connection via Bolt protocol at configured URI (default `bolt://localhost:7688`). Config: `--memgraph`, `--memgraph-push <uri>`, `--memgraph-push-mode`, `--memgraph-subgraph-size N`. (PRD §9, workflow 13)

#### Scenario: Push to Memgraph via Bolt
- **WHEN** `--memgraph --memgraph-push bolt://localhost:7688` is set
- **THEN** graph data SHALL be pushed to Memgraph using auto-selected push mode

### Requirement: Workflow 14 — observability stack (OTLP traces, metrics, logs, debug)
Module `Graphos.Infrastructure.Observability` SHALL export: `runWithTracing :: Text -> GraphosConfig -> IO a -> IO a`; `data MetricsStore` with `incCounter`, `decCounter`, `setGauge`, `observeHistogram` using `atomicModifyIORef'`; Prometheus `/metrics` HTTP endpoint when `--metrics PORT`; debug-trace JSONL writer to `graphos-out/debug/`. Module `Graphos.Infrastructure.Observability.SDK` SHALL initialize OTLP exporter. Module `Graphos.Infrastructure.Logging` SHALL export leveled logging with OTLP log bridge: `setLogTraceContext` injects `trace_id` when span active. Spans per pipeline stage. Env vars: `OTEL_EXPORTER_OTLP_ENDPOINT` (default `http://localhost:4318`), `OTEL_SERVICE_NAME` (default `graphos`), `OTEL_SDK_DISABLED` (kill switch). CLI: `--otel`, `--otel-endpoint`, `--metrics PORT`. (PRD §10, workflow 14)

#### Scenario: Traces with --otel flag
- **WHEN** `graphos <path> --otel` is run
- **THEN** spans SHALL be created for detect/extract/build/cluster/infer/analyze/export with timing

#### Scenario: Prometheus endpoint serves metrics
- **WHEN** `--metrics 9090` is set
- **THEN** `curl http://localhost:9090/metrics` SHALL return Prometheus exposition format

#### Scenario: Logs correlate with traces
- **WHEN** a log is written during an active span
- **THEN** log SHALL include `trace_id` attribute for Grafana click-through

### Requirement: Workflow 15 — config init (graphos init)
CLI `graphos init` SHALL generate a `graphos.yaml` file in the current directory with default values: LSP server mappings from `defaultServerMap`, file extension categories, empty neo4j/memgraph sections, default resolution, default observability settings. (PRD §13, §14, workflow 15)

#### Scenario: graphos init creates config file
- **WHEN** user runs `graphos init`
- **THEN** a `graphos.yaml` SHALL be created with all default config sections populated

### Requirement: Workflow 16 — LSP server discovery (graphos lservers)
CLI `graphos lservers` SHALL scan PATH for known LSP server commands from `Infrastructure.LSP.ServerMap.defaultServerMap`. For each found server: verify executable exists, check capabilities via `Infrastructure.LSP.Capabilities` (documentSymbol, references, workspaceSymbol, callHierarchy), and output a table of language, server command, supported capabilities. (PRD §6.1, §13, workflow 16)

#### Scenario: Detect installed LSP servers
- **WHEN** `graphos lservers` is run
- **THEN** output SHALL be a table showing each detected server's language, command, and supported capabilities

