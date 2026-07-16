## ADDED Requirements

### Requirement: Workflow 01 Stage 1 Detect — file detection and categorization
Module `Graphos.UseCase.Detect` SHALL export: `detectFiles :: FilePath -> GraphosConfig -> IO Detection` where `data Detection = Detection { fileCategories :: Map FileType [FilePath], totalFiles :: Int, warnings :: [Text] }`. SHALL recursively scan directory, categorize by `cfgFileExtensions` mapping (CodeFile/DocFile/PaperFile/ImageFile/VideoFile/AudioFile), respect `.gitignore` via `Infrastructure.FileSystem.Ignore`, exclude sensitive files via `Infrastructure.FileSystem.Sensitive`. Module `Graphos.UseCase.Detect.Paper` SHALL handle `.pdf` detection. (PRD §3.2 Detect, workflow 01 stage 1)

#### Scenario: Detect categorizes files by extension
- **WHEN** `detectFiles` scans a directory with `.hs`, `.md`, `.pdf`, `.png` files
- **THEN** `fileCategories` SHALL map CodeFile → [.hs files], DocFile → [.md files], PaperFile → [.pdf files], ImageFile → [.png files]

#### Scenario: Detect respects .gitignore
- **WHEN** `.gitignore` excludes `dist/`
- **THEN** `detectFiles` SHALL NOT include files from `dist/`

### Requirement: Workflow 01 Stage 2 Extract — parallel per language with SHA256 cache
Module `Graphos.UseCase.Extract` SHALL export: `extractFiles :: Detection -> GraphosConfig -> PipelineState -> IO [Extraction]`. SHALL group code files by LSP server, spawn one server per language (parallel via `cfgThreads`), use SHA256 cache to skip unchanged files. Route per type: code → LSP → tree-sitter → stub; `.hs` → `UseCase.Extract.Haskell`; `.md` → `UseCase.Extract.Markdown`; papers → PDF mining; images → LLM vision; video/audio → Whisper → LLM. (PRD §3.2 Extract, workflow 01 stage 2)

#### Scenario: Parallel extraction by language
- **WHEN** detection finds `.hs` and `.py` files
- **THEN** `extractFiles` SHALL spawn `haskell-language-server` and `pyright-langserver` concurrently

#### Scenario: SHA256 cache skips unchanged files
- **WHEN** 95 of 100 files have unchanged hashes
- **THEN** only 5 files SHALL be re-extracted

### Requirement: Workflow 01 Stage 3 Build — merge extractions, build adjacency maps
Module `Graphos.UseCase.Build` SHALL export: `buildGraph :: [Extraction] -> Bool -> (LabeledGraph, PipelineState)`. SHALL merge all extractions, deduplicate nodes by `NodeId` (richer metadata wins), deduplicate edges by `EdgeId`, build `gAdjFwd` and `gAdjBack` from edge set. Save checkpoint to `graphos-out/cache/graph.checkpoint.json`. (PRD §3.2 Build, workflow 01 stage 3)

#### Scenario: Build merges extractions with deduplication
- **WHEN** two extractions contain same `NodeId`
- **THEN** `buildGraph` SHALL merge keeping the node with more non-Nothing fields

#### Scenario: Checkpoint saved after Build
- **WHEN** Build completes
- **THEN** `graph.checkpoint.json` SHALL exist in `graphos-out/cache/`

### Requirement: Workflow 01 Stage 4 Cluster — Leiden + merge + cohesion
Module `Graphos.UseCase.Cluster` SHALL export: `clusterGraph :: LabeledGraph -> Resolution -> (CommunityMap, CohesionMap)`. SHALL (1) convert to FGL via `Domain.Graph.FGL.toFGL`, (2) run `Domain.Community.detectCommunitiesWithResolution`, (3) `mergeSmallCommunities` for below `resMinSize`, (4) `scoreAllCohesion`. Two modes: full Leiden or fast single-node clustering. (PRD §3.2 Cluster, §5, workflow 01 stage 4)

#### Scenario: Cluster produces CommunityMap and CohesionMap
- **WHEN** `clusterGraph` runs on a graph with 200 nodes
- **THEN** result SHALL include `CommunityMap` with ≥1 community and `CohesionMap` with all values in [0,1]

### Requirement: Workflow 01 Stage 5 Infer — edge density inference + re-cluster
Module `Graphos.UseCase.Infer` SHALL export: `inferEdges :: LabeledGraph -> CommunityMap -> Double -> (LabeledGraph, CommunityMap, CohesionMap)`. Density settings: Sparse (0.0) → no inferred edges; Normal → bridge edges + transitive dependencies; Dense → + shared context edges; Maximum → + lower thresholds. After inference, re-cluster enriched graph. (PRD §3.2 Infer, workflow 01 stage 5)

#### Scenario: Sparse density adds no edges
- **WHEN** `inferEdges` is called with density 0.0
- **THEN** the resulting `LabeledGraph` SHALL have the same edge count as the input

#### Scenario: Normal density adds bridge edges
- **WHEN** `inferEdges` is called with Normal density
- **THEN** the resulting graph SHALL contain additional inferred bridge and transitive edges

### Requirement: Workflow 01 Stage 6 Analyze — god nodes, bridges, surprises, suggested questions
Module `Graphos.UseCase.Analyze` SHALL export: `analyze :: LabeledGraph -> CommunityMap -> CohesionMap -> Analysis`. SHALL compute community statistics, identify god nodes via `Domain.Graph.Analysis.godNodes`, find bridge nodes via `bridgeNodes`, detect surprising connections (edges between distant communities), generate suggested questions for exploration. (PRD §3.2 Analyze, workflow 01 stage 6)

#### Scenario: Analysis identifies god nodes and bridges
- **WHEN** `analyze` runs on a graph with ≥1 community
- **THEN** `analysisGodNodes` SHALL contain top-N nodes by degree, `analysisBridgeNodes` SHALL contain articulation points

### Requirement: Workflow 01 Stage 7 Export — all output formats
Module `Graphos.UseCase.Export` SHALL export: `exportAll :: LabeledGraph -> Analysis -> GraphosConfig -> Detection -> IO ()`. Always produce: `graph.json` (via `Infrastructure.Export.JSON`), `GRAPH_REPORT.md` (via `Infrastructure.Export.Report`), `graph.html` unless `--no-viz` (via `Infrastructure.Export.HTML`). Optional: `--community-graph` → `CommunityGraph`, `--obsidian` → Obsidian vault, `--neo4j` → Neo4j push, `--memgraph` → Memgraph push, `--svg` → SVG, `--graphml` → GraphML. Post-export: remove `graph.checkpoint.json`. (PRD §3.2 Export, §12, workflow 01 stage 7)

#### Scenario: Default export produces 3 formats
- **WHEN** `exportAll` runs without optional flags
- **THEN** `graphos-out/` SHALL contain `graph.json`, `GRAPH_REPORT.md`, `graph.html`

#### Scenario: Post-export checkpoint removal
- **WHEN** export completes successfully
- **THEN** `graph.checkpoint.json` SHALL NOT exist

### Requirement: Workflow 01 — skip-clustering mode
With `--no-cluster`, the pipeline SHALL run stages 1–3 (Detect → Extract → Build), skip stages 4–5 (Cluster → Infer), and go directly to report + export. (Workflow 01 skip-clustering)

#### Scenario: Skip-clustering omits community assignment
- **WHEN** `--no-cluster` flag is set
- **THEN** pipeline SHALL NOT run Leiden; `analysisCommunities` SHALL be empty