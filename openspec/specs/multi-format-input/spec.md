# multi-format-input Specification

## Purpose
TBD - created by archiving change graphos-product. Update Purpose after archive.
## Requirements
### Requirement: UseCase.Detect — file detection and categorization
Module `Graphos.UseCase.Detect` SHALL export `detectFiles :: FilePath -> GraphosConfig -> IO Detection` where `Detection` contains files grouped by extension and category (CodeFile, DocFile, PaperFile, ImageFile, VideoFile, AudioFile). SHALL respect `.gitignore` patterns via `Infrastructure.FileSystem.Ignore`. Module `Graphos.UseCase.Detect.Paper` SHALL handle `.pdf` file detection specifically. (PRD §3.2 Detect stage)

#### Scenario: Detect files by category
- **WHEN** `detectFiles` scans a directory containing `.hs`, `.md`, `.pdf`, `.png` files
- **THEN** files SHALL be categorized as CodeFile, DocFile, PaperFile, ImageFile respectively

#### Scenario: Respect .gitignore
- **WHEN** a `.gitignore` excludes `dist/` directory
- **THEN** `detectFiles` SHALL NOT include files from `dist/`

### Requirement: UseCase.Ingest — single file/URL ingestion
Module `Graphos.UseCase.Ingest` SHALL export `ingestFile :: FilePath -> GraphosConfig -> IO Extraction`. For code files: route to LSP/tree-sitter/stub extraction. For docs: route to LLM extraction via `Infrastructure.LLM.OpenAI`. For images: route to LLM vision extraction. For papers (.pdf): citation mining + concept extraction. For video/audio: Whisper transcription → LLM extraction. Module `Graphos.UseCase.IngestIndex` SHALL handle embedding generation via `Infrastructure.LLM.Embedding`. (PRD §11.1, workflow 10)

#### Scenario: Ingest a PDF paper
- **WHEN** `ingestFile` is called on a `.pdf` file
- **THEN** it SHALL extract citations and concepts as nodes with `Cites`/`RelatesTo` edges

#### Scenario: Ingest a URL
- **WHEN** `ingestFile` is called on a `https://` URL
- **THEN** it SHALL fetch the content via HTTP, then extract based on content type

### Requirement: Infrastructure.FileSystem.Cache — SHA256 cache
Module `Graphos.Infrastructure.FileSystem.Cache` SHALL export: `computeHash :: FilePath -> IO Text`, `loadCached :: FilePath -> IO (Maybe (Map FilePath Text))`, `saveCached :: FilePath -> Map FilePath Text -> IO ()`, `checkSemanticCache :: FilePath -> Text -> IO (Maybe Extraction)`, `saveSemanticCache :: FilePath -> Text -> Extraction -> IO ()`. Cache files SHALL be stored in `graphos-out/cache/*.sha256`. On re-runs with `--update`, only files with changed hashes SHALL be re-extracted. (PRD §3.4, §11.2)

#### Scenario: Skip unchanged files on incremental run
- **WHEN** 95 of 100 files have unchanged SHA256 hashes
- **THEN** the system SHALL re-extract only 5 changed files and reuse cached results for the other 95

#### Scenario: Cache persists across runs
- **WHEN** a run saves SHA256 cache, then a second `--update` run starts
- **THEN** the second run SHALL load the saved cache and compare hashes

### Requirement: Infrastructure.FileSystem.Watcher — watch mode
Module `Graphos.Infrastructure.FileSystem.Watcher` SHALL export `watchDirectory :: GraphosWatchConfig -> FilePath -> (Event -> IO ()) -> IO ()`. Uses `fsnotify` for recursive directory watching. `data GraphosWatchConfig = GraphosWatchConfig { watchDebounce :: NominalDiffTime }` (default 0.5s debounce). On file change, trigger an incremental pipeline run. (PRD §3.4, workflow 03)

#### Scenario: Watch detects modification
- **WHEN** a source file is modified during watch mode
- **THEN** the watcher SHALL detect the change within debounce interval and trigger incremental pipeline

#### Scenario: Debounce rapid changes
- **WHEN** 10 files change within 0.5 seconds
- **THEN** the watcher SHALL coalesce changes and trigger a single incremental run

### Requirement: Infrastructure.FileSystem.OfficeConvert — .docx/.xlsx → markdown
Module `Graphos.Infrastructure.FileSystem.OfficeConvert` SHALL export `convertOffice :: FilePath -> IO (Either Text Text)` that converts `.docx` and `.xlsx` files to markdown text for subsequent LLM extraction. (PRD §11.1)

#### Scenario: Convert docx to markdown
- **WHEN** `convertOffice` is called on a `.docx` file
- **THEN** it SHALL return `Right markdownText` containing the document content

### Requirement: UseCase.Merge — combine two knowledge graphs
Module `Graphos.UseCase.Merge` SHALL export `mergeGraphs :: LabeledGraph -> LabeledGraph -> LabeledGraph`. SHALL merge nodes/edges from both graphs, deduplicate by `NodeId`, and re-cluster the combined graph using Leiden. Triggered via `graphos merge <a> <b> -o <dir>`. (PRD §13, workflow 09)

#### Scenario: Merge two codebase graphs
- **WHEN** `mergeGraphs` combines two graphs with overlapping node IDs
- **THEN** duplicate node IDs SHALL be merged (richer metadata wins), edges SHALL be unioned, and the result SHALL be a valid `LabeledGraph`

