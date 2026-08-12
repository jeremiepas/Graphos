# 02-incremental-pipeline Specification

## Purpose
TBD - created by archiving change graphos-product. Update Purpose after archive.
## Requirements
### Requirement: Workflow 02 — incremental pipeline with --update
Module `Graphos.UseCase.Pipeline` SHALL export `runIncrementalPipeline :: FilePath -> GraphosConfig -> IO ()`. Flow: (1) load `graphos-out/graph.json` (previous result) via `UseCase.Load`, (2) load SHA256 hashes from `graphos-out/cache/`, (3) detect files — compare current SHA256 vs cached via `Infrastructure.FileSystem.Cache`, (4) split into changed (re-extract) and unchanged (reuse cached extractions), (5) merge old + new extractions via `UseCase.Build.buildGraph`, (6) Cluster → Infer → Analyze → Export. (PRD §3.4, workflow 02)

#### Scenario: Incremental skips unchanged files
- **WHEN** `--update` is used and 95/100 files have matching hashes
- **THEN** only 5 files SHALL be re-extracted; 95 reused from cache

#### Scenario: Incremental with no changes
- **WHEN** `--update` is used and no files changed
- **THEN** pipeline SHALL skip extraction, build, and cluster stages

### Requirement: Workflow 02 — checkpoint resume from interruption
If `graph.checkpoint.json` exists from an interrupted run, the pipeline SHALL resume from the last completed stage: after Detect → resume from Extract; after Extract → resume from Build; after Build → resume from Cluster; after Cluster → resume from Infer+Analyze+Export. (PRD §3.3, workflow 02)

#### Scenario: Resume from Build checkpoint
- **WHEN** checkpoint exists indicating Build completed
- **THEN** pipeline SHALL resume from Cluster stage, skipping Detect/Extract/Build

### Requirement: Workflow 03 — watch mode with --watch
Module `Graphos.Infrastructure.FileSystem.Watcher` SHALL export: `watchDirectory :: GraphosWatchConfig -> FilePath -> (Event -> IO ()) -> IO ()`. `data GraphosWatchConfig = GraphosWatchConfig { watchDebounce :: !NominalDiffTime }` (default 0.5s). Uses `fsnotify` for recursive watching. Flow: (1) run full pipeline initially, (2) enter watch loop, (3) on file change → run incremental pipeline, (4) filter: respect .gitignore + sensitive files. All standard flags preserved. (PRD §3.4, workflow 03)

#### Scenario: Watch detects file change
- **WHEN** a source file is modified during `--watch`
- **THEN** watcher SHALL detect change within debounce interval and trigger incremental pipeline

#### Scenario: Watch debounces rapid changes
- **WHEN** 10 files change within 0.5 seconds
- **THEN** watcher SHALL coalesce and trigger a single incremental run

