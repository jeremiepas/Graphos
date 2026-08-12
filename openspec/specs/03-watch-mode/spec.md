# 03-watch-mode Specification

## Purpose
TBD - created by archiving change graphos-product. Update Purpose after archive.
## Requirements
### Requirement: Workflow 03 — watch mode continuous file monitoring
Module `Graphos.Infrastructure.FileSystem.Watcher` SHALL export: `watchDirectory :: GraphosWatchConfig -> FilePath -> (Event -> IO ()) -> IO ()`. `data GraphosWatchConfig = GraphosWatchConfig { watchDebounce :: !NominalDiffTime }` with default 0.5s debounce. Uses `fsnotify` for recursive directory watching. CLI: `graphos <path> --watch`. Flow: (1) full pipeline initially, (2) enter watch loop, (3) on file change → run incremental pipeline on changed files, (4) respect .gitignore and sensitive files, (5) return to watching. All standard flags preserved across incremental re-runs. Ctrl+C stops. (PRD §3.4, workflow 03)

#### Scenario: Watch detects file change
- **WHEN** a source file is modified during `--watch`
- **THEN** watcher SHALL detect change within debounce interval and trigger incremental pipeline

#### Scenario: Debounce prevents rapid re-triggering
- **WHEN** 10 files change within 0.5 seconds
- **THEN** watcher SHALL coalesce into a single incremental pipeline run

