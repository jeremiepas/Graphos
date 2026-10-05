## MODIFIED Requirements

### Requirement: Workflow 03 — watch mode continuous file monitoring

Module `Graphos.Infrastructure.FileSystem.Watcher` SHALL export: `watchDirectory :: GraphosWatchConfig -> FilePath -> (Event -> IO ()) -> IO ()` for single-root watching, and a multi-root entry point that watches a list of source roots concurrently with the same debounce semantics. `data GraphosWatchConfig = GraphosWatchConfig { watchDebounce :: !NominalDiffTime }` with default 0.5s debounce. Uses `fsnotify` for recursive directory watching. CLI: `graphos <path> --watch` (single root) or `graphos --watch` with `sources:` configured (one watcher per source root). Event callbacks SHALL receive structured file-path values, not joined strings. Flow: (1) full pipeline initially, (2) enter watch loop over all roots, (3) on file change → attribute the event to the owning source by longest-prefix match and run the incremental pipeline on the changed files with source-qualified paths, (4) respect each root's .gitignore, .graphosignore, and per-source ignore patterns, (5) return to watching. All standard flags preserved across incremental re-runs. Ctrl+C stops all watchers. (PRD §3.4, workflow 03)

#### Scenario: Watch detects file change

- **WHEN** a source file is modified during `--watch`
- **THEN** watcher SHALL detect change within debounce interval and trigger incremental pipeline

#### Scenario: Debounce prevents rapid re-triggering

- **WHEN** 10 files change within 0.5 seconds
- **THEN** watcher SHALL coalesce into a single incremental pipeline run

#### Scenario: Multi-root watch attributes events to sources

- **WHEN** `--watch` runs with two configured sources and a file changes in the second source
- **THEN** the event SHALL trigger the incremental pipeline exactly once with the file identified as `sourceName/relativePath`

#### Scenario: Nested roots claimed once

- **WHEN** two configured source roots overlap (worktree inside parent repository) and a file changes in the nested root
- **THEN** the event SHALL be attributed to the nested source only and SHALL NOT be processed twice