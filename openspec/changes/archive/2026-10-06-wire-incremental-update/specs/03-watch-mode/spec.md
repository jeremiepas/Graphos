## MODIFIED Requirements

### Requirement: Workflow 03 — watch mode continuous file monitoring

Module `Graphos.Infrastructure.FileSystem.Watcher` SHALL export `watchDirectory :: GraphosWatchConfig -> FilePath -> (Event -> IO ()) -> IO ()`. `data GraphosWatchConfig = GraphosWatchConfig { watchDebounce :: !NominalDiffTime }` with default 0.5s debounce. Uses `fsnotify` for recursive directory watching. CLI: `graphos <path> --watch`. Flow: (1) full pipeline initially, (2) enter watch loop, (3) on file change → run incremental pipeline on changed files, with changed-file extractions written through to the persistent extraction cache, (4) generate embeddings for the merged graph via the content-addressed embedding cache so changed nodes gain vectors and unchanged texts are cache hits, (5) respect .gitignore and sensitive files, (6) return to watching. All standard flags preserved across incremental re-runs. Ctrl+C stops. (PRD §3.4, workflow 03)

#### Scenario: Watch detects file change
- **WHEN** a source file is modified during `--watch`
- **THEN** watcher SHALL detect change within debounce interval and trigger incremental pipeline

#### Scenario: Debounce prevents rapid re-triggering
- **WHEN** 10 files change within 0.5 seconds
- **THEN** watcher SHALL coalesce into a single incremental pipeline run

#### Scenario: Changed nodes gain embeddings
- **WHEN** a source file is modified during `--watch` and the incremental pipeline completes with embeddings enabled
- **THEN** the persisted graph's embedding sidecar contains vectors for the changed file's nodes, and unchanged nodes' texts were not re-embedded (served from cache)

#### Scenario: Watch update writes through to extraction cache
- **WHEN** a source file is modified during `--watch` and extraction succeeds
- **THEN** the file's extraction is saved to the persistent extraction cache keyed by its content hash, so a subsequent `--update` run on the same content is a cache hit