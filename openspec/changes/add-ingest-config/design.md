# Design: add-ingest-config

## Context

The `graphos ingest <file>` command (workflow 10-ingest) has no configuration section in `graphos.yaml`. All ingest behavior is controlled via two CLI flags (`--embed` and `--output`), with remaining settings either hardcoded or inherited from `PipelineConfig` defaults tuned for full-codebase runs (resolution=1.0, minCommSize=3, maxLeidenIterations=50). Single-file graphs are 5-50 nodes — these defaults produce poor clusters.

The config resolution model (built-in defaults → global config → project config → CLI flags) already exists for all other config sections (labeling, embedding, observability, etc.) but is absent for ingest. The `--embed` flag is a boolean with no way to *disable* what config enables.

The existing `IngestIndex` type stores only `NodeId → [Double]` (embedding vectors), with no content hash tracking for deduplication.

## Goals / Non-Goals

**Goals:**
- Full `ingest:` config section in `graphos.yaml` with embed default, merge, deduplicate, cluster tuning, per-category overrides, URL settings, and index path.
- Built-in `defaultIngestConfig` has `embed: false` (backward compatible — no behavior change without config file).
- `graphos init` scaffold has `embed: true` (codebase-optimized default).
- Three-state CLI resolution: `--embed` (force on), `--no-embed` (force off), no flag (use config).
- Per-category embed/granularity overrides that inherit from top-level when `Nothing`.
- `IngestIndex` v2 format with `files` map for SHA256 deduplication, backward-compatible with v1.
- URL ingestion settings configurable (timeout, retry, user-agent).

**Non-Goals:**
- Per-directory overrides (use `.graphosignore` for exclusion).
- LSP extraction granularity changes (separate concern).
- Leiden algorithmic performance fixes (separate change).
- Streaming or batch ingest (single-file only for now).
- Changing the `file_extensions` built-in defaults (only the scaffold template changes).

## Decisions

### D1 — `IngestConfig` in Domain.Config (Domain layer)

New pure data types in `Graphos.Domain.Config.Ingest`:

```
IngestConfig
  icEmbed            :: Bool                -- default: False (backward compat)
  icEmbedModel       :: Maybe String        -- Nothing = inherit from embedding.model
  icEmbedDimension   :: Maybe Int           -- Nothing = inherit from embedding.dimension
  icMerge            :: Bool                -- default: True
  icDeduplicate      :: Bool                -- default: True
  icResolution       :: Double              -- default: 0.8 (smaller graphs)
  icMinCommSize      :: Int                 -- default: 2 (smaller communities)
  icMaxLeidenIter    :: Int                 -- default: 20 (converges fast)
  icIndexPath        :: String              -- default: "graphos-out/index.json"
  icUrl              :: IngestUrlConfig
  icCategories       :: IngestCategories

IngestUrlConfig
  iucTimeout    :: Int          -- default: 30
  iucUserAgent  :: String       -- default: "graphos/0.1.0"
  iucRetry      :: Int          -- default: 1

IngestCategoryConfig
  iccEmbed       :: Maybe Bool             -- Nothing = inherit from top-level
  iccGranularity :: Maybe Granularity      -- Nothing = inherit from top-level

IngestCategories
  icatCode     :: Maybe IngestCategoryConfig
  icatDoc      :: Maybe IngestCategoryConfig
  icatPaper    :: Maybe IngestCategoryConfig
  icatImage    :: Maybe IngestCategoryConfig
  icatVideo    :: Maybe IngestCategoryConfig
  icatOffice   :: Maybe IngestCategoryConfig
```

`defaultIngestConfig` has `icEmbed = False` for backward compatibility. The `graphos init` scaffold sets `embed: true`.

`GraphosConfig` gains `gcIngest :: IngestConfig`. `ConfigFile` gains `cfIngest :: Maybe IngestConfig`.

- **Alternatives considered:**
  - *Boolean-only embed override* — rejected: per-category control is essential (code vs images).
  - *Reusing `embedding:` section for ingest* — rejected: `embedding:` controls model/dimension/provider; ingest needs separate merge/dedup/cluster settings.
  - *Numeric embed level per category* — rejected: `Maybe Bool` is clearer (inherit or override) and matches the three-state CLI pattern.

### D2 — Three-state CLI resolution for embed

`IngestCmd` changes from `Bool` to `Maybe Bool`:

```
IngestCmd FilePath (Maybe Bool) FilePath
  -- file    embed-override  output
```

CLI flags:
- `--embed` → `Just True`
- `--no-embed` → `Just False`
- (no flag) → `Nothing`

Resolution in `app/Main.hs`:
```
effectiveEmbed = case cliEmbed of
  Just b  -> b               -- CLI wins
  Nothing -> icEmbed ingestCfg  -- config wins
```

Category-level resolution in `UseCase.Ingest`:
```
effectiveEmbedForCategory category =
  case categoryOverride of
    Just (IngestCategoryConfig (Just b) _) -> b    -- category override
    Just (IngestCategoryConfig Nothing _)   -> effectiveEmbed  -- inherit
    Nothing                                 -> effectiveEmbed  -- no category config
```

- **Alternatives considered:**
  - *Keep Bool and add --no-embed as separate flag* — rejected: `Maybe Bool` is the canonical three-state pattern; avoids parsing two conflicting flags.
  - *Config-only resolution (no --no-embed)* — rejected: users need CLI override for CI scripts.

### D3 — IngestIndex v2 format with deduplication

Extend `IngestIndex` with file-level dedup tracking:

```
IngestIndex
  iiVersion :: !Int                          -- format version (2)
  iiFiles   :: !(Map FilePath FileEntry)      -- source file → hash + timestamp
  iiNodes   :: !(Map NodeId [Double])          -- node → embedding vector

FileEntry
  feHash       :: !Text    -- SHA256 of source file content
  feIngestedAt :: !Text    -- ISO 8601 timestamp
```

Backward compatibility: on load, if `version` key absent → v1 format, `iiFiles` populated as empty (dedup disabled for that session). On save, always write v2.

Dedup logic in `UseCase.Ingest`:
1. Compute SHA256 of the file being ingested.
2. Look up `FilePath` in `iiFiles`.
3. If found and hash matches → skip extraction, return cached result.
4. If found and hash differs → re-extract, update entry.
5. If not found → extract, add entry.

- **Alternatives considered:**
  - *Separate dedup index file* — rejected: one file to manage, atomic updates, simpler.
  - *Hash in node metadata* — rejected: nodes can be shared across files (module imports); dedup is per-file, not per-node.

### D4 — Merge vs standalone mode

`icMerge` controls whether ingest output merges into the existing `graph.json`:

- `merge: true` (default) — load existing graph, extract single file, merge nodes/edges, re-cluster, write updated graph.
- `merge: false` — produce standalone `graph.json` for just the ingested file (useful for testing or isolated analysis).

Implementation: when `merge: false`, `runSingleFilePipeline` skips loading the existing graph and writes to a separate output (e.g., `graphos-out/ingest/<filename>/graph.json`).

- **Alternatives considered:**
  - *Separate CLI command for standalone* — rejected: `merge: false` in config is more discoverable and avoids CLI flag proliferation.
  - *Always merge* — rejected: standalone mode is useful for debugging and CI where you want to inspect a single file's graph.

### D5 — Per-category overrides with inheritance

`IngestCategories` has optional overrides per file category. Resolution:

```
resolveEmbedForCategory :: Bool -> IngestCategories -> FileCategory -> Bool
resolveEmbedForCategory topLevel cats category =
  case categoryConfig of
    Just cfg -> fromMaybe topLevel (iccEmbed cfg)  -- override or inherit
    Nothing  -> topLevel                            -- no category config
  where categoryConfig = case category of
          CodeFiles  -> icatCode cats
          DocFiles   -> icatDoc cats
          PaperFiles -> icatPaper cats
          ImageFiles -> icatImage cats
          VideoFiles -> icatVideo cats
          OfficeFiles -> icatOffice cats
```

Same pattern for `granularity`: `resolveGranularityForCategory`.

- **Alternatives considered:**
  - *Flat per-extension config (like extractors)* — rejected: categories are already the abstraction level users think at; per-extension would be hundreds of entries.
  - *No inheritance (require all fields)* — rejected: `Maybe` fields allow concise config; `inherit from top-level` is the common case.

### D6 — IngestUrlConfig for URL fetching

Current `ingest` function hardcodes timeout (no timeout), user-agent (none), and retry (0). New `IngestUrlConfig` fields:

- `iucTimeout :: Int` — seconds to wait for HTTP response (default: 30).
- `iucUserAgent :: String` — HTTP User-Agent header (default: `"graphos/0.1.0"`).
- `iucRetry :: Int` — number of retries on failure (default: 1).

These are used in `UseCase.Ingest` by passing them through to the HTTP client calls.

- **Alternatives considered:**
  - *Global HTTP config (not ingest-specific)* — rejected: URL fetching is only used by ingest; no other code path does HTTP GETs for content.
  - *No retry (keep current behavior)* — rejected: network operations fail transiently; 1 retry is standard.

### D7 — Scaffold template embed: true, built-in default embed: false

`defaultIngestConfig` has `icEmbed = False` — this preserves the current behavior where `graphos ingest file.hs` does not generate embeddings without `--embed`.

`graphos init` generates `graphos.yaml` with `embed: true` under `ingest:` — this is the recommended setting for codebase analysis.

This two-default pattern is intentional:
- **Users without a config file** (just `graphos ingest file.hs`) get backward-compatible behavior (no embeddings).
- **Users who run `graphos init`** get the recommended codebase-optimized config (embeddings on).

- **Alternatives considered:**
  - *Change built-in default to true* — rejected: breaking change; users who don't have a config file would suddenly start hitting Ollama for embeddings.
  - *Only CLI flag, no config default* — rejected: this is the current situation and it's the problem being solved.

## Risks / Trade-offs

- [Three-state CLI is unfamiliar] → `--embed`/`--no-embed` is intuitive; the `Maybe Bool` type is internal. Document in `--help`.
- [v2 index format breaks existing index.json] → backward-compatible load: if `version` key absent, treat as v1 (no dedup). Always save as v2. Test with both formats.
- [Per-category overrides add complexity] → `Maybe` fields mean the common case (no override) is just omitting the section. Only power users set category overrides.
- [Separate cluster params for ingest may confuse users] → clearly document in config comments and `--help` that ingest uses its own resolution/minCommSize/iterations, distinct from the full pipeline.
- [SHA256 computation for dedup adds latency] → SHA256 is fast (~1GB/s on modern hardware); for source files (<100KB typically) this is negligible. Dedup saves the much-more-expensive extraction + embedding step.

## Verification Strategy (Check)

- **Unit (Hspec, `cabal test`):**
  - `IngestConfig` FromJSON round-trip (all fields, partial fields with defaults, empty section).
  - `mergeConfig` and `mergeGraphosConfig` thread `gcIngest` correctly.
  - Three-state embed resolution: `Just True` → True, `Just False` → False, `Nothing` → config value.
  - Category resolution: `Nothing` inherits, `Just (Just True)` overrides to True, `Just Nothing` inherits.
  - v1 index.json loads without error (backward compat).
  - v2 index.json loads with `files` map.
  - Dedup logic: same hash → skip, different hash → re-extract, new file → add.

- **Integration (`cabal run graphos -- ingest`):**
  - `graphos ingest file.hs` with no config → no embeddings (backward compat).
  - `graphos ingest file.hs --embed` → embeddings generated.
  - `graphos ingest file.hs --no-embed` with `embed: true` in config → no embeddings (CLI wins).
  - `graphos ingest file.hs` with `embed: true` in config → embeddings generated (no flag needed).
  - `graphos init` → `graphos.yaml` contains `ingest:` section with `embed: true`.

- **Build gate:** `cabal build` clean with dev `-Wall -Werror` flags.

## Iteration & Rollback (Act)

- **If v2 index format causes issues in production:** fallback to v1 format by skipping dedup; both formats load correctly.
- **If per-category overrides are rarely used:** consider simplifying to top-level only in a future iteration.
- **Rollback:** remove `gcIngest` from `GraphosConfig`, revert CLI to `Bool`, remove v2 index fields. All changes are additive; rollback is clean.
- **Standardize:** document the `ingest:` section in PRD §14 and `docs/workflows/10-ingest.md` on archive.