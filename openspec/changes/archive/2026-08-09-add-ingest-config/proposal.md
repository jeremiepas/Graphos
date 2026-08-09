# Proposal: add-ingest-config

## Why

The `graphos ingest <file>` command has no configuration section in `graphos.yaml`. All ingest behavior is controlled via two CLI flags (`--embed` and `--output`), with the remaining settings either hardcoded or inherited from pipeline defaults that are tuned for full-codebase runs — not single-file ingestion.

Consequences:
- **No way to default `--embed` to true in config**: users must pass `--embed` every time, even though embeddings are the primary value of single-file ingest (semantic search over the codebase).
- **No per-category control**: images and videos shouldn't be embedded by default, but code and docs should. Currently it's all-or-nothing.
- **Cluster parameters are wrong for ingest**: single-file graphs are small (5-50 nodes). The pipeline defaults (resolution=1.0, minCommSize=3, maxLeidenIterations=50) produce poor communities on tiny graphs.
- **No deduplication**: re-ingesting an unchanged file wastes compute and creates duplicate nodes. There is no index tracking of what was ingested.
- **No merge control**: ingest always merges into the existing graph, with no way to produce a standalone per-file graph.
- **URL ingestion has no configurable timeout, retry, or user-agent**: these are hardcoded in `UseCase.Ingest`.
- **Config resolution is incomplete**: the documented priority order (built-in defaults → global config → project config → CLI flags) is not fully implemented for ingest — CLI `--embed` cannot *disable* what config enables.

## What Changes

- A new **`ingest:` config section** in `graphos.yaml` with all ingest-related settings: embed default, merge, deduplicate, cluster tuning, per-category overrides, URL settings, and index path.
- **`IngestConfig` domain type** in `Graphos.Domain.Config` (pure, no IO) with sensible defaults optimized for codebase analysis.
- **`IngestUrlConfig`** for URL fetch settings (timeout, retry, user-agent).
- **`IngestCategories`** with per-category embed/granularity overrides (inherit from top-level when `Nothing`).
- **`IngestIndex` v2 format** adding a `files` map (FilePath → SHA256 hash + timestamp) for deduplication, backward-compatible with v1.
- **CLI `--no-embed` flag** enabling three-state resolution: `Just True` (CLI `--embed`), `Just False` (CLI `--no-embed`), `Nothing` (use config).
- **`graphos init` scaffold** includes `ingest:` section with `embed: true` (the recommended codebase-optimized default), while the built-in `defaultIngestConfig` has `embed: false` (backward compatible).
- **`file_extensions` in scaffold**: only `.md` in doc, only `.pdf` in paper — optimized for codebases.

Out of scope: per-directory overrides, LSP extraction granularity changes, Leiden algorithmic fixes, streaming ingest.

## Capabilities

### New Capabilities
- `ingest-config`: full ingest configuration in `graphos.yaml` with embed default, merge, deduplicate, cluster tuning, per-category overrides, URL settings, and index path (workflows: 10-ingest).

### Modified Capabilities
- `ingest`: CLI gains `--no-embed` flag; `IngestCmd` embed field changes from `Bool` to `Maybe Bool` for three-state resolution (workflows: 10-ingest).
- `config-loader`: `ConfigFile` and `mergeConfig` gain `ingest` section; `GraphosConfig` gains `gcIngest` field (workflows: 01-full-pipeline, 02-incremental-pipeline, 10-ingest).
- `ingest-index`: `IngestIndex` gains v2 format with `files` map and `version` field for deduplication support (workflows: 10-ingest).

## Impact

- **Code**: `Domain.Config` (new `IngestConfig` types), `Domain.Types.Ingest` (v2 index format with `FileEntry`), `Domain.Types.Pipeline` (`cfgIngest` field), `Infrastructure.Config` (YAML parsing, merging), `UseCase.Ingest` (read from config, dedup, merge control, URL settings, category resolution), `UseCase.Pipeline` (thread ingest cluster settings), `UseCase.IngestIndex` (v2 load/save), `CLI.Parser` (`--no-embed`, `Maybe Bool`), `app/Main.hs` (ingest handler, config resolution, scaffold template update).
- **Behavior**: `graphos ingest` now reads cluster/embed/merge/dedup settings from config; `graphos init` generates `ingest:` section with codebase-optimized defaults; `--no-embed` can override `embed: true` in config.
- **Docs**: CHANGELOG entry for new config section and v2 index format; `docs/workflows/10-ingest.md` updated with config section reference.
- **Tests**: Hspec unit tests for IngestConfig FromJSON/toJSON, merge resolution, three-state CLI override, v2 index backward compatibility, category embed resolution.

## PDCA Cycle

- **Plan**: Hypothesis — single-file ingest needs its own config section with codebase-optimized defaults. Success criteria: (1) `graphos ingest file.hs` with `embed: true` in config generates embeddings without `--embed` flag; (2) `--no-embed` overrides config `embed: true`; (3) deduplication skips re-ingesting unchanged files; (4) single-file cluster uses resolution=0.8, minCommSize=2; (5) v1 index.json loads without error (backward compat); (6) `cabal test` passes.
- **Do**: Implement IngestConfig types, v2 IngestIndex, CLI `--no-embed`, config resolution, category overrides, dedup, scaffold update (see design.md, tasks.md).
- **Check**: `cabal test` for config parsing, merge resolution, index v1/v2 compat; manual test of `graphos ingest` with and without config; `graphos init` produces correct scaffold.
- **Act**: If category resolution is too complex for initial users, simplify to top-level only; if dedup SHA256 tracking has edge cases (binary files, encoding), document limitations.