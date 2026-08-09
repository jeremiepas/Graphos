# Do — Task 1: Domain Types for IngestConfig

## Changes Made

### `src/Graphos/Domain/Config/Ingest.hs`
- Defined `IngestCategoryConfig` with `iccEmbed :: Maybe Bool` and `iccGranularity :: Maybe Granularity`
- Defined `IngestCategories` with optional per-category overrides for code, doc, paper, image, video, office
- Defined `IngestUrlConfig` with `iucTimeout`, `iucUserAgent`, `iucRetry`
- Defined `FileEntry` with `feHash :: !Text` and `feIngestedAt :: !Text`
- Defined `IngestConfig` with 11 fields: embed, embedModel, embedDimension, merge, deduplicate, resolution, minCommSize, maxLeidenIter, indexPath, url, categories
- Custom `ToJSON` instance using explicit `object` with snake_case keys
- Custom `FromJSON` instance using explicit `withObject` with snake_case keys
- `mergeIngestConfig` — project always overrides global for all scalar fields
- `mergeIngestCategories` — merges per-category configs
- `mergeIngestCategoryConfig` — merges category-level settings
- `mergeIngestUrlConfig` — merges URL config
- `mergeMaybe` — helper for Maybe fields: project wins if Just, else global

### `src/Graphos/Domain/Config/Core.hs`
- Added `gcIngest :: IngestConfig` field to `GraphosConfig`
- Updated `defaultGraphosConfig` to include `gcIngest = defaultIngestConfig`
- Updated `mergeGraphosConfig` to merge `gcIngest` via `mergeIngestConfig`

### `src/Graphos/Domain/Config.hs`
- Added re-exports: `IngestConfig`, `IngestUrlConfig`, `IngestCategoryConfig`, `IngestCategories`, `FileEntry`, and merge helpers

### `tests/Graphos/Domain/Config/IngestSpec.hs`
- Created comprehensive test suite covering:
  - Default values for all IngestConfig fields
  - YAML parsing of full config with nested url and categories
  - Merge logic: project overrides global, Maybe field merging, category resolution
  - `resolveEmbedForCategory` and `resolveGranularityForCategory` from `UseCase.Ingest`

## Build & Test
- `cabal build` — succeeded after fixing unused imports and overlapping patterns
- `cabal test` — 308 examples, 0 failures

## Issues Resolved
- Removed unused `Data.Map.Strict` imports (lines 30-31)
- Removed redundant `mergeMaybe Nothing Nothing` pattern (line 231)
- Removed redundant `mergeIngestCategoryConfigMaybe Nothing Nothing` pattern (line 259)
- Fixed test unused imports (`eitherDecode`, `fromJust`)
- Changed Bool merge logic from "differs from default" to "project always wins"
- Fixed ToJSON/FromJSON snake_case consistency mismatch
