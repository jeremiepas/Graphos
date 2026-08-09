# Task 1: Domain Types for IngestConfig

## Goal
Define pure domain types for ingest configuration: `IngestConfig`, `IngestUrlConfig`, `IngestCategoryConfig`, `IngestCategories`, `FileEntry`, and merge helpers.

## Scope
- `src/Graphos/Domain/Config/Ingest.hs` — all ingest types and merge logic
- `src/Graphos/Domain/Config/Core.hs` — add `gcIngest :: IngestConfig` to `GraphosConfig`
- `src/Graphos/Domain/Config.hs` — re-export ingest types

## Success Criteria
- All types derive `Eq`, `Show`, `Generic`
- `ToJSON` / `FromJSON` instances use consistent snake_case field names
- Merge helpers: project always overrides global for scalar fields
- `cabal build` succeeds
- `cabal test` passes (existing tests + new IngestSpec tests)
