# Task 2: IngestIndex v2 with deduplication helpers

## Goal
Add helper functions to `IngestIndex` for SHA256-based file deduplication: `lookupFileHash`, `addFileEntry`, `isFileUpToDate`.

## Scope
- `src/Graphos/Domain/Types/Ingest.hs` — add 3 helper functions

## Success Criteria
- `lookupFileHash` returns stored hash or Nothing
- `addFileEntry` inserts file entry into iiFiles map
- `isFileUpToDate` returns True when hash matches, False otherwise
- `cabal build` succeeds
- `cabal test` passes (308 examples)
