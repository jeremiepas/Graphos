# Do — Task 2: IngestIndex v2 with deduplication helpers

## Changes Made

### `src/Graphos/Domain/Types/Ingest.hs`
- Added `lookupFileHash :: FilePath -> IngestIndex -> Maybe Text` — looks up stored SHA256 for a file path
- Added `addFileEntry :: FilePath -> FileEntry -> IngestIndex -> IngestIndex` — inserts file entry into iiFiles map
- Added `isFileUpToDate :: FilePath -> Text -> IngestIndex -> Bool` — compares current hash with stored hash, returns True if unchanged

## Implementation Details
- `lookupFileHash` delegates to `Map.lookup` on `iiFiles`, then extracts `feHash`
- `addFileEntry` uses `Map.insert` to update `iiFiles`
- `isFileUpToDate` uses `lookupFileHash` internally: matches → True, Nothing → False

## Build & Test
- `cabal build` — succeeded (33 modules)
- `cabal test` — 308 examples, 0 failures
