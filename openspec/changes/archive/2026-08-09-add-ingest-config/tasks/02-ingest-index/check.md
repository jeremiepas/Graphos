# Check — Task 2: IngestIndex v2 with deduplication helpers

## Verification Results

### Build
- `cabal build` — PASS (33 modules compiled, no errors)
- `cabal test` — PASS (308 examples, 0 failures)

### Spec Compliance
- ✅ `lookupFileHash` returns `Just hash` when file exists in index, `Nothing` otherwise
- ✅ `addFileEntry` inserts entry into `iiFiles` map
- ✅ `isFileUpToDate` returns `True` when hash matches, `False` when different or missing
- ✅ Backward compatible: v1/v2 JSON loading still works (existing tests pass)
- ✅ `FileEntry` type already defined in `Config.Ingest` and imported here

## Test Coverage
- Existing 308 tests cover all IngestConfig, merge, and category resolution logic
- No new tests added yet (dedup tests will come in Task 5 with UseCase.Ingest integration)
