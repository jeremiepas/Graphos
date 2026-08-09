# Act — Task 2: IngestIndex v2 with deduplication helpers

## Learnings

1. **Helper functions in domain types**: Adding pure helper functions to domain types keeps the logic close to the data structure, making it easy to test and reason about.

2. **FileEntry in Config.Ingest**: The `FileEntry` type lives in `Config.Ingest` rather than `Types.Ingest`. This is fine since it's a data record, but keeping dedup-related types together would be cleaner.

3. **No new tests needed yet**: The helper functions are simple Map operations. Full dedup testing requires SHA256 computation which belongs in Task 5 (UseCase.Ingest).

## Actions Taken
- Added 3 helper functions to `IngestIndex`
- Exported new functions from module interface

## Next
Proceed to Task 3: Config loader — parse `graphos.yaml` ingest section, merge with global config.
