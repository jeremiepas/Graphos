# Spec: ingest-index

## ADDED Requirements

### Requirement: IngestIndex supports v2 format with file-level deduplication

The system SHALL extend `IngestIndex` with a `files` map that tracks per-file SHA256 hashes and ingestion timestamps, and it SHALL remain backward-compatible with existing v1 index files.

- **Plan:** Add `iiVersion :: !Int` and `iiFiles :: !(Map FilePath FileEntry)` to `IngestIndex`, define `FileEntry`, and implement v1/v2 JSON loading logic.
- **Do:** Modify `Domain.Types.Ingest` JSON instances so a missing `version` key implies v1 with `iiFiles = Map.empty`, and saves always write v2. Add helper functions `lookupFileHash`, `addFileEntry`, `isFileUpToDate`.
- **Check:** `cabal test` passes with v1 and v2 index round-trips.
- **Act:** If v1 compatibility proves complex, keep `iiFiles = Map.empty` for v1 loads.

#### Scenario: v1 index loads without error
- **WHEN** an existing `index.json` without a `version` key is loaded
- **THEN** `iiVersion` is `1` and `iiFiles` is empty

#### Scenario: v2 index round-trips
- **WHEN** a v2 `index.json` containing a `files` map is loaded and saved
- **THEN** the loaded structure matches the original

### Requirement: SHA256-based deduplication decisions

The system SHALL use the `files` map to skip unchanged files, re-extract changed files, and add new files.

- **Plan:** Implement `isFileUpToDate` that compares the current file SHA256 with the stored entry.
- **Do:** Use the helper in `UseCase.Ingest` to gate extraction when `icDeduplicate` is enabled.
- **Check:** Same hash returns `True`, different hash returns `False`, missing file returns `False`.
- **Act:** If SHA256 computation needs a dependency, prefer what is already in the dependency tree.

#### Scenario: Unchanged file is skipped
- **WHEN** the file hash matches the stored hash
- **THEN** `isFileUpToDate` returns `True`

#### Scenario: Modified file is re-extracted
- **WHEN** the file hash differs from the stored hash
- **THEN** `isFileUpToDate` returns `False`

#### Scenario: New file has no stored entry
- **WHEN** the file path is absent from `iiFiles`
- **THEN** `isFileUpToDate` returns `False`
