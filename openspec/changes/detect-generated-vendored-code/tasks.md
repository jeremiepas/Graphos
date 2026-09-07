## 1. Domain

- [x] 1.1 Add `FileClass` (`Source | Generated | Vendored | Minified`) and `DetectionMode` (`Exclude | Collapse | Off`) types with explicit exports
- [x] 1.2 Add `DetectionConfig` (generator signatures, vendor segments, `minifiedLineThreshold`, mode) with a validating smart constructor
- [x] 1.3 Implement pure `classifyFile :: DetectionConfig -> FileMeta -> FileClass`
- [x] 1.4 Write Hspec/QuickCheck tests for classifier (header, vendor, minified, negative cases)

## 2. Config

- [x] 2.1 Extend Config loader with a `detection:` block mapping to `DetectionConfig`
- [x] 2.2 Provide defaults (`node_modules`, `vendor`, `third_party`; threshold 5000; mode Exclude)
- [x] 2.3 Add CLI flags `--detect-mode`, `--minified-threshold`, `--no-detect`

## 3. Infrastructure

- [x] 3.1 Add leading-content reader (first 40 lines / 8 KB) producing `FileMeta`
- [x] 3.2 Add path-segment vendor matcher
- [x] 3.3 Emit INFO detection summary after Detect

## 4. UseCase / Pipeline

- [x] 4.1 Wire classification into the Detect stage before Extract
- [x] 4.2 Implement Exclude and Collapse behaviors (collapse emits one node with `childCount`)
- [x] 4.3 Thread detection counts into the pipeline result for reporting

## 5. Verification

- [x] 5.1 `cabal build --flag dev` passes with `-Werror`
- [x] 5.2 `cabal test` green including new classifier suite
- [x] 5.3 Smoke run on a repo with a generated bindings file confirms it is excluded/collapsed and no >5%-of-graph community remains
