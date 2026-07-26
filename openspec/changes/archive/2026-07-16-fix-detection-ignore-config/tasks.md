<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  PASS rule: task PASSES only when its Check passes AND its Act is OK.
  RETRY rule: if Act is NOT OK, record the failed attempt, start a new P→D→C→A.
-->

## 1. Expand hardcoded ignore list

- [x] 1.P Plan: Expand `isIgnored` in `UseCase.Detect` from 12 to ~30 entries covering common build artifacts, dependency directories, IDE folders, and cache directories. Check criteria: (1) All original entries remain, (2) New entries include `target/`, `vendor/`, `.next/`, `.gradle/`, `.idea/`, `.vscode/`, `.cache/`, `.cargo/`, etc., (3) `cabal test` passes. Affected: `src/Graphos/UseCase/Detect.hs`.
- [x] 1.D Do: Add the following entries to `isIgnored`: `target`, `vendor`, `.next`, `.nuxt`, `.gradle`, `.idea`, `.vscode`, `.cache`, `.cargo`, `bower_components`, `.direnv`, `.sass-cache`, `coverage`, `.pytest_cache`, `.mypy_cache`, `.tox`, `__pypackages__`, `.pdm-build`, `.yarn`, `.pnpm-store`, `.svn`, `.hg`, `.DS_Store`, `DerivedData`, `.build`, `.sass-cache`, `.elixir_ls`, `.clj-kondo`, `.lsp`. Keep all existing entries.
- [x] 1.C Check: (1) `grep "target\|vendor\|.next\|.gradle\|.idea" src/Graphos/UseCase/Detect.hs` returns matches. (2) All 12 original entries still present. (3) `cabal test` passes.
- [x] 1.A Act: If any new entry causes a false positive (e.g., a project has a `vendor/` directory that should be processed), it can be re-included via `.graphosignore` negation. Otherwise mark done.

### Attempt history (1)

<!-- empty unless a retry is needed -->

## 2. Add loadGitignore and NegatePattern to Ignore module

- [x] 2.P Plan: Add `loadGitignore :: FilePath -> IO [IgnorePattern]` and `NegatePattern` type to `Infrastructure.FileSystem.Ignore`. Support blank lines, comments (`#`), directory patterns (`dir/`), glob patterns (`*.log`), and negation (`!pattern`). Check criteria: (1) `loadGitignore` reads and parses `.gitignore` correctly, (2) `shouldIgnore` respects `NegatePattern`, (3) `cabal test` passes, (4) Hspec tests for all pattern types. Affected: `src/Graphos/Infrastructure/FileSystem/Ignore.hs`, new test file `tests/Graphos/Infrastructure/FileSystem/IgnoreSpec.hs`.
- [x] 2.D Do: (a) Add `NegatePattern IgnorePattern` to `IgnorePattern` type. (b) Add `loadGitignore` that reads `.gitignore`, parses lines (skip blanks/comments, handle `!` prefix as `NegatePattern`, handle `/` suffix as `PrefixPattern`, handle `*` as `GlobPattern`, otherwise `ExactPattern`). (c) Update `shouldIgnore` to handle `NegatePattern`: if a path matches a `NegatePattern`, it is NOT ignored even if a lower-priority pattern matched. (d) Add `mergeIgnorePatterns :: [[IgnorePattern]] -> [IgnorePattern]` that concatenates with negation semantics. (e) Write Hspec tests.
- [x] 2.C Check: (1) Test `loadGitignore` with a temp `.gitignore` containing `node_modules/`, `*.log`, `!important.log` — verify patterns parsed correctly. (2) Test `shouldIgnore [PrefixPattern "node_modules"] "node_modules/pkg/index.js"` returns True. (3) Test negation: `shouldIgnore [GlobPattern ".log", NegatePattern (ExactPattern "important.log")] "important.log"` returns False. (4) `cabal test` passes.
- [x] 2.A Act: If `.gitignore` parsing has edge cases (e.g., escaped `#`), document them as known limitations. Otherwise mark done.

### Attempt history (2)

<!-- empty unless a retry is needed -->

## 3. Wire .graphosignore and .gitignore into detection

- [x] 3.P Plan: Load `.graphosignore` and `.gitignore` patterns in `Pipeline.hs`, merge with hardcoded defaults, and pass to `detectFilesWithExtensions`. Update `findAllFilesWith` signature to accept `[IgnorePattern]` and apply `shouldIgnore`. Check criteria: (1) `.graphosignore` patterns are respected during detection, (2) `.gitignore` patterns are respected, (3) Hardcoded defaults still apply, (4) `cabal test` passes. Affected: `src/Graphos/UseCase/Detect.hs`, `src/Graphos/UseCase/Pipeline.hs`.
- [x] 3.D Do: (a) Add `IgnorePattern` import to `Pipeline.hs`. (b) In `runPipeline`, load patterns: `graphosPatterns <- loadGraphosignore inputPath`, `gitPatterns <- loadGitignore inputPath`, merge with `hardcodedIgnorePatterns`. (c) Pass merged patterns to `detectFilesWithExtensions`. (d) Update `findAllFilesWith` to accept `[IgnorePattern]` parameter. (e) Update `isIgnored` to use both the hardcoded list AND `shouldIgnore` with the merged patterns. (f) Export `hardcodedIgnorePatterns :: [IgnorePattern]` from `Detect` for use in tests.
- [x] 3.C Check: (1) Create temp project with `.graphosignore` containing `src/internal/` — verify `src/internal/` files are not detected. (2) Create temp project with `.gitignore` containing `*.log` — verify `.log` files are not detected. (3) Run on a project with `node_modules/` — verify `node_modules/` files are not detected. (4) `cabal test` passes.
- [x] 3.A Act: If function signature changes break other call sites, update them. If performance degrades from pattern matching, consider caching compiled patterns. Otherwise mark done.

### Attempt history (3)

<!-- empty unless a retry is needed -->

## 4. Switch Pipeline to config-driven detection

- [x] 4.P Plan: Change `Pipeline.hs` to call `detectFilesWithExtensions` with `gcFileExtensions` from `GraphosConfig` instead of `detectFiles`. Pass `GraphosConfig` through `PipelineConfig`. Check criteria: (1) `graphos.yaml` `file_extensions` controls what gets detected, (2) No config file → same behavior as before (defaults match `allSupportedExtensions`), (3) `cabal test` passes. Affected: `src/Graphos/UseCase/Pipeline.hs`, `src/Graphos/UseCase/Detect.hs`.
- [x] 4.D Do: (a) Change `detectFiles (cfgInputPath configWithStreaming)` to `detectFilesWithExtensions (cfgInputPath configWithStreaming) (gcFileExtensions (cfgGraphosConfig configWithStreaming)) ignorePatterns` in `Pipeline.hs`. (b) Verify `PipelineConfig` already contains `cfgGraphosConfig :: GraphosConfig` (it does). (c) Update `detectFilesWithExtensions` to accept the `[IgnorePattern]` parameter from task 3. (d) Ensure the call chain: `runPipeline` → `loadIgnorePatterns` → `detectFilesWithExtensions`.
- [x] 4.C Check: (1) Create `graphos.yaml` with `file_extensions: { code: [.py] }` — verify only `.py` files are detected as code. (2) Remove `graphos.yaml` — verify all default extensions are detected. (3) `cabal test` passes. (4) Run `graphos .` on a project without `graphos.yaml` — verify behavior matches current (pre-change) behavior.
- [x] 4.A Act: If `detectFilesWithExtensions` signature change breaks other callers, update them. If default extensions don't match old `allSupportedExtensions`, fix the defaults. Otherwise mark done.

### Attempt history (4)

<!-- empty unless a retry is needed -->

## 5. Integration test and documentation

- [x] 5.P Plan: Write Hspec integration tests covering the full detection flow: config-driven extensions, `.graphosignore`, `.gitignore`, negation, and hardcoded defaults. Check criteria: (1) All scenarios from specs pass, (2) `cabal test` passes, (3) Test coverage for `Ignore` module > 80%. Affected: new test files.
- [x] 5.D Do: Write `tests/Graphos/Infrastructure/FileSystem/IgnoreSpec.hs` with tests for: (a) `loadGraphosignore` parsing, (b) `loadGitignore` parsing, (c) `shouldIgnore` with all pattern types, (d) negation patterns, (e) merged pattern priority. Write `tests/Graphos/UseCase/DetectSpec.hs` with tests for: (a) config-driven detection, (b) hardcoded ignore list, (c) file pattern filtering. Add tests to `graphos.cabal`.
- [x] 5.C Check: (1) `cabal test` passes with all new tests. (2) Test coverage for `Ignore` module > 80% (manual check). (3) All spec scenarios from `gitignore-support/spec.md` and `detection/spec.md` have corresponding test cases.
- [x] 5.A Act: If any test reveals a bug in the implementation, fix it and re-run. If all tests pass, mark done.

### Attempt history (5)

<!-- empty unless a retry is needed -->