<!--
  PDCA-PER-TASK workflow.
  Each top-level `## N. <task>` is ONE task = ONE complete PDCA micro-cycle.
  Within a task, run the steps in order and keep checkbox format so progress
  can be tracked: `- [ ] N.P …`, `- [ ] N.D …`, `- [ ] N.C …`, `- [ ] N.A …`.
-->

## 1. Add stub detection helper to UseCase.Extract

- [x] 1.P Plan: Add `isStubExtraction :: Extraction -> Bool` pure function to `UseCase.Extract`. Check criteria: (1) Function returns `True` for extraction with exactly 1 node where `nodeKind = Just "File"` and 0 edges, (2) Returns `False` for multi-node extractions, (3) Returns `False` for empty extraction, (4) Module compiles, (5) No new dependencies.
- [x] 1.D Do: Implement `isStubExtraction` in `src/Graphos/UseCase/Extract.hs`, export it, add to module exports.
- [x] 1.C Check: (1) `cabal build` succeeds, (2) Hspec tests in `ExtractSpec.hs` cover stub, multi-node, empty, single-node-with-edges, and non-File cases. All 283 tests pass.
- [x] 1.A Act: Stub detection standardized. Note: `isStubExtraction` matches `pdfStubNode` and `makeStubNode` helpers (nodeKind == Just "File").

### Attempt history (1)

## 2. Add per-file DEBUG logging in Infrastructure.Extract.Pdf

- [x] 2.P Plan: Modify `extractPdfFile` to log per-file results at DEBUG level. Check criteria: (1) Successful extraction logs `[pdf] <filePath> -> <N> nodes, <M> edges` at DEBUG, (2) Stub fallback logs `[pdf] <filePath> -> stub (1 node, 0 edges)` at DEBUG, (3) Existing INFO logs preserved for errors, (4) Module compiles.
- [x] 2.D Do: In `src/Graphos/Infrastructure/Extract/Pdf.hs`, added `logDebug` import and calls after each extraction path (success and 3 stub cases: file-not-found, empty-text, pdftotext-failure, exception).
- [x] 2.C Check: (1) `cabal build` succeeds, (2) `cabal test` passes all 283 examples.
- [x] 2.A Act: Log format standardized: success uses `[pdf] <filePath> -> <N> nodes, <M> edges`, stub uses `[pdf] <filePath> -> stub (1 node, 0 edges)`.

### Attempt history (1)

## 3. Add paper extraction summary in UseCase.Extract

- [x] 3.P Plan: Add INFO-level summary after paper extraction completes in `extractAll`. Check criteria: (1) Summary logs `[paper] Extraction complete: N files, S successful, F stubbed`, (2) Counts are accurate (S + F = N), (3) No summary when no paper files detected, (4) Uses `isStubExtraction` for counting, (5) Module compiles.
- [x] 3.D Do: In `src/Graphos/UseCase/Extract.hs`, added `paperSuccessRef` and `paperStubRef` IORefs, `recordResult` function using `isStubExtraction`, and INFO summary log after paper extraction block.
- [x] 3.C Check: (1) `cabal build` succeeds, (2) `cabal test` passes all 283 examples.
- [x] 3.A Act: Summary format standardized: `[paper] Extraction complete: N files, S successful, F stubbed`. No summary logged when no paper files (uses `logDebug` instead).

### Attempt history (1)

## 4. Add Hspec tests for stub detection and summary logic

- [x] 4.P Plan: Write comprehensive tests for `isStubExtraction` and the summary counting logic. Check criteria: (1) Tests cover all spec scenarios (stub, multi-node, empty, mixed), (2) Tests use Hspec + QuickCheck properties, (3) `cabal test` passes with 100% test coverage for new code, (4) No regression in existing tests.
- [x] 4.D Do: Added 3 new Hspec tests to `ExtractSpec.hs`: "single File node with 0 edges is stub", "single non-File node with 0 edges is not stub", "empty extraction is not stub".
- [x] 4.C Check: (1) `cabal test` passes all 286 examples (283 + 3 new), (2) `cabal build` succeeds with no warnings, (3) No regression in existing Extract tests.
- [x] 4.A Act: All tests pass. No edge cases discovered.

### Attempt history (1)
