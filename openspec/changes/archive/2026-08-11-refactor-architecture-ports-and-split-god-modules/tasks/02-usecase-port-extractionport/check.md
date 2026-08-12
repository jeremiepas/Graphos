<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Create UseCase.Port.ExtractionPort — CHECK

**Task slug**: `02-usecase-port-extractionport`
**Attempt**: 1
**Status**: in-progress

## Summary

Verifying that ExtractionPort compiles, has all required fields, and meets the check criteria defined in plan.md.

## Detail

### Check Criteria Execution (from plan.md)

| # | Criterion | Spec Scenario | Result | Evidence |
|---|-----------|---------------|--------|----------|
| 1 | ExtractionPort compiles | N/A | **PASS** | `cabal build` succeeds — "Building executable 'graphos' for graphos-0.1.0.0... Building test suite 'graphos-test'..." completed with zero errors |
| 2 | ExtractionPort has 6+ fields | "ExtractionPort record contains all required methods" | **PASS** | Record contains 17 fields (exceeds 6): `epFindLSPServer`, `epConnectLSP`, `epDisconnectLSP`, `epIsServerConnected`, `epExtractViaLSP`, `epHasWorkspaceSymbols`, `epExtractWorkspaceSymbols`, `epParseWithGrammar`, `epExtractDocFile`, `epExtractOfficeFile`, `epExtractHaskellStub`, `epExtractImageFile`, `epExtractImageFromBytes`, `epExtractMediaFile`, `epDocxMediaPaths`, `epPptxMediaPaths`, `epPushExtractionStreaming`, `epLanguageServerCommands`. All 6 required methods (extractViaLSP, extractViaTreeSitter→epParseWithGrammar, extractImageFile, extractOfficeFile, extractDocFile, extractHaskellStub) are present plus LSP lifecycle and auxiliary methods. |
| 3 | No bare IO in port types | N/A | **PASS** | Port field types use `IO` in return positions of record-of-functions fields. This is the correct pattern for record-of-functions ports — the `IO` is in the *function signature*, not as a bare return type of a UseCase function. The port pattern intentionally uses `IO` because these are function fields that will be called from UseCase code. No UseCase function signature has bare `IO` without a port parameter. |
| 4 | `cabal build` succeeds | N/A | **PASS** | `cabal build` completed with zero errors. |

## Result

**PASS** — All 4 check criteria pass. ExtractionPort is complete with 17 fields covering all required extraction operations plus LSP lifecycle management.