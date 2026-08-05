<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Create UseCase.Port.ExtractionPort — PLAN

**Task slug**: `02-usecase-port-extractionport`
**Attempt**: 1
**Status**: pending

## Summary

Create the `UseCase.Port.ExtractionPort` record type that decouples UseCase.Extract from direct Infrastructure imports. This port will define fields for LSP extraction, TreeSitter extraction, image extraction, office extraction, markdown extraction, and Haskell stub extraction.

## Detail

### Scope

This task creates a single new module: `src/Graphos/UseCase/Port/ExtractionPort.hs`. The module already exists (63 lines) with a partial implementation. The task is to verify and complete it so it satisfies the spec scenarios.

The ExtractionPort record must contain methods that mirror the current Infrastructure function signatures used by UseCase.Extract:
- `extractViaLSP` — mirrors `Infrastructure.LSP.Client.extractFilesWithLSP` and related LSP extraction
- `extractViaTreeSitter` — mirrors `Infrastructure.Extract.TreeSitter` extraction
- `extractImageFile` — mirrors `Infrastructure.LLM.Vision` image extraction
- `extractOfficeFile` — mirrors `Infrastructure.FileSystem.OfficeConvert` office extraction
- `extractDocFile` — mirrors markdown/doc extraction
- `extractHaskellStub` — mirrors Haskell stub extraction

### Affected Modules

- `src/Graphos/UseCase/Port/ExtractionPort.hs` — the new/updated port module (already exists, may need completion)
- `src/Graphos/UseCase/Extract.hs` — currently imports Infrastructure directly (will be refactored in Task 6, not this task)
- `src/Graphos/UseCase/Extract/Image.hs` — imports Infrastructure.LLM.Vision
- `src/Graphos/UseCase/Extract/Office.hs` — imports Infrastructure.FileSystem.OfficeConvert
- `src/Graphos/UseCase/Extract/Markdown.hs` — imports Infrastructure.Logging

### Prerequisites

- Task 1 (Domain.Config split) is complete ✅

### Risks

- Port field types must match actual Infrastructure signatures precisely — any mismatch will surface during Task 6 (wiring)
- `MonadIO m =>` constraints may be needed for some port fields; avoid bare `IO` in port signatures

## Check Criteria

| # | Criterion | Spec Scenario | PASS Condition | FAIL Condition |
|---|-----------|---------------|----------------|-----------------|
| 1 | ExtractionPort compiles | N/A | `cabal build` succeeds with no errors | Build fails |
| 2 | ExtractionPort has 6+ fields | "ExtractionPort record contains all required methods" | Record contains: `extractViaLSP`, `extractViaTreeSitter`, `extractImageFile`, `extractOfficeFile`, `extractDocFile`, `extractHaskellStub` | Missing any required field |
| 3 | No bare IO in port types | N/A | Port field types use `MonadIO m =>` or `IO` is wrapped in domain-appropriate types, not bare `IO` in UseCase-facing signatures | Bare `IO` return type without constraint |
| 4 | `cabal build` succeeds | N/A | Zero compilation errors | Any compilation error |