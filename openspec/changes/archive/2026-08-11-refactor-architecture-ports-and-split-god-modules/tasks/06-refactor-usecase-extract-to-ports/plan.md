<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 6 — Refactor UseCase.Extract to use ExtractionPort — PLAN

**Task slug**: `06-refactor-usecase-extract-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

Replace all Infrastructure imports in `UseCase.Extract` (and its sub-modules) with `ExtractionPort` from `UseCase.Port.ExtractionPort`. This is the first major UseCase module to be port-wired, establishing the pattern for Tasks 7-8.

## Detail

### Scope

Refactor these modules to use ports instead of direct Infrastructure imports:
- `src/Graphos/UseCase/Extract.hs` — currently imports Infrastructure.LSP.Client, Infrastructure.Logging
- `src/Graphos/UseCase/Extract/Image.hs` — imports Infrastructure.LLM.Vision, Infrastructure.Logging
- `src/Graphos/UseCase/Extract/Office.hs` — imports Infrastructure.FileSystem.OfficeConvert, Infrastructure.Logging
- `src/Graphos/UseCase/Extract/Markdown.hs` — imports Infrastructure.Logging

All functions currently using direct IO must accept `AppEnv` (or `ExtractionPort`) parameter and delegate through port methods.

Current Infrastructure imports to eliminate:
```
UseCase.Extract.hs → Infrastructure.LSP.Client, Infrastructure.Logging
UseCase.Extract.Image.hs → Infrastructure.LLM.Vision, Infrastructure.Logging
UseCase.Extract.Office.hs → Infrastructure.FileSystem.OfficeConvert, Infrastructure.Logging
UseCase.Extract.Markdown.hs → Infrastructure.Logging
```

### Affected Modules

- `src/Graphos/UseCase/Extract.hs` — add AppEnv parameter, replace Infrastructure calls with port calls
- `src/Graphos/UseCase/Extract/Image.hs` — replace Infrastructure imports with port calls
- `src/Graphos/UseCase/Extract/Office.hs` — replace Infrastructure imports with port calls
- `src/Graphos/UseCase/Extract/Markdown.hs` — replace Infrastructure imports with port calls
- `app/Main.hs` — update call sites to pass AppEnv
- `src/Graphos/UseCase/Pipeline.hs` — update call sites (may call extractAll)

### Prerequisites

- Tasks 2-5 complete (ExtractionPort, AppEnv, Wiring)
- Task 1 complete ✅

### Risks

- `extractAll` currently has `IO` in its type signature — changing to `AppEnv -> IO` changes the public API; all callers must be updated
- Sub-modules (Image, Office, Markdown) need to thread the port through — consider passing ExtractionPort directly to sub-functions
- The 26 Infrastructure imports in UseCase must go to zero (minus Port modules); this is the first real test of the port pattern

## Check Criteria

| # | Criterion | Spec Scenario | PASS Condition | FAIL Condition |
|---|-----------|---------------|----------------|-----------------|
| 1 | No Infrastructure imports in UseCase.Extract | "No Domain-to-Infrastructure imports in UseCase.Extract" | `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/Extract.hs` returns zero | Any Infrastructure import found |
| 2 | No Infrastructure imports in UseCase.Extract sub-modules | "No Domain-to-Infrastructure imports in UseCase.Extract" | `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/Extract/*.hs` returns zero (excluding Port modules) | Any Infrastructure import found |
| 3 | `cabal build` succeeds | N/A | Zero compilation errors | Build failure |
| 4 | `cabal test` passes | N/A | All tests pass | Any test failure |
| 5 | `extractAll` takes AppEnv parameter | "UseCase.Extract delegates to port, not Infrastructure" | `extractAll` signature includes `AppEnv` (or `ExtractionPort`) parameter | Still takes individual IO args |