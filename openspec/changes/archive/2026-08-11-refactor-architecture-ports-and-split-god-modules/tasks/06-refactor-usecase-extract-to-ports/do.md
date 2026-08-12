<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 6 — Refactor UseCase.Extract to use ExtractionPort — DO

**Task slug**: `06-refactor-usecase-extract-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. Replace all Infrastructure imports in UseCase.Extract with ExtractionPort from UseCase.Port.ExtractionPort. Add AppEnv parameter to all extraction functions.

## Detail

### What needs to be implemented

1. **Add `AppEnv` parameter to `extractAll`** and all extraction functions in `UseCase.Extract`
2. **Replace Infrastructure imports**:
   - `UseCase.Extract.hs` → replace `Infrastructure.LSP.Client`, `Infrastructure.Logging` with port calls
   - `UseCase.Extract.Image.hs` → replace `Infrastructure.LLM.Vision`, `Infrastructure.Logging` with `AppEnv.llmPort`, `AppEnv.loggingPort`
   - `UseCase.Extract.Office.hs` → replace `Infrastructure.FileSystem.OfficeConvert`, `Infrastructure.Logging` with `AppEnv.extractionPort`, `AppEnv.loggingPort`
   - `UseCase.Extract.Markdown.hs` → replace `Infrastructure.Logging` with `AppEnv.loggingPort`
3. **Update callers**: `UseCase.Pipeline.hs`, `app/Main.hs` must pass `AppEnv` to extraction functions
4. **Verify zero Infrastructure imports** in UseCase.Extract* (excluding Port modules)

### Current Infrastructure imports to eliminate

```
UseCase.Extract.Image.hs → Infrastructure.LLM.Vision, Infrastructure.Logging
UseCase.Extract.Office.hs → Infrastructure.FileSystem.OfficeConvert, Infrastructure.Logging
UseCase.Extract.Markdown.hs → Infrastructure.Logging
```

### Concrete changes needed

- Modify `src/Graphos/UseCase/Extract.hs` — add AppEnv parameter, use `extractionPort`
- Modify `src/Graphos/UseCase/Extract/Image.hs` — use `llmPort`, `loggingPort`
- Modify `src/Graphos/UseCase/Extract/Office.hs` — use `extractionPort`, `loggingPort`
- Modify `src/Graphos/UseCase/Extract/Markdown.hs` — use `loggingPort`
- Update `src/Graphos/UseCase/Pipeline.hs` — pass AppEnv to extraction calls
- Update `app/Main.hs` — pass AppEnv to extraction calls

## Result

NOT YET IMPLEMENTED — awaiting Do phase.