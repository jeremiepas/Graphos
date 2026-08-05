<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Create UseCase.Port.FileSystemPort, LoggingPort, ObservabilityPort, LLMPort — PLAN

**Task slug**: `04-usecase-remaining-ports`
**Attempt**: 1
**Status**: pending

## Summary

Create the remaining 4 port modules: FileSystemPort, LoggingPort, ObservabilityPort, and LLMPort. Each defines a record type with fields matching current Infrastructure function signatures.

## Detail

### Scope

Four modules to create/verify:
1. **FileSystemPort** (currently 21 lines) — fields for: `loadPipelineCheckpoint`, `savePipelineCheckpoint`, `clearPipelineCheckpoint`, `loadIgnorePatterns`, and canonical path resolution. Must NOT import Infrastructure.FileSystem.Ignore types directly (use Domain types or duplicate type if needed — see note about `AnnotatedPattern` in FileSystemPort which currently imports it).

2. **LoggingPort** (currently 22 lines) — fields for: `logInfo`, `logDebug`, `logTrace`, `logWarn`, `logError`. Simple 5-field record mapping to `Infrastructure.Logging` log levels.

3. **ObservabilityPort** (currently 27 lines) — fields for: span creation, metric recording, flushing. Maps to `Infrastructure.Observability.SDK`.

4. **LLMPort** (currently 46 lines) — fields for: `callLLM`, `parseLabelsFromResponse`, `generateEmbedding`, `analyzeImage`, `validateUrl`. Maps to `Infrastructure.LLM.OpenAI`, `Infrastructure.LLM.Embedding`, `Infrastructure.LLM.Vision`, `Infrastructure.Security`.

### Affected Modules

- `src/Graphos/UseCase/Port/FileSystemPort.hs` — port (21 lines, needs completion)
- `src/Graphos/UseCase/Port/LoggingPort.hs` — port (22 lines, needs completion)
- `src/Graphos/UseCase/Port/ObservabilityPort.hs` — port (27 lines, needs completion)
- `src/Graphos/UseCase/Port/LLMPort.hs` — port (46 lines, needs completion)

### Prerequisites

- Task 1 complete ✅
- Tasks 2-3 should be complete for pattern consistency

### Risks

- FileSystemPort currently imports `Infrastructure.FileSystem.Ignore (AnnotatedPattern)` — this violates the port principle (ports should NOT import Infrastructure). The `AnnotatedPattern` type must either be moved to Domain or duplicated in the port.
- LLMPort's `validateUrl` comes from `Infrastructure.Security` — the port must abstract over this cleanly.
- ObservabilityPort may need to handle OTel span context types without importing Infrastructure.

## Check Criteria

| # | Criterion | Spec Scenario | PASS Condition | FAIL Condition |
|---|-----------|---------------|----------------|-----------------|
| 1 | All 4 modules compile | N/A | `cabal build` succeeds | Build fails |
| 2 | FileSystemPort has checkpoint and ignore methods | "FileSystemPort contains checkpoint and ignore methods" | Record has `loadPipelineCheckpoint`, `savePipelineCheckpoint`, `clearPipelineCheckpoint`, `loadIgnorePatterns` | Missing any field |
| 3 | LoggingPort has 5 log levels | "LoggingPort provides all log levels" | Record has `logInfo`, `logDebug`, `logTrace`, `logWarn`, `logError` | Missing any level |
| 4 | ObservabilityPort has span and metric methods | N/A | Record has span creation and metric recording methods | Missing span or metric methods |
| 5 | LLMPort has callLLM, embedding, vision, and validation methods | "LLMPort contains all LLM methods" | Record has `callLLM`, `parseLabelsFromResponse`, `generateEmbedding`, `analyzeImage`, `validateUrl` | Missing any method |
| 6 | `cabal build` succeeds | N/A | Zero compilation errors | Any compilation error |