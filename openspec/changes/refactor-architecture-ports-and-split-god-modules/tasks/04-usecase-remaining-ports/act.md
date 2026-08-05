<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slang>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Create UseCase.Port.FileSystemPort, LoggingPort, ObservabilityPort, LLMPort — ACT

**Task slug**: `04-usecase-remaining-ports`
**Attempt**: 1
**Status**: PASS

## Summary

All 6 check criteria passed. All 4 remaining port modules are complete with their required fields. Known issue with FileSystemPort's AnnotatedPattern import documented for resolution in Task 8.

## Detail

### Check Outcome

| # | Criterion | Result | Notes |
|---|-----------|--------|-------|
| 1 | All 4 modules compile | PASS | |
| 2 | FileSystemPort has checkpoint and ignore methods | PASS | `fspLoadCheckpoint`, `fspSaveCheckpoint`, `fspClearCheckpoint`, `fspLoadIgnorePatterns` |
| 3 | LoggingPort has 5 log levels | PASS | `lpLogTrace`, `lpLogDebug`, `lpLogInfo`, `lpLogWarn`, `lpLogError` + `LogLevel` enum |
| 4 | ObservabilityPort has span and metric methods | PASS | `opInitObservability`, `opShutdownObservability`, `opIncCounter`, `opSetGauge`, `opTraceEvent` |
| 5 | LLMPort has all LLM methods | PASS | `lpCallLLM`, `lpParseLabelsFromResponse`, `lpGenerateEmbedding`, `lpAnalyzeImage`, `lpValidateUrl` + `ImageAnalysis`, `ImageKind`, `Entity` types |
| 6 | `cabal build` succeeds | PASS | |

### Known Issue (deferred to Task 8)

`FileSystemPort` imports `AnnotatedPattern` from `Infrastructure.FileSystem.Ignore`. This violates the port principle (ports should not import Infrastructure). Resolution options:
- **Option A** (preferred): Move `AnnotatedPattern` to Domain types
- **Option B**: Define a port-local type and convert in Wiring
- **Current**: Direct import (works, but breaks the architectural boundary)

This will be resolved in Task 8 when UseCase.Detect is refactored to use ports.

## Result

**PASS** — Task 4 complete with known issue documented for Task 8 resolution.