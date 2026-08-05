<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 8 — Refactor UseCase.Export, Ingest, Label, Detect to use ports — DO

**Task slug**: `08-refactor-usecase-others-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

NOT YET IMPLEMENTED. Replace all remaining Infrastructure imports across UseCase modules with port interfaces. This is the final sweep that eliminates all UseCase→Infrastructure violations.

## Detail

### What needs to be implemented

1. **UseCase.Export** (5 Infrastructure imports → exportPort):
   - `Infrastructure.Export.HTML` → `exportPort`
   - `Infrastructure.Export.Obsidian` → `exportPort`
   - `Infrastructure.Export.Report` → `exportPort`
   - `Infrastructure.Export.Neo4j` → `exportPort`
   - `Infrastructure.Export.Memgraph` → `exportPort`

2. **UseCase.Ingest** (3 Infrastructure imports → llmPort + loggingPort + security):
   - `Infrastructure.LLM.Embedding` → `llmPort`
   - `Infrastructure.Security (validateUrl)` → `llmPort.lpValidateUrl`
   - `Infrastructure.Logging (LogEnv, logInfo)` → `loggingPort`

3. **UseCase.IngestIndex** (1 Infrastructure import → llmPort):
   - `Infrastructure.LLM.Embedding` → `llmPort.lpGenerateEmbedding`

4. **UseCase.Label** (1 Infrastructure import → llmPort):
   - `Infrastructure.LLM.OpenAI (callLLM, parseLabelsFromResponse)` → `llmPort`

5. **UseCase.Detect** (1 Infrastructure import → fileSystemPort):
   - `Infrastructure.FileSystem.Ignore (AnnotatedPattern, shouldIgnore)` → need to resolve AnnotatedPattern type

### Known issue: AnnotatedPattern

`FileSystemPort` currently imports `AnnotatedPattern` from `Infrastructure.FileSystem.Ignore`. This must be resolved:
- **Option A**: Move `AnnotatedPattern` to Domain types
- **Option B**: Define a port-local type and convert in Wiring
- **Option C**: Re-export from FileSystemPort (current approach, violates port principle)

Decision should be made during implementation. Option A (move to Domain) is cleanest since `AnnotatedPattern` is a data type, not an IO operation.

### Concrete changes needed

- Modify `src/Graphos/UseCase/Export.hs` — add AppEnv parameter, use `exportPort`
- Modify `src/Graphos/UseCase/Ingest.hs` — use `llmPort`, `loggingPort`
- Modify `src/Graphos/UseCase/IngestIndex.hs` — use `llmPort`
- Modify `src/Graphos/UseCase/Label.hs` — use `llmPort`
- Modify `src/Graphos/UseCase/Detect.hs` — use `fileSystemPort`
- Fix `FileSystemPort` AnnotatedPattern import issue
- Update callers in Pipeline and Main

## Result

NOT YET IMPLEMENTED — awaiting Do phase.