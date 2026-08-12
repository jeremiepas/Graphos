<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 8 — Refactor UseCase.Export, Ingest, Label, Detect to use ports — PLAN

**Task slug**: `08-refactor-usecase-others-to-ports`
**Attempt**: 1
**Status**: pending

## Summary

Replace remaining Infrastructure imports in UseCase.Export, UseCase.Ingest, UseCase.IngestIndex, UseCase.Label, and UseCase.Detect with port interfaces. This eliminates all remaining UseCase→Infrastructure violations.

## Detail

### Scope

Refactor these modules to use ports:
- **UseCase.Export** (5 Infrastructure imports: Export.HTML, Export.Obsidian, Export.Report, Export.Neo4j, Export.Memgraph) → use `exportPort`
- **UseCase.Ingest** (3 Infrastructure imports: LLM.Embedding, Security.validateUrl, Logging.logInfo) → use `llmPort` + `loggingPort`
- **UseCase.IngestIndex** (1 Infrastructure import: LLM.Embedding) → use `llmPort`
- **UseCase.Label** (1 Infrastructure import: LLM.OpenAI.callLLM, parseLabelsFromResponse) → use `llmPort`
- **UseCase.Detect** (1 Infrastructure import: FileSystem.Ignore.AnnotatedPattern, shouldIgnore) → use `fileSystemPort`

### Affected Modules

- `src/Graphos/UseCase/Export.hs`
- `src/Graphos/UseCase/Ingest.hs`
- `src/Graphos/UseCase/IngestIndex.hs`
- `src/Graphos/UseCase/Label.hs`
- `src/Graphos/UseCase/Detect.hs` and `src/Graphos/UseCase/Detect/` sub-modules

### Prerequisites

- Tasks 2-7 complete (all ports, AppEnv, Wiring, Extract and Pipeline refactored)

### Risks

- UseCase.Detect imports `AnnotatedPattern` from Infrastructure — this type needs to be in Domain or the port must abstract over it
- UseCase.Ingest needs both `llmPort` and `loggingPort` — functions must accept `AppEnv` to get both
- UseCase.IngestIndex uses embedding functions — LLMPort must expose `generateEmbedding`

## Check Criteria

| # | Criterion | Spec Scenario | PASS Condition | FAIL Condition |
|---|-----------|---------------|----------------|-----------------|
| 1 | No Infrastructure imports in UseCase (excluding Port modules) | "No Infrastructure imports in UseCase" | `rg "^import.*Graphos\.Infrastructure" src/Graphos/UseCase/` returns zero (excluding Port modules and re-export modules) | Any Infrastructure import found |
| 2 | `cabal build` succeeds | N/A | Zero compilation errors | Build failure |
| 3 | `cabal test` passes | N/A | All tests pass | Any test failure |