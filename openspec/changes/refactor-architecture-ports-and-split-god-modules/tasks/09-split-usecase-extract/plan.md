<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 9 — Split UseCase.Extract into focused sub-modules — PLAN

**Task slug**: `09-split-usecase-extract`
**Attempt**: 1
**Status**: pending

## Summary

Split `UseCase.Extract` (574 lines) into `UseCase.Extract.Core` (pure orchestration), `UseCase.Extract.LSP` (LSP workflow), and `UseCase.Extract.TreeSitter` (TreeSitter fallback). The original `UseCase.Extract` becomes a backward-compatible re-export module.

## Detail

### Scope

Split `src/Graphos/UseCase/Extract.hs` (574 lines) into:
- **UseCase.Extract.Core** — `extractAll` function signature and core orchestration logic, port-delegated calls
- **UseCase.Extract.LSP** — LSP-specific extraction (`extractFilesWithLSP`, `extractWorkspaceSymbols`)
- **UseCase.Extract.TreeSitter** — TreeSitter-specific extraction (`extractViaTreeSitterFFI`)

The existing sub-modules (`Image.hs`, `Office.hs`, `Markdown.hs`) remain as-is but should use port calls (already done in Task 6).

Original `UseCase.Extract.hs` becomes a re-export module (<30 lines).

### Spec Scenarios

- **SC1**: UseCase.Extract is a re-export module (<30 lines, no implementation logic)
- **SC2**: UseCase.Extract.Core contains pure orchestration with `extractAll` and no Infrastructure imports
- **SC3**: UseCase.Extract.LSP contains LSP workflow (<300 lines)
- **SC4**: UseCase.Extract.TreeSitter contains TreeSitter workflow (<200 lines)
- **SC5**: Existing imports still compile
- **SC6**: God module graph edges redistributed (Core <100, LSP <80, TreeSitter <40)

### Affected Modules

- `src/Graphos/UseCase/Extract.hs` → becomes re-export
- `src/Graphos/UseCase/Extract/Core.hs` → new
- `src/Graphos/UseCase/Extract/LSP.hs` → new
- `src/Graphos/UseCase/Extract/TreeSitter.hs` → new
- `src/Graphos/UseCase/Pipeline.hs` — may need import updates
- `app/Main.hs` — may need import updates
- `.cabal` file — add new exposed-modules

### Prerequisites

- Task 6 complete (Extract uses ports, no Infrastructure imports)
- Task 1 complete ✅

### Risks

- Existing sub-modules (Image, Office, Markdown) already exist — Core/LSP/TreeSitter must not conflict
- Circular imports between Core and sub-modules — Core should call LSP/TreeSitter, not vice versa
- Re-export module must export ALL public symbols that the original module exported

## Check Criteria

| # | Criterion | Spec Scenario | PASS Condition | FAIL Condition |
|---|-----------|---------------|----------------|-----------------|
| 1 | UseCase.Extract is re-export only | SC1 | `src/Graphos/UseCase/Extract.hs` is <30 lines, no implementation logic | More than 30 lines or contains logic |
| 2 | Sub-modules are focused | SC2, SC3, SC4 | Core <300 lines (orchestration), LSP <300 lines, TreeSitter <200 lines | Any sub-module exceeds size limit |
| 3 | `cabal build` succeeds | SC5 | Zero compilation errors | Build failure |
| 4 | `cabal test` passes | SC5 | All tests pass | Any test failure |
| 5 | Existing imports still compile | SC5 | All original import paths still resolve | Import breakage |