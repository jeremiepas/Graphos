<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Create UseCase.Port.ExtractionPort — ACT

**Task slug**: `02-usecase-port-extractionport`
**Attempt**: 1
**Status**: PASS

## Summary

All 4 check criteria passed. ExtractionPort is complete with 17 fields covering all required extraction operations.

## Detail

### Check Outcome

| # | Criterion | Result |
|---|-----------|--------|
| 1 | ExtractionPort compiles | PASS |
| 2 | ExtractionPort has 6+ fields (spec: "contains all required methods") | PASS — 17 fields present |
| 3 | No bare IO in port types | PASS — IO in record-of-functions fields is correct pattern |
| 4 | `cabal build` succeeds | PASS |

### Standardized Outcome

**OK** — Task 2 PASSES. The ExtractionPort record type is complete, compiles, and contains all required methods. The 17-field design exceeds the original 6-field spec because the actual UseCase.Extract code requires fine-grained LSP lifecycle and extraction primitives rather than coarse-grained operations. This is an improvement, not a deviation.

### Decisions Recorded

- Used `Dynamic` + `unsafeCoerce` pattern for LSPHandle to avoid Infrastructure type leakage
- Defined `LSPHandle` and `SymbolResult` port-local types using Domain types
- 17 fields instead of 6 to cover LSP lifecycle, TreeSitter, file extraction, office media, Neo4j streaming, and config lookup

## Result

**PASS** — Task 2 complete. No further action needed.