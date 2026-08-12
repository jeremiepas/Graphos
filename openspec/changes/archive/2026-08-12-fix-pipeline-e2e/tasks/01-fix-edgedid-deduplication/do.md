<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 1 — Fix EdgeId deduplication in production code — DO

**Task slug**: `01-fix-edgedid-deduplication`
**Attempt**: 1
**Status**: PASS

## Summary

Replaced all `EdgeId ""` constructions in production edge-creation code with deterministic composite keys of the form `source->target:relation`, eliminating silent deduplication caused by `Map.fromList` dropping duplicate keys.

## Detail

### Implementation

- **Primary file modified**: `src/Infrastructure/LSP/Extraction.hs`
- **Function `makeEdge`**: Changed the `EdgeId` construction from `EdgeId ""` to `EdgeId (source <> "->" <> target <> ":" <> relationToText relation)`. This is the single edge-creation smart constructor used throughout the extraction pipeline.
- **Function `symbolTreeToEdges`**: Updated to use `makeEdge` with proper source/target/relation parameters instead of placeholder empty `EdgeId ""`.
- **Test helpers**: Updated any test smart constructors and test fixtures that created edges with `EdgeId ""` to use the new composite key format.

### Key decisions

1. **Composite key format** `source->target:relation` chosen over hash-based keys (e.g. SHA256) because: (a) human-readable in debug output, (b) deterministic and easy to verify, (c) sufficiently unique since the triple (source, target, relation) is functionally unique in the AST.
2. **No Domain/UseCase changes** — EdgeId construction is purely an Infrastructure concern. The Domain `Edge` type and `EdgeId` newtype remain untouched.
3. **No backward compatibility concern** — this is version 0.1.0.0; no external consumers exist for edge IDs.

### Concrete changes

- `src/Infrastructure/LSP/Extraction.hs`: `makeEdge` EdgeId constructor updated
- `src/Infrastructure/LSP/Extraction.hs`: `symbolTreeToEdges` edge creation path updated
- Test files: any edge construction with `EdgeId ""` replaced with composite key format

## Result

**PASS**

- `cabal build` — zero warnings (exited 0)
- `cabal test` — 90/90 tests passing (exited 0)
- `grep -r 'EdgeId ""' src/` — zero matches (exit code 1)
- All edge IDs now follow `source->target:relation` format
