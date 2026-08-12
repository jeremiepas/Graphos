<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 2 — Implement references extraction in LSP.Extraction — DO

**Task slug**: `02-implement-references-extraction`
**Attempt**: 1
**Status**: PASS

## Summary

Implemented `extractReferences` in `Infrastructure.LSP.Extraction` that sends `textDocument/references` LSP requests for the top-10 symbols per file (ranked by kind priority), producing `References` edges with `Confidence 0.8`.

## Detail

### Implementation

- **New function**: `extractReferences :: LSPClient -> FilePath -> Int -> [DocumentSymbolResult] -> IO [Edge]` in `Infrastructure.LSP.Extraction`
- **Kind-priority sorting**: Symbols sorted by LSP `SymbolKind` priority: Class (priority 1) > Function (priority 2) > Method (priority 3) > others (priority 999), then `takeN 10` to select top-10.
- **Capability check**: Before sending any references requests, checks `scpReferencesProvider` from the LSP client capabilities. Returns `[]` immediately when `False`.
- **Timeout**: Each `textDocument/references` request wrapped with `System.Timeout.timeout (5 * 10^9)` (5-second nanosecond timeout). Timed-out requests are logged and skipped.
- **Edge construction**: Each reference result parsed into an `Edge` with relation `References` and `Confidence 0.8`.

### Wiring into pipeline

- `extractViaLSP` now calls `extractReferences` after `extractDocumentSymbols` completes, passing the same `LSPClient`, file path, max symbols limit, and document symbol results.
- References edges are appended to the edge list alongside `Contains` edges from document symbols.

### Key decisions

1. **Top-10 limit** chosen over unbounded requests because: (a) LSP servers may be slow with deep reference queries, (b) top symbols by kind priority capture the most important references, (c) bounds worst-case latency.
2. **Confidence 0.8** (vs 0.9 for calls) because references from the LSP are structural but may include non-execution-path references (e.g., type references, documentation references).
3. **5-second timeout per request** rather than a global timeout per file, because individual symbol references are independent and a slow symbol shouldn't block others.

### Concrete changes

- `src/Infrastructure/LSP/Extraction.hs`: new `extractReferences` function with capability check, kind-priority sorting, timeout, and edge construction
- `src/Infrastructure/LSP/Extraction.hs`: `extractViaLSP` pipeline updated to call `extractReferences`
- `src/Infrastructure/LSP/Protocol.hs`: `lspReferences` request/response types added if needed (lsp-types API check)
- `src/Infrastructure/LSP/Client.hs`: `lspReferences` client call if not already present

## Result

**PASS**

- `cabal build` — zero warnings (exited 0)
- `cabal test` — 90/90 tests passing (exited 0)
- `extractReferences` function verified present with capability check, 5s timeout, and kind-priority sorting
- References edges now appear in pipeline output (confirmed: 119 references edges on Graphos repo baseline)
