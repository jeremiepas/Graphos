# Task 2 — Implement references extraction in LSP.Extraction — PLAN

**Task slug**: `02-implement-references-extraction`
**Attempt**: 1
**Status**: pending

## Summary

Add `extractReferences` function to `Infrastructure.LSP.Extraction` that sends `textDocument/references` LSP requests for the top-10 top-level symbols per file, producing `References` edges with `Confidence 0.8`.

## Detail

### Scope

- **New function**: `extractReferences :: LSPClient -> FilePath -> Int -> [DocumentSymbolResult] -> IO [Edge]` in `Infrastructure.LSP.Extraction`
- **Wire into**: `extractViaLSP` pipeline after `extractDocumentSymbols`
- **Kind-priority sorting**: Class (priority 5) > Function (12) > Method (6) > others for top-10 selection
- **Capability check**: Only send requests when `scpReferencesProvider = True`
- **Timeout**: 5-second per request via `System.Timeout.timeout`

### Check Criteria

**What tests/gates will be run:**
- `cabal build` — zero warnings (with `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`)
- `cabal test` — all tests pass (90/90, existing tests must still pass)
- Manual verification: `extractReferences` function present with capability check, timeout, and sorting

**What spec scenarios this task must satisfy:**
- `lsp-edge-extraction` spec — **"References extraction produces cross-file edges"**: Given an LSP server connected to a Haskell project, when `extractViaLSP` processes `Client.hs`, the extraction SHALL contain `References` edges for each symbol referencing `Client.hs` symbols from other files.
- `lsp-edge-extraction` spec — **"References request skipped when capability unavailable"**: Given an LSP server with `referencesProvider = false`, when `extractViaLSP` processes a file, no `references` requests SHALL be sent; only `Contains` edges SHALL be produced.
- `lsp-edge-extraction` spec — **"Top-10 symbols limit per file"**: Given a file with 50 top-level symbols, when `extractViaLSP` processes it, references SHALL be requested for at most 10 symbols, prioritized by kind (Class > Function > Method > others).

**What the exact PASS conditions are:**
1. `cabal build` exits with code 0, zero warnings
2. `cabal test` exits with code 0, all 90 tests pass
3. Function signature exists: `extractReferences :: LSPClient -> FilePath -> Int -> [DocumentSymbolResult] -> IO [Edge]`
4. `scpReferencesProvider` capability checked before sending requests
5. `System.Timeout.timeout (5 * 10^9)` applied per request (5 nanosecond = 5 seconds)
6. `takeN 10` with kind-priority sorting after filtering

**What would constitute a FAIL:**
- `cabal build` fails due to missing imports or type errors (e.g., `lsp-types` API mismatch)
- `cabal test` fails because existing LSP extraction tests assume no references edges
- Capability check missing — sends requests to servers that don't support `referencesProvider`
- No kind-priority sorting — first-10 symbols by document order (may miss important references)

### Affected Modules

| Module | Layer |
|--------|-------|
| `Infrastructure.LSP.Extraction` | Infrastructure — new `extractReferences` function, wiring into `extractViaLSP` |
| `Infrastructure.LSP.Protocol` | Infrastructure — may need `lspReferences` request/response types |
| `Infrastructure.LSP.Client` | Infrastructure — `lspReferences` client call |

### Prerequisites

- Task 1 completed (EdgeId deduplication fix) — references edges depend on unique EdgeId
- `lsp-types` package available with `textDocument/references` types
- LSP server capability flags (`scpReferencesProvider`) accessible

### Risks

| Risk | Mitigation |
|------|------------|
| LSP servers may not support `references` | Capability check — skip gracefully when `referencesProvider = False` |
| Slow on large repos with many files | Top-10 limit + 5s timeout per request caps worst case |
| `lsp-types` API may not expose references types | Check `lsp-types` version; if missing, defer to next version |
| Test coverage gap — no existing test for references | Document limitation; add assertion in test suite if time permits |

## Result

Pending — first cycle.
