# Task 3 — Implement call hierarchy extraction — PLAN

**Task slug**: `03-implement-call-hierarchy-extraction`
**Attempt**: 1
**Status**: pending

## Summary

Replace the `extractCallHierarchy` stub in `Infrastructure.LSP.Extraction` with a real implementation that sends `callHierarchy/incomingCalls` LSP requests, parses incoming calls, and produces `Calls` edges with `Confidence 0.9`.

## Detail

### Scope

- **Replace stub**: `extractCallHierarchy` currently returns `[]` — implement real LSP request flow
- **Request flow**: `callHierarchy/prepare` → `callHierarchy/incomingCalls` for each prepared item
- **Parse response**: Convert `[CallHierarchyIncomingCall]` into `[Edge]` of type `Calls` with `Confidence 0.9`
- **Limit**: Top-5 symbols per file (smaller limit than references, since call hierarchy is more expensive)
- **Capability check**: Only when `scpCallHierarchyProvider = True`
- **Wire into**: `extractViaLSP` pipeline after references extraction

### Check Criteria

**What tests/gates will be run:**
- `cabal build` — zero warnings (with `-Wall -Wcompat -Wincomplete-uni-patterns -Werror`)
- `cabal test` — all tests pass (90/90)
- Manual verification: `extractCallHierarchy` implements prepare→incoming calls flow

**What spec scenarios this task must satisfy:**
- `lsp-edge-extraction` spec — **"Call hierarchy extraction returns incoming calls"**: Given an LSP server with `callHierarchyProvider = true`, when `extractCallHierarchy` is called for symbol "main", it SHALL return a list of incoming calls as `Calls` edges.
- `lsp-edge-extraction` spec — **"Call hierarchy skipped when capability unavailable"**: Given an LSP server with `callHierarchyProvider = false`, when `extractCallHierarchy` is called, it SHALL return `[]` without sending any requests.

**What the exact PASS conditions are:**
1. `cabal build` exits with code 0, zero warnings
2. `cabal test` exits with code 0, all 90 tests pass
3. `extractCallHierarchy` calls `callHierarchy/prepare` then `callHierarchy/incomingCalls`
4. `scpCallHierarchyProvider` capability checked before sending requests
5. Response parsed into `Calls` edges with `Confidence 0.9`
6. Top-5 symbol limit enforced

**What would constitute a FAIL:**
- `cabal build` fails — `lsp-types` missing `CallHierarchy` types or wrong module
- `cabal test` fails — existing tests expect stub behavior (empty list)
- Capability check missing — crashes on servers without `callHierarchyProvider`
- No prepare→incomingCalls two-phase flow — single request won't work per LSP spec

### Affected Modules

| Module | Layer |
|--------|-------|
| `Infrastructure.LSP.Extraction` | Infrastructure — `extractCallHierarchy` implementation, wiring |
| `Infrastructure.LSP.Protocol` | Infrastructure — add `lspCallHierarchyPrepareWithId`, `lspCallHierarchyIncomingWithId` |
| `Infrastructure.LSP.Client` | Infrastructure — client calls for prepare/incoming |

### Prerequisites

- Task 2 completed (references extraction) — call hierarchy is the second cross-file edge type
- `lsp-types` package supports `CallHierarchyItem` and `CallHierarchyIncomingCall` types
- LSP server capability flags (`scpCallHierarchyProvider`) accessible

### Risks

| Risk | Mitigation |
|------|------------|
| Call hierarchy not supported by most LSP servers | Capability check — skip gracefully when `callHierarchyProvider = False` |
| Two-phase request (prepare→incoming) adds latency | 5s timeout per symbol; top-5 limit |
| `lsp-types` API evolution | Check current version; if types missing, defer or add to lsp-types PR |
| Edge confidence 0.9 vs 0.8 for references | Deliberate: call hierarchy is higher confidence (direct call graph) |

## Result

Pending — first cycle.
