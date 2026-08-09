## Context

PDF extraction in Graphos uses `pdftotext` (from poppler-utils) to extract text, then parses it through `Domain.PdfStructure` to produce hierarchical nodes and `Contains` edges. When `pdftotext` is unavailable or fails, the extractor falls back to a stub node (1 node, 0 edges) via `pdfStubNode`.

Currently, per-file logging only appears on failure paths (file not found, empty text, pdftotext failure, exception). Successful extractions log a single line like `[pdf] file.pdf -> 123 nodes, 122 edges` inside `Infrastructure.Extract.Pdf`, but the calling code in `UseCase.Extract` only logs `"[paper] Extraction complete"` at DEBUG level with no counts.

Users running pipelines on PDF-heavy inputs see no indication of whether papers were actually parsed or silently degraded to stubs. The pipeline summary at the top level only shows total nodes/edges across all file types, making it impossible to distinguish "PDFs produced 0 nodes" from "PDFs were not detected."

## Goals / Non-Goals

**Goals:**
- Surface extraction quality for PDFs immediately after extraction completes (Plan: users can diagnose PDF issues without post-hoc graph inspection)
- Add per-file DEBUG logging so developers can trace individual PDF extraction results
- Add an INFO-level summary showing file/success/stub counts

**Non-Goals:**
- No changes to extraction logic or stub creation behavior
- No new config options or CLI flags
- No changes to the `pdf-extraction` spec requirements
- No stub detection for other extractors (image, office) — that's a follow-up

## Decisions

| Decision | Rationale | Alternatives Considered |
|---|---|---|
| Stub detection via `isStubExtraction :: Extraction -> Bool` | Pure function, no IO, reusable across extractors | Pattern-match on nodeMap size + edgeMap size; harder to test, less reusable |
| Summary logged in `UseCase.Extract` after paper extraction | UseCase layer orchestrates extraction; has access to accumulator maps | Log in `Infrastructure.Extract.Pdf` per file; but summary needs cross-file aggregation |
| Per-file logging in `Infrastructure.Extract.Pdf` | Closest point to actual extraction result; minimal refactoring | Pass result up and log in UseCase; adds coupling, duplicates logic |
| DEBUG level for per-file, INFO for summary | Per-file is verbose (one per PDF); summary is high-level (one total) | Both at DEBUG; users miss summary when not running with `-v` |
| Stub heuristic: 1 node with kind="File" + 0 edges | Matches `pdfStubNode` output exactly; simple and reliable | Check node label pattern or node ID prefix; fragile, parser-dependent |

## Risks / Trade-offs

| Risk | Mitigation |
|---|---|
| Stub detection may match non-stub single-file extractions (e.g., a PDF with only a title and no sections) | The heuristic is conservative: a real extraction with content will almost always produce multiple nodes. If false positives occur, refine to check `nodeLineEnd` (stub has `Nothing`, real extractions have `Just n`) |
| Logging volume for large PDF corpora (100+ files) | Per-file logs are DEBUG level; only the summary appears at INFO. Users running with verbose mode will see all per-file logs |
| No change to existing logging format | The per-file format already exists for success cases (`[pdf] path -> N nodes, M edges`). The stub format extends this pattern with `→ stub (1 node, 0 edges)` |

## Verification Strategy (Check)

1. **Compilation**: `cabal build` succeeds with no warnings
2. **Unit test**: Property test for `isStubExtraction` — stub extraction → True; multi-node → False; empty → False
3. **Integration**: Run `graphos ingest <pdf-file>` with `pdftotext` available → verify INFO summary shows `N successful, 0 stubbed`
4. **Integration**: Run `graphos ingest <pdf-file>` without `pdftotext` → verify INFO summary shows `0 successful, N stubbed`
5. **Integration**: Run pipeline with mixed PDF/code files → verify summary only for PDFs, doesn't affect code extraction logging

## Iteration & Rollback (Act)

- **If Check fails (compilation)**: Fix type errors, recompile. The changes are additive (logging only), so rollback is trivial (git revert).
- **If Check fails (stub detection false positives)**: Refine heuristic to also check `nodeLineEnd /= Nothing` for non-stubs. The function is pure and testable.
- **If Check fails (logging noise)**: Adjust log levels or message format based on user feedback.
- **Act**: If the stub detection pattern proves useful, standardize it across other extractors (image, office) in a follow-up change.

## Migration Plan

No migration needed. This is purely additive logging — no behavior change, no config change, no data format change.

## Open Questions

- Should the stub detection also check `nodeLineEnd` to be more precise? (Current decision: no, keep it simple. Can refine later.)
- Should the summary include per-file details in a separate DEBUG log? (Current decision: no, keep summary at INFO with counts only. Per-file details are already in per-file DEBUG logs.)
