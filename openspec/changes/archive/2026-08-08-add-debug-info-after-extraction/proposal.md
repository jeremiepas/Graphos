## Why

PDF (paper) extraction silently falls back to stub nodes when `pdftotext` is unavailable or fails, producing minimal graph content (1 node, 0 edges per PDF). Users running the pipeline on PDF-heavy inputs get no visibility into whether their papers were actually parsed or silently degraded — the only signal is a sparse graph with no indication of why. This change adds a debug summary after paper extraction completes, surfacing file counts, successful extractions, stub node counts, and per-file node/edge totals so users can immediately diagnose extraction quality.

## What Changes

- **Debug summary after paper extraction**: `UseCase.Extract` logs a structured summary after all PDF files are extracted, showing total paper files, successful extractions, stub node count, and per-file node/edge totals.
- **Per-file extraction logging in `Infrastructure.Extract.Pdf`**: Each PDF file's extraction result is logged at `DEBUG` level (currently only failure paths log), including node count, edge count, and whether the result is a stub.
- **Stub node detection helper**: A lightweight pure function to classify whether an `Extraction` represents a stub (single node, no edges, `nodeKind = "File"`) for the summary.

## Capabilities

### New Capabilities
- `pdf-extraction-debug`: Debug logging and summary reporting for PDF/paper extraction results

### Modified Capabilities
- None (the existing `pdf-extraction` spec's requirements remain unchanged; this adds observability, not behavioral change)

## Impact

- **Affected code**: `src/Graphos/UseCase/Extract.hs` (summary after paper extraction), `src/Graphos/Infrastructure/Extract/Pdf.hs` (per-file logging)
- **No API changes**: All changes are logging additions; no type signatures change
- **No new dependencies**: Uses existing `LoggingPort` and `Data.Text`
- **No spec deltas needed** for `pdf-extraction` — this is pure observability

## PDCA Cycle

- **Plan**: After paper extraction completes, users see a structured debug summary showing file count, successful extractions, stub count, and per-file breakdown. Success metric: users can answer "how many PDFs actually extracted vs stubbed?" without reading logs or inspecting the graph.
- **Do**: Add per-file DEBUG logging in `Infrastructure.Extract.Pdf`, add stub-detection helper, add summary logging in `UseCase.Extract` after paper extraction completes.
- **Check**: Run pipeline with and without `pdftotext`; verify summary appears in logs with correct counts; verify stub detection is accurate; verify no compilation errors with `cabal build`.
- **Act**: If summary format is unclear or noisy, adjust log levels and wording. If stub detection proves unreliable, refine the heuristic. Standardize the pattern for other extractors (image, office) in a follow-up.
