## Why

PDFs are detected and categorized as `PaperFiles` by the pipeline, but then **silently dropped** — `extractAll` in `UseCase.Extract` processes code, doc, office, and image files, but has no extraction path for `PaperFiles`. The result: `graphos ingest Maison-Rustique-T5.pdf` produces zero nodes. Additionally, URL ingestion for PDFs creates only a stub `[PDF content - to be fetched]` with no actual content extraction.

The test file `Maison-Rustique-T5.pdf` — a 55K-line 19th-century French agricultural encyclopedia with rich hierarchical structure (TITRE → CHAPITRE → SECTION → § → items) — perfectly demonstrates what PDF ingestion should produce: a navigable knowledge graph with community-detected thematic clusters.

## What Changes

1. **Add PDF extraction infrastructure** — new `Infrastructure.Pdf` module that calls `pdftotext` (poppler-utils) to extract text, parses structural headers (TITRE, CHAP, seCT, §, etc.), and builds an `Extraction` with hierarchical nodes and edges.

2. **Route PaperFiles through extraction** — `UseCase.Extract.extractAll` gains a `PaperFiles` path alongside code/doc/office/image, routing through a new `epExtractPdfFile` port function.

3. **Three granularity levels** — PDF extraction supports Small (file + top-level titles), Medium (file + sections + subsections), and Large/Paragraph (one node per paragraph, default). Maps to existing `Granularity` type (File/Function/Fine).

4. **TOC detection and skip** — Detect table-of-contents pages (characterized by dot-leader lines, page number references like `ib.`, sequential section listings) and skip them to avoid duplicate nodes.

5. **Soft dependency on poppler-utils** — When `pdftotext` is not installed, log a warning and create a stub node (matching the pattern of other extractors on failure). Pipeline continues.

6. **Fix URL PDF ingestion** — The `PdfUrl` case in `ingest` currently writes `[PDF content - to be fetched]`. After this change, it downloads the PDF and routes it through the same `extractPdfFile` pipeline.

7. **Add `poppler-utils` to shell.nix** — Include in Nix dev shell so `pdftotext` is available by default.

## Capabilities

### New Capabilities
- `pdf-extraction`: Extract structured nodes and edges from PDF files using pdftotext, with hierarchical section detection, TOC skipping, and three granularity levels (Small/Medium/Large).

### Modified Capabilities
- `file-ingestion`: PaperFiles are now routed through extraction instead of being silently dropped; URL PDF ingestion now downloads and extracts content instead of creating stubs.

## Impact

- **New module**: `src/Graphos/Infrastructure/Extract/Pdf.hs` — PDF text extraction and structure parsing
- **New port field**: `epExtractPdfFile :: PipelineConfig -> FilePath -> IO Extraction` in `ExtractionPort`
- **Modified module**: `src/Graphos/UseCase/Extract.hs` — add PaperFiles processing path in `extractAll`
- **Modified module**: `src/Graphos/Infrastructure/Wiring.hs` — wire `epExtractPdfFile`
- **Modified module**: `src/Graphos/UseCase/Ingest.hs` — fix PdfUrl case to download and extract
- **Modified module**: `src/Graphos/Domain/Config/Extraction.hs` — add PDF extractor config
- **Modified file**: `shell.nix` — add `poppler-utils` dependency
- **New tests**: `test/Graphos/Infrastructure/Extract/PdfSpec.hs` — Hspec + QuickCheck tests
- **Test fixture**: `Maison-Rustique-T5.pdf` used as integration test

## PDCA Cycle

- **Plan**: PDF files produce a rich knowledge graph with hierarchical structure, community clusters, and bridge nodes. Success measured by: Maison-Rustique-T5.pdf produces >0 nodes, communities detected with correct thematic grouping, bridge nodes identified between sections.
- **Do**: Implement PDF extraction pipeline (pdftotext → parse → nodes/edges → community detection). Test with Maison-Rustique-T5.pdf.
- **Check**: Run `graphos ingest Maison-Rustique-T5.pdf` and verify: (1) nodes extracted with PaperFile type, (2) section hierarchy reflected in Contains edges, (3) communities correctly cluster related sections (e.g., "Engrais" sections together), (4) bridge nodes connect across communities, (5) missing pdftotext produces warning + stub, (6) granularity levels produce expected node counts.
- **Act**: If structural heuristics need tuning for other PDF types, adjust regex patterns and add to test corpus. Feed findings into next iteration for table extraction and OCR support.