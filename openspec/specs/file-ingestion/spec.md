## MODIFIED Requirements

### Requirement: PaperFiles routing in extraction pipeline
The `UseCase.Extract.extractAll` function SHALL process `PaperFiles` alongside `CodeFiles`, `DocFiles`, `OfficeFiles`, and `ImageFiles`. PaperFiles SHALL be routed through the new `epExtractPdfFile` port function. The extraction result SHALL be accumulated into the merged extraction using the same pattern as office and image files.

### Requirement: URL PDF ingestion produces actual content
The `ingest` function in `UseCase.Ingest`, when encountering a `PdfUrl`, SHALL:
1. Download the PDF file to a temporary directory
2. Route the downloaded file through `extractPdfFile` for content extraction
3. Return an `IngestResult` with the extracted content, not a stub

If the download fails, the function SHALL log a warning and fall back to the current stub behavior (`[PDF content - to be fetched]`).

### Requirement: ExtractionPort includes PDF extraction
The `ExtractionPort` record SHALL include a new field:
```haskell
, epExtractPdfFile :: PipelineConfig -> FilePath -> IO Extraction
```

This field SHALL be wired in `Infrastructure.Wiring` to call `Infrastructure.Extract.Pdf.extractPdfFile`.

## PDCA framing for this requirement

- **Plan**: PDFs (both local files and URLs) are fully extracted, producing structured knowledge graphs. PaperFiles are no longer silently dropped.
- **Check**: Verify `graphos ingest <pdf-file>` produces nodes. Verify URL PDF ingestion downloads and extracts. Verify `cabal test` passes with new PaperFiles routing.
- **Act**: If URL download is unreliable, add retry logic and timeout configuration. If PDF extraction is too slow for large files, add progress logging.