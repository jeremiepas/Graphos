# File Ingestion

## Purpose

Route PaperFiles (local and URL PDFs) through the extraction pipeline so they produce structured graph nodes instead of being silently dropped.

## Requirements
### Requirement: PaperFiles routing in extraction pipeline
The `UseCase.Extract.extractAll` function SHALL process `PaperFiles` alongside `CodeFiles`, `DocFiles`, `OfficeFiles`, and `ImageFiles`. PaperFiles SHALL be routed through the new `epExtractPdfFile` port function. The extraction result SHALL be accumulated into the merged extraction using the same pattern as office and image files.

#### Scenario: PaperFile produces extraction nodes
- **WHEN** `extractAll` is called on a detection containing a `.pdf` PaperFile
- **THEN** the merged extraction includes nodes produced by `epExtractPdfFile` for that file

### Requirement: URL PDF ingestion produces actual content
The `ingest` function in `UseCase.Ingest`, when encountering a `PdfUrl`, SHALL:
1. Download the PDF file to a temporary directory
2. Route the downloaded file through `extractPdfFile` for content extraction
3. Return an `IngestResult` with the extracted content, not a stub

If the download fails, the function SHALL log a warning and fall back to the current stub behavior (`[PDF content - to be fetched]`).

#### Scenario: URL PDF is downloaded and extracted
- **WHEN** `ingest` encounters a `PdfUrl` pointing to a reachable PDF
- **THEN** the file is downloaded to a temporary directory and the returned `IngestResult` carries extracted content rather than a stub

#### Scenario: Download failure falls back to stub
- **WHEN** `ingest` encounters a `PdfUrl` whose download fails
- **THEN** a warning is logged and the `IngestResult` falls back to `[PDF content - to be fetched]`

### Requirement: ExtractionPort includes PDF extraction
The `ExtractionPort` record SHALL include a new field:
```haskell
, epExtractPdfFile :: PipelineConfig -> FilePath -> IO Extraction
```

This field SHALL be wired in `Infrastructure.Wiring` to call `Infrastructure.Extract.Pdf.extractPdfFile`.

#### Scenario: ExtractionPort wires PDF extraction
- **WHEN** `Infrastructure.Wiring` builds the `ExtractionPort`
- **THEN** `epExtractPdfFile` is wired to `Infrastructure.Extract.Pdf.extractPdfFile`
