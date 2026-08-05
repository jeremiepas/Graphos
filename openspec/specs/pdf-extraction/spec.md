## ADDED Requirements

### Requirement: PDF text extraction via pdftotext
The system SHALL call `pdftotext` (from poppler-utils) as a subprocess to extract plain text from PDF files. When `pdftotext` is not available on the system PATH, the system SHALL log a warning message and create a single stub node for the PDF file, then continue the pipeline without crashing.

### Requirement: PDF structural parsing
The system SHALL parse `pdftotext` output using regex heuristics to detect hierarchical sections:
- ALL CAPS lines ≥ 4 words → Level 1 (Title)
- Numbered sections (`1.`, `1.1`) → Level by dot count
- `CHAP.`, `TITRE` prefix → Level 2
- `seCT.` prefix → Level 3
- `§` prefix → Level 4
- Lettered items (`A.`, `B.`) after § → Level 5

When no structural patterns are detected, the system SHALL fall back to paragraph-level extraction using blank-line separation.

### Requirement: TOC detection and skip
The system SHALL detect table-of-contents pages and skip them during extraction. A page SHALL be classified as TOC when ALL of:
- ≥ 60% of non-empty lines contain dot-leaders (`...` 3+ consecutive dots)
- Lines end with page references (`ib.` or numeric page numbers)
- No paragraph-length text blocks (lines > 100 chars without dots)

### Requirement: Three granularity levels for PDF extraction
The system SHALL support three granularity levels for PDF extraction, mapped to the existing `Granularity` type:

| Granularity | PDF Level | Description |
|---|---|---|
| `GranularityFile` | Small | File node + top-level titles only (~5-10 nodes) |
| `GranularityFunction` | Medium | File + CHAP + seCT + § nodes (~20-50 nodes) |
| `GranularityFine` (default) | Large/Paragraph | All levels + one node per paragraph (~100-500 nodes) |

### Requirement: PDF node types
All PDF-extracted nodes SHALL use `nodeFileType = PaperFile`. Nodes SHALL include:
- File node: `nodeKind = "File"`, label = filename without extension
- Title nodes: `nodeKind = "Title"`, `nodeLineStart` = line number in extracted text
- Section nodes: `nodeKind = "Section"`, `nodeLineStart` = line number
- Paragraph nodes: `nodeKind = "Paragraph"`, `nodeLineStart` = start line, `nodeLineEnd` = end line

### Requirement: PDF edge construction
The system SHALL create the following edges for PDF-extracted content:
- File → Title: `Contains` edge (confidence 1.0)
- Title → Chapter: `Contains` edge (confidence 1.0)
- Chapter → Section: `Contains` edge (confidence 1.0)
- Section → Subsection: `Contains` edge (confidence 1.0)
- Section → Paragraph: `Contains` edge (confidence 0.9)
- Cross-references between sections sharing keywords: `References` edge (confidence 0.7)

### Requirement: Clean architecture separation
PDF extraction SHALL follow the project's clean architecture layers:
- **Domain**: Pure PDF structure types and parser (`Domain.PdfStructure`)
- **UseCase**: PaperFiles routing in `UseCase.Extract`, port field in `UseCase.Port.ExtractionPort`
- **Infrastructure**: IO-bound `pdftotext` subprocess call (`Infrastructure.Extract.Pdf`)

The pure parser SHALL be independently testable with no IO dependencies.

### Requirement: poppler-utils in Nix shell
The system's `shell.nix` SHALL include `poppler-utils` as a build input so that `pdftotext` is available in the development shell by default.

## PDCA framing for this requirement

- **Plan**: PDF files produce structured knowledge graphs with hierarchical nodes, Contains edges, and community clusters. Success: Maison-Rustique-T5.pdf yields >0 nodes with correct PaperFile type and hierarchical Contains edges.
- **Check**: Run `graphos ingest Maison-Rustique-T5.pdf --granularity fine` and verify node count >0, community detection produces thematic clusters, bridge nodes connect sections across clusters.
- **Act**: If heuristics miss structural patterns, adjust regex and add PDF fixtures. If paragraph granularity produces too many nodes for large PDFs, consider Medium as default for PDFs above a size threshold.