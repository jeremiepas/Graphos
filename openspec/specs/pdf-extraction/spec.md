# Pdf Extraction

## Purpose

Extract structured hierarchical content from PDF files using `pdftotext` and regex heuristics, producing File/Title/Section/Paragraph nodes with Contains and References edges.

## Requirements
### Requirement: PDF text extraction via pdftotext
The system SHALL call `pdftotext` (from poppler-utils) as a subprocess to extract plain text from PDF files. When `pdftotext` is not available on the system PATH, the system SHALL log a warning message and create a single stub node for the PDF file, then continue the pipeline without crashing.

#### Scenario: pdftotext extracts text
- **WHEN** a PDF file is ingested and `pdftotext` is available on PATH
- **THEN** plain text is extracted from the PDF and forwarded to structural parsing

#### Scenario: Missing pdftotext produces a stub node
- **WHEN** `pdftotext` is not on the system PATH during PDF ingestion
- **THEN** a warning is logged, a single stub node is created for the file, and the pipeline continues without crashing

### Requirement: PDF structural parsing
The system SHALL parse `pdftotext` output using regex heuristics to detect hierarchical sections:
- ALL CAPS lines ≥ 4 words → Level 1 (Title)
- Numbered sections (`1.`, `1.1`) → Level by dot count
- `CHAP.`, `TITRE` prefix → Level 2
- `seCT.` prefix → Level 3
- `§` prefix → Level 4
- Lettered items (`A.`, `B.`) after § → Level 5

When no structural patterns are detected, the system SHALL fall back to paragraph-level extraction using blank-line separation.

#### Scenario: ALL CAPS line classified as Title
- **WHEN** a line of `pdftotext` output is ALL CAPS with ≥ 4 words
- **THEN** it is classified as a Level 1 Title node

#### Scenario: Numbered section classified by dot count
- **WHEN** a line begins with `1.2.3` followed by a section title
- **THEN** it is classified as a Level 3 Section node

#### Scenario: No structure falls back to paragraphs
- **WHEN** no structural patterns are detected in the extracted text
- **THEN** extraction falls back to paragraph-level nodes split on blank lines

### Requirement: TOC detection and skip
The system SHALL detect table-of-contents pages and skip them during extraction. A page SHALL be classified as TOC when ALL of:
- ≥ 60% of non-empty lines contain dot-leaders (`...` 3+ consecutive dots)
- Lines end with page references (`ib.` or numeric page numbers)
- No paragraph-length text blocks (lines > 100 chars without dots)

#### Scenario: TOC page skipped
- **WHEN** a page has ≥ 60% dot-leader lines ending in page references and no paragraph-length blocks
- **THEN** that page is classified as a table of contents and skipped during extraction

#### Scenario: Content page not skipped
- **WHEN** a page contains paragraph-length text blocks without dot-leaders
- **THEN** that page is not classified as TOC and is processed normally

### Requirement: Three granularity levels for PDF extraction
The system SHALL support three granularity levels for PDF extraction, mapped to the existing `Granularity` type:

| Granularity | PDF Level | Description |
|---|---|---|
| `GranularityFile` | Small | File node + top-level titles only (~5-10 nodes) |
| `GranularityFunction` | Medium | File + CHAP + seCT + § nodes (~20-50 nodes) |
| `GranularityFine` (default) | Large/Paragraph | All levels + one node per paragraph (~100-500 nodes) |

#### Scenario: File granularity yields few nodes
- **WHEN** a PDF is ingested with `GranularityFile`
- **THEN** only the file node and top-level titles are produced (~5-10 nodes)

#### Scenario: Fine granularity yields paragraph nodes
- **WHEN** a PDF is ingested with `GranularityFine`
- **THEN** one node per paragraph is produced in addition to all structural levels (~100-500 nodes)

### Requirement: PDF node types
All PDF-extracted nodes SHALL use `nodeFileType = PaperFile`. Nodes SHALL include:
- File node: `nodeKind = "File"`, label = filename without extension
- Title nodes: `nodeKind = "Title"`, `nodeLineStart` = line number in extracted text
- Section nodes: `nodeKind = "Section"`, `nodeLineStart` = line number
- Paragraph nodes: `nodeKind = "Paragraph"`, `nodeLineStart` = start line, `nodeLineEnd` = end line

#### Scenario: PDF nodes carry PaperFile type
- **WHEN** any node is produced from PDF extraction
- **THEN** its `nodeFileType` is `PaperFile` and its `nodeKind` matches one of File/Title/Section/Paragraph

### Requirement: PDF edge construction
The system SHALL create the following edges for PDF-extracted content:
- File → Title: `Contains` edge (confidence 1.0)
- Title → Chapter: `Contains` edge (confidence 1.0)
- Chapter → Section: `Contains` edge (confidence 1.0)
- Section → Subsection: `Contains` edge (confidence 1.0)
- Section → Paragraph: `Contains` edge (confidence 0.9)
- Cross-references between sections sharing keywords: `References` edge (confidence 0.7)

#### Scenario: Contains edges link hierarchy
- **WHEN** a Title and a Section beneath it are extracted from a PDF
- **THEN** a `Contains` edge with confidence 1.0 connects the Title to the Section

#### Scenario: Cross-references connect keyword-sharing sections
- **WHEN** two extracted sections share keywords
- **THEN** a `References` edge with confidence 0.7 connects them

### Requirement: Clean architecture separation
PDF extraction SHALL follow the project's clean architecture layers:
- **Domain**: Pure PDF structure types and parser (`Domain.PdfStructure`)
- **UseCase**: PaperFiles routing in `UseCase.Extract`, port field in `UseCase.Port.ExtractionPort`
- **Infrastructure**: IO-bound `pdftotext` subprocess call (`Infrastructure.Extract.Pdf`)

The pure parser SHALL be independently testable with no IO dependencies.

#### Scenario: Pure parser testable without IO
- **WHEN** `Domain.PdfStructure` is unit-tested with sample text
- **THEN** parsing succeeds with no IO monad or subprocess dependency

### Requirement: poppler-utils in Nix shell
The system's `shell.nix` SHALL include `poppler-utils` as a build input so that `pdftotext` is available in the development shell by default.

#### Scenario: poppler-utils available in nix-shell
- **WHEN** a developer enters the nix-shell
- **THEN** `pdftotext` is available on PATH via the `poppler-utils` build input
