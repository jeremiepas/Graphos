# Office Extraction Capability

## Purpose

Extract structured knowledge from office documents (DOCX, PPTX, XLSX) by parsing their ZIP/XML content into markdown-equivalent nodes, integrating with the existing document extraction pipeline.

## Requirements

### Requirement: DOCX text extraction to markdown
The system SHALL parse .docx files as ZIP archives, extract text content from `word/document.xml`, and convert paragraph styles to markdown headers (Title → `#`, Heading1 → `##`, Heading2 → `###`, Heading3 → `####`, Heading4 → `#####`). The resulting markdown SHALL be fed through the existing `extractDocFile` pipeline to produce graph nodes.

#### Scenario: DOCX with headings produces header nodes
- **WHEN** a .docx file contains paragraphs with styles "Title", "Heading 1", "Heading 2"
- **THEN** the system produces header nodes with nodeKind "Header" at levels 1, 2, 3, and Contains edges from the file node to each header

#### Scenario: DOCX with no styles produces file node
- **WHEN** a .docx file contains only plain text paragraphs with no heading styles
- **THEN** the system produces a file node with nodeKind "File" and the text content stored in nodeExtra

#### Scenario: Malformed DOCX produces stub node with warning
- **WHEN** a .docx file cannot be parsed (corrupt ZIP, missing document.xml)
- **THEN** the system produces a single stub node and logs a warning, without crashing the pipeline

### Requirement: PPTX slide extraction to markdown
The system SHALL parse .pptx files as ZIP archives, extract text from each slide in `ppt/slides/slideN.xml`, and produce markdown with `## Slide N` headers for each slide. Text from shapes (`<a:t>` elements) SHALL be collected as paragraph text under each slide header.

#### Scenario: PPTX with 3 slides produces slide header nodes
- **WHEN** a .pptx file contains 3 slides with text content
- **THEN** the system produces 3 header nodes with labels like "Slide 1", "Slide 2", "Slide 3" and Contains edges from the file node to each slide

#### Scenario: PPTX with embedded images extracts media paths
- **WHEN** a .pptx file contains embedded images in `ppt/media/`
- **THEN** the system produces ImageFile nodes for each embedded image, linked via Contains edges to the slide they appear on

### Requirement: XLSX table extraction to markdown
The system SHALL parse .xlsx files as ZIP archives, extract cell data from `xl/worksheets/sheet1.xml`, and produce markdown tables. Each sheet SHALL become a `## Sheet N` section.

#### Scenario: XLSX with data produces table nodes
- **WHEN** an .xlsx file contains cells with text and numeric data
- **THEN** the system produces a file node and header nodes for each sheet, with cell data available in nodeExtra

#### Scenario: Empty XLSX produces file node
- **WHEN** an .xlsx file contains empty worksheets
- **THEN** the system produces a single file node with nodeKind "File"

### Requirement: Legacy .doc/.ppt format handling
The system SHALL detect .doc and .ppt file extensions, produce a stub node with nodeKind "File" and a warning log message recommending conversion to .docx/.pptx, and skip extraction.

#### Scenario: .doc file produces stub with warning
- **WHEN** a .doc file is detected
- **THEN** the system produces a single stub node and logs "Legacy .doc format detected — convert to .docx for full extraction"

### Requirement: Office file detection and routing
The system SHALL detect .docx, .pptx, .xlsx, .doc, .ppt file extensions and route them to the office extraction pipeline. The `FileCategory` enum SHALL include `OfficeFiles`. The `FileType` enum SHALL include `OfficeFile`. The `FileExtensionConfig` SHALL include a `fecOffice` field with default extensions `[.docx, .pptx, .xlsx, .doc, .ppt]`.

#### Scenario: .docx file detected and categorized
- **WHEN** the detection phase scans a directory containing a .docx file
- **THEN** the file is categorized as `OfficeFiles` with `FileType = OfficeFile`

#### Scenario: graphos.yaml overrides office extensions
- **WHEN** graphos.yaml contains `file_extensions: office: [.docx, .pptx]`
- **THEN** only .docx and .pptx are detected as office files (user override)