## ADDED Requirements

### Requirement: Per-file PDF extraction logging
The system SHALL log a `DEBUG`-level message after each PDF file is extracted, reporting the file path, node count, edge count, and whether the result is a stub extraction. The log message SHALL follow the format: `[pdf] <filePath> → <nodeCount> nodes, <edgeCount> edges` for successful extractions and `[pdf] <filePath> → stub (1 node, 0 edges)` for stub results.

#### Scenario: Successful PDF extraction logs node/edge count
- **WHEN** `pdftotext` is available and extracts text from a PDF file
- **THEN** the system logs a DEBUG message showing the file path, node count, and edge count

#### Scenario: Stub PDF extraction logs stub indicator
- **WHEN** `pdftotext` is unavailable or fails for a PDF file
- **THEN** the system logs a DEBUG message showing the file path followed by "stub (1 node, 0 edges)"

### Requirement: Paper extraction summary
The system SHALL log an `INFO`-level summary after all PDF files have been extracted, reporting the total number of paper files, the number of successful extractions, and the number of stub extractions. The summary SHALL follow the format: `[paper] Extraction complete: <n> files, <success> successful, <stub> stubbed` where `<stub>` equals total minus successful.

#### Scenario: Summary with all successful extractions
- **WHEN** all PDF files are successfully extracted (pdftotext available)
- **THEN** the system logs: `[paper] Extraction complete: N files, N successful, 0 stubbed`

#### Scenario: Summary with mixed results
- **WHEN** some PDF files succeed and some fall back to stubs
- **THEN** the system logs: `[paper] Extraction complete: N files, S successful, F stubbed` where S + F = N

#### Scenario: Summary with all stubs
- **WHEN** no PDF files can be extracted (pdftotext unavailable)
- **THEN** the system logs: `[paper] Extraction complete: N files, 0 successful, N stubbed`

#### Scenario: Summary with no PDF files
- **WHEN** no paper files are detected in the input
- **THEN** the system does not log the paper extraction summary

### Requirement: Stub detection helper
The system SHALL provide a pure function `isStubExtraction :: Extraction -> Bool` that returns `True` when an extraction represents a stub (exactly one node with `nodeKind = "File"`, zero edges) and `False` otherwise. This function SHALL be defined in `UseCase.Extract` and used by the summary logging logic.

#### Scenario: Stub extraction detected
- **WHEN** an extraction has exactly 1 node with `nodeKind = "File"` and 0 edges
- **THEN** `isStubExtraction` returns `True`

#### Scenario: Full extraction not detected as stub
- **WHEN** an extraction has more than 1 node or has edges
- **THEN** `isStubExtraction` returns `False`

#### Scenario: Empty extraction not detected as stub
- **WHEN** an extraction has 0 nodes
- **THEN** `isStubExtraction` returns `False`
