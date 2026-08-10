## ADDED Requirements

### Requirement: Periodic Extraction Progress Logging

During file extraction, the pipeline SHALL log a progress summary every 50 processed files (across all categories: code, doc, office, image, paper). The log line SHALL include the number of files processed so far and the total number of files.

#### Scenario: Progress logged during extraction
- **WHEN** `graphos .` is run with 488 total files
- **THEN** log lines appear like `[extract] Processed 50/488 files (10%)`, `[extract] Processed 100/488 files (20%)`, etc., at approximate intervals of 50 files

#### Scenario: Fewer than 50 files
- **WHEN** `graphos .` is run with 30 total files
- **THEN** no progress log lines appear (only the final "Extracted X nodes, Y edges" summary)

#### Scenario: Progress across concurrent categories
- **WHEN** extraction runs with `--threads 4` and processes code/doc/office files concurrently
- **THEN** the progress counter reflects the total across all categories, not per-category

### Requirement: Startup Grammar Availability Warning

At the start of extraction (Step 2), the pipeline SHALL check whether any configured tree-sitter grammar name in `graphos.yaml` lacks a corresponding FFI binding in `getGrammarPtr`. If any are missing, it SHALL log a single warning listing all affected file extensions and grammar names.

#### Scenario: Missing grammar binding detected
- **WHEN** `graphos.yaml` configures `.nix` with `grammar: nix` but `getGrammarPtr "nix"` returns `Nothing`
- **THEN** a single warning line appears: `[extract] WARNING: No tree-sitter grammar binding for: nix (.nix). Files will use stub extraction.`

#### Scenario: All grammars available
- **WHEN** all configured grammar names have FFI bindings
- **THEN** no grammar warning is logged