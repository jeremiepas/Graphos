## ADDED Requirements

### Requirement: Ignore file discovery

The system SHALL load ignore patterns from a `.graphosignore` file located at the
root of the scanned `PATH`, and SHALL log the number of patterns loaded.

#### Scenario: ignore file at scan root is loaded
- **WHEN** `graphos ./source` runs and `./source/.graphosignore` exists with 3 patterns
- **THEN** an INFO log reports `Loaded 3 ignore patterns` and the file path

#### Scenario: no ignore file present
- **WHEN** no `.graphosignore` exists at the scan root
- **THEN** discovery proceeds with zero ignore patterns and no error

### Requirement: Pattern matching semantics

The system SHALL match ignore patterns against scan-root-relative, normalized
paths using gitignore-style globs supporting `*`, `**`, leading-`/` anchoring,
and `#` comments.

#### Scenario: double-star matches nested file
- **WHEN** a pattern is `**/wsc_sdk/src/lib.rs`
- **THEN** a file at `edr/wsc/wsc-registration-rust/wsc_sdk/src/lib.rs` is excluded

#### Scenario: comment lines ignored
- **WHEN** a line begins with `#`
- **THEN** it is treated as a comment and contributes no pattern

#### Scenario: anchored pattern does not match nested
- **WHEN** a pattern is `/lib.rs`
- **THEN** only a top-level `lib.rs` is excluded, not `src/lib.rs`

### Requirement: CLI ignore flag

The system SHALL accept a repeatable `--ignore GLOB` flag whose patterns are
merged with those from `.graphosignore`.

#### Scenario: CLI ignore excludes file
- **WHEN** `graphos ./source --ignore '**/lib.rs'` runs
- **THEN** all matching `lib.rs` files are excluded from extraction

### Requirement: Ignore application and reporting

The system SHALL apply ignore patterns during the Detect stage so ignored files
are never extracted, and SHALL log the count of ignored files.

#### Scenario: ignored file not extracted
- **WHEN** a file matches an ignore pattern
- **THEN** it produces zero nodes and zero edges
- **AND** an INFO log reports the total number of ignored files
