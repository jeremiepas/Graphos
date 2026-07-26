## ADDED Requirements

### Requirement: Load and apply .gitignore patterns during detection

The detection pipeline SHALL read `.gitignore` from the project root directory and apply its patterns to filter out files and directories during recursive traversal. Patterns SHALL be parsed into `IgnorePattern` values and merged with `.graphosignore` and hardcoded defaults.

- **Plan**: Respect `.gitignore` so that build artifacts, dependency directories, and other ignored files are excluded from detection.
- **Do**: Add `loadGitignore :: FilePath -> IO [IgnorePattern]` to `Infrastructure.FileSystem.Ignore`. Parse blank lines, comments (`#`), directory patterns (`dir/`), glob patterns (`*.log`), and negation (`!pattern`). Merge with other sources.
- **Check**: Files matching `.gitignore` patterns are not detected. Negation patterns re-include previously ignored files.
- **Act**: If full `.gitignore` spec is needed later, extend the parser incrementally.

#### Scenario: .gitignore excludes common directories
- **WHEN** a project root contains `.gitignore` with `node_modules/` and `build/`
- **AND** the project has `node_modules/lodash/index.js` and `build/output.js`
- **THEN** neither `node_modules/lodash/index.js` nor `build/output.js` appears in the detected files

#### Scenario: .gitignore negation re-includes files
- **WHEN** `.gitignore` contains `*.log` followed by `!important.log`
- **THEN** `debug.log` is ignored but `important.log` is detected

#### Scenario: Missing .gitignore is non-fatal
- **WHEN** the project root has no `.gitignore` file
- **THEN** detection proceeds using only hardcoded defaults and `.graphosignore` (if present)
- **AND** no error is raised

### Requirement: Merge ignore patterns from three sources with priority

Ignore patterns SHALL be merged from three sources: hardcoded defaults (lowest priority), `.gitignore` (middle), and `.graphosignore` (highest priority). A file or directory ignored by any source is skipped unless re-included by a negation pattern in a higher-priority source.

- **Plan**: Layer the three ignore sources so that `.graphosignore` can override `.gitignore`, and `.gitignore` can override defaults.
- **Do**: Merge patterns as: `hardcoded ++ gitignorePatterns ++ graphosignorePatterns`. Apply left-to-right; later patterns (higher priority) can negate earlier ones via `NegatePattern`.
- **Check**: `.graphosignore` patterns override `.gitignore` patterns. Hardcoded defaults are always applied.

#### Scenario: .graphosignore overrides .gitignore
- **WHEN** `.gitignore` contains `vendor/` and `.graphosignore` contains `!vendor/`
- **THEN** `vendor/` directory IS traversed (`.graphosignore` overrides `.gitignore`)

#### Scenario: Hardcoded defaults cannot be negated
- **WHEN** hardcoded defaults include `.git` and `.graphosignore` contains `!.git`
- **THEN** `.git` directory is STILL ignored (hardcoded defaults are always applied)

#### Scenario: All three sources combine
- **WHEN** hardcoded defaults include `node_modules`, `.gitignore` includes `*.log`, and `.graphosignore` includes `dist/`
- **THEN** `node_modules/`, `*.log` files, and `dist/` are all excluded from detection