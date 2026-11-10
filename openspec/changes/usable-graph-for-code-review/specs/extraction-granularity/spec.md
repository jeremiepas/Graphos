## MODIFIED Requirements

### Requirement: Granularity resolution order

The effective granularity for a file MUST be resolved as: CLI `--granularity` flag, else per-extension `ExtractorConfig` override, else global `extraction.granularity` (top-level `granularity` key of `graphos.yaml`), else the built-in default `function` (PRD §14). Resolution SHALL be a pure function of configuration values, and the resolved level SHALL be the one handed to every extractor adapter: the tree-sitter FFI wiring MUST convert with the level resolved for the file, never with a constant.

#### Scenario: CLI flag wins over all config

- **WHEN** the CLI passes `--granularity fine` and config sets global `function` with a `.json` override of `file`
- **THEN** every file is extracted at `fine` level

#### Scenario: Per-extension override wins over global

- **WHEN** no CLI flag is given, global granularity is `function`, and `.json` has a `file` override
- **THEN** `.ts` files extract at `function` and `.json` files extract at `file`

#### Scenario: Built-in default applies

- **WHEN** neither CLI flag nor any config granularity is set
- **THEN** files extract at `function` level

#### Scenario: Global file level reaches the tree-sitter converter

- **WHEN** a project `graphos.yaml` sets top-level `granularity: file`, its `.ts` extractor entry
  sets no granularity, and the pipeline runs over `example/ts-lsp-test`
- **THEN** every `.ts` file contributes exactly one `Module` node and the log line
  `Granularity: file` matches the emitted node count, verifiable by `cabal test` as an
  integration scenario
