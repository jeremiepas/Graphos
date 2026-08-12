# Spec: cli-embed-resolution


## Purpose

Three-state embed CLI flags with category-level resolution.

## Requirements

### Requirement: Ingest CLI supports three-state embed resolution

The system SHALL allow `--embed` to force embedding on, `--no-embed` to force embedding off, and the absence of either flag to defer to configuration.

- **Plan:** Change `IngestCmd` from `IngestCmd FilePath Bool FilePath` to `IngestCmd FilePath (Maybe Bool) FilePath`, and add both flags to the parser.
- **Do:** Use optparse-applicative patterns to produce `Just True`, `Just False`, or `Nothing`, and resolve the final embed value in `app/Main.hs` with `fromMaybe (icEmbed ingestCfg)`.
- **Check:** `cabal build` succeeds and the parser emits the expected `Maybe Bool` for each flag combination.
- **Act:** If mutually-exclusive flag parsing is awkward, use `(<|>)` with `flag'`.

#### Scenario: --embed forces embedding on
- **WHEN** the user passes `--embed`
- **THEN** the CLI embed override is `Just True`

#### Scenario: --no-embed forces embedding off
- **WHEN** the user passes `--no-embed`
- **THEN** the CLI embed override is `Just False`

#### Scenario: No embed flag uses config
- **WHEN** neither `--embed` nor `--no-embed` is passed
- **THEN** the CLI embed override is `Nothing`

### Requirement: Category-level embed and granularity overrides inherit from top-level

The system SHALL resolve per-category embed and granularity using category-specific overrides when present, otherwise falling back to top-level ingest settings.

- **Plan:** Implement `resolveEmbedForCategory` and `resolveGranularityForCategory` in `UseCase.Ingest`.
- **Do:** Use `Maybe IngestCategoryConfig` fields, where `Just b` overrides and `Nothing` inherits.
- **Check:** `.hs` files with `categories.code.embed: true` embed even if top-level `embed: false`.
- **Act:** If per-category complexity is unnecessary for initial users, simplify to top-level only in a future iteration.

#### Scenario: Category override enables embedding
- **WHEN** top-level `embed` is `False` and `categories.code.embed` is `Just True`
- **THEN** a code file is embedded

#### Scenario: Category inherits top-level setting
- **WHEN** top-level `embed` is `True` and category config has no embed override
- **THEN** that category uses the top-level `True` value
