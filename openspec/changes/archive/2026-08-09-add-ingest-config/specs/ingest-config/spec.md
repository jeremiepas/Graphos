# Spec: ingest-config

## ADDED Requirements

### Requirement: IngestConfig pure domain types with sensible defaults

The system SHALL provide a pure, IO-free `IngestConfig` type in the Domain layer that captures all ingest-related settings, with a built-in default that preserves backward compatibility (`icEmbed = False`).

- **Plan:** Define `IngestConfig`, `IngestUrlConfig`, `IngestCategoryConfig`, and `IngestCategories` in `Graphos.Domain.Config.Ingest` and re-export from `Graphos.Domain.Config`.
- **Do:** Implement the types with `Eq`, `Show`, `Generic`, and Aeson `ToJSON`/`FromJSON` instances, optional fields for inheritance, and `defaultIngestConfig`.
- **Check:** `cabal build` succeeds and `defaultIngestConfig.icEmbed == False`.
- **Act:** If field naming conflicts with YAML snake_case conventions, adjust `fieldLabelModifier`.

#### Scenario: Default config is backward compatible
- **WHEN** `defaultIngestConfig` is evaluated
- **THEN** `icEmbed` is `False`

#### Scenario: Aeson round-trip preserves optional fields
- **WHEN** an `IngestConfig` is serialized to JSON and parsed back
- **THEN** all fields match, including nested `Maybe` category fields

### Requirement: GraphosConfig carries ingest configuration

The system SHALL extend `GraphosConfig` with a `gcIngest` field and include it in default and merge logic.

- **Plan:** Add `gcIngest :: IngestConfig` to `GraphosConfig` in `Core.hs` and thread it through `defaultGraphosConfig` and `mergeGraphosConfig`.
- **Do:** Update `Core.hs` exports and merge behavior; use dedicated `mergeIngestConfig` if nested `Maybe` merging requires it.
- **Check:** `mergeGraphosConfig` preserves `gcIngest` merge semantics (global → project override).
- **Act:** If the generic merge pattern does not fit nested `Maybe` categories, write a dedicated merge function.

#### Scenario: Merging project config overrides global ingest config
- **WHEN** a project config sets `ingest.embed: true` while global config does not set it
- **THEN** the merged config has `icEmbed = True`

#### Scenario: Merging preserves defaults for unset fields
- **WHEN** neither global nor project config provides an `ingest:` section
- **THEN** the merged config uses `defaultIngestConfig`
