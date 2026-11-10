## MODIFIED Requirements

### Requirement: Workflow 15 — config init (graphos init)

CLI `graphos init` SHALL generate a `graphos.yaml` file in the current directory with all default config sections: `lsp` (from `defaultServerMap` — 30+ language mappings), `language_ids`, `file_extensions` (default categories per extension), `observability` (otel disabled, default endpoint, default service name), `neo4j` (empty/disabled), `memgraph` (empty/disabled), `labeling` (default model/endpoint/batch/temperature), a commented `sources:` section (one example entry showing `name`, `path`, and `ignore`), and a commented `output:` key. Module `Graphos.Infrastructure.Config` SHALL export `generateDefaultConfig :: IO GraphosConfig` and `writeConfigYaml :: FilePath -> GraphosConfig -> IO ()`. (PRD §14, workflow 15)

#### Scenario: graphos init creates valid config

- **WHEN** `graphos init` is run in an empty directory
- **THEN** `graphos.yaml` SHALL be created with all sections; `lsp` SHALL contain ≥30 entries; `observability.otel_enabled` SHALL be false

#### Scenario: Config cascade: defaults → global → project → CLI

- **WHEN** `~/.config/graphos/graphos.yaml` has `observability.otel_enabled: true` and project config has no observability section
- **THEN** the merged config SHALL have `otel_enabled: true` (global fills in)

#### Scenario: init template documents sources and output

- **WHEN** the generated `graphos.yaml` is inspected
- **THEN** it SHALL contain a commented `sources:` section with an example entry and a commented `output:` key