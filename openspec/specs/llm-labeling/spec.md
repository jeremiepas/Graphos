# LLM Labeling

## Purpose

Label graph nodes using LLM API with configurable provider, model, and authentication.
## Requirements
### Requirement: Default labeling provider is Ollama

The system SHALL default `LabelingConfig` to `provider: "ollama"`, `model: "llama3.2"`, `apiKey: ""`, `baseUrl: "http://localhost:11434/v1"`, `batchSize: 20`. When no labeling section is provided in graphos.yaml, these defaults SHALL be used. These defaults SHALL apply to both the full pipeline (`graphos <path> --label`) and the single-file ingest pipeline (`graphos ingest <file> --label`).

#### Scenario: No labeling config in graphos.yaml

- **WHEN** no `labeling` section exists in graphos.yaml
- **THEN** `LabelingConfig` defaults to `{provider: "ollama", model: "llama3.2", apiKey: "", baseUrl: "http://localhost:11434/v1", batchSize: 20, headers: Map.empty}`

#### Scenario: Explicit OpenAI config still works

- **WHEN** graphos.yaml contains `labeling: {provider: openai, model: gpt-4o-mini, apiKey: "${OPENAI_API_KEY}", baseUrl: "https://api.openai.com/v1"}`
- **THEN** the system uses OpenAI for labeling with Bearer token auth for both the full pipeline and `graphos ingest --label`

### Requirement: LabelingConfig headers field

The system SHALL add `labelingHeaders :: Map String String` to `LabelingConfig` with default value `Map.empty`. The `FromJSON` instance SHALL parse a `headers` key from YAML as a string-to-string mapping, defaulting to empty map when absent.

#### Scenario: Labeling with custom headers in YAML
- **WHEN** graphos.yaml contains `labeling: {provider: litellm, baseUrl: "http://proxy:4000/v1", headers: {X-API-Key: "${LITELLM_KEY}"}}`
- **THEN** `callLLM` includes `-H "X-API-Key: <resolved>"` in the curl request

### Requirement: `graphos ingest --label` triggers LLM community labeling

The `graphos ingest` command SHALL accept a `--label` flag (mirroring the existing `--label` on the full pipeline, PRD §13.2). When set, the single-file ingest pipeline SHALL invoke `labelCommunities` after clustering (using the `gcLabeling` config from `graphos.yaml`, per the `llm-labeling` spec defaults) and SHALL pass the resulting `Map CommunityId Text` to `epExportAll` so both `graph.json` (`community_labels` key) and `graph.html` carry the labels. When `--label` is absent, the ingest pipeline SHALL NOT call the LLM and SHALL pass `Nothing` (preserving current behavior).

- Plan: give `graphos ingest` the same labeling capability as the full pipeline so ingested files get human-readable community labels in the frontend.
- Do: add `--label` to `ingestOpts`; thread the flag into `cfgLabel` in the `IngestCmd` handler; in `runSingleFilePipeline`, call `labelCommunities` when `cfgLabel` is set and pass the result to `epExportAll`.
- Check: the scenarios below verify the flag is wired and the labels reach both export formats.
- Act: if the LLM endpoint is unavailable, the pipeline SHALL log a warning and fall back to `Nothing` (no crash); document this in the spec.

#### Scenario: `--label` flag present on ingest

- **WHEN** `graphos ingest <file> --label` is run with a working LLM endpoint configured in `graphos.yaml`
- **THEN** the ingest pipeline calls `labelCommunities` and the resulting `graph.json` contains a non-empty `community_labels` map, and `graph.html`'s embedded `_communityAggregatesData` entries have `label` values from that map (not `"Community <id>"`)

#### Scenario: `--label` flag absent on ingest preserves current behavior

- **WHEN** `graphos ingest <file>` is run without `--label`
- **THEN** the ingest pipeline does NOT call the LLM, `graph.json` has no `community_labels` key, and `graph.html`'s community aggregates use the `"Community <id>"` fallback for `label`

#### Scenario: LLM endpoint unavailable during ingest --label

- **WHEN** `graphos ingest <file> --label` is run but the LLM endpoint is unreachable
- **THEN** the pipeline logs a labeling warning, continues the export with `Nothing` for labels, and exits successfully (non-zero exit only if extraction itself failed)

