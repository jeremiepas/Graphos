# 11-community-labeling Specification

## Purpose
TBD - created by archiving change graphos-product. Update Purpose after archive.
## Requirements
### Requirement: Workflow 11 — LLM-based community labeling
Module `Graphos.Domain.Labeling` SHALL export: `batchCommunities :: Int -> CommunityMap -> [[(CommunityId, [NodeId])]]` (groups of N per LLM call), `labelPrompt :: [(CommunityId, [NodeId])] -> LabeledGraph -> Text` (lists members + cohesion + stats). Module `Graphos.UseCase.Label` SHALL export: `labelCommunities :: LabeledGraph -> CommunityMap -> CohesionMap -> LabelingConfig -> IO (Map CommunityId Text)`. Flow: (1) batch communities (default 20 per call from `labelBatchSize`), (2) generate prompt per batch, (3) call LLM via `Infrastructure.LLM.OpenAI`, (4) parse labels into Map. CLI: `--label` flag. Config in `graphos.yaml`: `labeling.model` (default "llama3.2"), `labeling.endpoint` (default "http://localhost:11434/v1"), `labeling.batch_size` (default 20), `labeling.temperature` (default 0.3). Without `--label`: use numeric IDs (Community 1, Community 2, etc.). (PRD §5, workflow 11)

#### Scenario: LLM labels communities
- **WHEN** `--label` is set with a running Ollama instance
- **THEN** each community SHALL get a human-readable name like "Configuration Parsing & Validation"

#### Scenario: No label uses numeric IDs
- **WHEN** `--label` is not set
- **THEN** communities SHALL use numeric identifiers: Community 1, Community 2, etc.

