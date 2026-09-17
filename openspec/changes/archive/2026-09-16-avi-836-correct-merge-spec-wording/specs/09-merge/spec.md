# 09-merge

Delta — correct `mergeGraphs` conflict-resolution wording to match `Data.Map.union` left bias (old-wins).

## MODIFIED Requirements

### Requirement: Workflow 09 — merge two knowledge graphs

Module `Graphos.UseCase.Merge` SHALL export: `mergeGraphs :: LabeledGraph -> LabeledGraph -> LabeledGraph`. CLI: `graphos merge <path-a> <path-b> -o <output-dir>`. Flow: (1) load graph A and graph B, (2) merge via `Domain.Graph.Core.mergeGraphs` (deduplicate by NodeId — **old-wins**: on a conflicting NodeId the left operand / view A wins, since `Data.Map.union` is left-biased (`gNodes A <> gNodes B` keeps A's content at shared keys and unions B's exclusive keys); edges unioned old-wins too; preserve A's directed flag), (3) re-cluster merged graph via Leiden (old community IDs discarded), (4) infer edges, (5) analyze, (6) export to output-dir. (PRD §13, workflow 09)

#### Scenario: Merge deduplicates and re-clusters
- **WHEN** merging two graphs with overlapping NodeIds
- **THEN** duplicate NodeIds merged (**old-wins**: view A / left operand wins on conflict; non-conflicting content from both views is unioned), edges unioned, result re-clustered with fresh community IDs
