## ADDED Requirements

### Requirement: Workflow 07 — select_context five-strategy pipeline
Module `Graphos.UseCase.SelectContext` SHALL export: `selectContext :: LabeledGraph -> CommunityMap -> CohesionMap -> Text -> Maybe Int -> Bool -> SelectedContext`. Step 1: classify `QueryComplexity` (Focused/Module/CrossModule/Architectural/Exploratory). Step 2: select strategy: Focused+Module → CommunityAware (match node → include community + bridges); CrossModule → PathBased (shortest path + neighbors); Architectural → GodNodeBridges (god nodes + all bridges + community structure); Exploratory → RelevanceWeightedBFS (BFS weighted by label similarity +3, same community +2, extracted confidence +2, inferred +1, bridge +2, god +1). Step 3: allocate `defaultBudget` per complexity. Step 4: execute selection within budget. `include_history` controls community 0 inclusion. (PRD §7, workflow 07)

#### Scenario: CommunityAware for Focused query
- **WHEN** query targets a specific function "parseConfig"
- **THEN** result SHALL include parseConfig's community members + bridge nodes, within 500 graph tokens

#### Scenario: PathBased for CrossModule query
- **WHEN** query spans "Auth" and "Database"
- **THEN** result SHALL include shortest path + immediate neighbors within 2500 graph tokens

#### Scenario: GodNodeBridges for Architectural query
- **WHEN** query asks about system architecture
- **THEN** result SHALL include god nodes + all bridge nodes + community structure within 3000 graph tokens

#### Scenario: RelevanceWeightedBFS for Exploratory query
- **WHEN** query is broad like "error handling"
- **THEN** result SHALL include nodes scored by relevance (label similarity, community, confidence, bridge/god status) within 2000 graph tokens

### Requirement: Workflow 07 — compact markdown formatting
Module `Graphos.UseCase.FormatContext` SHALL export: `formatContextForLLM :: SelectedContext -> Text`. Output: `## Nodes` (each: id, kind, signature, lines, community, degree, bridge), `## Edges` (each: from→to, relation, confidence), `## Communities` (each: label, size, cohesion, bridges). Node metadata per PRD §7.3: kind (+1 tok), line_start+line_end (+3), signature (+5–10), community_id (+1), degree (+1), is_bridge (+1). (PRD §7.3, workflow 07)

#### Scenario: Markdown output includes all metadata
- **WHEN** `formatContextForLLM` is called on a `SelectedContext`
- **THEN** output SHALL be valid markdown with `## Nodes`, `## Edges`, `## Communities` sections; each node includes kind, signature, line range, community, degree, bridge flag