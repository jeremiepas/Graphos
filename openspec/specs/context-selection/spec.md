# context-selection Specification

## Purpose
TBD - created by archiving change graphos-product. Update Purpose after archive.
## Requirements
### Requirement: Domain.Context — QueryComplexity classifier and budget allocation
Module `Graphos.Domain.Context` SHALL define `data QueryComplexity = Focused | Module | CrossModule | Architectural | Exploratory`. Classification logic: single-function mention → Focused; module/namespace mention → Module; mentions across modules → CrossModule; system/architecture keywords → Architectural; broad/exploratory terms → Exploratory. Token budget per complexity: `Focused` → `ContextBudget { cbGraph = 500, cbSource = 2000, cbHeadroom = 0.75 }`; `Module` → `1500, 4000, 0.55`; `CrossModule` → `2500, 3000, 0.55`; `Architectural` → `3000, 1000, 0.70`; `Exploratory` → `2000, 2000, 0.65`. Override via `--budget N`. (PRD §7.1, §7.2)

#### Scenario: Classify single-function query as Focused
- **WHEN** query contains a specific function name like "mkEmail"
- **THEN** `QueryComplexity = Focused` and `cbGraph = 500`

#### Scenario: Classify architecture query
- **WHEN** query contains terms like "system overview" or "architecture"
- **THEN** `QueryComplexity = Architectural` and `cbGraph = 3000`

### Requirement: UseCase.SelectContext — five-strategy context selection
Module `Graphos.UseCase.SelectContext` SHALL export `selectContext :: LabeledGraph -> CommunityMap -> CohesionMap -> Text -> Maybe Int -> Bool -> SelectedContext`. Strategy mapping: Focused → `CommunityAware` (match node → include community members + bridges); Module → `CommunityAware` + bridge nodes; CrossModule → `PathBased` (shortest path + neighbors along path); Architectural → `GodNodeBridges` (top god nodes + all bridge nodes + community structure); Exploratory → `RelevanceWeightedBFS` (BFS weighted by keyword match score). (PRD §7.1)

#### Scenario: Focused query returns community-aware context
- **WHEN** `selectContext` processes a Focused query for node "parseConfig"
- **THEN** result SHALL include all nodes in "parseConfig"'s community plus bridge nodes, within 500 graph tokens

#### Scenario: CrossModule query returns path
- **WHEN** `selectContext` processes a CrossModule query between "Auth" and "Database" modules
- **THEN** result SHALL include the shortest path between relevant nodes plus their immediate neighbors, within 2500 graph tokens

### Requirement: UseCase.FormatContext — compact markdown output
Module `Graphos.UseCase.FormatContext` SHALL export `formatContextForLLM :: SelectedContext -> Text`. Output SHALL be compact markdown with: `## Nodes` section (each node: id, kind, signature, line_start-line_end, community_id, degree, is_bridge), `## Edges` section (each edge: from → to, relation, weight), `## Communities` section (each community: label, member count, cohesion, bridges). Target token cost per PRD §7.3: node metadata ~50 tokens, edge ~20 tokens, community ~100 tokens. (PRD §7.3)

#### Scenario: Node output includes all metadata fields
- **WHEN** a node with all metadata is formatted
- **THEN** the output SHALL include: `id`, `kind`, `signature`, `lines` (start-end), `community`, `degree`, `bridge` fields

#### Scenario: Community output includes bridges
- **WHEN** a community with bridge nodes is formatted
- **THEN** the output SHALL list bridge nodes connecting to other communities

### Requirement: UseCase.Conversation — community 0 non-polluting chat memory
Module `Graphos.UseCase.Conversation` SHALL export: `addConversation :: LabeledGraph -> CommunityMap -> ConversationNode -> (LabeledGraph, CommunityMap)` (adds conversation node to community 0 with one-way edges: conv → code). Community 0 SHALL be a synthetic community created after Leiden runs. Edges SHALL be one-way: `conversation → code` only. Code node degrees SHALL NOT change. `selectContext` SHALL exclude community 0 by default; include when `include_history = True`. Conversations SHALL be persisted to `graphos-out/memory/conv_*.md` in YAML frontmatter + markdown body format. (PRD §8.3)

#### Scenario: Add conversation preserves code degrees
- **WHEN** `addConversation` is called with a conversation referencing node X
- **THEN** node X's degree in `LabeledGraph.gNodes` SHALL remain unchanged; the edge SHALL only exist in community 0

#### Scenario: Exclude community 0 by default
- **WHEN** `selectContext` is called with `include_history = False`
- **THEN** the result SHALL NOT contain any community 0 conversation nodes or edges

#### Scenario: Include community 0 when requested
- **WHEN** `selectContext` is called with `include_history = True`
- **THEN** the result SHALL include relevant community 0 conversation nodes

### Requirement: Infrastructure.FileSystem.Conversation — persist and reload conversation files
Module `Graphos.Infrastructure.FileSystem.Conversation` SHALL export: `saveConversation :: ConversationNode -> FilePath -> IO ()`, `loadConversations :: FilePath -> IO [ConversationNode]`. File format: YAML frontmatter (`id:`, `question:`, `summary:`, `source_nodes:`, `timestamp:`) + markdown body. Files SHALL be named `conv_<id>.md` in `graphos-out/memory/`. (PRD §8.3)

#### Scenario: Save and reload conversation
- **WHEN** a conversation is saved via `saveConversation` and then `loadConversations` is called
- **THEN** the reloaded `ConversationNode` SHALL match the original in all fields

