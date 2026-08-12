# full-pipeline Specification

## Purpose
TBD - created by archiving change graphos-product. Update Purpose after archive.
## Requirements
### Requirement: Domain.Types — Node, Edge, Graph, Pipeline, Analysis, Ingest, Extraction
Module `Graphos.Domain.Types.Node` SHALL define `newtype NodeId = NodeId Text`, `data FileType = CodeFile | DocFile | PaperFile | ImageFile | VideoFile | AudioFile`, and `data Node = Node { nodeId :: !NodeId, nodeLabel :: !Text, nodeFileType :: !FileType, nodeSourceFile :: !Text, nodeLineStart :: !(Maybe Int), nodeLineEnd :: !(Maybe Int), nodeSignature :: !(Maybe Text), nodeCommunityId :: !(Maybe CommunityId), nodeKind :: !(Maybe Text), nodeDegree :: !(Maybe Int), nodeIsBridge :: !(Maybe Bool), nodeExtra :: !(Maybe Value) }`. All fields strict (`!`). Module `Graphos.Domain.Types.Edge` SHALL define `newtype EdgeId = EdgeId Text`, `data Relation = Calls | Imports | Extends | Implements | References | Contains | DependsOn | Inferred`, `newtype Confidence = Confidence Double`, and `data Edge = Edge { edgeId :: !EdgeId, edgeSource :: !NodeId, edgeTarget :: !NodeId, edgeRelation :: !Relation, edgeWeight :: !Double, edgeConfidence :: !Confidence }`. Module `Graphos.Domain.Types.Graph` SHALL define `data Extraction = Extraction { extNodes :: Map NodeId Node, extEdges :: Map EdgeId Edge }`, `data LabeledGraph = LabeledGraph { gNodes :: Map NodeId Node, gEdges :: Map EdgeId Edge, gAdjFwd :: Map NodeId (Set NodeId), gAdjBack :: Map NodeId (Set NodeId) }`, `type CommunityId = Int`, `type CommunityMap = Map CommunityId [NodeId]`, and `type CohesionMap = Map CommunityId Double`. Module `Graphos.Domain.Types.Pipeline` SHALL define the seven-stage pipeline state type tracking which stages have completed. (PRD §4.1 Domain.Types)

#### Scenario: Node construction with strict fields
- **WHEN** a `Node` value is constructed
- **THEN** all fields SHALL be strict (no unevaluated thunks allowed), and `NodeId` SHALL be a newtype over `Text`

#### Scenario: LabeledGraph adjacency maps
- **WHEN** a `LabeledGraph` is built
- **THEN** `gAdjFwd` SHALL map each node to its forward neighbors and `gAdjBack` SHALL map each node to its backward neighbors, enabling O(log N) neighbor lookup

### Requirement: Domain.Graph.Core — build, merge, deduplicate, adjacency
Module `Graphos.Domain.Graph.Core` SHALL export pure functions: `buildGraph :: [Extraction] -> Bool -> LabeledGraph` (merges extractions, deduplicates nodes/edges, `Bool` = directed flag), `mergeExtractions :: Extraction -> Extraction -> Extraction`, `mergeGraphs :: LabeledGraph -> LabeledGraph -> LabeledGraph`, `insertNode :: Node -> LabeledGraph -> LabeledGraph`, `insertEdge :: Edge -> LabeledGraph -> LabeledGraph`, `neighbors :: LabeledGraph -> NodeId -> Set NodeId`, `degree :: LabeledGraph -> NodeId -> Int`. All functions pure, no IO. (PRD §4.3, §3.2 Build stage)

#### Scenario: Build graph from extractions with deduplication
- **WHEN** two extractions contain the same `NodeId`
- **THEN** `buildGraph` SHALL merge them keeping the richer metadata (non-Nothing fields take precedence)

#### Scenario: Adjacency maps auto-computed
- **WHEN** `buildGraph` runs on a list of extractions
- **THEN** `gAdjFwd` and `gAdjBack` SHALL be populated by deriving from the edge set

### Requirement: Domain.Graph.FGL — bidirectional adapter to FGL Gr
Module `Graphos.Domain.Graph.FGL` SHALL export: `type FGLGraph = Gr FGLNodeLabel FGLEdgeLabel`, `toFGL :: Map NodeId Node -> Map EdgeId Edge -> FGLGraph`, `fromFGL :: FGLGraph -> Map NodeId Node -> Map EdgeId Edge -> (Map NodeId Node, Map EdgeId Edge)`, `nidToInt :: NodeId -> Int`. The module SHALL NOT import `Graphos.Domain.Graph` (avoids cyclic deps). It SHALL operate on raw `Map`/`Set` components. Conversion SHALL use `nidToInt` (hash Text → Int) for FGL's Int-indexed `Gr`. (PRD §4.3)

#### Scenario: Round-trip preserves graph structure
- **WHEN** `toFGL` converts a graph and `fromFGL` converts it back
- **THEN** the resulting `gNodes` and `gEdges` SHALL contain the same node and edge count as the original

#### Scenario: nidToInt is injective for distinct NodeIds
- **WHEN** two distinct `NodeId` values are hashed via `nidToInt`
- **THEN** the resulting `Int` values SHALL be distinct (no hash collisions for realistic node counts)

### Requirement: Domain.Graph.Query — BFS, DFS, shortest path, subgraph
Module `Graphos.Domain.Graph.Query` SHALL export pure functions: `breadthFirstSearch :: LabeledGraph -> NodeId -> Int -> [NodeId]` (BFS with depth limit), `depthFirstSearch :: LabeledGraph -> NodeId -> Int -> [NodeId]` (DFS with depth limit), `shortestPath :: LabeledGraph -> NodeId -> NodeId -> Maybe [NodeId]`, `subgraph :: LabeledGraph -> [NodeId] -> LabeledGraph`. All via FGL adapter internally. (PRD §3.2, §7)

#### Scenario: BFS returns nodes in breadth-first order
- **WHEN** `breadthFirstSearch` is called on a known graph fixture
- **THEN** returned nodes SHALL be ordered by distance from start node

#### Scenario: Shortest path returns minimal path
- **WHEN** `shortestPath` is called between two connected nodes
- **THEN** the returned path SHALL be the shortest by edge count; disconnected nodes SHALL return `Nothing`

### Requirement: Domain.Graph.Analysis — god nodes, bridges, surprising, suggested questions
Module `Graphos.Domain.Graph.Analysis` SHALL export: `godNodes :: LabeledGraph -> Int -> [(NodeId, Int)]` (top-N by degree), `bridgeNodes :: LabeledGraph -> [NodeId]` (articulation points via FGL), `surprisingConnections :: LabeledGraph -> CommunityMap -> [(NodeId, NodeId, Double)]`. Module `Graphos.Domain.Analysis` SHALL export: `analyze :: LabeledGraph -> CommunityMap -> CohesionMap -> Analysis`, `suggestQuestions :: Analysis -> [Text]`. (PRD §3.2 Analyze stage, §4.1 Domain.Analysis)

#### Scenario: God nodes returns top-N by degree
- **WHEN** `godNodes graph 5` is called
- **THEN** the result SHALL contain exactly 5 nodes sorted by descending degree

#### Scenario: Bridge nodes returns articulation points
- **WHEN** `bridgeNodes` is called on a graph where removing node X disconnects the graph
- **THEN** node X SHALL appear in the result list

### Requirement: Domain.Community — Leiden detection with resolution, merge, cohesion, representatives
Module `Graphos.Domain.Community` SHALL export: `detectCommunities :: LabeledGraph -> CommunityMap`, `detectCommunitiesWithResolution :: LabeledGraph -> Resolution -> CommunityMap`, `cohesionScore :: LabeledGraph -> CommunityMap -> CommunityId -> Double`, `scoreAllCohesion :: LabeledGraph -> CommunityMap -> CohesionMap`, `mergeSmallCommunities :: LabeledGraph -> CommunityMap -> Int -> MergeStrategy -> CommunityMap`. `data Resolution = Resolution { resGamma :: !Double, resMinSize :: !Int, resMergeInto :: !MergeStrategy, resMaxIterations :: !Int }`. `data MergeStrategy = MergeToNeighbor`. `defaultResolution :: Resolution` with gamma=1.0, minSize=3, maxIterations=50. Leiden SHALL have three phases: local moving, refinement (cohesion > 0.5 gate), aggregation. Loop until stable or max iterations. (PRD §5.1, §5.2, §5.4)

#### Scenario: Leiden terminates within max iterations
- **WHEN** `detectCommunitiesWithResolution` runs with `resMaxIterations = 10`
- **THEN** the algorithm SHALL terminate after at most 10 full iterations

#### Scenario: Cohesion score in [0,1]
- **WHEN** `cohesionScore` is computed for any community
- **THEN** the result SHALL be ≥ 0.0 and ≤ 1.0

#### Scenario: Small community merge
- **WHEN** a community has 2 nodes and `resMinSize = 3`
- **THEN** `mergeSmallCommunities` SHALL merge it into the neighboring community with the most shared edges

### Requirement: Domain.Community.Label — community labels
Module `Graphos.Domain.Community.Label` SHALL export: `labelCommunity :: [Node] -> [Edge] -> Text` (pure label suggestion from representative nodes). (PRD §5, workflow 11)

#### Scenario: Label derived from representative nodes
- **WHEN** `labelCommunity` receives representative nodes and their edges
- **THEN** it SHALL produce a Text label summarizing the community's purpose

### Requirement: Domain.Context — query complexity, budgets, selection types, conversation memory
Module `Graphos.Domain.Context` SHALL define: `data QueryComplexity = Focused | Module | CrossModule | Architectural | Exploratory`, `data ContextBudget = ContextBudget { cbGraph :: !Int, cbSource :: !Int, cbHeadroom :: !Double }`, `data SelectionStrategy = CommunityAware | PathBased | GodNodeBridges | RelevanceWeightedBFS`, `data SelectedContext = SelectedContext { scNodes :: [(NodeId, Node)], scEdges :: [(EdgeId, Edge)], scCommunities :: [(CommunityId, [NodeId], Double)], scBudget :: ContextBudget }`, `data ConversationNode = ConversationNode { convId :: Text, convQuestion :: Text, convSummary :: Text, convSourceNodes :: [NodeId], convTimestamp :: UTCTime }`. Budget defaults per PRD §7.2: Focused (500/2000/75%), Module (1500/4000/55%), CrossModule (2500/3000/55%), Architectural (3000/1000/70%), Exploratory (2000/2000/65%). (PRD §7.1, §7.2)

#### Scenario: Focused query budget
- **WHEN** `QueryComplexity = Focused`
- **THEN** `cbGraph = 500`, `cbSource = 2000`, `cbHeadroom = 0.75`

#### Scenario: Architectural query budget
- **WHEN** `QueryComplexity = Architectural`
- **THEN** `cbGraph = 3000`, `cbSource = 1000`, `cbHeadroom = 0.70`

### Requirement: Domain.Config — GraphosConfig with LSP, file extensions, observability, Neo4j, Memgraph
Module `Graphos.Domain.Config` SHALL define: `data GraphosConfig = GraphosConfig { cfgLsp :: Map Text LSPServerConfig, cfgLanguageIds :: Map Text Text, cfgFileExtensions :: Map Text FileType, cfgObservability :: ObservabilityConfig, cfgNeo4j :: Maybe Neo4jConfig, cfgMemgraph :: Maybe MemgraphConfig, cfgThreads :: Int, cfgDirected :: Bool, cfgResolution :: Resolution }`. `data LSPServerConfig = LSPServerConfig { lspCommand :: Text, lspArgs :: [Text], lspLanguageId :: Text }`. `data ObservabilityConfig = ObservabilityConfig { otelEnabled :: Bool, otelEndpoint :: Text, otelServiceName :: Text, metricsPort :: Maybe Int }`. `data Neo4jConfig = Neo4jConfig { neo4jUri :: Text, neo4jUser :: Text, neo4jPassword :: Text, neo4jPushMode :: PushMode, neo4jSubgraphSize :: Int }`. `data PushMode = FullPush | SubgraphPush | CommunityPush`. (PRD §14)

#### Scenario: Default config values
- **WHEN** no config files exist and no CLI flags set
- **THEN** `cfgDirected = False`, `cfgResolution = defaultResolution`, `cfgLsp` SHALL contain 30+ default LSP server mappings, `cfgThreads` SHALL equal number of cores

### Requirement: Domain.Extraction — extraction result validation
Module `Graphos.Domain.Extraction` SHALL define validation functions that verify extraction results have consistent internal references (edge source/target nodes exist, no orphan edges). (PRD §4.1 Domain.Extraction)

#### Scenario: Validate extraction rejects orphan edges
- **WHEN** an `Extraction` contains an edge whose `edgeSource` is not in `extNodes`
- **THEN** validation SHALL return an error indicating the orphan edge

