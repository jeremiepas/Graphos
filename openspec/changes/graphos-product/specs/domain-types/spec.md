## ADDED Requirements

### Requirement: Domain.Types.Node — NodeId, FileType, Node with 12 strict fields
Module `Graphos.Domain.Types.Node` SHALL define: `newtype NodeId = NodeId Text`; `data FileType = CodeFile | DocFile | PaperFile | ImageFile | VideoFile | AudioFile` deriving (Eq, Show, Generic, ToJSON, FromJSON); `data Node = Node { nodeId :: !NodeId, nodeLabel :: !Text, nodeFileType :: !FileType, nodeSourceFile :: !Text, nodeLineStart :: !(Maybe Int), nodeLineEnd :: !(Maybe Int), nodeSignature :: !(Maybe Text), nodeCommunityId :: !(Maybe CommunityId), nodeKind :: !(Maybe Text), nodeDegree :: !(Maybe Int), nodeIsBridge :: !(Maybe Bool), nodeExtra :: !(Maybe Value) }` deriving (Eq, Show, Generic, ToJSON, FromJSON). All fields strict (`!`) per PRD memory optimization. (PRD §4.1, §16.2 StrictData)

#### Scenario: Node construction with strict fields
- **WHEN** a `Node` value is constructed with all fields
- **THEN** all 12 fields SHALL be strict (no unevaluated thunks), and `NodeId` SHALL be `newtype NodeId = NodeId Text`

#### Scenario: FileType covers all input categories
- **WHEN** `FileType` is enumerated
- **THEN** it SHALL have exactly 6 constructors: CodeFile, DocFile, PaperFile, ImageFile, VideoFile, AudioFile

### Requirement: Domain.Types.Edge — EdgeId, Relation (8 constructors), Confidence, Edge
Module `Graphos.Domain.Types.Edge` SHALL define: `newtype EdgeId = EdgeId Text`; `data Relation = Calls | Imports | Extends | Implements | References | Contains | DependsOn | Inferred` deriving (Eq, Show, Generic, ToJSON, FromJSON); `newtype Confidence = Confidence Double` deriving (Eq, Show); `data Edge = Edge { edgeId :: !EdgeId, edgeSource :: !NodeId, edgeTarget :: !NodeId, edgeRelation :: !Relation, edgeWeight :: !Double, edgeConfidence :: !Confidence }` deriving (Eq, Show, Generic, ToJSON, FromJSON). Functions: `relationToText :: Relation -> Text`, `textToRelation :: Text -> Maybe Relation`. (PRD §4.1)

#### Scenario: Relation has 8 constructors
- **WHEN** `Relation` is enumerated
- **THEN** it SHALL have exactly 8 constructors: Calls, Imports, Extends, Implements, References, Contains, DependsOn, Inferred

### Requirement: Domain.Types.Graph — LabeledGraph with adjacency maps, CommunityMap, CohesionMap
Module `Graphos.Domain.Types.Graph` SHALL define: `data Extraction = Extraction { extNodes :: Map NodeId Node, extEdges :: Map EdgeId Edge }`; `emptyExtraction :: Extraction`; `type CommunityId = Int`; `type CommunityMap = Map CommunityId [NodeId]`; `type CohesionMap = Map CommunityId Double`; `data LabeledGraph = LabeledGraph { gNodes :: Map NodeId Node, gEdges :: Map EdgeId Edge, gAdjFwd :: Map NodeId (Set NodeId), gAdjBack :: Map NodeId (Set NodeId) }`; `data GraphDiff = GraphDiff { diffAddedNodes :: Map NodeId Node, diffRemovedNodes :: Map NodeId Node, diffAddedEdges :: Map EdgeId Edge, diffRemovedEdges :: Map EdgeId Edge }`; `data PushMode = FullPush | SubgraphPush | CommunityPush` deriving (Eq, Show, Generic, ToJSON, FromJSON). (PRD §4.1, §9.1)

#### Scenario: LabeledGraph adjacency maps
- **WHEN** a `LabeledGraph` is built
- **THEN** `gAdjFwd` SHALL map each node to forward neighbors, `gAdjBack` to backward neighbors

### Requirement: Domain.Types.Pipeline — seven-stage pipeline state
Module `Graphos.Domain.Types.Pipeline` SHALL define `data PipelineState` tracking completion of stages: Detect → Extract → Build → Cluster → Infer → Analyze → Export. Functions: `initialPipelineState :: PipelineState`, `advanceStage :: PipelineStage -> PipelineState -> PipelineState`, `checkpointPath :: Text`. (PRD §3.1, §3.3)

#### Scenario: Pipeline state advances through stages
- **WHEN** `advanceStage Build` is called on a state at Extract
- **THEN** the state SHALL record Build as completed

### Requirement: Domain.Types.Analysis — Analysis result with god nodes, bridges, surprises
Module `Graphos.Domain.Types.Analysis` SHALL define: `data Analysis = Analysis { analysisGodNodes :: [(NodeId, Int)], analysisBridgeNodes :: [NodeId], analysisSurprisingConnections :: [(NodeId, NodeId, Double)], analysisSuggestedQuestions :: [Text], analysisCommunities :: CommunityMap, analysisCohesion :: CohesionMap }`. (PRD §3.2 Analyze, workflow 01 stage 6)

#### Scenario: Analysis contains all required fields
- **WHEN** an `Analysis` value is constructed
- **THEN** it SHALL contain god nodes, bridge nodes, surprising connections, suggested questions, community map, and cohesion map

### Requirement: Domain.Types.Ingest — IngestResult and IngestIndex
Module `Graphos.Domain.Types.Ingest` SHALL define: `data IngestResult = IngestResult { irExtraction :: Extraction, irEmbeddings :: Maybe (Map NodeId [Double]) }`; `data IngestIndex = IngestIndex { iiNodes :: Map NodeId [Double] }`. Functions: `lookupEmbedding :: NodeId -> IngestIndex -> Maybe [Double]`, `mergeIndex :: IngestIndex -> IngestIndex -> IngestIndex` (right-biased). (PRD §11, workflow 10)

#### Scenario: IngestIndex O(1) lookup
- **WHEN** `lookupEmbedding` is called with a known `NodeId`
- **THEN** it SHALL return `Just [Double]` in O(1)

### Requirement: Domain.Config — GraphosConfig with all sub-configs
Module `Graphos.Domain.Config` SHALL define: `data GraphosConfig = GraphosConfig { cfgLsp :: Map Text LSPServerConfig, cfgLanguageIds :: Map Text Text, cfgFileExtensions :: Map Text FileType, cfgObservability :: ObservabilityConfig, cfgNeo4j :: Maybe Neo4jConfig, cfgMemgraph :: Maybe MemgraphConfig, cfgThreads :: !Int, cfgDirected :: !Bool, cfgResolution :: Resolution, cfgEdgeDensity :: !Double, cfgLabeling :: Maybe LabelingConfig }`; `data LSPServerConfig = LSPServerConfig { lspCommand :: !Text, lspArgs :: ![Text], lspLanguageId :: !Text }`; `data ObservabilityConfig = ObservabilityConfig { otelEnabled :: !Bool, otelEndpoint :: !Text, otelServiceName :: !Text, metricsPort :: !(Maybe Int) }`; `data Neo4jConfig = Neo4jConfig { neo4jUri :: !Text, neo4jUser :: !Text, neo4jPassword :: !Text, neo4jPushMode :: !PushMode, neo4jSubgraphSize :: !Int }`; `data MemgraphConfig` analogous; `data LabelingConfig = LabelingConfig { labelModel :: !Text, labelEndpoint :: !Text, labelBatchSize :: !Int, labelTemperature :: !Double }`. (PRD §14)

#### Scenario: Default GraphosConfig values
- **WHEN** no config files exist and no CLI flags set
- **THEN** `cfgDirected = False`, `cfgResolution = defaultResolution`, `cfgEdgeDensity = 0.0`, `cfgThreads = numCapabale`, `cfgObservability = ObservabilityConfig False "http://localhost:4318" "graphos" Nothing`

### Requirement: Domain.Context — QueryComplexity, ContextBudget, SelectedContext, ConversationNode
Module `Graphos.Domain.Context` SHALL define: `data QueryComplexity = Focused | Module | CrossModule | Architectural | Exploratory` deriving (Eq, Show); `data ContextBudget = ContextBudget { cbGraph :: !Int, cbSource :: !Int, cbHeadroom :: !Double }`; `data SelectionStrategy = CommunityAware | PathBased | GodNodeBridges | RelevanceWeightedBFS`; `data SelectedContext = SelectedContext { scNodes :: [(NodeId, Node)], scEdges :: [(EdgeId, Edge)], scCommunities :: [(CommunityId, [NodeId], Double)], scBudget :: !ContextBudget, scStrategy :: !SelectionStrategy }`; `defaultBudget :: QueryComplexity -> ContextBudget` returning values per workflow 07; `data ConversationNode = ConversationNode { convId :: !Text, convQuestion :: !Text, convSummary :: !Text, convSourceNodes :: ![NodeId], convTimestamp :: !UTCTime }`. (PRD §7, workflow 07)

#### Scenario: defaultBudget for Focused
- **WHEN** `defaultBudget Focused` is called
- **THEN** result SHALL be `ContextBudget { cbGraph = 500, cbSource = 2000, cbHeadroom = 0.75 }`

#### Scenario: defaultBudget for Architectural
- **WHEN** `defaultBudget Architectural` is called
- **THEN** result SHALL be `ContextBudget { cbGraph = 3000, cbSource = 1000, cbHeadroom = 0.70 }`

### Requirement: Domain.Extraction — validation: no orphan edges
Module `Graphos.Domain.Extraction` SHALL define: `validateExtraction :: Extraction -> Either [Text] Extraction`. SHALL verify every edge's `edgeSource` and `edgeTarget` exist in `extNodes`. Return `Left errors` listing orphan edges, or `Right extraction` if valid. (PRD §4.1)

#### Scenario: Valid extraction passes validation
- **WHEN** all edges reference existing nodes
- **THEN** `validateExtraction` SHALL return `Right extraction`

#### Scenario: Orphan edge fails validation
- **WHEN** an edge references a `NodeId` not in `extNodes`
- **THEN** `validateExtraction` SHALL return `Left` with error listing the orphan edge