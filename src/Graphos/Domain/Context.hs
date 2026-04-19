-- | Domain types for LLM context optimization.
-- Pure data types for context selection, budgeting, and conversation memory.
-- No IO dependencies — all logic in Domain/UseCase layers.
module Graphos.Domain.Context
  ( -- * Context selection types
    QueryComplexity(..)
  , ContextBudget(..)
  , SelectedContext(..)
  , SelectionStrategy(..)

    -- * Conversation memory types
  , ConversationNode(..)
  , ConversationRelation(..)

    -- * Chat history community
  , chatCommunityId
  , enrichWithChatHistory
  , chatEdgesForConversation
  , conversationNodeToNode

    -- * Smart constructors
  , defaultBudget
  , budgetForComplexity
  , emptySelectedContext
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject, withText)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Graphos.Domain.Types (NodeId, CommunityId, Node(..), Edge(..), Relation(..), Confidence(..), FileType(..), CommunityMap, GodNode)

-- ───────────────────────────────────────────────
-- Query complexity classification
-- ───────────────────────────────────────────────

-- | How complex a query is — determines context budget allocation.
--   The small LLM classifies queries into these categories.
data QueryComplexity
  = Focused        -- ^ Single function/class lookup
  | ModuleLevel    -- ^ Query about one module/community
  | CrossModule    -- ^ Query spanning multiple modules
  | Architectural  -- ^ Understanding overall structure (god nodes + bridges)
  | Exploratory    -- ^ Broad, unfocused query
  deriving (Eq, Show, Generic, Ord, Bounded, Enum)

instance ToJSON QueryComplexity where
  toJSON Focused       = "focused"
  toJSON ModuleLevel   = "module"
  toJSON CrossModule   = "cross_module"
  toJSON Architectural = "architectural"
  toJSON Exploratory   = "exploratory"

instance FromJSON QueryComplexity where
  parseJSON = withText "QueryComplexity" $ \t -> case t of
    "focused"       -> pure Focused
    "module"        -> pure ModuleLevel
    "cross_module"  -> pure CrossModule
    "architectural" -> pure Architectural
    "exploratory"   -> pure Exploratory
    _               -> fail $ "Unknown query complexity: " ++ T.unpack t

-- ───────────────────────────────────────────────
-- Context budget
-- ───────────────────────────────────────────────

-- | Token budget for context selection.
--   Controls how much graph context to include for the big LLM.
data ContextBudget = ContextBudget
  { cbTotalTokens     :: Int     -- ^ Total token budget for graph context
  , cbGraphRatio      :: Double  -- ^ Fraction of budget for graph structure (0.1–0.4)
  , cbSourceRatio     :: Double  -- ^ Fraction of budget for source code snippets
  , cbComplexity      :: QueryComplexity
  , cbMaxNodes        :: Int     -- ^ Maximum number of nodes to include
  , cbMaxEdges        :: Int     -- ^ Maximum number of edges to include
  } deriving (Eq, Show, Generic)

instance ToJSON ContextBudget where
  toJSON b = object
    [ "total_tokens"  .= cbTotalTokens b
    , "graph_ratio"   .= cbGraphRatio b
    , "source_ratio"  .= cbSourceRatio b
    , "complexity"    .= cbComplexity b
    , "max_nodes"     .= cbMaxNodes b
    , "max_edges"     .= cbMaxEdges b
    ]

instance FromJSON ContextBudget where
  parseJSON = withObject "ContextBudget" $ \v -> ContextBudget
    <$> v .: "total_tokens"
    <*> v .: "graph_ratio"
    <*> v .: "source_ratio"
    <*> v .: "complexity"
    <*> v .: "max_nodes"
    <*> v .: "max_edges"

-- | Default budget with 3000 total tokens
defaultBudget :: ContextBudget
defaultBudget = budgetForComplexity ModuleLevel 3000

-- | Compute budget based on query complexity and total available tokens.
--   Graph ratio varies by complexity level:
--   Focused=0.1, ModuleLevel=0.2, CrossModule=0.3, Architectural=0.4, Exploratory=0.25
budgetForComplexity :: QueryComplexity -> Int -> ContextBudget
budgetForComplexity complexity totalTokens =
  let (graphRatio, sourceRatio, maxN, maxE) = case complexity of
        Focused       -> (0.10, 0.70, 10,  20)
        ModuleLevel   -> (0.20, 0.55, 30,  60)
        CrossModule   -> (0.30, 0.40, 50, 100)
        Architectural -> (0.40, 0.20, 40,  80)
        Exploratory   -> (0.25, 0.45, 40,  80)
  in ContextBudget
    { cbTotalTokens  = totalTokens
    , cbGraphRatio   = graphRatio
    , cbSourceRatio  = sourceRatio
    , cbComplexity   = complexity
    , cbMaxNodes     = maxN
    , cbMaxEdges     = maxE
    }

-- ───────────────────────────────────────────────
-- Context selection result
-- ───────────────────────────────────────────────

-- | Strategy used for context selection
data SelectionStrategy
  = CommunityAware         -- ^ Select based on community membership
  | RelevanceWeightedBFS  -- ^ BFS with relevance scoring
  | PathBased              -- ^ Shortest path between two concepts
  | DifferentialContext    -- ^ Incremental update from previous context
  deriving (Eq, Show, Generic, Ord)

instance ToJSON SelectionStrategy where
  toJSON CommunityAware        = "community_aware"
  toJSON RelevanceWeightedBFS = "relevance_weighted_bfs"
  toJSON PathBased             = "path_based"
  toJSON DifferentialContext   = "differential_context"

instance FromJSON SelectionStrategy where
  parseJSON = withText "SelectionStrategy" $ \t -> case t of
    "community_aware"         -> pure CommunityAware
    "relevance_weighted_bfs"  -> pure RelevanceWeightedBFS
    "path_based"              -> pure PathBased
    "differential_context"    -> pure DifferentialContext
    _                         -> fail $ "Unknown selection strategy: " ++ T.unpack t

-- | The result of context selection — a compact subgraph ready for LLM formatting.
data SelectedContext = SelectedContext
  { scNodes         :: [(NodeId, Node)]              -- ^ Selected nodes with their data
  , scEdges         :: [Edge]                        -- ^ Selected edges
  , scCommunities   :: Map CommunityId [NodeId]      -- ^ Relevant communities and their members
  , scCommunityLabels :: Map CommunityId Text        -- ^ Human-readable community labels
  , scBridgeNodes   :: [NodeId]                      -- ^ Bridge/articulation nodes
  , scGodNodes      :: [GodNode]                     -- ^ High-degree hub nodes
  , scStrategy      :: SelectionStrategy             -- ^ Strategy used for selection
  , scBudget        :: ContextBudget                 -- ^ Budget that was applied
  , scMatchScore    :: Double                        -- ^ Overall relevance score of this selection
  } deriving (Eq, Show, Generic)

instance ToJSON SelectedContext where
  toJSON sc = object
    [ "nodes"           .= scNodes sc
    , "edges"           .= scEdges sc
    , "communities"     .= scCommunities sc
    , "community_labels" .= scCommunityLabels sc
    , "bridge_nodes"    .= scBridgeNodes sc
    , "god_nodes"       .= scGodNodes sc
    , "strategy"        .= scStrategy sc
    , "budget"          .= scBudget sc
    , "match_score"     .= scMatchScore sc
    ]

-- | Empty context for when no matches found
emptySelectedContext :: ContextBudget -> SelectedContext
emptySelectedContext budget = SelectedContext
  { scNodes           = []
  , scEdges           = []
  , scCommunities     = Map.empty
  , scCommunityLabels = Map.empty
  , scBridgeNodes     = []
  , scGodNodes         = []
  , scStrategy        = CommunityAware
  , scBudget          = budget
  , scMatchScore      = 0.0
  }

-- ───────────────────────────────────────────────
-- Conversation memory types
-- ───────────────────────────────────────────────

-- | A conversation exchange stored as a graph node.
--   Enables cross-session memory: the graph remembers past Q&A.
data ConversationNode = ConversationNode
  { convId            :: Text          -- ^ Unique exchange ID
  , convQuestion      :: Text          -- ^ User's question (normalized)
  , convSummary       :: Text          -- ^ LLM's response summary (generated by small LLM)
  , convTimestamp     :: Text          -- ^ ISO 8601 timestamp
  , convRelevantNodes :: [NodeId]      -- ^ Code nodes referenced in this exchange
  , convTokensUsed    :: Int           -- ^ Token cost of this exchange
  } deriving (Eq, Show, Generic)

instance ToJSON ConversationNode where
  toJSON c = object
    [ "id"              .= convId c
    , "question"        .= convQuestion c
    , "summary"         .= convSummary c
    , "timestamp"       .= convTimestamp c
    , "relevant_nodes"  .= convRelevantNodes c
    , "tokens_used"     .= convTokensUsed c
    ]

instance FromJSON ConversationNode where
  parseJSON = withObject "ConversationNode" $ \v -> ConversationNode
    <$> v .: "id"
    <*> v .: "question"
    <*> v .: "summary"
    <*> v .: "timestamp"
    <*> v .: "relevant_nodes"
    <*> v .: "tokens_used"

-- | Relation types for conversation edges.
--   Links a conversation to code nodes it references.
data ConversationRelation
  = Discusses         -- ^ This conversation discusses this code node
  | RelatesTo        -- ^ This conversation is related to this code node
  | FollowUpFrom     -- ^ This conversation is a follow-up to another conversation
  deriving (Eq, Show, Generic, Ord)

instance ToJSON ConversationRelation where
  toJSON Discusses     = "discusses"
  toJSON RelatesTo    = "relates_to"
  toJSON FollowUpFrom = "follow_up_from"

instance FromJSON ConversationRelation where
  parseJSON = withText "ConversationRelation" $ \t -> case t of
    "discusses"      -> pure Discusses
    "relates_to"     -> pure RelatesTo
    "follow_up_from" -> pure FollowUpFrom
    _                -> fail $ "Unknown conversation relation: " ++ T.unpack t

-- ───────────────────────────────────────────────
-- Chat history community
-- ───────────────────────────────────────────────

-- | Reserved community ID for chat history nodes.
--   Added AFTER Leiden detection to prevent polluting community structure.
--   Chat nodes are placed here so they're discoverable in the graph,
--   but they don't inflate code node degrees or skew community detection.
chatCommunityId :: CommunityId
chatCommunityId = 0

-- | Enrich a CommunityMap with a synthetic chat history community.
--   This MUST be called after Leiden detection, never before.
--   Chat nodes are added to community 0 with well-known ID for easy filtering.
enrichWithChatHistory :: CommunityMap -> [ConversationNode] -> CommunityMap
enrichWithChatHistory commMap convs =
  let chatNodeIds = map convId convs
      existing = Map.findWithDefault [] chatCommunityId commMap
  in Map.insert chatCommunityId (existing ++ chatNodeIds) commMap

-- | Create one-way edges from a conversation node to the code nodes it references.
--   Edges go conversation → code (one direction only).
--   This ensures code nodes don't get inflated degrees from chat references.
chatEdgesForConversation :: ConversationNode -> [Edge]
chatEdgesForConversation conv =
  [ Edge
    { edgeSource         = convId conv
    , edgeTarget         = codeNodeId
    , edgeRelation       = References
    , edgeConfidence     = Inferred
    , edgeConfidenceScore = 0.8
    , edgeSourceFile     = "memory/" <> convId conv <> ".md"
    , edgeSourceLocation = Nothing
    , edgeWeight         = 1.0
    }
  | codeNodeId <- convRelevantNodes conv
  ]

-- | Convert a ConversationNode to a graph Node for insertion.
--   Stored as DocumentFile with "memory/" source prefix for identification.
conversationNodeToNode :: ConversationNode -> Node
conversationNodeToNode conv = Node
  { nodeId           = convId conv
  , nodeLabel        = convQuestion conv
  , nodeFileType     = DocumentFile
  , nodeSourceFile   = "memory/" <> convId conv <> ".md"
  , nodeSourceLocation = Nothing
  , nodeLineEnd      = Nothing
  , nodeKind         = Just "Conversation"
  , nodeSignature    = Nothing
  , nodeSourceUrl    = Nothing
  , nodeCapturedAt   = Just (convTimestamp conv)
  , nodeAuthor       = Nothing
  , nodeContributor  = Nothing
  }