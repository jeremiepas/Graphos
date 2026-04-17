-- | Core domain types for Graphos.
-- These are pure data types with no IO dependencies.
-- All domain logic operates on these types.
{-# LANGUAGE LambdaCase #-}
module Graphos.Domain.Types
  ( -- * Node types
    NodeId
  , Node(..)
  , FileType(..)

    -- * Edge types
  , EdgeId
  , Edge(..)
  , Relation(..)
  , relationToText
  , textToRelation
  , Confidence(..)
  , confidenceScore

    -- * Hyperedge types
  , Hyperedge(..)

    -- * Extraction types
  , Extraction(..)
  , emptyExtraction

    -- * Graph types
  , LabeledGraph(..)

    -- * Community types
  , CommunityId
  , CommunityMap
  , CohesionMap

    -- * Analysis types
  , Analysis(..)
  , GodNode(..)
  , SurprisingConnection(..)
  , SuggestedQuestion(..)
  , GraphDiff(..)

    -- * Detection types
  , Detection(..)
  , FileCategory(..)

    -- * Configuration
  , PipelineConfig(..)
  , EdgeDensity(..)
  , defaultConfig
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), (.:?), withObject, withText)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Unique identifier for a node (derived from file + entity name)
type NodeId = Text

-- | Unique identifier for an edge
type EdgeId = Text

-- | Community identifier (integer)
type CommunityId = Int

-- | File type classification
data FileType
  = CodeFile
  | DocumentFile
  | PaperFile
  | ImageFile
  | VideoFile
  deriving (Eq, Show, Generic)

instance ToJSON FileType where
  toJSON CodeFile     = "code"
  toJSON DocumentFile = "document"
  toJSON PaperFile    = "paper"
  toJSON ImageFile    = "image"
  toJSON VideoFile    = "video"

instance FromJSON FileType where
  parseJSON = withText "FileType" $ \t -> case t of
    "code"     -> pure CodeFile
    "document" -> pure DocumentFile
    "paper"    -> pure PaperFile
    "image"    -> pure ImageFile
    "video"    -> pure VideoFile
    _          -> fail $ "Unknown file type: " ++ T.unpack t

-- | A node in the knowledge graph
data Node = Node
  { nodeId           :: NodeId
  , nodeLabel        :: Text
  , nodeFileType     :: FileType
  , nodeSourceFile   :: Text
  , nodeSourceLocation :: Maybe Text
  , nodeSourceUrl    :: Maybe Text
  , nodeCapturedAt   :: Maybe Text
  , nodeAuthor       :: Maybe Text
  , nodeContributor  :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON Node where
  toJSON n = object
    [ "id"              .= nodeId n
    , "label"           .= nodeLabel n
    , "file_type"       .= nodeFileType n
    , "source_file"     .= nodeSourceFile n
    , "source_location" .= nodeSourceLocation n
    , "source_url"      .= nodeSourceUrl n
    , "captured_at"     .= nodeCapturedAt n
    , "author"          .= nodeAuthor n
    , "contributor"     .= nodeContributor n
    ]

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v -> Node
    <$> v .: "id"
    <*> v .: "label"
    <*> v .: "file_type"
    <*> v .: "source_file"
    <*> v .:? "source_location"
    <*> v .:? "source_url"
    <*> v .:? "captured_at"
    <*> v .:? "author"
    <*> v .:? "contributor"

-- | Relation types for edges
data Relation
  = Calls
  | Implements
  | References
  | Cites
  | ConceptuallyRelatedTo
  | SharesDataWith
  | SemanticallySimilarTo
  | RationaleFor
  | Imports
  | ImportsFrom
  | Contains
  | Method
  | Extends
  | Overrides
  | DependsOn
  deriving (Eq, Show, Generic, Ord)

instance ToJSON Relation where
  toJSON = toJSON . relationToText

instance FromJSON Relation where
  parseJSON = withText "Relation" $ \t ->
    case textToRelation t of
      Just r  -> pure r
      Nothing -> fail $ "Unknown relation: " ++ T.unpack t

relationToText :: Relation -> Text
relationToText = \case
  Calls                    -> "calls"
  Implements                -> "implements"
  References                -> "references"
  Cites                    -> "cites"
  ConceptuallyRelatedTo    -> "conceptually_related_to"
  SharesDataWith           -> "shares_data_with"
  SemanticallySimilarTo    -> "semantically_similar_to"
  RationaleFor             -> "rationale_for"
  Imports                  -> "imports"
  ImportsFrom              -> "imports_from"
  Contains                  -> "contains"
  Method                   -> "method"
  Extends                  -> "extends"
  Overrides                -> "overrides"
  DependsOn                -> "depends_on"

textToRelation :: Text -> Maybe Relation
textToRelation = \case
  "calls"                    -> Just Calls
  "implements"               -> Just Implements
  "references"               -> Just References
  "cites"                    -> Just Cites
  "conceptually_related_to"  -> Just ConceptuallyRelatedTo
  "shares_data_with"         -> Just SharesDataWith
  "semantically_similar_to"  -> Just SemanticallySimilarTo
  "rationale_for"            -> Just RationaleFor
  "imports"                  -> Just Imports
  "imports_from"             -> Just ImportsFrom
  "contains"                 -> Just Contains
  "method"                   -> Just Method
  "extends"                  -> Just Extends
  "overrides"                -> Just Overrides
  "depends_on"               -> Just DependsOn
  _                          -> Nothing

-- | Confidence level for an edge
data Confidence = Extracted | Inferred | Ambiguous
  deriving (Eq, Show, Generic, Ord)

instance ToJSON Confidence where
  toJSON Extracted = "EXTRACTED"
  toJSON Inferred   = "INFERRED"
  toJSON Ambiguous  = "AMBIGUOUS"

instance FromJSON Confidence where
  parseJSON = withText "Confidence" $ \case
    "EXTRACTED" -> pure Extracted
    "INFERRED"  -> pure Inferred
    "AMBIGUOUS" -> pure Ambiguous
    t           -> fail $ "Unknown confidence: " ++ T.unpack t

-- | Convert confidence to a numeric score
confidenceScore :: Confidence -> Double
confidenceScore Extracted = 1.0
confidenceScore Inferred  = 0.7
confidenceScore Ambiguous  = 0.2

-- | An edge in the knowledge graph
data Edge = Edge
  { edgeSource        :: NodeId
  , edgeTarget        :: NodeId
  , edgeRelation      :: Relation
  , edgeConfidence    :: Confidence
  , edgeConfidenceScore :: Double
  , edgeSourceFile    :: Text
  , edgeSourceLocation :: Maybe Text
  , edgeWeight        :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON Edge where
  toJSON e = object
    [ "source"           .= edgeSource e
    , "target"           .= edgeTarget e
    , "relation"         .= edgeRelation e
    , "confidence"       .= edgeConfidence e
    , "confidence_score" .= edgeConfidenceScore e
    , "source_file"      .= edgeSourceFile e
    , "source_location"  .= edgeSourceLocation e
     , "weight"           .= edgeWeight e
     ]

instance FromJSON Edge where
  parseJSON = withObject "Edge" $ \v -> Edge
    <$> v .: "source"
    <*> v .: "target"
    <*> v .: "relation"
    <*> v .: "confidence"
    <*> v .: "confidence_score"
    <*> v .: "source_file"
    <*> v .:? "source_location"
    <*> v .: "weight"

-- | A hyperedge connecting 3+ nodes
data Hyperedge = Hyperedge
  { hyperedgeId          :: Text
  , hyperedgeLabel       :: Text
  , hyperedgeNodes       :: [NodeId]
  , hyperedgeRelation    :: Text
  , hyperedgeConfidence  :: Confidence
  , hyperedgeConfidenceScore :: Double
  , hyperedgeSourceFile  :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Hyperedge where
  toJSON h = object
    [ "id"               .= hyperedgeId h
    , "label"            .= hyperedgeLabel h
    , "nodes"            .= hyperedgeNodes h
    , "relation"         .= hyperedgeRelation h
    , "confidence"       .= hyperedgeConfidence h
    , "confidence_score" .= hyperedgeConfidenceScore h
     , "source_file"      .= hyperedgeSourceFile h
     ]

instance FromJSON Hyperedge where
  parseJSON = withObject "Hyperedge" $ \v -> Hyperedge
    <$> v .: "id"
    <*> v .: "label"
    <*> v .: "nodes"
    <*> v .: "relation"
    <*> v .: "confidence"
    <*> v .: "confidence_score"
    <*> v .: "source_file"

-- | Result of extracting entities from files
data Extraction = Extraction
  { extractionNodes      :: [Node]
  , extractionEdges      :: [Edge]
  , extractionHyperedges :: [Hyperedge]
  , extractionInputTokens  :: Int
  , extractionOutputTokens :: Int
  } deriving (Eq, Show, Generic)

emptyExtraction :: Extraction
emptyExtraction = Extraction
  { extractionNodes      = []
  , extractionEdges      = []
  , extractionHyperedges = []
  , extractionInputTokens  = 0
  , extractionOutputTokens = 0
  }

instance ToJSON Extraction where
  toJSON e = object
    [ "nodes"         .= extractionNodes e
    , "edges"         .= extractionEdges e
    , "hyperedges"    .= extractionHyperedges e
    , "input_tokens"  .= extractionInputTokens e
    , "output_tokens" .= extractionOutputTokens e
    ]

-- | A graph with community labels
data LabeledGraph = LabeledGraph
  { lgNodes      :: [(NodeId, Node)]
  , lgEdges      :: [Edge]
  , lgHyperedges :: [Hyperedge]
  , lgCommunityMap :: CommunityMap
  } deriving (Eq, Show)

-- | Community membership map: community id → member node ids
type CommunityMap = Map CommunityId [NodeId]

-- | Cohesion scores per community
type CohesionMap = Map CommunityId Double

-- | Analysis results
data Analysis = Analysis
  { analysisCommunities :: CommunityMap
  , analysisCohesion     :: CohesionMap
  , analysisGodNodes     :: [GodNode]
  , analysisSurprises    :: [SurprisingConnection]
  , analysisQuestions    :: [SuggestedQuestion]
  } deriving (Eq, Show)

-- | A god node (high-degree hub)
data GodNode = GodNode
  { gnId    :: NodeId
  , gnLabel :: Text
  , gnEdges :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON GodNode where
  toJSON g = object
    [ "id"     .= gnId g
    , "label"  .= gnLabel g
    , "edges"  .= gnEdges g
    ]

instance FromJSON GodNode where
  parseJSON = withObject "GodNode" $ \v -> GodNode
    <$> v .: "id"
    <*> v .: "label"
    <*> v .: "edges"

-- | A surprising cross-community connection
data SurprisingConnection = SurprisingConnection
  { scSource      :: Text
  , scTarget      :: Text
  , scSourceFiles :: [Text]
  , scConfidence  :: Confidence
  , scRelation    :: Text
  , scWhy         :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON SurprisingConnection where
  toJSON s = object
    [ "source"        .= scSource s
    , "target"        .= scTarget s
    , "source_files"  .= scSourceFiles s
    , "confidence"    .= scConfidence s
    , "relation"      .= scRelation s
    , "why"           .= scWhy s
    ]

-- | A suggested question from graph analysis
data SuggestedQuestion = SuggestedQuestion
  { sqType     :: Text
  , sqQuestion :: Maybe Text
  , sqWhy      :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON SuggestedQuestion where
  toJSON q = object
    [ "type"     .= sqType q
    , "question" .= sqQuestion q
    , "why"      .= sqWhy q
    ]

-- | Diff between two graph snapshots
data GraphDiff = GraphDiff
  { gdNewNodes    :: [Node]
  , gdRemovedNodes :: [(NodeId, Text)]
  , gdNewEdges    :: [Edge]
  , gdRemovedEdges :: [Edge]
  , gdSummary     :: Text
  } deriving (Eq, Show)

-- | File detection result
data Detection = Detection
  { detectionTotalFiles  :: Int
  , detectionTotalWords  :: Int
  , detectionNeedsGraph   :: Bool
  , detectionWarning      :: Maybe Text
  , detectionFiles        :: Map FileCategory [FilePath]
  } deriving (Eq, Show)

-- | File categories
data FileCategory
  = CodeFiles
  | DocFiles
  | PaperFiles
  | ImageFiles
  | VideoFiles
  deriving (Eq, Show, Ord, Generic)

instance ToJSON FileCategory where
  toJSON CodeFiles  = "code"
  toJSON DocFiles   = "document"
  toJSON PaperFiles = "paper"
  toJSON ImageFiles = "image"
  toJSON VideoFiles = "video"

-- | Pipeline configuration
data PipelineConfig = PipelineConfig
  { cfgInputPath    :: FilePath
  , cfgOutputDir    :: FilePath
  , cfgDirected     :: Bool
  , cfgDeepMode    :: Bool
  , cfgNoViz        :: Bool
  , cfgUpdate       :: Bool
  , cfgClusterOnly  :: Bool
  , cfgObsidian     :: Bool
  , cfgObsidianDir  :: Maybe FilePath
  , cfgNeo4j        :: Bool
  , cfgNeo4jPush    :: Maybe Text  -- URI
  , cfgMCP          :: Bool
  , cfgSVG          :: Bool
  , cfgGraphML      :: Bool
  , cfgWatch        :: Bool
  , cfgWiki         :: Bool
  , cfgVerbose      :: Bool  -- ^ --verbose: show DEBUG level logs
  , cfgDebug        :: Bool  -- ^ --debug: show TRACE level logs + internal details
  , cfgEdgeDensity  :: EdgeDensity  -- ^ how many inferred edges to add
  , cfgResolution   :: Double       -- ^ community resolution: higher = fewer larger communities (default: 1.0)
  , cfgMinCommSize  :: Int          -- ^ minimum community size; smaller ones get merged (default: 3)
  , cfgThreads      :: Int          -- ^ number of parallel extraction threads (default: 1)
  , cfgCommunityGraph :: Bool      -- ^ export community-level graph JSON for LLM navigation
  } deriving (Eq, Show)

-- | Edge density level for inference
-- Controls how aggressively the pipeline infers additional edges between nodes.
data EdgeDensity
  = Sparse    -- ^ No inferred edges, only extracted ones
  | Normal    -- ^ Community bridge edges + transitive deps (default)
  | Dense     -- ^ All inferred edges: bridges + transitive + shared context
  | Maximum   -- ^ Dense + lower thresholds for shared context
  deriving (Eq, Show, Read)

defaultConfig :: PipelineConfig
defaultConfig = PipelineConfig
  { cfgInputPath    = "."
  , cfgOutputDir    = "graphos-out"
  , cfgDirected     = False
  , cfgDeepMode    = False
  , cfgNoViz        = False
  , cfgUpdate       = False
  , cfgClusterOnly  = False
  , cfgObsidian     = False
  , cfgObsidianDir  = Nothing
  , cfgNeo4j        = False
  , cfgNeo4jPush    = Nothing
  , cfgMCP          = False
  , cfgSVG          = False
  , cfgGraphML      = False
  , cfgWatch        = False
  , cfgWiki         = False
  , cfgVerbose      = False
  , cfgDebug        = False
  , cfgEdgeDensity  = Normal
  , cfgResolution   = 1.0
  , cfgMinCommSize  = 3
  , cfgThreads      = 1
  , cfgCommunityGraph = False
  }