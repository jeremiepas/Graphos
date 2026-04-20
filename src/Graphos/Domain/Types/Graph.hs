-- | Graph-level types: hyperedges, extractions, labeled graphs, communities, and diffs.
-- Pure data types with no IO dependencies.
module Graphos.Domain.Types.Graph
  ( -- * Hyperedge types
    Hyperedge(..)

    -- * Extraction types
  , Extraction(..)
  , emptyExtraction

    -- * Graph types
  , LabeledGraph(..)

    -- * Community types
  , CommunityId
  , CommunityMap
  , CohesionMap

    -- * Diff types
  , GraphDiff(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Graphos.Domain.Types.Node (NodeId, Node)
import Graphos.Domain.Types.Edge (Edge, Confidence)

-- | Community identifier (integer)
type CommunityId = Int

-- | Community membership map: community id → member node ids
type CommunityMap = Map CommunityId [NodeId]

-- | Cohesion scores per community
type CohesionMap = Map CommunityId Double

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

-- | Empty extraction with no nodes, edges, or tokens
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
  { lgNodes       :: [(NodeId, Node)]
  , lgEdges       :: [Edge]
  , lgHyperedges  :: [Hyperedge]
  , lgCommunityMap :: CommunityMap
  } deriving (Eq, Show)

-- | Diff between two graph snapshots
data GraphDiff = GraphDiff
  { gdNewNodes    :: [Node]
  , gdRemovedNodes :: [(NodeId, Text)]
  , gdNewEdges    :: [Edge]
  , gdRemovedEdges :: [Edge]
  , gdSummary     :: Text
  } deriving (Eq, Show)