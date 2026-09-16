{-# LANGUAGE StrictData #-}
module Graphos.Domain.Types.Graph
  ( -- * Extraction types
    Extraction(..)
  , emptyExtraction
  , extractionFromLists
  , extNodes
  , extEdges

     -- * Graph types
   , LabeledGraph(..)


     -- * Community types
   , CommunityId
   , CommunityMap
   , CohesionMap
   , NullModel(..)

    -- * Push mode
  , PushMode(..)

    -- * Diff types
  , GraphDiff(..)

    -- * Hyperedge types (legacy)
  , Hyperedge(..)
  ) where

import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject, withText, Value(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Graphos.Domain.Types.Node (NodeId, Node(..))
import Graphos.Domain.Types.Edge (EdgeId(..), Edge(..), Confidence)

type CommunityId = Int

type CommunityMap = Map CommunityId [NodeId]

type CohesionMap = Map CommunityId Double

-- | Null models for the weighted modularity baseline (AVI-535 §5).
--
-- Both are degree-preserving configuration null models; they differ only in how
-- self-loops and cross-community edges are counted (undirected vs. directed).
data NullModel
  = DefaultNullModel        -- ^ Undirected degree-preserving configuration null model (§5.1)
  | DirectedNullModel       -- ^ Directed degree-preserving configuration null model (§5.3)
  deriving (Eq, Show, Generic)

instance ToJSON NullModel where
  toJSON DefaultNullModel = String "degree_config_undirected"
  toJSON DirectedNullModel = String "degree_config_directed"

instance FromJSON NullModel where
  parseJSON (String s) = case s of
    "degree_config_undirected" -> pure DefaultNullModel
    "degree_config_directed"   -> pure DirectedNullModel
    _                        -> fail ("unknown NullModel tag: " ++ T.unpack s)
  parseJSON _                = fail "expected a string tag for NullModel"

data Extraction = Extraction
  { extractionNodes :: !(Map NodeId Node)
  , extractionEdges :: !(Map EdgeId Edge)
  } deriving (Eq, Show, Generic)

instance NFData Extraction

emptyExtraction :: Extraction
emptyExtraction = Extraction
  { extractionNodes = Map.empty
  , extractionEdges = Map.empty
  }

extractionFromLists :: [Node] -> [Edge] -> Extraction
extractionFromLists nodes edges = Extraction
  { extractionNodes = Map.fromList [(nodeId n, n) | n <- nodes]
  , extractionEdges = Map.fromList [(edgeId e, e) | e <- edges]
  }

extNodes :: Extraction -> Map NodeId Node
extNodes = extractionNodes

extEdges :: Extraction -> Map EdgeId Edge
extEdges = extractionEdges

instance ToJSON Extraction where
  toJSON e = object
    [ "nodes" .= extractionNodes e
    , "edges" .= extractionEdges e
    ]

instance FromJSON Extraction where
  parseJSON = withObject "Extraction" $ \v -> Extraction
    <$> v .: "nodes"
    <*> v .: "edges"

data LabeledGraph = LabeledGraph
  { gNodes  :: !(Map NodeId Node)
  , gEdges  :: !(Map EdgeId Edge)
  , gAdjFwd :: !(Map NodeId (Set NodeId))
  , gAdjBack :: !(Map NodeId (Set NodeId))
  } deriving (Eq, Show, Generic)

instance NFData LabeledGraph

instance ToJSON LabeledGraph where
  toJSON g = object
    [ "nodes"   .= gNodes g
    , "edges"   .= gEdges g
    , "adj_fwd" .= gAdjFwd g
    , "adj_back" .= gAdjBack g
    ]

instance FromJSON LabeledGraph where
  parseJSON = withObject "LabeledGraph" $ \v -> LabeledGraph
    <$> v .: "nodes"
    <*> v .: "edges"
    <*> v .: "adj_fwd"
    <*> v .: "adj_back"

data PushMode
  = FullPush
  | SubgraphPush
  | CommunityPush
  deriving (Eq, Show, Generic, Ord, Bounded, Enum)

instance NFData PushMode

instance ToJSON PushMode where
  toJSON FullPush      = "full"
  toJSON SubgraphPush  = "subgraph"
  toJSON CommunityPush = "community"

instance FromJSON PushMode where
  parseJSON = withText "PushMode" $ \t -> case t of
    "full"      -> pure FullPush
    "subgraph"  -> pure SubgraphPush
    "community" -> pure CommunityPush
    _           -> fail $ "Unknown push mode: " ++ T.unpack t

data GraphDiff = GraphDiff
  { diffAddedNodes   :: !(Map NodeId Node)
  , diffRemovedNodes  :: !(Map NodeId Node)
  , diffAddedEdges   :: !(Map EdgeId Edge)
  , diffRemovedEdges  :: !(Map EdgeId Edge)
  } deriving (Eq, Show, Generic)

instance NFData GraphDiff

data Hyperedge = Hyperedge
  { hyperedgeId          :: !Text
  , hyperedgeLabel       :: !Text
  , hyperedgeNodes       :: ![NodeId]
  , hyperedgeRelation    :: !Text
  , hyperedgeConfidence  :: !Confidence
  } deriving (Eq, Show, Generic)

instance NFData Hyperedge

instance ToJSON Hyperedge where
  toJSON h = object
    [ "id"               .= hyperedgeId h
    , "label"            .= hyperedgeLabel h
    , "nodes"            .= hyperedgeNodes h
    , "relation"         .= hyperedgeRelation h
    , "confidence"       .= hyperedgeConfidence h
    ]

instance FromJSON Hyperedge where
  parseJSON = withObject "Hyperedge" $ \v -> Hyperedge
    <$> v .: "id"
    <*> v .: "label"
    <*> v .: "nodes"
    <*> v .: "relation"
    <*> v .: "confidence"