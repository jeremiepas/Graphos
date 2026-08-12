-- | Research View domain types: `ResearchView`, `ResearchNode`, `ResearchCommunity`,
-- `ResearchMetadata` plus `ToJSON` instances for the CLI/HTTP JSON output contract.
--
-- The JSON field names follow the design spec: e.g. `rvNodes` serialises as `"nodes"`,
-- `rnDiscoveredBy` as `"discovered_by"`, etc.
--
-- Pure — no IO, fully testable.
{-# LANGUAGE StrictData #-}
module Graphos.Domain.Query.Research
  ( -- * Research view
    ResearchView(..)
  , ResearchNode(..)

    -- * Community view
  , ResearchCommunity(..)

    -- * Metadata
  , ResearchMetadata(..)

    -- * Utility
  , lookupResearchNode
  ) where

import Data.Aeson (Value, ToJSON(..), object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))

import Graphos.Domain.Types
  ( NodeId, Node(..)
  , Edge(..), edgeRelation, edgeConfidence
  , CommunityId
  )
import Graphos.Domain.Community (CommunityComposition(..))
import Graphos.Domain.Graph.Core (Graph(..), gHash)

-- ---------------------------------------------------------------------------
-- ResearchView
-- ---------------------------------------------------------------------------

-- | A deduplicated, attributed view of the multi-query union subgraph.
data ResearchView = ResearchView
  { rvTerms       :: ![Text]
  , rvNodes       :: ![ResearchNode]
  , rvEdges       :: ![Edge]
  , rvCommunities :: !(Map CommunityId ResearchCommunity)
  , rvMetadata    :: !ResearchMetadata
  } deriving (Eq, Show, Generic)

instance NFData ResearchView

-- | Per-node entry in the research view.  A node is deduplicated by `NodeId`
-- but carries discovery attribution from every query term that matched it.
data ResearchNode = ResearchNode
  { rnNode          :: !Node
  , rnDiscoveredBy  :: ![Text]
  , rnBestScore     :: !Double
  , rnScores        :: ![(Text, Double)]
  } deriving (Eq, Show, Generic)

instance NFData ResearchNode

-- | Community summary in the research view.
data ResearchCommunity = ResearchCommunity
  { rcLabel       :: !(Maybe Text)
  , rcComposition :: !(Maybe CommunityComposition)
  , rcMemberCount :: !Int
  } deriving (Eq, Show, Generic)

instance NFData ResearchCommunity

-- | Timestamp, hash, and counts attached to a `ResearchView`.
data ResearchMetadata = ResearchMetadata
  { rmGeneratedAt :: !UTCTime
  , rmGraphHash   :: !Text
  , rmNodeCount   :: !Int
  , rmEdgeCount   :: !Int
  } deriving (Eq, Show, Generic)

instance NFData ResearchMetadata

-- ---------------------------------------------------------------------------
-- ToJSON instances
-- ---------------------------------------------------------------------------

instance ToJSON ResearchView where
  toJSON rv = object
    [ "terms"        .= rvTerms rv
    , "nodes"        .= map toJSON (rvNodes rv)
    , "edges"        .= map toJSON (rvEdges rv)
    , "communities"  .= object [(Key.fromText (T.pack (show (k :: CommunityId))), toJSON v) | (k, v) <- Map.toList (rvCommunities rv)]
    , "metadata"     .= toJSON (rvMetadata rv)
    ]

instance ToJSON ResearchNode where
  toJSON n = object
    [ "id"             .= nodeId (rnNode n)
    , "label"          .= nodeLabel (rnNode n)
    , "source_file"    .= nodeSourceFile (rnNode n)
    , "community"      .= nodeCommunityId (rnNode n)
    , "discovered_by"  .= rnDiscoveredBy n
    , "best_score"     .= rnBestScore n
    , "scores"         .= object [(Key.fromText t, toJSON s) | (t, s) <- rnScores n]
    ]

instance ToJSON ResearchCommunity where
  toJSON c = object
    [ "label"          .= rcLabel c
    , "composition"    .= rcComposition c
    , "member_count"   .= rcMemberCount c
    ]

instance ToJSON ResearchMetadata where
  toJSON m = object
    [ "generated_at"  .= rmGeneratedAt m
    , "graph_hash"    .= rmGraphHash m
    , "node_count"    .= rmNodeCount m
    , "edge_count"    .= rmEdgeCount m
    ]

-- | Look up a `ResearchNode` by `NodeId` from a `ResearchView`.
lookupResearchNode :: NodeId -> ResearchView -> Maybe ResearchNode
lookupResearchNode nid rv =
  let nodeMap = Map.fromList (map (\n -> (nodeId (rnNode n), n)) (rvNodes rv))
   in Map.lookup nid nodeMap
