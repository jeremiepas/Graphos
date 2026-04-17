-- | Community-level graph export for LLM navigation.
-- Produces a condensed graph where nodes = communities and edges = inter-community connections.
-- Optimized for LLM context window consumption — typically 10-50 nodes instead of thousands.
module Graphos.Infrastructure.Export.CommunityGraph
  ( exportCommunityGraph
  ) where

import Data.Aeson (ToJSON(..), object, (.=), encode)
import qualified Data.ByteString.Lazy as BSL
import Data.List (sortOn, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types
  ( NodeId, CommunityId, CommunityMap, Node(..)
  , edgeSource, edgeTarget
  , nodeLabel )
import Graphos.Domain.Graph (Graph, gNodes, gEdges, neighbors)
import Graphos.Domain.Community (cohesionScore)

-- ───────────────────────────────────────────────
-- JSON types for community graph
-- ───────────────────────────────────────────────

data CommunityNode = CommunityNode
  { cnId         :: CommunityId
  , cnLabel      :: Text
  , cnSize       :: Int
  , cnCohesion   :: Double
  , cnTopMembers :: [Text]
  } deriving (Show)

instance ToJSON CommunityNode where
  toJSON n = object
    [ "id"         .= cnId n
    , "label"      .= cnLabel n
    , "size"       .= cnSize n
    , "cohesion"   .= cnCohesion n
    , "top_members" .= cnTopMembers n
    ]

data CommunityEdge = CommunityEdge
  { ceSource     :: CommunityId
  , ceTarget     :: CommunityId
  , ceEdgeCount  :: Int
  , ceBridgeNodes :: [Text]
  } deriving (Show)

instance ToJSON CommunityEdge where
  toJSON e = object
    [ "source"      .= ceSource e
    , "target"      .= ceTarget e
    , "edge_count"  .= ceEdgeCount e
    , "bridge_nodes" .= ceBridgeNodes e
    ]

data CommunityGraphJSON = CommunityGraphJSON
  { cgnNodes :: [CommunityNode]
  , cgnEdges :: [CommunityEdge]
  , cgnSummary :: Text
  } deriving (Show)

instance ToJSON CommunityGraphJSON where
  toJSON g = object
    [ "nodes"   .= cgnNodes g
    , "edges"   .= cgnEdges g
    , "summary" .= cgnSummary g
    ]

-- ───────────────────────────────────────────────
-- Export function
-- ───────────────────────────────────────────────

-- | Export a community-level graph as JSON.
-- Nodes are communities (with label, size, cohesion, top members).
-- Edges are inter-community connections (with edge count and bridge nodes).
exportCommunityGraph :: Graph -> CommunityMap -> FilePath -> IO ()
exportCommunityGraph g commMap outPath = do
  let reverseIdx = buildNodeCommunityMap commMap
      commNodes = buildCommunityNodes g commMap
      commEdges = buildCommunityEdges g commMap reverseIdx
      summary = T.pack $ show (Map.size commMap) ++ " communities, "
             ++ show (length commEdges) ++ " inter-community connections"
      cg = CommunityGraphJSON
        { cgnNodes = commNodes
        , cgnEdges = commEdges
        , cgnSummary = summary
        }
  BSL.writeFile outPath (encode cg)

-- ───────────────────────────────────────────────
-- Build community nodes
-- ───────────────────────────────────────────────

buildCommunityNodes :: Graph -> CommunityMap -> [CommunityNode]
buildCommunityNodes g commMap =
  [ CommunityNode
    { cnId       = cid
    , cnLabel    = communityLabel g members
    , cnSize     = length members
    , cnCohesion = cohesionScore g members
    , cnTopMembers = take 5 [nodeLabel n | nid <- members
                                         , Just n <- [Map.lookup nid (gNodes g)]]
    }
  | (cid, members) <- Map.toList commMap
  ]

-- | Simple label: use the most common word from top-degree member labels
communityLabel :: Graph -> [NodeId] -> Text
communityLabel g members =
  let nodesWithDeg = [(n, degree') | nid <- members
                                    , Just n <- [Map.lookup nid (gNodes g)]
                                    , let degree' = Set.size (neighbors g nid)]
      sorted = sortOn (\(_, d) -> negate d) nodesWithDeg
      topLabels = take 3 [nodeLabel n | (n, _) <- sorted]
  in if null topLabels then "Unnamed" else T.intercalate ", " topLabels

-- ───────────────────────────────────────────────
-- Build community edges
-- ───────────────────────────────────────────────

buildCommunityEdges :: Graph -> CommunityMap -> Map NodeId CommunityId -> [CommunityEdge]
buildCommunityEdges g _commMap reverseIdx =
  let -- Count edges between communities
      edgeCounts :: Map (CommunityId, CommunityId) (Int, [NodeId])
      edgeCounts = Map.fromListWith (\(c1, b1) (c2, b2) -> (c1 + c2, nub (b1 ++ b2)))
        [ let srcComm = Map.findWithDefault (-1) (edgeSource e) reverseIdx
              tgtComm = Map.findWithDefault (-1) (edgeTarget e) reverseIdx
              -- Normalize direction: smaller community id first
              (c1, c2) = if srcComm <= tgtComm then (srcComm, tgtComm) else (tgtComm, srcComm)
              bridgeNode = edgeSource e
          in ((c1, c2), (1, [bridgeNode]))
        | (_, e) <- Map.toList (gEdges g)
        , let srcC = Map.findWithDefault (-1) (edgeSource e) reverseIdx
              tgtC = Map.findWithDefault (-1) (edgeTarget e) reverseIdx
        , srcC /= tgtC  -- only inter-community edges
        , srcC >= 0 && tgtC >= 0
        ]
  in [ CommunityEdge
     { ceSource     = c1
     , ceTarget     = c2
     , ceEdgeCount  = count
     , ceBridgeNodes = take 5 bridges
     }
     | ((c1, c2), (count, bridges)) <- Map.toList edgeCounts
     ]

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

buildNodeCommunityMap :: CommunityMap -> Map NodeId CommunityId
buildNodeCommunityMap commMap = Map.fromList
  [(nid, cid) | (cid, members) <- Map.toList commMap, nid <- members]