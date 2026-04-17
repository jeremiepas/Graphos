-- | Edge inference - create additional edges by analyzing graph structure.
-- 
-- After initial extraction, the graph often has sparse connections between
-- communities. This module infers new edges based on:
--   * Community centroids (highest-degree nodes) → ConceptuallyRelatedTo
--   * Shared neighbors → SharesDataWith
--   * Transitive dependencies → DependsOn
--   * Bridge nodes (articulation points) → RationaleFor
module Graphos.UseCase.Infer
  ( -- * Community-bridging inference
    inferCommunityBridges
  , inferTransitiveDeps
  , inferSharedContextEdges
    -- * Density-controlled inference
  , inferEdges
    -- * Bridge node classification
  , classifyBridgeNodes
  , BridgeClassification(..)
  ) where

import Data.List (sortOn, nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Domain.Graph
  ( Graph, gNodes, gEdges, neighbors, degree
  , articulationPoints, biconnectedComponents
  , edgeBetweenness
  )

-- ───────────────────────────────────────────────
-- Bridge classification
-- ───────────────────────────────────────────────

-- | Classification of a bridge node in the graph
data BridgeClassification = BridgeClassification
  { bcNodeId        :: NodeId
  , bcIsArticulation :: Bool
  , bcBccCount      :: Int        -- How many biconnected components it bridges
  , bcBetweenness   :: Double     -- Edge betweenness score
  , bcCommunities   :: [CommunityId]  -- Which communities it connects
  } deriving (Eq, Show)

-- | Classify bridge nodes using fgl-powered graph algorithms.
-- Articulation points are nodes whose removal would disconnect the graph.
-- Biconnected components show tightly connected clusters.
classifyBridgeNodes :: Graph -> CommunityMap -> [BridgeClassification]
classifyBridgeNodes g commMap =
  let artPoints = articulationPoints g
      bccs = biconnectedComponents g
      between = edgeBetweenness g
      -- Map each node to its communities
      nodeComm = nodeCommunityMap commMap
      -- Count BCC membership for each art point
      bccMembership = Map.fromListWith (+)
        [ (nid, 1)
        | comp <- bccs
        , nid <- comp
        , nid `elem` artPoints
        ]
  in [ BridgeClassification
       { bcNodeId        = nid
       , bcIsArticulation = True
       , bcBccCount      = Map.findWithDefault 1 nid bccMembership
       , bcBetweenness   = sum [score | ((s,t), score) <- Map.toList between
                                       , s == nid || t == nid]
       , bcCommunities   = case Map.lookup nid nodeComm of
                              Just cid -> [cid]  -- bridge nodes often sit between communities
                              Nothing  -> []
       }
     | nid <- artPoints
     ]

-- ───────────────────────────────────────────────
-- Community-bridging inference
-- ───────────────────────────────────────────────

-- | Infer edges between community centroids.
-- For each pair of communities, connect their highest-degree nodes
-- with ConceptuallyRelatedTo edges. This creates inter-cluster bridges
-- that make the graph more connected and help discovery.
--
-- Returns inferred edges (to be merged into the graph).
inferCommunityBridges :: Graph -> CommunityMap -> [Edge]
inferCommunityBridges g commMap =
  let centroids = communityCentroids g commMap
      communityIds = Map.keys centroids
      -- Generate edges between centroids of different communities
      pairs = [(cid1, cid2) | cid1 <- communityIds
                             , cid2 <- communityIds
                             , cid1 < cid2]
  in [makeBridgeEdge srcNid tgtNid | (cid1, cid2) <- pairs
                                    , Just srcNid <- [Map.lookup cid1 centroids]
                                    , Just tgtNid <- [Map.lookup cid2 centroids]
                                    , notEdgeAlready g srcNid tgtNid
                                    ]

-- | Infer transitive dependency edges:
-- If A → B and B → C (both Imports/DependsOn), infer A → C (DependsOn).
inferTransitiveDeps :: Graph -> [Edge]
inferTransitiveDeps g =
  let edges = Map.toList (gEdges g)
      -- Collect import/dependency edges
      depEdges = [((s, t), e) | ((s, t), e) <- edges
                               , edgeRelation e `elem` [Imports, ImportsFrom, DependsOn]]
      -- Build predecessor map: target → list of sources
      predMap = Map.fromListWith (++) [(t, [s]) | ((s, t), _) <- depEdges]
      -- For each node, find transitive deps
      transitiveDeps = nubBy (\a b -> edgeSource a == edgeSource b && edgeTarget a == edgeTarget b)
        [makeTransitiveDepEdge src tgt
        | ((src, mid), _) <- depEdges
        , Just targets <- [Map.lookup mid predMap]
        , tgt <- targets
        , tgt /= src  -- no self-loops
        , notEdgeAlready g src tgt  -- don't duplicate
        ]
  in transitiveDeps

-- | Infer "shares data with" edges: if two nodes share 2+ common neighbors,
-- they likely share data.
inferSharedContextEdges :: Graph -> Int -> [Edge]
inferSharedContextEdges g minShared =
  let nodes = Map.keys (gNodes g)
      -- For each pair of nodes, count shared neighbors
      pairs = [(n1, n2) | n1 <- nodes, n2 <- nodes, n1 < n2]
      sharedEdgePairs = [(n1, n2, sharedCount)
                       | (n1, n2) <- pairs
                       , let n1Nbrs = neighbors g n1
                             n2Nbrs = neighbors g n2
                             shared = Set.intersection n1Nbrs n2Nbrs
                             sharedCount = Set.size shared
                       , sharedCount >= minShared
                       , notEdgeAlready g n1 n2
                       ]
  in [makeSharedDataEdge n1 n2 count | (n1, n2, count) <- sharedEdgePairs]

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Get the centroid (highest-degree node) of each community
communityCentroids :: Graph -> CommunityMap -> Map CommunityId NodeId
communityCentroids g commMap = Map.fromList
  [ (cid, centroidOf g members)
  | (cid, members) <- Map.toList commMap
  , not (null members)
  ]
  where
    centroidOf g' members =
      let scored = sortOn (\n -> negate (fromIntegral (degree g' n) :: Double)) members
      in case scored of (x:_) -> x; [] -> error "centroidOf: empty community"

-- | Check if an edge already exists between two nodes (in either direction)
notEdgeAlready :: Graph -> NodeId -> NodeId -> Bool
notEdgeAlready g src tgt =
  Map.notMember (src, tgt) (gEdges g) && Map.notMember (tgt, src) (gEdges g)

-- | Map each node to its community
nodeCommunityMap :: CommunityMap -> Map NodeId CommunityId
nodeCommunityMap commMap = Map.fromList
  [(nid, cid) | (cid, nids) <- Map.toList commMap, nid <- nids]

-- | Make a bridge edge between community centroids
makeBridgeEdge :: NodeId -> NodeId -> Edge
makeBridgeEdge src tgt = Edge
  { edgeSource         = src
  , edgeTarget         = tgt
  , edgeRelation       = ConceptuallyRelatedTo
  , edgeConfidence     = Inferred
  , edgeConfidenceScore = 0.5
  , edgeSourceFile     = T.pack "inferred:community-bridge"
  , edgeSourceLocation = Nothing
  , edgeWeight         = 0.5
  }

-- | Make a transitive dependency edge
makeTransitiveDepEdge :: NodeId -> NodeId -> Edge
makeTransitiveDepEdge src tgt = Edge
  { edgeSource         = src
  , edgeTarget         = tgt
  , edgeRelation       = DependsOn
  , edgeConfidence     = Inferred
  , edgeConfidenceScore = 0.4
  , edgeSourceFile     = T.pack "inferred:transitive-dep"
  , edgeSourceLocation = Nothing
  , edgeWeight         = 0.4
  }

-- | Make a shared-data edge
makeSharedDataEdge :: NodeId -> NodeId -> Int -> Edge
makeSharedDataEdge src tgt sharedCount = Edge
  { edgeSource         = src
  , edgeTarget         = tgt
  , edgeRelation       = SharesDataWith
  , edgeConfidence     = Inferred
  , edgeConfidenceScore = min 0.9 (0.2 * fromIntegral sharedCount)
  , edgeSourceFile     = T.pack "inferred:shared-context"
  , edgeSourceLocation = Nothing
  , edgeWeight         = min 0.9 (0.2 * fromIntegral sharedCount)
  }

-- ───────────────────────────────────────────────
-- Density-controlled inference
-- ───────────────────────────────────────────────

-- | Infer edges based on density level.
--
--   * Sparse:   No inferred edges at all (only what was extracted)
--   * Normal:   Community bridges + transitive deps (recommended default)
--   * Dense:    Normal + shared-context edges (min 3 shared neighbors)
--   * Maximum:  Dense + shared-context edges (min 2 shared neighbors)
inferEdges :: EdgeDensity -> Graph -> CommunityMap -> [Edge]
inferEdges Sparse   _ _ = []
inferEdges Normal g cm = inferCommunityBridges g cm ++ inferTransitiveDeps g
inferEdges Dense   g cm = inferCommunityBridges g cm ++ inferTransitiveDeps g ++ inferSharedContextEdges g 3
inferEdges Maximum g cm = inferCommunityBridges g cm ++ inferTransitiveDeps g ++ inferSharedContextEdges g 2