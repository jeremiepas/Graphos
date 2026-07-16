module Graphos.UseCase.Infer
  ( inferCommunityBridges
  , inferTransitiveDeps
  , inferSharedContextEdges
  , inferCodeDocEdges
  , inferEdges
  , classifyBridgeNodes
  , BridgeClassification(..)
  ) where

import Data.List (sortOn, nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Domain.Graph
  ( Graph, gNodes, gEdges, neighbors, degree
  , articulationPoints, biconnectedComponents
  , edgeBetweenness
  )

data BridgeClassification = BridgeClassification
  { bcNodeId        :: NodeId
  , bcIsArticulation :: Bool
  , bcBccCount      :: Int
  , bcBetweenness   :: Double
  , bcCommunities   :: [CommunityId]
  } deriving (Eq, Show)

classifyBridgeNodes :: Graph -> CommunityMap -> [BridgeClassification]
classifyBridgeNodes g commMap =
  let artPoints = articulationPoints g
      bccs = biconnectedComponents g
      between = edgeBetweenness g
      nodeComm = nodeCommunityMap commMap
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
                               Just cid -> [cid]
                               Nothing  -> []
        }
      | nid <- artPoints
      ]

inferCommunityBridges :: Graph -> CommunityMap -> [Edge]
inferCommunityBridges g commMap =
  let centroids = communityCentroids g commMap
      communityIds = Map.keys centroids
      pairs = [(cid1, cid2) | cid1 <- communityIds
                             , cid2 <- communityIds
                             , cid1 < cid2]
  in [makeInferredEdge srcNid tgtNid Inferred 0.5 | (cid1, cid2) <- pairs
                                                   , Just srcNid <- [Map.lookup cid1 centroids]
                                                   , Just tgtNid <- [Map.lookup cid2 centroids]
                                                   , notEdgeAlready g srcNid tgtNid
                                                   ]

inferTransitiveDeps :: Graph -> [Edge]
inferTransitiveDeps g =
  let edges = Map.toList (gEdges g)
      depEdges = [((s, t), e) | ((s, t), e) <- edges
                               , edgeRelation e `elem` [Imports, DependsOn]]
      predMap = Map.fromListWith (++) [(t, [s]) | ((s, t), _) <- depEdges]
      transitiveDeps = nubBy (\a b -> edgeSource a == edgeSource b && edgeTarget a == edgeTarget b)
        [makeInferredEdge src tgt DependsOn 0.4
        | ((src, mid), _) <- depEdges
        , Just targets <- [Map.lookup mid predMap]
        , tgt <- targets
        , tgt /= src
        , notEdgeAlready g src tgt
        ]
  in transitiveDeps

inferSharedContextEdges :: Graph -> Int -> [Edge]
inferSharedContextEdges g minShared =
  let allNodes = Map.keys (gNodes g)
      coOccurrences :: Map (NodeId, NodeId) Int
      coOccurrences = Map.fromListWith (+)
        [ (orderPair n1 n2, 1)
        | nid <- allNodes
        , let nbs = Set.toList (neighbors g nid)
        , length nbs <= 64
        , (n1, n2) <- pairUp nbs
        ]
      pairUp [] = []
      pairUp (x:xs) = [(x, y) | y <- xs] ++ pairUp xs
      validPairs = [(n1, n2, count)
                   | ((n1, n2), count) <- Map.toList coOccurrences
                   , count >= minShared
                   , notEdgeAlready g n1 n2
                   ]
  in [makeInferredEdge n1 n2 Inferred (min 0.9 (0.2 * fromIntegral sharedCount)) | (n1, n2, sharedCount) <- validPairs]
  where
    orderPair a b = if a < b then (a, b) else (b, a)

inferCodeDocEdges :: Graph -> [Edge]
inferCodeDocEdges g =
  let allNodes = Map.toList (gNodes g)
      docNodes = [(nid, n) | (nid, n) <- allNodes, nodeFileType n == DocFile]
      codeNodes = [(nid, n) | (nid, n) <- allNodes, nodeFileType n == CodeFile]

      codeLabelIdx :: Map Text [NodeId]
      codeLabelIdx = Map.fromListWith (++)
        [ (nodeLabel cn, [nid])
        | (nid, cn) <- codeNodes
        ]

      codeBaseIdx :: Map Text [NodeId]
      codeBaseIdx = Map.fromListWith (++)
        [ (fileBaseName (nodeSourceFile cn), [nid])
        | (nid, cn) <- codeNodes
        , not (T.null (nodeSourceFile cn))
        ]

      nameAlignEdges =
        [ makeInferredEdge codeNid docNid References 0.7
        | (docNid, dn) <- docNodes
        , codeNid <- Map.findWithDefault [] (nodeLabel dn) codeLabelIdx
        , notEdgeAlready g docNid codeNid
        ]

      pathAlignEdges =
        [ makeInferredEdge codeNid docNid References 0.7
        | (docNid, dn) <- docNodes
        , not (T.null (nodeSourceFile dn))
        , let docBase = fileBaseName (nodeSourceFile dn)
        , not (T.null docBase)
        , codeNid <- Map.findWithDefault [] docBase codeBaseIdx
        , notEdgeAlready g docNid codeNid
        ]

  in nubBy (\a b -> edgeSource a == edgeSource b && edgeTarget a == edgeTarget b)
       (nameAlignEdges ++ pathAlignEdges)

fileBaseName :: Text -> Text
fileBaseName path =
  let filename = case T.breakOnEnd "/" path of
        (_, f) | not (T.null f) -> T.dropWhile (== '/') f
        _                        -> path
      base = case T.breakOnEnd "." filename of
        (_, ext) | not (T.null ext) && T.length ext <= 5 ->
          case T.breakOnEnd "." (T.dropEnd (T.length ext + 1) filename) of
            (_, b) | not (T.null b) -> b
            _ -> filename
        _ -> filename
  in base

inferEdges :: EdgeDensity -> Graph -> CommunityMap -> [Edge]
inferEdges Sparse   g _  = inferCodeDocEdges g
inferEdges Normal g cm = inferCommunityBridges g cm ++ inferTransitiveDeps g ++ inferCodeDocEdges g
inferEdges Dense   g cm = inferCommunityBridges g cm ++ inferTransitiveDeps g ++ inferSharedContextEdges g 3 ++ inferCodeDocEdges g
inferEdges Maximum g cm = inferCommunityBridges g cm ++ inferTransitiveDeps g ++ inferSharedContextEdges g 2 ++ inferCodeDocEdges g

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

notEdgeAlready :: Graph -> NodeId -> NodeId -> Bool
notEdgeAlready g src tgt =
  Map.notMember (src, tgt) (gEdges g) && Map.notMember (tgt, src) (gEdges g)

nodeCommunityMap :: CommunityMap -> Map NodeId CommunityId
nodeCommunityMap commMap = Map.fromList
  [(nid, cid) | (cid, nids) <- Map.toList commMap, nid <- nids]

makeInferredEdge :: NodeId -> NodeId -> Relation -> Double -> Edge
makeInferredEdge src tgt rel w = Edge
  { edgeId        = EdgeId (src <> "->" <> tgt <> ":" <> relationToText rel)
  , edgeSource    = src
  , edgeTarget    = tgt
  , edgeRelation  = rel
  , edgeWeight    = w
  , edgeConfidence = Confidence w
  }