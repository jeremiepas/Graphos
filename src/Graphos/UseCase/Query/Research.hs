{-# LANGUAGE StrictData #-}
-- | Research view use case: multi-query union + induced subgraph extraction.
--
-- `buildResearchView` runs a scored query for each input term, folds the
-- matched nodes into a deduplicated map carrying per-term discovery
-- attribution, optionally expands the union with `--subgraph` seed terms, and
-- induces (and refines) the subgraph over the resulting node set.
module Graphos.UseCase.Query.Research
  ( buildResearchView
  , buildResearchViewIO
  , expandWithSeeds
  ) where

import Data.Time (UTCTime(..), Day(ModifiedJulianDay), getCurrentTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)

import Graphos.Domain.Types
  ( NodeId, Node(..)
  , Edge(..), edgeRelation, edgeConfidence, edgeWeight, Confidence(..)
  , Relation(..), textToRelation, relationToText
  , EdgeId(..), CommunityId, CommunityMap
  , FileType(..)
  )
import Graphos.Domain.Community (CommunityComposition(..))
import Graphos.Domain.Query.Research
import Graphos.Domain.Graph.Core (Graph(..), gHash, gNodes, gEdges)
import Graphos.Domain.Graph.Index (GraphIndex(..), bfsFromSet, communityMembers)
import Graphos.UseCase.Query
  ( queryGraphWithIndexScored
  , QueryResponse(..)
  , ScoredNode(..)
  )
import Graphos.UseCase.Query.Refine
  ( EdgeMode(..)
  , refineEdges
  )

-- | Build the research view for a set of query terms.
--
-- Runs `queryGraphWithIndexScored` for each term (budget 2000), folds the
-- matched nodes into a deduplicated map with per-term discovery attribution,
-- optionally expands the union with seed terms (`--subgraph`), then induces
-- and refines the subgraph over the final node set.
buildResearchView
  :: Graph
  -> GraphIndex
  -> CommunityMap
  -> Map CommunityId CommunityComposition
  -> [Text]              -- ^ query terms
  -> [Text]              -- ^ seed terms (--subgraph), may be empty
  -> Maybe EdgeMode      -- ^ edge refinement mode
  -> ResearchView
buildResearchView g idx commMap comps terms seeds mbMode = do
  let mode = maybe Semantic id mbMode
      queries :: [QueryResponse]
      queries = map (\t -> queryGraphWithIndexScored g idx t (T.pack "bfs") 2000) terms

      -- Fold all scored nodes into a ResearchNode map, tracking per-term
      -- discovery attribution and best score.
      nodeMap :: Map NodeId ResearchNode
      nodeMap = foldQueryResponses (zip terms queries)

      -- Replace reconstructed nodes with the originals from gNodes so line
      -- info, file type, signature, etc. survive into the view.
      nodeMap' :: Map NodeId ResearchNode
      nodeMap' = Map.map fixNode nodeMap
        where
          fixNode n =
            let real = Map.findWithDefault (rnNode n) (nodeId (rnNode n)) (gNodes g)
             in n { rnNode = real }

      unionIds :: Set NodeId
      unionIds = Map.keysSet nodeMap'

      -- Seed expansion: add seed-matched nodes + 1-hop BFS neighbours. This is
      -- additive — the expanded set only ever grows the original union.
      expandedIds :: Set NodeId
      expandedIds = case seeds of
        [] -> unionIds
        _  -> expandWithSeeds g idx unionIds seeds

      -- Final node list over the expanded set: attributed nodes keep their
      -- discovery info; any extra nodes (seed hits / BFS neighbours) get an
      -- unattributed entry so every induced edge has both endpoints present.
      finalNodes :: [ResearchNode]
      finalNodes = map lookupOrUnattributed (Set.toList expandedIds)
        where
          lookupOrUnattributed nid =
            case Map.lookup nid nodeMap' of
              Just rn -> rn
              Nothing -> unattributedNode (Map.findWithDefault undefined nid (gNodes g))

      -- Fill each node's scores with every input term (0 for non-matching).
      filledNodes :: [ResearchNode]
      filledNodes = map (fillScores terms) finalNodes

      nodeNodes :: Map NodeId Node
      nodeNodes = Map.fromList [ (nodeId (rnNode n), rnNode n) | n <- filledNodes ]

      inducedEdges :: [(NodeId, NodeId, Text, Double)]
      inducedEdges =
        [ (edgeSource e, edgeTarget e, relationToText (edgeRelation e), edgeWeight e)
        | e <- Map.elems (gEdges g)
        , edgeSource e `Set.member` expandedIds
        , edgeTarget e `Set.member` expandedIds
        ]

      refinedEdges :: [Edge]
      refinedEdges = map edgeFromTuple (refineEdges mode nodeNodes inducedEdges)

      commIds :: [CommunityId]
      commIds =
        [ cid | n <- filledNodes
              , Just cid <- [nodeCommunityId (rnNode n)]
              , cid /= 0 ]

      commMapOut :: Map CommunityId ResearchCommunity
      commMapOut = Map.fromList
        [ (cid, ResearchCommunity
               { rcLabel       = Just (T.pack ("Community " ++ show cid))
              , rcComposition = Map.lookup cid comps
              , rcMemberCount = length (communityMembers cid commMap)
              })
        | cid <- nub commIds ]

      termIdx :: Map Text Int
      termIdx = Map.fromList (zip terms [0 :: Int ..])

      sortedNodes :: [ResearchNode]
      sortedNodes = sortOn rankOf filledNodes
        where
          rankOf n = case rnDiscoveredBy n of
            t:_ -> (False, Map.findWithDefault (length terms) t termIdx, nodeId (rnNode n))
            []  -> (True, length terms, nodeId (rnNode n))

  ResearchView
    { rvTerms       = terms
    , rvNodes       = sortedNodes
    , rvEdges       = refinedEdges
    , rvCommunities = commMapOut
    , rvMetadata    = ResearchMetadata
      { rmGeneratedAt = utctEpoch
      , rmGraphHash   = gHash g
      , rmNodeCount   = length sortedNodes
      , rmEdgeCount   = length refinedEdges
      }
    }
  where
    edgeFromTuple :: (NodeId, NodeId, Text, Double) -> Edge
    edgeFromTuple (src, tgt, rel, conf) =
      let eid = EdgeId (src <> "-" <> tgt)
          rel' = case textToRelation rel of
            Just r -> r
            Nothing -> Inferred
       in Edge { edgeId = eid
              , edgeSource = src
              , edgeTarget = tgt
              , edgeRelation = rel'
              , edgeWeight = conf
              , edgeConfidence = Confidence conf
              , edgeExtra = Nothing
              }

unattributedNode :: Node -> ResearchNode
unattributedNode n = ResearchNode
  { rnNode = n
  , rnDiscoveredBy = []
  , rnBestScore = 0
  , rnScores = []
  }

-- | Augment a node's scores with every input term, using score 0 for any term
-- whose query did not return the node.
fillScores :: [Text] -> ResearchNode -> ResearchNode
fillScores allTerms rn =
  let existing = Map.fromList (rnScores rn)
      newScores = [ (t, Map.findWithDefault 0 t existing) | t <- allTerms ]
   in rn { rnScores = newScores }

utctEpoch :: UTCTime
utctEpoch = UTCTime (ModifiedJulianDay (25568 :: Integer)) 0

-- | Fold a list of query responses into a ResearchNode map.
--
-- Each response's scored nodes are accumulated into the map, tracking
-- `rnDiscoveredBy` (in term order), `rnScores` (per-term), and keeping the
-- maximum as `rnBestScore`. Nodes are deduplicated by `NodeId`.
foldQueryResponses :: [(Text, QueryResponse)] -> Map NodeId ResearchNode
foldQueryResponses entries =
  let scoredNodeToNode sn = Node
          { nodeId = snNodeId sn
          , nodeLabel = fromText (snLabel sn)
          , nodeFileType = CodeFile
          , nodeSourceFile = fromText (snSourceFile sn)
          , nodeLineStart = Nothing
          , nodeLineEnd = Nothing
          , nodeSignature = Nothing
          , nodeCommunityId = snCommunityId sn
          , nodeKind = Nothing
          , nodeDegree = Nothing
          , nodeIsBridge = Nothing
          , nodeExtra = Nothing
          , nodePresentBits = 0
          }
      mergeNode :: ResearchNode -> ResearchNode -> ResearchNode
      mergeNode existing newRn =
        case rnDiscoveredBy newRn of
          term:otherTerms ->
            case rnScores newRn of
              (term', snScore'):_ ->
                let n = rnNode newRn
                    newDisc = term : otherTerms ++ rnDiscoveredBy existing
                    newScores = (term', snScore') : rnScores existing
                    newBest = max (rnBestScore existing) snScore'
                in existing { rnNode = n, rnDiscoveredBy = newDisc, rnScores = newScores, rnBestScore = newBest }
              [] -> existing
          [] -> existing
      createNode :: Text -> ScoredNode -> ResearchNode
      createNode term sn =
        let n = scoredNodeToNode sn
        in ResearchNode { rnNode = n, rnDiscoveredBy = [term], rnBestScore = snScore sn, rnScores = [(term, snScore sn)] }
      processTerm :: Map NodeId ResearchNode -> Text -> [ScoredNode] -> Map NodeId ResearchNode
      processTerm acc term scoredNodes =
        let insertOne :: Map NodeId ResearchNode -> ScoredNode -> Map NodeId ResearchNode
            insertOne a sn = let nid = snNodeId sn in Map.insertWith mergeNode nid (createNode term sn) a
        in foldl insertOne acc scoredNodes
      acc0 :: Map NodeId ResearchNode
      acc0 = Map.empty
    in foldl (\acc (term, qr) -> processTerm acc term (qrespNodes qr)) acc0 entries

-- | IO wrapper that attaches the real `getCurrentTime` timestamp.
buildResearchViewIO
  :: Graph
  -> GraphIndex
  -> CommunityMap
  -> Map CommunityId CommunityComposition
  -> [Text]
  -> [Text]
  -> Maybe EdgeMode
  -> IO ResearchView
buildResearchViewIO g idx commMap comps terms seeds mbMode = do
  t <- getCurrentTime
  let rv = buildResearchView g idx commMap comps terms seeds mbMode
  pure rv { rvMetadata = (rvMetadata rv) { rmGeneratedAt = t } }

-- | Expand the union node set with 1-hop BFS from matched nodes of seed terms.
--
-- Runs a scored query for each seed term, adds the matched nodes to the union,
-- then expands by one BFS hop. Additive: never removes any node.
expandWithSeeds
  :: Graph
  -> GraphIndex
  -> Set NodeId           -- ^ current union of node ids
  -> [Text]               -- ^ seed terms to expand from
  -> Set NodeId
expandWithSeeds g idx union seeds =
  let queries :: [QueryResponse]
      queries = map (\s -> queryGraphWithIndexScored g idx s (T.pack "bfs") 2000) seeds
      matched :: Set NodeId
      matched = Set.union union (Set.fromList
        [ snNodeId n
        | q <- queries
        , n <- qrespNodes q
        ])
      -- 1-hop BFS from matched nodes
      expanded :: Set NodeId
      expanded = Set.union matched (bfsFromSet idx matched 1 10000)
   in expanded

nub :: (Ord a) => [a] -> [a]
nub = go Map.empty
  where
    go _seen [] = []
    go _seen (x:xs) =
      if Map.member x _seen
      then go _seen xs
      else x : go (Map.insert x () _seen) xs
