{-# LANGUAGE StrictData #-}
-- | Research view use case: multi-query union + induced subgraph extraction.
--
-- `buildResearchView` runs a scored query for each input term, folds results
-- into a deduplicated node map, and extracts the induced edge subgraph.
module Graphos.UseCase.Query.Research
  ( buildResearchView
  , buildResearchViewIO
  , expandWithSeeds
  ) where

import Data.Time (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (Day(..))
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
  , Edge(..), edgeRelation, edgeConfidence, Confidence(..)
  , Relation(..), textToRelation, relationToText
  , EdgeId(..), CommunityId, CommunityMap
  , FileType(..)
  )
import Graphos.Domain.Community (CommunityComposition(..))
import Graphos.Domain.Query.Research
import Graphos.Domain.Graph.Core (Graph(..), gHash)
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
-- Runs `queryGraphWithIndexScored` for each term (using default budget of 2000),
-- folds results into a deduplicated node map, and extracts the induced edge
-- subgraph.
buildResearchView
  :: Graph
  -> GraphIndex
  -> CommunityMap
  -> Map CommunityId CommunityComposition
  -> [Text]              -- ^ query terms
  -> Maybe EdgeMode      -- ^ edge refinement mode
  -> ResearchView
buildResearchView g idx commMap comps terms mbMode = do
  let mode = maybe Semantic id mbMode
      queries :: [QueryResponse]
      queries = map (\t -> queryGraphWithIndexScored g idx t (T.pack "bfs") 2000) terms

      -- Fold all scored nodes into a ResearchNode map
      nodeMap :: Map NodeId ResearchNode
      nodeMap = foldQueryResponses (zip terms queries)

      unionIds :: Set NodeId
      unionIds = Map.keysSet nodeMap

      -- Extract induced edges
      allEdges :: [(NodeId, NodeId, Text, Double)]
      allEdges =
        [ (edgeSource e, edgeTarget e, relationToText (edgeRelation e), edgeWeight e)
        | e <- Map.elems (gEdges g)
        , edgeSource e `Set.member` unionIds
        , edgeTarget e `Set.member` unionIds
        ]

      nodeNodes :: Map NodeId Node
      nodeNodes = Map.map rnNode nodeMap

      refinedEdges :: [Edge]
      refinedEdges = map edgeFromTuple
        (refineEdges mode nodeNodes allEdges)

      -- Collect communities for nodes in the union
      commIds :: [CommunityId]
      commIds =
        [ cid'
        | n <- Map.elems nodeMap
        , let cid = nodeCommunityId (rnNode n)
        , Just cid' <- [cid]
        , cid' /= 0
        ]

      commMapOut :: Map CommunityId ResearchCommunity
      commMapOut = Map.fromList
        [ (cid, ResearchCommunity
              { rcLabel       = Nothing
              , rcComposition = Map.lookup cid comps
              , rcMemberCount = length (communityMembers cid commMap)
              })
        | cid <- nub commIds
        ]

  let nodeList = Map.elems nodeMap
      termMap :: Map Text Int
      termMap = Map.fromList (zip terms [0 :: Int ..])
      -- Order nodeList by term discovery order (sort by the first discovering term)
      sortedNodes :: [ResearchNode]
      sortedNodes = sortOn (\n -> maybe (length terms) snd (findFirstDiscoverer termMap n)) nodeList
  ResearchView
    { rvTerms       = terms
    , rvNodes       = sortedNodes
    , rvEdges       = refinedEdges
    , rvCommunities = commMapOut
    , rvMetadata    = ResearchMetadata
      { rmGeneratedAt = utctEpoch
      , rmGraphHash   = gHash g
      , rmNodeCount   = length nodeList
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

utctEpoch :: UTCTime
utctEpoch = UTCTime (ModifiedJulianDay (25568 :: Integer)) 0

-- | Fold a list of query responses into a ResearchNode map.
-- Each response's scored nodes are accumulated into the map, tracking
-- `rnDiscoveredBy` (in term order), `rnScores` (per-term),
-- and keeping the maximum as `rnBestScore`.
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

findFirstDiscoverer :: Map Text Int -> ResearchNode -> Maybe (Text, Int)
findFirstDiscoverer termMap n =
  let disc = rnDiscoveredBy n
  in case disc of
       [] -> Nothing
       first:_ -> Just (first, Map.findWithDefault (length termMap) first termMap)

-- | IO wrapper that attaches the real `getCurrentTime` timestamp.
buildResearchViewIO
  :: Graph
  -> GraphIndex
  -> CommunityMap
  -> Map CommunityId CommunityComposition
  -> [Text]
  -> Maybe EdgeMode
  -> IO ResearchView
buildResearchViewIO g idx commMap comps terms mbMode = do
  t <- getCurrentTime
  let rv = buildResearchView g idx commMap comps terms mbMode
  pure rv { rvMetadata = (rvMetadata rv) { rmGeneratedAt = t } }

-- | Expand the union node set with 1-hop BFS from matched nodes of seed terms.
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
      expanded = Set.union matched (bfsFromSet idx matched 1)
  in expanded

nub :: (Ord a) => [a] -> [a]
nub = go Map.empty
  where
    go _seen [] = []
    go _seen (x:xs) =
      if Map.member x _seen
      then go _seen xs
      else x : go (Map.insert x () _seen) xs
