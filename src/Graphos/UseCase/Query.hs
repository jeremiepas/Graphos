-- | Graph querying - BFS, DFS, shortest path
module Graphos.UseCase.Query
  ( queryGraph
  , pathQuery
  , explainNode
  , saveQueryResult
  , queryArticulationPoints
  , queryBiconnectedComponents
  , queryDominatorTree
  , QueryResult(..)
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, shortestPath, breadthFirstSearch, depthFirstSearch, gNodes, gEdges
                            , articulationPoints, biconnectedComponents, dominators)

-- | Query result
data QueryResult = QueryResult
  { qrNodes   :: [(NodeId, Text)]
  , qrEdges   :: [(Text, Text, Text, Confidence)]
  , qrTraverse :: Text
  } deriving (Eq, Show)

-- | Query the graph by terms - natural language query.
-- Finds nodes matching the query terms, traverses the graph from them,
-- and returns the subgraph with edges for richer answers.
queryGraph :: Graph -> Text -> Text -> Int -> QueryResult
queryGraph g query mode _budget =
  let terms = filter ((> 2) . T.length) (T.words (T.toLower query))
      scored = sortOn (\x -> negate (matchScore (snd x) terms))
                  [(nid, nodeLabel n) | (nid, n) <- Map.toList (gNodes g)]
      startNodes = take 5 [nid | (nid, label) <- scored, matchScore label terms > 0]
      subgraphNodes = if mode == T.pack "dfs"
                      then Set.unions [depthFirstSearch g nid 6 | nid <- startNodes]
                      else Set.unions [breadthFirstSearch g nid 3 | nid <- startNodes]
      nodeLabels = [(nid, nodeLabel n) | (nid, n) <- Map.toList (gNodes g), nid `Set.member` subgraphNodes]
      nodeLblMap = Map.fromList nodeLabels
      edges = [ ( fromMaybeLbl src nodeLblMap
               , fromMaybeLbl tgt nodeLblMap
               , relationToText (edgeRelation e)
               , edgeConfidence e
               )
             | ((src, tgt), e) <- Map.toList (gEdges g)
             , src `Set.member` subgraphNodes
             , tgt `Set.member` subgraphNodes
             ]
  in QueryResult
    { qrNodes    = nodeLabels
    , qrEdges    = edges
    , qrTraverse = mode
    }

fromMaybeLbl :: NodeId -> Map NodeId Text -> Text
fromMaybeLbl nid m = Map.findWithDefault nid nid m

-- | Find shortest path between two concepts
pathQuery :: Graph -> Text -> Text -> Maybe [NodeId]
pathQuery g fromTerm toTerm =
  let fromNode = findBestNode g fromTerm
      toNode   = findBestNode g toTerm
  in case (fromNode, toNode) of
       (Just f, Just t) -> shortestPath g f t
       _ -> Nothing

-- | Explain a single node - all its connections
explainNode :: Graph -> Text -> Maybe Node
explainNode g term =
  let best = findBestNode g term
  in fmap (\nid -> Map.findWithDefault (Node
    { nodeId           = T.pack "unknown"
    , nodeLabel        = T.pack "unknown"
    , nodeFileType     = CodeFile
    , nodeSourceFile   = T.pack ""
    , nodeSourceLocation = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Nothing
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }) nid (gNodes g)) best

-- ───────────────────────────────────────────────
-- Query Save-Result (feedback loop)
-- ───────────────────────────────────────────────

-- | Save a Q&A result to graphos-out/memory/ for future extraction.
-- Creates a markdown file with YAML frontmatter so it gets picked up on --update.
saveQueryResult :: FilePath -> Text -> Text -> Text -> [Text] -> IO ()
saveQueryResult outputDir question answer answerType sourceNodes = do
  let memDir = outputDir ++ "/memory"
  createDirectoryIfMissing True memDir
  now <- getCurrentTime
  let timestamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H%M%SZ" now)
      filename = "qa_" ++ T.unpack timestamp ++ ".md"
      filepath = memDir ++ "/" ++ filename
      frontmatter = T.unlines
        [ "---"
        , "question: " <> quoteWrap question
        , "answer_type: " <> answerType
        , "source_nodes: [" <> T.intercalate ", " (map quoteWrap sourceNodes) <> "]"
        , "captured_at: " <> quoteWrap timestamp
        , "---"
        ]
      content = frontmatter <> "\n# Q: " <> question <> "\n\n" <> answer <> "\n"
  writeFile filepath (T.unpack content)
  where
    quoteWrap t = "\"" <> t <> "\""

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

matchScore :: Text -> [Text] -> Int
matchScore label terms = sum [1 | t <- terms, T.toLower t `T.isInfixOf` T.toLower label]

findBestNode :: Graph -> Text -> Maybe NodeId
findBestNode g term =
  let terms = T.words (T.toLower term)
      scored = [(nid, matchScore (nodeLabel n) terms) | (nid, n) <- Map.toList (gNodes g)]
      best = sortOn (\ (_, s) -> negate s) scored
  in case best of
       ((nid, score):_) | score > 0 -> Just nid
       _ -> Nothing

-- ───────────────────────────────────────────────
-- Advanced graph queries (fgl-powered)
-- ───────────────────────────────────────────────

-- | Find articulation points — nodes whose removal disconnects the graph.
-- Useful for identifying critical bridge nodes in the knowledge graph.
queryArticulationPoints :: Graph -> [NodeId]
queryArticulationPoints = articulationPoints

-- | Find biconnected components — maximal subgraphs with no articulation points.
-- Each component represents a tightly connected cluster that remains connected
-- even if any single node is removed.
queryBiconnectedComponents :: Graph -> [[NodeId]]
queryBiconnectedComponents = biconnectedComponents

-- | Compute the dominator tree from a given start node.
-- A dominator d of node n is a node that appears on every path from start to n.
-- Useful for control-flow analysis and understanding graph structure.
queryDominatorTree :: Graph -> NodeId -> Map NodeId (Maybe NodeId)
queryDominatorTree = dominators