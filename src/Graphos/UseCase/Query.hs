-- | Graph querying - BFS, DFS, shortest path
--
-- Optimized: uses GraphIndex for O(k×hits) term lookup instead of O(N) full-scan,
-- and direct adjacency-map BFS instead of FGL conversion.
module Graphos.UseCase.Query
  ( queryGraph
  , queryGraphWithIndex
  , pathQuery
  , pathQueryWithIndex
  , explainNode
  , explainNodeWithIndex
  , saveQueryResult
  , queryArticulationPoints
  , queryBiconnectedComponents
  , queryDominatorTree
  , QueryResult(..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, shortestPath, depthFirstSearch, gNodes, gEdges
                            , articulationPoints, biconnectedComponents, dominators)
import Graphos.Domain.Graph.Index (GraphIndex(..), buildIndex, findMatchingNodes, bfsFromSet)

-- | Query result
data QueryResult = QueryResult
  { qrNodes   :: [(NodeId, Text)]
  , qrEdges   :: [(Text, Text, Text, Confidence)]
  , qrTraverse :: Text
  } deriving (Eq, Show)

-- | Query the graph using the precomputed GraphIndex.
-- O(k×log N + hits) term matching, O(V_subgraph + E_subgraph) traversal.
-- This is the optimized path — 10-100× faster on large graphs.
queryGraphWithIndex :: Graph -> GraphIndex -> Text -> Text -> Int -> QueryResult
queryGraphWithIndex g idx query mode _budget =
  let terms = filter ((> 2) . T.length) (T.words (T.toLower query))
      -- O(k×log N) lookup via inverted index instead of O(N) full-scan
      matched = findMatchingNodes terms idx
      startNodes = take 5 [nid | (nid, _score) <- matched, _score > 0]
      -- Direct BFS on adjacency map — no FGL conversion needed
      subgraphNodes = if mode == T.pack "dfs"
                      then Set.unions [depthFirstSearch g nid 6 | nid <- startNodes]
                      else bfsFromSet idx (Set.fromList startNodes) 3
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

-- | Query the graph by terms - legacy O(N) full-scan path.
-- Kept for backward compatibility. Prefer queryGraphWithIndex.
queryGraph :: Graph -> Text -> Text -> Int -> QueryResult
queryGraph g query mode budget =
  -- Fall back to index-less query (builds a temporary index)
  let idx = Graphos.Domain.Graph.Index.buildIndex g Map.empty
  in queryGraphWithIndex g idx query mode budget

fromMaybeLbl :: NodeId -> Map NodeId Text -> Text
fromMaybeLbl nid m = Map.findWithDefault nid nid m

-- | Find shortest path between two concepts (using index for fast node lookup)
pathQueryWithIndex :: Graph -> GraphIndex -> Text -> Text -> Maybe [NodeId]
pathQueryWithIndex g idx fromTerm toTerm =
  let fromNode = findBestNodeWithIndex idx fromTerm
      toNode   = findBestNodeWithIndex idx toTerm
  in case (fromNode, toNode) of
       (Just f, Just t) -> shortestPath g f t
       _ -> Nothing

-- | Explain a single node using the index for fast lookup
explainNodeWithIndex :: Graph -> GraphIndex -> Text -> Maybe Node
explainNodeWithIndex g idx term =
  let best = findBestNodeWithIndex idx term
  in fmap (\nid -> Map.findWithDefault (Node
    { nodeId           = T.pack "unknown"
    , nodeLabel        = T.pack "unknown"
    , nodeFileType     = CodeFile
    , nodeSourceFile   = T.pack ""
  , nodeLineStart    = Nothing
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Nothing
    , nodeSignature    = Nothing
    }) nid (gNodes g)) best

-- | Find shortest path between two concepts
pathQuery :: Graph -> Text -> Text -> Maybe [NodeId]
pathQuery g fromTerm toTerm =
  let idx = Graphos.Domain.Graph.Index.buildIndex g Map.empty
  in pathQueryWithIndex g idx fromTerm toTerm

-- | Explain a single node - all its connections
explainNode :: Graph -> Text -> Maybe Node
explainNode g term =
  let idx = Graphos.Domain.Graph.Index.buildIndex g Map.empty
  in explainNodeWithIndex g idx term

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

-- | Find the best matching node using the inverted index.
-- O(k×log N + hits) instead of O(N×k) full-scan.
findBestNodeWithIndex :: GraphIndex -> Text -> Maybe NodeId
findBestNodeWithIndex idx term =
  let terms = T.words (T.toLower term)
      matched = findMatchingNodes terms idx
  in case matched of
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
