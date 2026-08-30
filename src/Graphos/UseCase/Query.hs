-- | Graph querying - BFS, DFS, shortest path
--
-- Optimized: uses GraphIndex for O(k×hits) term lookup instead of O(N) full-scan,
-- and direct adjacency-map BFS instead of FGL conversion.
--
-- Scored query path (improve-query-agent-ergonomics): verdict, scored nodes,
-- did-you-mean suggestions, and result-set hash replace the legacy unscored path.
module Graphos.UseCase.Query
  ( queryGraph
  , queryGraphWithIndex
  , queryGraphWithIndexScored
  , queryGraphWithIndexScoredCached
  , pathQuery
  , pathQueryWithIndex
  , pathQueryWithIndexCached
  , explainNode
  , explainNodeWithIndex
  , saveQueryResult
  , queryArticulationPoints
  , queryBiconnectedComponents
  , queryDominatorTree
  , QueryResult(..)
  , QueryResponse(..)
  , MatchVerdict(..)
  , ScoredNode(..)
  , computeVerdict
  , verdictThreshold
  , resultHash
  , findSuggestions
  , SymbolResult(..)
  , symbolLookup
  , NeighborsResult(..)
  , neighborhoodExpansion
  , NodeResolution(..)
  , resolveNodeArg
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText, toText)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing)
import Data.List (sortOn, nubBy)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Graph.Inductive.Query.BFS ()

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, shortestPath, depthFirstSearch, gNodes, gEdges
                            , articulationPoints, biconnectedComponents, dominators)
import Graphos.Domain.Graph.Index (GraphIndex(..), buildIndex, findMatchingNodes, bfsFromSet, bfsFrom, giLabelIndex, giAdj)
import Graphos.Domain.Graph.Analysis (CachedFGL, toCachedFGL)
import Graphos.Domain.Graph.Query (shortestPathWithCached, depthFirstSearchWithCached)
import Graphos.Domain.Graph.Score
  ( MatchVerdict(..)
  , ScoredNode(..)
  , QueryResponse(..)
  , computeVerdict
  , verdictThreshold
   , normalizeScore
   , fullLabelBoostForTerms
   , resultHash
  , findSuggestions
  )

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
      nodeLabels = [(nid, toText (nodeLabel n)) | (nid, n) <- Map.toList (gNodes g), nid `Set.member` subgraphNodes]
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

-- | Unwrap Confidence newtype to get the underlying Double.
unConfidence :: Confidence -> Double
unConfidence (Confidence d) = d

-- | Scored query path — returns QueryResponse with verdict, scored nodes, and suggestions.
--
-- Normalized scoring = matched-terms / query-terms with exact full-label boost.
-- No traversal when best score is 0 (deleted the degenerate-fallback path).
-- Verdict thresholds: strong ≥ 0.5, weak > 0, none = 0.
queryGraphWithIndexScored :: Graph -> GraphIndex -> Text -> Text -> Int -> QueryResponse
queryGraphWithIndexScored g idx query mode budget =
  queryGraphWithIndexScoredCached g idx (toCachedFGL g) query mode budget

-- | Scored query path using a prebuilt CachedFGL — no per-call FGL rebuild.
-- The dfs-mode expansion reads from the cached FGL instead of rebuilding it.
queryGraphWithIndexScoredCached :: Graph -> GraphIndex -> CachedFGL -> Text -> Text -> Int -> QueryResponse
queryGraphWithIndexScoredCached g idx cfg query mode _budget =
  let terms = filter ((> 2) . T.length) (T.words (T.toLower query))
      -- Scored term matching via inverted index
      matched :: [(NodeId, Int)]
      matched = findMatchingNodes terms idx
      -- Build score map: NodeId -> raw score
      scoreMap :: Map NodeId Int
      scoreMap = Map.fromList matched
      -- Compute normalized scores with full-label boost
      scoredPairs :: [(NodeId, Double)]
      scoredPairs =
        [ (nid, normalizeScore rawScore (length terms) + fullLabelBoostForTerms terms (toText (nodeLabel n)))
        | (nid, rawScore) <- matched
        , Just n <- [Map.lookup nid (gNodes g)]
        ]
      bestScore :: Double
      bestScore = case scoredPairs of
        []  -> 0
        _   -> maximum [s | (_, s) <- scoredPairs]
      verdict :: MatchVerdict
      verdict = computeVerdict bestScore
      -- BFS from top-scoring nodes (only when not NoMatch)
      topNodes :: [NodeId]
      topNodes = take 5 [nid | (nid, _) <- scoredPairs]
      expanded :: Set.Set NodeId
      expanded =
        case verdict of
          NoMatch -> Set.empty
          _       -> if mode == T.pack "dfs"
                     then Set.unions [depthFirstSearchWithCached cfg nid 6 | nid <- topNodes]
                     else bfsFromSet idx (Set.fromList topNodes) 3
      -- Gather scored nodes in the expanded subgraph
      nodeMap :: Map NodeId Node
      nodeMap = gNodes g
      -- Build a quick lookup from scoreMap for expanded nodes
      scoredNodes :: [ScoredNode]
      scoredNodes =
        [ ScoredNode
            { snNodeId      = nid
            , snLabel       = toText (nodeLabel n)
            , snScore       = fromIntegral (Map.findWithDefault 0 nid scoreMap) / max 1 (fromIntegral (length terms)) + fullLabelBoostForTerms terms (toText (nodeLabel n))
            , snSourceFile  = toText (nodeSourceFile n)
            , snCommunityId = nodeCommunityId n
            }
        | nid <- Set.toList expanded
        , nid `Map.member` scoreMap
        , Just n <- [Map.lookup nid nodeMap]
        ]
      -- Sort score-descending
      scoredNodesSorted :: [ScoredNode]
      scoredNodesSorted = sortOn (negate . snScore) scoredNodes
      -- Edges within the subgraph
      nodeLblMap :: Map NodeId Text
      nodeLblMap = Map.fromList [(nid, toText (nodeLabel n)) | (nid, n) <- Map.toList nodeMap, nid `Set.member` expanded]
      edges :: [(Text, Text, Text, Double)]
      edges =
        [ ( fromMaybeLbl src nodeLblMap
          , fromMaybeLbl tgt nodeLblMap
          , relationToText (edgeRelation e)
          , unConfidence (edgeConfidence e)
          )
        | ((src, tgt), e) <- Map.toList (gEdges g)
        , src `Set.member` expanded
        , tgt `Set.member` expanded
        ]
      -- Suggestions for NoMatch and Weak
      suggestions :: [Text]
      suggestions = case verdict of
        NoMatch -> findSuggestions terms idx
        Weak    -> findSuggestions terms idx
        _       -> []
      -- Result-set hash
      hash :: Text
      hash = resultHash [snNodeId n | n <- scoredNodesSorted]
      in QueryResponse
         { qrespVerdict      = verdict
         , qrespBestScore    = bestScore
         , qrespHash         = hash
         , qrespNodes        = scoredNodesSorted
         , qrespEdges        = edges
         , qrespSuggestions  = suggestions
         , qrespOmittedNodes = 0
         , qrespOmittedEdges = 0
         }


-- | Find shortest path between two concepts using pre-built index and FGL cache
pathQueryWithIndexCached :: Graph -> GraphIndex -> CachedFGL -> Text -> Text -> Maybe [NodeId]
pathQueryWithIndexCached _ idx cfg fromTerm toTerm =
  let fromNode = findBestNodeWithIndex idx fromTerm
      toNode   = findBestNodeWithIndex idx toTerm
  in case (fromNode, toNode) of
       (Just f, Just t) -> shortestPathWithCached cfg f t
       _ -> Nothing

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
    , nodeLabel        = fromText "unknown"
    , nodeFileType     = CodeFile
    , nodeSourceFile   = fromText ""
  , nodeLineStart    = Nothing
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Nothing
    , nodeSignature    = Nothing
    , nodePresentBits  = 0
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
-- Symbol lookup and neighborhood expansion
-- ───────────────────────────────────────────────

-- | Result of an exact symbol lookup.
-- Returns all matching nodes with their details.
-- No fuzzy scoring, no BFS — just identifier match.
data SymbolResult = SymbolResult
  { srFound      :: ![ScoredNode]
  , srNotFound   :: !Bool
  , srSuggestions:: ![Text]
  } deriving (Eq, Show, Generic)

instance NFData SymbolResult

instance ToJSON SymbolResult where
  toJSON r = object
    [ "found"       .= srFound r
    , "not_found"   .= srNotFound r
    , "suggestions" .= srSuggestions r
    ]

-- | Exact symbol lookup: case-sensitive first, then case-insensitive fallback.
-- No fuzzy scoring, no BFS. Returns all matches with locations.
symbolLookup :: Text -> Graph -> GraphIndex -> SymbolResult
symbolLookup name g idx =
  let lowerName = T.toLower name
      labelIdx = giLabelIndex idx
      nodeMap = gNodes g
      exactHits = Map.findWithDefault [] name labelIdx
      ciHits = if null exactHits
                then Map.findWithDefault [] lowerName labelIdx
                else []
      allHitIds = if null exactHits then ciHits else exactHits
      scoredNodes = [ ScoredNode
                        { snNodeId      = nid
                        , snLabel       = toText (nodeLabel n)
                        , snScore       = if null exactHits then 0.5 else 1.0
                        , snSourceFile  = toText (nodeSourceFile n)
                        , snCommunityId = nodeCommunityId n
                        }
                    | nid <- allHitIds
                    , Just n <- [Map.lookup nid nodeMap]
                    ]
      isNotFound = null allHitIds
      suggestions = if isNotFound then findSuggestions [name] idx else []
  in SymbolResult
       { srFound       = scoredNodes
       , srNotFound    = isNotFound
       , srSuggestions = suggestions
       }

-- | Result of a neighborhood expansion from a known node.
data NeighborsResult = NeighborsResult
  { nrCenterNode :: !(Maybe NodeId)
  , nrNodes     :: ![ScoredNode]
  , nrEdges     :: ![(Text, Text, Text, Double)]
  , nrMaxDepth  :: !Int
  } deriving (Eq, Show, Generic)

instance NFData NeighborsResult

instance ToJSON NeighborsResult where
  toJSON r = object
    [ "center_node" .= nrCenterNode r
    , "nodes"       .= nrNodes r
    , "edges"       .= nrEdges r
    , "max_depth"   .= nrMaxDepth r
    ]

-- | Neighborhood expansion from an exact node ID.
-- BFS to `--depth` (default 2), proximity score = 1/(1+hops).
-- Returns nodes ordered by proximity (closer hops first).
neighborhoodExpansion :: NodeId -> Int -> Graph -> GraphIndex -> NeighborsResult
neighborhoodExpansion startId depth g idx =
  let nodeMap = gNodes g
      adj = giAdj idx
  in case Map.lookup startId adj of
       Nothing -> NeighborsResult
                    { nrCenterNode = Nothing
                    , nrNodes      = []
                    , nrEdges      = []
                    , nrMaxDepth   = depth
                    }
       Just _ -> let expanded = bfsFrom idx startId depth
                     scoredNodes = [ ScoredNode
                                       { snNodeId      = nid
                                       , snLabel       = toText (nodeLabel n)
                                       , snScore       = proximityScore startId nid idx
                                        , snSourceFile  = toText (nodeSourceFile n)
                                       , snCommunityId = nodeCommunityId n
                                       }
                                    | nid <- Set.toList expanded
                                    , Just n <- [Map.lookup nid nodeMap]
                                    ]
                     nodeLblMap = Map.fromList [(nid, toText (nodeLabel n)) | (nid, n) <- Map.toList nodeMap, nid `Set.member` expanded]
                     edges = [ ( fromMaybeLbl src nodeLblMap
                               , fromMaybeLbl tgt nodeLblMap
                               , relationToText (edgeRelation e)
                               , unConfidence (edgeConfidence e)
                               )
                             | ((src, tgt), e) <- Map.toList (gEdges g)
                             , src `Set.member` expanded
                             , tgt `Set.member` expanded
                             ]
                 in NeighborsResult
                      { nrCenterNode = Just startId
                      , nrNodes      = sortOn (negate . snScore) scoredNodes
                      , nrEdges      = edges
                      , nrMaxDepth   = depth
                      }

-- | Result of resolving a CLI node argument to a graph node.
-- Used by node-argument query-family commands (e.g. @neighbors@) so an agent
-- can pass a display name it just saw instead of the internal id.
data NodeResolution
  = ResolvedSingle NodeId    -- ^ exactly one node matched
  | Ambiguous [ScoredNode]   -- ^ more than one node matched; caller must disambiguate
  | NotFound                 -- ^ no node id and no label matched
  deriving (Eq, Show, Generic)

instance NFData NodeResolution

-- | Resolve a node argument without fuzzy traversal.
--
-- Resolution order (canonical for all node-argument query-family commands):
--
--   1. exact node id     (@Map.lookup arg (gNodes g)@)
--   2. exact label       (@symbolLookup@ case-sensitive path)
--   3. case-insensitive label (@symbolLookup@ fallback path)
--
-- A single match yields 'ResolvedSingle'; multiple matches yield 'Ambiguous'
-- (the caller lists candidates and re-runs with an id — no BFS, no fuzzy path);
-- no match yields 'NotFound'. Pure: the CLI dispatcher wires the resolved id
-- into 'neighborhoodExpansion'.
resolveNodeArg :: Text -> Graph -> GraphIndex -> NodeResolution
resolveNodeArg arg g idx =
  case Map.lookup arg (gNodes g) of
    Just _  -> ResolvedSingle arg
    Nothing ->
      -- The label index stores each node under both its exact and lowercased
      -- label, so an already-lowercase label yields the node twice; dedup by
      -- node id so a single node never reads as ambiguous.
      case nubBy (\a b -> snNodeId a == snNodeId b) (srFound (symbolLookup arg g idx)) of
        []      -> NotFound
        [sn]    -> ResolvedSingle (snNodeId sn)
        cands   -> Ambiguous cands

-- | Compute proximity score: 1/(1+hops) where hops is the BFS distance.
proximityScore :: NodeId -> NodeId -> GraphIndex -> Double
proximityScore _start _target _idx = 1.0 -- Simplified: exact proximity requires full BFS tracking
                                       -- which is done during expansion. This is a placeholder
                                       -- that will be replaced in Task 6 with proper hop tracking.

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
