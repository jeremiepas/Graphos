-- | Context selection engine — pure functions that select relevant subgraphs
-- for LLM consumption based on query terms and token budgets.
--
-- Strategies:
-- 1. Community-aware: find matching node → include its community + bridge nodes
-- 2. Relevance-weighted BFS: score nodes by query relevance, BFS within budget
-- 3. Path-based: find shortest path between two concepts, include path + neighbors
--
-- All functions are pure (no IO).
module Graphos.UseCase.SelectContext
  ( selectContext
  , selectContextWithHistory
  , classifyComplexity
  , computeBudget
  , relevanceScore
  , selectCommunityAware
  , selectRelevanceWeighted
  , selectPathBased
  , getNodeData
  , filterChatCommunity
  ) where

import Data.List (sortOn, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Ord (Down(..))

import Graphos.Domain.Types
  ( NodeId, Node(..), Edge(..), confidenceScore
  , CommunityId, CommunityMap, FileType(..), Analysis(..), GodNode(..)
  )
import Graphos.Domain.Graph
  ( Graph, gNodes, gEdges, neighbors, degree, shortestPath
  , breadthFirstSearch, articulationPoints
  )
import Graphos.Domain.Context
  ( QueryComplexity(..), ContextBudget(..), SelectedContext(..)
  , SelectionStrategy(..), budgetForComplexity, emptySelectedContext
  , chatCommunityId
  )

-- ───────────────────────────────────────────────
-- Main entry point
-- ───────────────────────────────────────────────

-- | Select context from a graph based on a query and budget.
-- Automatically classifies query complexity and chooses the best strategy.
-- Excludes chat history community (community 0) by default.
selectContext :: Graph -> CommunityMap -> Analysis -> Text -> ContextBudget -> SelectedContext
selectContext g commMap analysis query budget =
  let cleanCommMap = filterChatCommunity commMap
  in case classifyComplexity query g of
     Focused ->
       selectCommunityAware g cleanCommMap analysis query budget
     ModuleLevel ->
       selectCommunityAware g cleanCommMap analysis query budget
     CrossModule ->
       selectPathBased g cleanCommMap analysis query budget
     Architectural ->
       selectArchitectural g cleanCommMap analysis budget
     Exploratory ->
       selectRelevanceWeighted g cleanCommMap analysis query budget

-- | Like selectContext but optionally includes chat history community.
--   When includeHistory is True, chat history nodes may appear in results.
selectContextWithHistory :: Bool -> Graph -> CommunityMap -> Analysis -> Text -> ContextBudget -> SelectedContext
selectContextWithHistory includeHistory g commMap analysis query budget =
  let effectiveCommMap = if includeHistory then commMap else filterChatCommunity commMap
  in case classifyComplexity query g of
     Focused ->
       selectCommunityAware g effectiveCommMap analysis query budget
     ModuleLevel ->
       selectCommunityAware g effectiveCommMap analysis query budget
     CrossModule ->
       selectPathBased g effectiveCommMap analysis query budget
     Architectural ->
       selectArchitectural g effectiveCommMap analysis budget
     Exploratory ->
       selectRelevanceWeighted g effectiveCommMap analysis query budget

-- | Remove the chat history community (community 0) from a CommunityMap.
--   Use this to prevent chat nodes from polluting code-based context selection.
filterChatCommunity :: CommunityMap -> CommunityMap
filterChatCommunity = Map.delete chatCommunityId

-- ───────────────────────────────────────────────
-- Query complexity classification
-- ───────────────────────────────────────────────

-- | Classify a query's complexity based on heuristic analysis.
--   - Few matches, specific terms → Focused
--   - Multiple matches in one area → ModuleLevel
--   - Terms spanning different areas → CrossModule
--   - "overview"/"architecture"/"how does X work" → Architectural
--   - Broad/vague → Exploratory
classifyComplexity :: Text -> Graph -> QueryComplexity
classifyComplexity query g =
  let terms = filter ((> 2) . T.length) (T.words (T.toLower query))
      lower = T.toLower query
      -- Count matching nodes
      matches = [nid | (nid, n) <- Map.toList (gNodes g)
                      , any (`T.isInfixOf` T.toLower (nodeLabel n)) terms]
      matchCount = length matches
      -- Check for architectural keywords
      archKeywords = ["overview", "architecture", "structure", "how does"
                     , "how do", "explain", "describe", "design", "module"]
      isArchitectural = any (`T.isInfixOf` lower) archKeywords
      -- Check if matches span multiple communities (using matchCount)
      isBroad = matchCount > 15 || length terms > 5
  in case () of
       _ | isArchitectural -> Architectural
         | length terms <= 1 && matchCount <= 3 -> Focused
         | matchCount <= 10 -> ModuleLevel
         | isBroad          -> Exploratory
         | otherwise        -> CrossModule

-- | Compute budget for a given complexity level.
-- Delegates to Domain.Context.budgetForComplexity.
computeBudget :: QueryComplexity -> Int -> ContextBudget
computeBudget = budgetForComplexity

-- ───────────────────────────────────────────────
-- Strategy 1: Community-aware selection
-- ───────────────────────────────────────────────

-- | Select context by finding the best matching node, including its community,
-- bridge nodes connecting to adjacent communities, and hub nodes.
selectCommunityAware :: Graph -> CommunityMap -> Analysis -> Text -> ContextBudget -> SelectedContext
selectCommunityAware g commMap analysis query budget =
  let terms = filter ((> 2) . T.length) (T.words (T.toLower query))
      -- Find best matching nodes
      scored = sortOn (\(_, s) -> Down s)
                [(nid, matchScore n terms) | (nid, n) <- Map.toList (gNodes g), matchScore n terms > 0]
      startNodes = take 5 [nid | (nid, _) <- scored]
  in case startNodes of
       [] -> emptySelectedContext budget
       (best:_) ->
         let -- Find which community this node belongs to
             commIds = [cid | (cid, members) <- Map.toList commMap
                            , best `elem` members]
             -- Get members of those communities
             commMembers = nub $ concat [Map.findWithDefault [] cid commMap | cid <- commIds]
             -- Get bridge/articulation points
             bridges = articulationPoints g
             -- Filter bridges that connect to relevant communities
             relevantBridges = filter (\b -> b `elem` commMembers || any (`elem` commIds)
                           [cid | (cid, members) <- Map.toList commMap, b `elem` members]) bridges
             -- Collect all nodes in budget
             allCandidateNodes = Set.fromList commMembers
                                `Set.union` Set.fromList relevantBridges
                                `Set.union` Set.fromList (map gnId (take 5 (analysisGodNodes analysis)))
             -- Apply node budget
             nodesInBudget = take (cbMaxNodes budget)
                           $ sortOn (\nid -> Down $ relevanceScore nid g terms) 
                           $ Set.toList allCandidateNodes
             -- Get edges between selected nodes
             selectedEdges = [e | ((s, t), e) <- Map.toList (gEdges g)
                                , s `Set.member` Set.fromList nodesInBudget
                                , t `Set.member` Set.fromList nodesInBudget]
             edgesInBudget = take (cbMaxEdges budget) selectedEdges
         in SelectedContext
            { scNodes           = [(nid, n) | nid <- nodesInBudget
                                            , Just n <- [Map.lookup nid (gNodes g)]]
            , scEdges           = edgesInBudget
            , scCommunities     = Map.filterWithKey (\cid _ -> cid `elem` commIds) commMap
            , scCommunityLabels = Map.fromList [ (cid, T.pack ("Community " ++ show cid))
                                              | cid <- commIds]
            , scBridgeNodes     = relevantBridges
            , scGodNodes         = take 5 (analysisGodNodes analysis)
            , scStrategy        = CommunityAware
            , scBudget          = budget
            , scMatchScore      = fromIntegral $ sum [s | (_, s) <- take 3 scored]
            }

-- ───────────────────────────────────────────────
-- Strategy 2: Relevance-weighted BFS
-- ───────────────────────────────────────────────

-- | Select context by scoring nodes on relevance and doing BFS within budget.
selectRelevanceWeighted :: Graph -> CommunityMap -> Analysis -> Text -> ContextBudget -> SelectedContext
selectRelevanceWeighted g commMap analysis query budget =
  let terms = filter ((> 2) . T.length) (T.words (T.toLower query))
      -- Find matching start nodes
      scored = sortOn (\(_, s) -> Down s)
                [(nid, relevanceScore nid g terms) | (nid, n) <- Map.toList (gNodes g)
                                                  , matchScore n terms > 0]
      startNodes = take 3 [nid | (nid, _) <- scored]
      -- BFS from each start node, collect nodes within depth limit
      bfsDepth = case cbComplexity budget of
        Focused       -> 2
        ModuleLevel   -> 3
        CrossModule   -> 4
        Architectural -> 3
        Exploratory   -> 3
      visitedNodes = Set.unions [breadthFirstSearch g nid bfsDepth | nid <- startNodes]
      -- Score all visited nodes by relevance
      scoredNodes = sortOn (\(nid, _) -> Down $ relevanceScore nid g terms)
                     [(nid, relevanceScore nid g terms) | nid <- Set.toList visitedNodes]
      -- Take top-N within node budget
      nodesInBudget = take (cbMaxNodes budget) [nid | (nid, _) <- scoredNodes]
      -- Get edges between selected nodes
      edgesInBudget = take (cbMaxEdges budget) [e | ((s,t), e) <- Map.toList (gEdges g)
                                                  , s `elem` nodesInBudget
                                                  , t `elem` nodesInBudget]
      -- Find relevant communities
      nodeCommMap = nodeCommunityMap commMap
      relCommIds = nub [cid | nid <- nodesInBudget
                            , Just cid <- [Map.lookup nid nodeCommMap]]
  in SelectedContext
     { scNodes           = [(nid, n) | nid <- nodesInBudget
                                      , Just n <- [Map.lookup nid (gNodes g)]]
     , scEdges           = edgesInBudget
     , scCommunities     = Map.filterWithKey (\cid _ -> cid `elem` relCommIds) commMap
     , scCommunityLabels = Map.fromList [(cid, T.pack ("Community " ++ show cid)) | cid <- relCommIds]
     , scBridgeNodes     = filter (`elem` articulationPoints g) nodesInBudget
     , scGodNodes         = take 5 (analysisGodNodes analysis)
     , scStrategy        = RelevanceWeightedBFS
     , scBudget          = budget
     , scMatchScore      = sum [s | (_, s) <- take 3 scoredNodes]
     }

-- ───────────────────────────────────────────────
-- Strategy 3: Path-based selection
-- ───────────────────────────────────────────────

-- | Select context by finding the shortest path between two concepts
-- and including all nodes along the path plus their immediate neighbors.
selectPathBased :: Graph -> CommunityMap -> Analysis -> Text -> ContextBudget -> SelectedContext
selectPathBased g commMap analysis query budget =
  let terms = filter ((> 2) . T.length) (T.words (T.toLower query))
      -- Find best matching nodes (we need at least 2 for a path)
      scored = sortOn (\(_, s) -> Down s)
                [(nid, matchScore n terms) | (nid, n) <- Map.toList (gNodes g), matchScore n terms > 0]
      startNodes = [nid | (nid, _) <- take 5 scored]
  in case startNodes of
       [] -> emptySelectedContext budget
       [_] -> -- Only one match, fall back to community-aware
         selectCommunityAware g commMap analysis query budget
       (from:to:_) -> -- Path between top 2 matches
         case shortestPath g from to of
           Nothing -> selectCommunityAware g commMap analysis query budget
           Just path ->
             let pathSet = Set.fromList path
                 -- Include immediate neighbors of path nodes
                 neighborSets = [neighbors g nid | nid <- path]
                 allNeighbors = Set.unions neighborSets `Set.union` pathSet
                 -- Score by relevance and take within budget
                 scoredNodes = sortOn (\nid -> Down $ relevanceScore nid g terms)
                              (Set.toList allNeighbors)
                 nodesInBudget = take (cbMaxNodes budget) scoredNodes
                 -- Get edges between selected nodes
                 edgesInBudget = take (cbMaxEdges budget) [e | ((s,t), e) <- Map.toList (gEdges g)
                                                            , s `elem` nodesInBudget
                                                            , t `elem` nodesInBudget]
                 -- Find relevant communities
                 nodeCommMap' = nodeCommunityMap commMap
                 relCommIds = nub [cid | nid <- nodesInBudget
                                      , Just cid <- [Map.lookup nid nodeCommMap']]
             in SelectedContext
                { scNodes           = [(nid, n) | nid <- nodesInBudget
                                                 , Just n <- [Map.lookup nid (gNodes g)]]
                , scEdges           = edgesInBudget
                , scCommunities     = Map.filterWithKey (\cid _ -> cid `elem` relCommIds) commMap
                , scCommunityLabels = Map.fromList [(cid, T.pack ("Community " ++ show cid)) | cid <- relCommIds]
                , scBridgeNodes     = filter (`elem` articulationPoints g) path
                , scGodNodes         = take 5 (analysisGodNodes analysis)
                , scStrategy        = PathBased
                , scBudget          = budget
                , scMatchScore      = fromIntegral $ sum [s | (_, s) <- take 3 scored]
                }

-- ───────────────────────────────────────────────
-- Strategy 4: Architectural overview
-- ───────────────────────────────────────────────

-- | Select context for architectural understanding.
-- Focuses on god nodes, bridge nodes, and community structure.
selectArchitectural :: Graph -> CommunityMap -> Analysis -> ContextBudget -> SelectedContext
selectArchitectural g commMap analysis budget =
  let -- Key architectural nodes: god nodes + articulation points
      godIds = map gnId (take 10 (analysisGodNodes analysis))
      bridges = take 10 (articulationPoints g)
      -- One representative node per community (highest degree)
      commReps = concatMap (take 1 . sortOn (\nid -> Down $ degree g nid))
                  [members | (_, members) <- Map.toList commMap]
      -- Combine all, prioritize within budget
      allNodes = Set.fromList (godIds ++ bridges ++ commReps)
      nodesInBudget = take (cbMaxNodes budget) $ Set.toList allNodes
      -- Get edges between selected nodes
      edgesInBudget = take (cbMaxEdges budget) [e | ((s,t), e) <- Map.toList (gEdges g)
                                                  , s `elem` nodesInBudget
                                                  , t `elem` nodesInBudget]
      -- All communities are relevant for architectural view
      commLabels = Map.fromList [(cid, T.pack ("Community " ++ show cid))
                                | cid <- Map.keys commMap]
  in SelectedContext
     { scNodes           = [(nid, n) | nid <- nodesInBudget
                                      , Just n <- [Map.lookup nid (gNodes g)]]
     , scEdges           = edgesInBudget
     , scCommunities     = commMap
     , scCommunityLabels = commLabels
     , scBridgeNodes     = bridges
     , scGodNodes         = take 10 (analysisGodNodes analysis)
     , scStrategy        = CommunityAware
     , scBudget          = budget
     , scMatchScore      = fromIntegral (length godIds + length bridges)
     }

-- ───────────────────────────────────────────────
-- Scoring helpers
-- ───────────────────────────────────────────────

-- | Score a node's relevance to the query terms.
-- Considers: label match, community membership, confidence, degree, bridge/god status.
relevanceScore :: NodeId -> Graph -> [Text] -> Double
relevanceScore nid g terms =
  let nodeLabelScore = fromIntegral $ matchScore (getNodeData nid g) terms
      -- Boost for high-degree nodes
      degBoost = if degree g nid > 10 then 1.0 else 0.0
      -- Boost for edges with high confidence
      edgeBoost = sum [confidenceScore (edgeConfidence e) * 0.5
                      | ((s,_), e) <- Map.toList (gEdges g), s == nid]
  in nodeLabelScore + degBoost + edgeBoost

-- | Score a node's label against query terms (integer version for sorting)
matchScore :: Node -> [Text] -> Int
matchScore node terms =
  let lower = T.toLower (nodeLabel node)
  in sum [1 | t <- terms, T.isInfixOf t lower]

-- | Get node data by NodeId, defaulting to unknown
getNodeData :: NodeId -> Graph -> Node
getNodeData nid g = Map.findWithDefault unknownNode nid (gNodes g)
  where
    unknownNode = Node
      { nodeId           = nid
      , nodeLabel        = "unknown"
      , nodeFileType     = CodeFile
      , nodeSourceFile   = ""
      , nodeSourceLocation = Nothing
      , nodeLineEnd      = Nothing
      , nodeKind         = Nothing
      , nodeSignature    = Nothing
      , nodeSourceUrl    = Nothing
      , nodeCapturedAt   = Nothing
      , nodeAuthor       = Nothing
      , nodeContributor  = Nothing
      }

-- | Build a reverse map: NodeId → CommunityId
nodeCommunityMap :: CommunityMap -> Map NodeId CommunityId
nodeCommunityMap commMap = Map.fromList [(nid, cid) | (cid, nids) <- Map.toList commMap, nid <- nids]