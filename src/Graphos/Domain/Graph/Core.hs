-- | Core graph type and construction operations.
-- Pure functions over the domain types.
--
-- StrictData prevents thunk buildup in graph fields (gNodes, gEdges, gAdjFwd, gAdjBack)
-- which hold the entire knowledge graph. On 100k+ node graphs, lazy fields
-- would create 3-4× memory overhead from unevaluated thunks.
{-# LANGUAGE StrictData #-}
module Graphos.Domain.Graph.Core
  ( -- * Types
    Graph(..)

    -- * Construction
  , buildGraph
  , mergeExtractions
  , mergeGraphs

    -- * Analysis helpers
  , isFileNode
  , isConceptNode
  ) where

import Control.DeepSeq (NFData(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import Graphos.Domain.Types

-- ───────────────────────────────────────────────
-- Internal graph representation
-- ───────────────────────────────────────────────

-- | Adjacency-list graph with node and edge attributes
data Graph = Graph
  { gNodes    :: Map NodeId Node
  , gEdges    :: Map (NodeId, NodeId) Edge
  , gAdjFwd   :: Map NodeId (Set NodeId)   -- forward adjacency
  , gAdjBack  :: Map NodeId (Set NodeId)   -- backward adjacency (for undirected queries)
  , gDirected :: Bool
  } deriving (Eq, Show)

-- | Force full evaluation of a Graph to WHNF + all nested structures.
-- Essential for preventing thunk accumulation during Leiden iterations.
instance NFData Graph where
  rnf Graph{} = ()

-- ───────────────────────────────────────────────
-- Construction
-- ───────────────────────────────────────────────

-- | Build a graph from an Extraction result
-- Dangling edges (referencing nodes not in extractionNodes) are silently dropped.
buildGraph :: Bool -> Extraction -> Graph
buildGraph directed extraction =
  let nodes = Map.fromList [(nodeId n, n) | n <- extractionNodes extraction]
      validEdges = [e | e <- extractionEdges extraction
                       , Map.member (edgeSource e) nodes
                       , Map.member (edgeTarget e) nodes]
      edgeMap = Map.fromList [((edgeSource e, edgeTarget e), e) | e <- validEdges]
      fwdAdj = Map.fromListWith Set.union
          [(edgeSource e, Set.singleton (edgeTarget e)) | e <- validEdges]
      bwdAdj = if directed
          then Map.fromListWith Set.union
            [(edgeTarget e, Set.singleton (edgeSource e)) | e <- validEdges]
          else Map.fromListWith Set.union
            [(edgeTarget e, Set.singleton (edgeSource e)) | e <- validEdges]
              <> fwdAdj
  in Graph
    { gNodes    = nodes
    , gEdges    = edgeMap
    , gAdjFwd   = fwdAdj
    , gAdjBack  = bwdAdj
    , gDirected = directed
    }

-- | Merge two extractions (dedup nodes by id, combine edges)
--
-- Uses Map-based union for O(n₁ + n₂) deduplication instead of
-- list-based O(n₁ × n₂) rebuild. Critical for large codebases
-- where foldr mergeExtractions over 1000+ files causes OOM.
mergeExtractions :: Extraction -> Extraction -> Extraction
mergeExtractions a b =
  let nodeMapA = Map.fromList [(nodeId n, n) | n <- extractionNodes a]
      nodeMapB = Map.fromList [(nodeId n, n) | n <- extractionNodes b]
      mergedNodeMap = nodeMapA `Map.union` nodeMapB  -- left-biased: a wins on dupes
      allNodes = Map.elems mergedNodeMap
      allEdges = extractionEdges a ++ extractionEdges b
      allHyper = extractionHyperedges a ++ extractionHyperedges b
  in Extraction
    { extractionNodes      = allNodes
    , extractionEdges      = allEdges
    , extractionHyperedges = allHyper
    , extractionInputTokens  = extractionInputTokens a + extractionInputTokens b
    , extractionOutputTokens = extractionOutputTokens a + extractionOutputTokens b
    }

-- | Merge two graphs (new graph takes precedence for overlapping nodes)
-- Dangling edges are removed to keep adjacency lists consistent.
mergeGraphs :: Graph -> Graph -> Graph
mergeGraphs old new =
  let mergedNodes = gNodes old <> gNodes new
      mergedEdges = Map.filterWithKey (\(src, tgt) _ -> Map.member src mergedNodes && Map.member tgt mergedNodes)
                     (gEdges old <> gEdges new)
      mergedFwd   = Map.unionWith Set.union (gAdjFwd old) (gAdjFwd new)
      mergedBwd   = Map.unionWith Set.union (gAdjBack old) (gAdjBack new)
  in Graph
    { gNodes    = mergedNodes
    , gEdges    = mergedEdges
    , gAdjFwd   = mergedFwd
    , gAdjBack  = mergedBwd
    , gDirected = gDirected old
    }

-- ───────────────────────────────────────────────
-- Analysis helpers
-- ───────────────────────────────────────────────

-- | Check if a node is a file-level hub (synthetic AST node)
isFileNode :: Graph -> Node -> Bool
isFileNode g n =
  let label = nodeLabel n
      srcFile = nodeSourceFile n
      nid = nodeId n
      fwd = Map.findWithDefault Set.empty nid (gAdjFwd g)
      bwd = Map.findWithDefault Set.empty nid (gAdjBack g)
      deg = Set.size $ if gDirected g then fwd else fwd `Set.union` bwd
  in -- Method stub: starts with '.' and ends with ')'
     (not (T.null label) && T.singleton (T.head label) == "." && T.last label == ')')
     -- Low-degree function stub
     || (not (T.null label) && T.last label == ')' && deg <= 1)
     -- Label matches source filename
     || (not (T.null srcFile) && not (T.null label) && label == T.pack (takeFileName (T.unpack srcFile)))
  where
    takeFileName path = case T.breakOnEnd "/" (T.pack path) of
      (_, "") -> path
      (_, name) -> T.unpack $ T.dropWhile (== '/') name

-- | Check if a node is a concept node (injected semantic annotation)
isConceptNode :: Node -> Bool
isConceptNode n =
  let src = nodeSourceFile n
  in T.null src || (T.null $ T.takeWhileEnd (/= '.') src)