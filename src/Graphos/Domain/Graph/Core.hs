-- | Core graph type and construction operations.
-- Pure functions over the domain types.
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

import Data.List (nubBy)
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

-- ───────────────────────────────────────────────
-- Construction
-- ───────────────────────────────────────────────

-- | Build a graph from an Extraction result
buildGraph :: Bool -> Extraction -> Graph
buildGraph directed extraction =
  let nodes = Map.fromList [(nodeId n, n) | n <- extractionNodes extraction]
      edgeMap = Map.fromList [((edgeSource e, edgeTarget e), e) | e <- extractionEdges extraction]
      fwdAdj = Map.fromListWith Set.union
          [(edgeSource e, Set.singleton (edgeTarget e)) | e <- extractionEdges extraction]
      bwdAdj = if directed
          then Map.fromListWith Set.union
            [(edgeTarget e, Set.singleton (edgeSource e)) | e <- extractionEdges extraction]
          else Map.fromListWith Set.union
            [(edgeTarget e, Set.singleton (edgeSource e)) | e <- extractionEdges extraction]
              <> fwdAdj  -- undirected: edges go both ways
  in Graph
    { gNodes    = nodes
    , gEdges    = edgeMap
    , gAdjFwd   = fwdAdj
    , gAdjBack  = bwdAdj
    , gDirected = directed
    }

-- | Merge two extractions (dedup nodes by id, combine edges)
mergeExtractions :: Extraction -> Extraction -> Extraction
mergeExtractions a b =
  let allNodes = nubBy (\x y -> nodeId x == nodeId y) (extractionNodes a ++ extractionNodes b)
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
mergeGraphs :: Graph -> Graph -> Graph
mergeGraphs old new =
  let mergedNodes = gNodes old <> gNodes new
      mergedEdges = gEdges old <> gEdges new
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