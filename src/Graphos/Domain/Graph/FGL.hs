-- | Adapter between Graphos domain types and fgl (Functional Graph Library)
-- 
-- This module provides bidirectional conversion between Graphos's graph data
-- and fgl's Gr type, enabling the use of fgl's rich graph algorithm library
-- while preserving Graphos's domain model.
--
-- IMPORTANT: This module must NOT import Graphos.Domain.Graph to avoid
-- cyclic dependencies. It operates on the raw Map/Set components instead.

module Graphos.Domain.Graph.FGL
  ( -- * Type aliases for clarity
    FGLNodeLabel
  , FGLEdgeLabel
  , FGLGraph
  
    -- * Conversion functions
  , toFGL
  , fromFGL
  
    -- * Node ID mapping
  , nidToInt
  
    -- * Helper functions
  , fglNodeId
  , fglNodeData
  , fglEdgeRelation
  , fglEdgeConfidence
  , fglEdgeData
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Graph.Inductive.Graph (LNode, LEdge, mkGraph, labNodes, labEdges)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Graphos.Domain.Types
  ( NodeId, Node(..), Edge(..), Relation, Confidence
  , edgeSource, edgeTarget, edgeRelation, edgeConfidence
  )

-- | Node label in fgl representation: (NodeId, Node)
type FGLNodeLabel = (NodeId, Node)

-- | Edge label in fgl representation: (Relation, Confidence, Edge)
type FGLEdgeLabel = (Relation, Confidence, Edge)

-- | The fgl graph type specialized to our labels
type FGLGraph = Gr FGLNodeLabel FGLEdgeLabel

-- | Hash a NodeId (Text) to an Int for fgl node IDs
nidToInt :: NodeId -> Int
nidToInt nid = fromIntegral (T.foldl' (\acc c -> acc * 31 + fromIntegral (fromEnum c)) (0 :: Integer) nid `mod` fromIntegral (maxBound :: Int))

-- | Convert Graphos Maps to fgl's Gr
toFGL :: Map NodeId Node -> Map (NodeId, NodeId) Edge -> FGLGraph
toFGL nodeMap edgeMap =
  let
    -- Convert nodes
    fglNodes :: [LNode FGLNodeLabel]
    fglNodes = [(nidToInt nid, (nid, n)) | (nid, n) <- Map.toList nodeMap]
    
    -- Convert edges
    fglEdges :: [LEdge FGLEdgeLabel]
    fglEdges = [
      (nidToInt (edgeSource e), nidToInt (edgeTarget e), (edgeRelation e, edgeConfidence e, e))
      | e <- Map.elems edgeMap]
  in mkGraph fglNodes fglEdges

-- | Convert fgl's Gr back to Graphos components (Maps and Sets)
fromFGL :: FGLGraph -> Bool -> (Map NodeId Node, Map (NodeId, NodeId) Edge, Map NodeId (Set NodeId), Map NodeId (Set NodeId))
fromFGL gr directed =
  let
    -- Extract nodes from fgl graph
    nodeList = labNodes gr
    nodeMap = Map.fromList [(nid, n) | (_, (nid, n)) <- nodeList]
    
    -- Extract edges from fgl graph
    edgeList = labEdges gr
    edgeMap = Map.fromList [
      ((edgeSource e, edgeTarget e), e) |
      (_, _, (_, _, e)) <- edgeList]
    
    -- Build adjacency maps
    fwdAdj = Map.fromListWith Set.union [
      (edgeSource e, Set.singleton (edgeTarget e)) | e <- Map.elems edgeMap]
    
    bwdAdj = if directed
      then Map.fromListWith Set.union [
        (edgeTarget e, Set.singleton (edgeSource e)) | e <- Map.elems edgeMap]
      else Map.fromListWith Set.union [
        (edgeTarget e, Set.singleton (edgeSource e)) | e <- Map.elems edgeMap]
        <> fwdAdj  -- undirected: edges go both ways
  in (nodeMap, edgeMap, fwdAdj, bwdAdj)

-- | Extract NodeId from fgl node label
fglNodeId :: FGLNodeLabel -> NodeId
fglNodeId (nid, _) = nid

-- | Extract Node data from fgl node label
fglNodeData :: FGLNodeLabel -> Node
fglNodeData (_, n) = n

-- | Extract Relation from fgl edge label
fglEdgeRelation :: FGLEdgeLabel -> Relation
fglEdgeRelation (rel, _, _) = rel

-- | Extract Confidence from fgl edge label
fglEdgeConfidence :: FGLEdgeLabel -> Confidence
fglEdgeConfidence (_, conf, _) = conf

-- | Extract full Edge data from fgl edge label
fglEdgeData :: FGLEdgeLabel -> Edge
fglEdgeData (_, _, e) = e