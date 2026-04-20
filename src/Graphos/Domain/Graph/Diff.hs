-- | Graph diff — compare two graph snapshots.
-- Pure functions over the domain types.
module Graphos.Domain.Graph.Diff
  ( graphDiff
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Domain.Graph.Core (Graph(..))

-- | Compare two graph snapshots
graphDiff :: Graph -> Graph -> GraphDiff
graphDiff old new =
  let oldNodeIds = Map.keysSet (gNodes old)
      newNodeIds = Map.keysSet (gNodes new)
      addedIds   = newNodeIds `Set.difference` oldNodeIds
      removedIds = oldNodeIds `Set.difference` newNodeIds
      newNodes   = [n | (nid, n) <- Map.toList (gNodes new), nid `Set.member` addedIds]
      removedNodes = [(nid, nodeLabel n) | (nid, n) <- Map.toList (gNodes old), nid `Set.member` removedIds]
      oldEdgeKeys = Map.keysSet (gEdges old)
      newEdgeKeys = Map.keysSet (gEdges new)
      addedEdgeKeys = newEdgeKeys `Set.difference` oldEdgeKeys
      removedEdgeKeys = oldEdgeKeys `Set.difference` newEdgeKeys
      newEdges   = [e | (k, e) <- Map.toList (gEdges new), k `Set.member` addedEdgeKeys]
      removedEs  = [e | (k, e) <- Map.toList (gEdges old), k `Set.member` removedEdgeKeys]
      parts = []
        <> (if null newNodes then [] else [T.pack (show (length newNodes) ++ " new node(s)")])
        <> (if null newEdges then [] else [T.pack (show (length newEdges) ++ " new edge(s)")])
        <> (if null removedNodes then [] else [T.pack (show (length removedNodes) ++ " node(s) removed")])
        <> (if null removedEs then [] else [T.pack (show (length removedEs) ++ " edge(s) removed")])
  in GraphDiff
    { gdNewNodes    = newNodes
    , gdRemovedNodes = removedNodes
    , gdNewEdges    = newEdges
    , gdRemovedEdges = removedEs
    , gdSummary     = if null parts then "no changes" else T.intercalate ", " parts
    }