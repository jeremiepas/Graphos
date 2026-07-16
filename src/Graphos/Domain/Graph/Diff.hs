module Graphos.Domain.Graph.Diff
  ( graphDiff
  ) where

import qualified Data.Map.Strict as Map

import Graphos.Domain.Types
import Graphos.Domain.Graph.Core (Graph(..))

graphDiff :: Graph -> Graph -> GraphDiff
graphDiff old new =
  let addedNodes = gNodes new `Map.difference` gNodes old
      removedNodes = gNodes old `Map.difference` gNodes new
      addedEdgePairs = gEdges new `Map.difference` gEdges old
      removedEdgePairs = gEdges old `Map.difference` gEdges new
      toEdgeMap m = Map.fromList [(edgeId e, e) | e <- Map.elems m]
  in GraphDiff
    { diffAddedNodes   = addedNodes
    , diffRemovedNodes  = removedNodes
    , diffAddedEdges   = toEdgeMap addedEdgePairs
    , diffRemovedEdges  = toEdgeMap removedEdgePairs
    }