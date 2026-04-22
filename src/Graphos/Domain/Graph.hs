-- | Graph operations (re-export hub).
-- Build, merge, query, and diff knowledge graphs.
-- 
-- This module re-exports everything from sub-modules for backward compatibility.
-- Sub-modules: Core, Query, Analysis, Diff, FGL.
module Graphos.Domain.Graph
  ( -- * Types
    Graph
  , gNodes
  , gEdges
  
    -- * Construction
  , buildGraph
  , mergeExtractions
  , mergeGraphs
  
    -- * Queries
  , godNodes
  , neighbors
  , degree
  , shortestPath
  , breadthFirstSearch
  , depthFirstSearch
  , subgraph
  
    -- * Advanced queries (fgl-powered)
  , articulationPoints
  , biconnectedComponents
  , dominators
  
    -- * Analysis helpers
  , isFileNode
  , isConceptNode
  , edgeBetweenness
  
    -- * Diff
  , graphDiff
  ) where

import Graphos.Domain.Graph.Core (Graph, gNodes, gEdges, buildGraph, mergeExtractions, mergeGraphs, isFileNode, isConceptNode)
import Graphos.Domain.Graph.Query (neighbors, degree, shortestPath, breadthFirstSearch, depthFirstSearch, subgraph)
import Graphos.Domain.Graph.Analysis (godNodes, articulationPoints, biconnectedComponents, dominators, edgeBetweenness)
import Graphos.Domain.Graph.Diff (graphDiff)