-- | Graph operations (re-export hub).
-- Build, merge, query, and diff knowledge graphs.
-- 
-- This module re-exports everything from sub-modules for backward compatibility.
-- Sub-modules: Core, Query, Analysis, Diff, FGL, Index.
module Graphos.Domain.Graph
  ( -- * Types
    Graph(..)

    -- * Construction
  , buildGraph
  , buildGraphWithStats
  , buildGraphStats
  , buildGraphCounts
  , edgeDropCounts
  , unknownEndpointDropCount
  , duplicateKeyDropCount
  , mergeExtractions
  , mergeGraphs
  , addEdges
  
    -- * Queries
  , godNodes
  , neighbors
  , degree
  , shortestPath
  , shortestPathReachable
  , breadthFirstSearch
  , depthFirstSearch
  , stronglyConnectedComponents
  , subgraph
  
    -- * Advanced queries (fgl-powered)
  , articulationPoints
  , biconnectedComponents
  , dominators
  
    -- * Analysis helpers
  , isFileNode
  , isConceptNode
  , makeStubNode
  , edgeBetweenness
  , edgeBetweennessWith
  , defaultMaxSampledSources
  , defaultExactBetweennessNodeCap
  
    -- * Diff
  , graphDiff
  ) where

import Graphos.Domain.Graph.Core (Graph(..), buildGraph, buildGraphWithStats, buildGraphStats, buildGraphCounts, edgeDropCounts, unknownEndpointDropCount, duplicateKeyDropCount, mergeExtractions, mergeGraphs, addEdges, isFileNode, isConceptNode, makeStubNode)
import Graphos.Domain.Graph.Query (neighbors, degree, shortestPath, shortestPathReachable, breadthFirstSearch, depthFirstSearch, stronglyConnectedComponents, subgraph)
import Graphos.Domain.Graph.Analysis (godNodes, articulationPoints, biconnectedComponents, dominators, edgeBetweenness, edgeBetweennessWith, defaultMaxSampledSources, defaultExactBetweennessNodeCap)
import Graphos.Domain.Graph.Diff (graphDiff)