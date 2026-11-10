{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphos.Domain.Graph.UQIProjectionSpec where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck
import Graphos.Domain.Types
import qualified Graphos.Domain.Types as T
import Graphos.Domain.Graph (Graph(..), buildGraph, gNodes, gEdges, gDirected, gHash, neighbors, degree)
import Graphos.Domain.Graph.CollisionSpec ()

reindexGraph :: Graph -> Graph
reindexGraph g =
  let ks = sort (Map.keys (gNodes g))
      ps = reverse ks
      m  = Map.fromList (zip ks ps)
      nodes' = Map.fromList
        [ (p, (gNodes g Map.! k) { T.nodeId = p })
        | (k, p) <- zip ks ps
        ]
      edges' = Map.fromList
        [ ( EdgeId (m Map.! T.edgeSource e <> "->" <> m Map.! T.edgeTarget e)
          , e { T.edgeId     = EdgeId (m Map.! T.edgeSource e <> "->" <> m Map.! T.edgeTarget e)
               , T.edgeSource = m Map.! T.edgeSource e
               , T.edgeTarget = m Map.! T.edgeTarget e
               } )
        | e <- Map.elems (gEdges g)
        ]
  in buildGraph (gDirected g) (Extraction { extractionNodes = nodes', extractionEdges = edges' })

spec :: Spec
spec = do
  describe "UQI-1 projections commute with node re-indexing" $ do
    it "node re-indexing preserves node count, labels, and directed flag" $ property $
      \(g :: Graph) ->
        let re  = reindexGraph g
        in Map.size (gNodes re) == Map.size (gNodes g)
           .&&. gDirected re == gDirected g
           .&&. sort (map nodeLabel (Map.elems (gNodes re)))
                 == sort (map nodeLabel (Map.elems (gNodes g)))
    it "support-degree is invariant under node re-indexing" $ property $
      \(g :: Graph) ->
        let ks  = sort (Map.keys (gNodes g))
            m   = Map.fromList (zip ks (reverse ks))
            re  = reindexGraph g
        in conjoin
             [ counterexample (show k) (degree re (m Map.! k) == degree g k)
             | k <- ks
             ]
    it "neighbors are preserved under re-indexing" $ property $
      \(g :: Graph) ->
        let ks  = sort (Map.keys (gNodes g))
            m   = Map.fromList (zip ks (reverse ks))
            re  = reindexGraph g
        in conjoin
             [ counterexample (show k) $
                 neighbors re (m Map.! k) == Set.map (m Map.!) (neighbors g k)
             | k <- ks
             ]

  describe "UQI-2 structural and modularity reads use disjoint fields" $ do
    it "rebuilding adjacency from gNodes/gEdges leaves structural reads unchanged" $ property $
      \(g :: Graph) ->
        let rebuilt = buildGraph (gDirected g) (Extraction { extractionNodes = gNodes g, extractionEdges = Map.fromList [(edgeId e, e) | e <- Map.elems (gEdges g)] })
        in conjoin
             [ gNodes rebuilt == gNodes g
             , gEdges rebuilt == gEdges g
             , gDirected rebuilt == gDirected g
             , gHash rebuilt == gHash g
             ]
    it "rebuilding adjacency leaves modularity support readings unchanged" $ property $
      \(g :: Graph) ->
        let rebuilt = buildGraph (gDirected g) (Extraction { extractionNodes = gNodes g, extractionEdges = Map.fromList [(edgeId e, e) | e <- Map.elems (gEdges g)] })
        in counterexample "deg" (property
             ( Map.size (gNodes rebuilt) == Map.size (gNodes g)
             && and [ degree rebuilt v == degree g v | v <- Map.keys (gNodes g) ]))
