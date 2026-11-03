{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphos.Domain.Graph.UQIProjectionSpec where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Graphos.Domain.Types
import qualified Graphos.Domain.Types as T
import Graphos.Domain.Graph (Graph(..), buildGraph, gNodes, gEdges, gDirected, gHash, neighbors, degree)

reindexGraph :: Graph -> Graph
reindexGraph g =
  let ks = sort (Map.keys (gNodes g))
      ps = reverse ks
      m  = Map.fromList (zip ks ps)
      nodes' = Map.fromList [(p, gNodes g Map.! k) | (k, p) <- zip ks ps]
      edges' = Map.fromList [ ( EdgeId (m Map.! T.edgeSource e <> "->" <> m Map.! T.edgeTarget e),
                                   e { T.edgeId     = EdgeId (m Map.! T.edgeSource e <> "->" <> m Map.! T.edgeTarget e),
                                    T.edgeSource = m Map.! T.edgeSource e,
                                    T.edgeTarget = m Map.! T.edgeTarget e }
                               | e <- Map.elems (gEdges g) ]
  in buildGraph (gDirected g) (Extraction { extractionNodes = nodes' `Map.union` (Map.toList nodes')
                                                       ; extractionEdges = edges' })

spec :: Spec
spec = do
  describe "UQI-1 projection disjoint-field independence" $ do
    it "projection leaves structural reads unchanged" $ property $
      \(g) -> conjoin [ gNodes rebuilt == gNodes g,
                        , gEdges rebuilt == gEdges g,
                        , gDirected rebuilt == gDirected g,
                        , gHash rebuilt == gHash g ]
                      (buildGraph (gDirected g) (Extraction { extractionNodes = gNodes g
                                                             `Map.union` Map.toList (gAdjFwd g)
                                                             ; extractionEdges = Map.fromList [(edgeId e, e) | e <- Map.elems (gEdges g)] }))
    it "degree is invariant under reindexing" $ property $
      \(g) -> conjoin [ counterexample (show k) $
                        (degree rebuilt v == degree g v | v <- Map.keys (gNodes rebuilt)
                       | k <- sort (Map.keys (gNodes g)) )
                      (buildGraph (gDirected g) (Extraction { extractionNodes = gNodes g
                                                             `Map.union` Map.toList (gAdjFwd g)
                                                             ; extractionEdges = Map.fromList [(edgeId e, e) | e <- Map.elems (gEdges g)] }))
    it "neighbors are preserved under reindexing" $ property $
      \(g) -> conjoin [ counterexample (show k) $
                        (neighbors rebuilt (m Map.! k) == Set.map (m Map.!) (neighbors g k)
                       | k <- sort (Map.keys (gNodes g)) )
                      (buildGraph (gDirected g) (Extraction { extractionNodes = gNodes g
                                                             `Map.union` Map.toList (gAdjFwd g)
                                                             ; extractionEdges = Map.fromList [(edgeId e, e) | e <- Map.elems (gEdges g)] }))
   where
     conjoin :: Graph -> Graph -> Bool
     conjoin rebuilt old = all (\n -> nodeLabel n == label n) (Map.keysSet (gNodes rebuilt) `intersection` Map.keys (gNodes old))

spec
