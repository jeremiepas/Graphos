{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Determinism and view-permutation invariance of community detection.
--
-- Covers AVI-659 (M2): for consistent views, @cluster(merge(V)) ~=
-- cluster(merge(V_sigma))@ — the community map is invariant under view
-- permutation, with the community-label bijection being the identity on
-- @NodeId -> CommunityId@. Also asserts the INV-PARTITION property (every node
-- belongs to exactly one community).
--
-- Grounding: design.md theorem T3 and INV-DETERMINISTIC-CLUSTER in the
-- @merge-cluster-determinism-graph-half@ change. The backing implementation
-- ('Graphos.Domain.Community.detectCommunitiesWithResolution') orders nodes,
-- neighbours and community ids canonically ('Data.Map'\/'Data.Set' keys and
-- 'IntMap' keys), so every step is a deterministic function of its canonical
-- inputs and the loop is strictly bounded ('resMaxIterations', early stop on
-- @moved == 0@).
module Graphos.Domain.Community.PermutationSpec (spec) where

import Data.List (nub, sort, permutations)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Short (fromText)
import Test.Hspec
import Test.QuickCheck (forAll, property, choose, sublistOf, Gen)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, buildGraph)
import Graphos.Domain.Community
  ( detectCommunitiesWithResolution
  , buildReverseIndex
  , defaultResolution
  )

-- ───────────────────────────────────────────────
-- Test fixtures (positional construction, matching the exact Node/Edge
-- record layouts in Types/Node.hs and Types/Edge.hs — named-field punctuation
-- is not enabled in this build)
-- ───────────────────────────────────────────────

-- | A fresh code node keyed by an unique id.
testNode :: Text -> Node
testNode nid =
  Node nid (fromText nid) CodeFile (fromText "test.hs")
    (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

-- | A directed edge between two ids.
testEdge :: Text -> Text -> Edge
testEdge src tgt =
  Edge (EdgeId (src <> "->" <> tgt)) src tgt Calls 1.0 (Confidence 1.0) Nothing

-- | A "view": a disjoint, fully-connected component (triangle) of three nodes.
-- Triangles sit at the @resMinSize = 3@ threshold, so they survive the
-- small-community merge as stable communities.
type View = (Text, [Node], [Edge])

-- | Four node-disjoint triangles, each a separate consistent view.
views :: [View]
views =
  [ triangle "A" ("1", "2", "3")
  , triangle "B" ("1", "2", "3")
  , triangle "C" ("1", "2", "3")
  , triangle "D" ("1", "2", "3")
  ]

-- | A triangle view keyed by a label prefix; node ids are @prefix ++ suffix@
-- so the four triangles are mutually node-disjoint (e.g. @A1@, @B1@, ...).
triangle :: Text -> (Text, Text, Text) -> View
triangle prefix (a, b, c) =
  ( prefix
  , [testNode (prefix <> a), testNode (prefix <> b), testNode (prefix <> c)]
  , [testEdge (prefix <> a) (prefix <> b)
    , testEdge (prefix <> b) (prefix <> c)
    , testEdge (prefix <> a) (prefix <> c)]
  )

-- | Build the combined graph from the triangles listed in the given id order.
--
-- Because the triangles are node-disjoint, the resulting adjacency is
-- independent of concatenation order: 'buildGraph' derives adjacency purely
-- from the edge set, so any permutation of the views yields the same graph.
graphFromOrder :: [Text] -> Graph
graphFromOrder order =
  let nodes = concat [ns | (o, ns, _) <- views, o `elem` order]
      edges = concat [es | (o, _, es) <- views, o `elem` order]
  in buildGraph False $ extractionFromLists nodes edges

-- | Node -> community-id projection (the "community label per node").
nodeLabels :: CommunityMap -> Map.Map NodeId CommunityId
nodeLabels = buildReverseIndex

-- | A connected random graph (path plus random extra edges) on @n@ nodes.
-- Connectivity guarantees every node has a neighbour, so the invariant below
-- holds regardless of how small-community merging treats any node.
genGraph :: Gen ([Node], [(Text, Text)])
genGraph = do
  n <- choose (2 :: Int, 8 :: Int)
  let ids = map (\i -> T.pack ("n" ++ show i)) [1..n]
      nodes = map testNode ids
      path = zipWith (\a b -> (a, b)) ids (rotate ids)
      possible = [ (a, b) | a <- ids, b <- ids, a < b ]
  extra <- sublistOf possible
  return (nodes, path ++ extra)

rotate :: [a] -> [a]
rotate [] = []
rotate xs = tail xs ++ [head xs]

-- ───────────────────────────────────────────────
-- Spec
-- ───────────────────────────────────────────────

spec :: Spec
spec = do
  describe "detectCommunitiesWithResolution determinism (AVI-659 / T3)" $ do
    it "is a pure function of a fixed graph: identical output on repeated calls" $ do
      let g = graphFromOrder ["A", "B", "C", "D"]
          r = defaultResolution
      nodeLabels (detectCommunitiesWithResolution g r)
        `shouldBe` nodeLabels (detectCommunitiesWithResolution g r)

    it "assigns identical community labels to every node for every merge order" $ do
      let reference =
            nodeLabels (detectCommunitiesWithResolution (graphFromOrder ["A", "B", "C", "D"]) defaultResolution)
      map (\order -> nodeLabels (detectCommunitiesWithResolution (graphFromOrder order) defaultResolution))
          (permutations ["A", "B", "C", "D"])
        `shouldSatisfy` (all (== reference))

    it "is invariant under reversed merge order (canonical cid bijection is the identity)" $ do
      let reference =
            nodeLabels (detectCommunitiesWithResolution (graphFromOrder ["A", "B", "C", "D"]) defaultResolution)
      nodeLabels (detectCommunitiesWithResolution (graphFromOrder ["D", "C", "B", "A"]) defaultResolution)
        `shouldBe` reference

  describe "INV-PARTITION (every node in exactly one community)" $ do
    it "covers every graph node with no loss" $ do
      let g = graphFromOrder ["A", "B", "C", "D"]
          cm = detectCommunitiesWithResolution g defaultResolution
          allNodes = sort (Map.keys (gNodes g))
          members = sort (concat (Map.elems cm))
      members `shouldBe` allNodes

    it "has no duplicate membership across communities" $ do
      let cm = detectCommunitiesWithResolution (graphFromOrder ["A", "B", "C", "D"]) defaultResolution
          members = concat (Map.elems cm)
      length members `shouldBe` length (nub members)

    it "holds for every merge order" $ do
      let allNodes = sort (Map.keys (gNodes (graphFromOrder ["A", "B", "C", "D"])))
          check order =
            let cm = detectCommunitiesWithResolution (graphFromOrder order) defaultResolution
                members = sort (concat (Map.elems cm))
            in members == allNodes && length members == length (nub members)
      map check (permutations ["A", "B", "C", "D"]) `shouldSatisfy` (all id)

    it "INV-PARTITION for connected random graphs" $ property $
      forAll genGraph (\(nodes, edges) -> do
        let g = buildGraph False $ extractionFromLists nodes [testEdge u v | (u, v) <- edges]
            cm = detectCommunitiesWithResolution g defaultResolution
            allNodes = sort (Map.keys (gNodes g))
            members = sort (concat (Map.elems cm))
        members `shouldBe` allNodes)
