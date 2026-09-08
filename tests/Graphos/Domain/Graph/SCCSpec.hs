{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
-- | Spec for shortest-path reachability and SCC decomposition
-- (AVI-530 / AVI-518, spec: docs/math-requirements/AVI-518-shortest-path-scc.md).
module Graphos.Domain.Graph.SCCSpec where

import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)

import Graphos.Domain.Types
import Graphos.Domain.Graph

-- ───────────────────────────────────────────────
-- Fixtures
-- ───────────────────────────────────────────────

sccTestNode :: Text -> Node
sccTestNode nid = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "test.hs"
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodePresentBits  = 0
  }

sccTestEdge :: Text -> Text -> Edge
sccTestEdge src tgt = Edge
  { edgeId         = EdgeId (src <> "->" <> tgt)
  , edgeSource     = src
  , edgeTarget     = tgt
  , edgeRelation   = Calls
  , edgeConfidence = Confidence 1.0
  , edgeWeight     = 1.0
  , edgeExtra      = Nothing
  }

-- §8 worked example (directed): a↺b↺c cycle, c→d, d↺e cycle, d→f.
goldenGraph :: Graph
goldenGraph =
  buildGraph True $ extractionFromLists
    (map sccTestNode ["a", "b", "c", "d", "e", "f"])
    [ sccTestEdge "a" "b"
    , sccTestEdge "b" "c"
    , sccTestEdge "c" "a"
    , sccTestEdge "c" "d"
    , sccTestEdge "d" "e"
    , sccTestEdge "e" "d"
    , sccTestEdge "d" "f"
    ]

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- INV-7: SCC result is a partition of V.
isPartition :: Graph -> Map.Map Int [Text] -> Bool
isPartition g m =
  let nodes = Set.fromList (Map.keys (gNodes g))
      members = Set.fromList (concatMap snd (Map.toList m))
      comps = map Set.fromList (Map.elems m)
      nonEmpty = all (not . Set.null) comps
      disjoint = and [ Set.null (a `Set.intersection` b)
                     | (i, a) <- zip [0 :: Int ..] comps
                     , (j, b) <- zip [0 :: Int ..] comps
                     , i < j ]
  in nodes == members && nonEmpty && disjoint

-- INV-8: path is a genuine E-walk with correct endpoints and hop-minimal length.
isValidWalk :: Graph -> Text -> Text -> [Text] -> Bool
isValidWalk g s t p =
  not (null p) && head p == s && last p == t
  && all (\(u, v) -> Map.member (u, v) (gEdges g) || u == v) (zip p (drop 1 p))
  && isHopMinimal g p

-- BFS distance from s over E; Nothing = unreachable.
bfsDist :: Graph -> Text -> Map.Map Text Int
bfsDist g s = go [(s, 0)] (Map.singleton s 0)
  where
    go [] dist = dist
    go ((u, d) : rest) dist =
      let vs = [v | v <- Set.toList (Map.findWithDefault Set.empty u (gAdjFwd g))
               , not (Map.member v dist)]
          dist' = foldr (\v acc -> Map.insert v (d + 1) acc) dist vs
      in go (rest ++ [(v, d + 1) | v <- vs]) dist'

isHopMinimal :: Graph -> [Text] -> Bool
isHopMinimal g p =
  case bfsDist g (head p) Map.!? last p of
    Just d  -> length p - 1 == d
    Nothing -> False

-- ───────────────────────────────────────────────
-- Spec
-- ───────────────────────────────────────────────

spec :: Spec
spec = do
  describe "stronglyConnectedComponents (§8 golden example)" $ do
    it "finds SCCs {a,b,c}, {d,e}, {f}" $ do
      let scc = stronglyConnectedComponents goldenGraph
          comps = sort (map sort (Map.elems scc))
      comps `shouldBe` [["a", "b", "c"], ["d", "e"], ["f"]]

    it "labels component ids by ascending minimum NodeId" $ do
      let scc = stronglyConnectedComponents goldenGraph
      Map.lookup 0 scc `shouldBe` Just ["a", "b", "c"]
      Map.lookup 1 scc `shouldBe` Just ["d", "e"]
      Map.lookup 2 scc `shouldBe` Just ["f"]

    it "condensation is the chain {a,b,c} -> {d,e} -> {f} (Thm 2.3)" $ do
      let scc = stronglyConnectedComponents goldenGraph
          compOf n = head [cid | (cid, ms) <- Map.toList scc, n `elem` ms]
          arcs = [ (compOf u, compOf v)
                 | (u, v) <- Map.keys (gEdges goldenGraph)
                 , compOf u /= compOf v ]
          -- Chain a-b-c (id 0) -> d-e (id 1) -> f (id 2)
          arcPairs = [(0, 1), (1, 2)]
      sort arcs `shouldBe` arcPairs

  describe "shortestPathReachable (§8 golden example)" $ do
    it "a reaches f (a ⤳ f via a->b->c->d->f)" $
      shortestPathReachable goldenGraph "a" "f" `shouldBe` True

    it "f does not reach a (¬(f ⤳ a))" $
      shortestPathReachable goldenGraph "f" "a" `shouldBe` False

    it "is reflexive (a ⤳ a)" $
      shortestPathReachable goldenGraph "a" "a" `shouldBe` True

    it "agrees with shortestPath /= Nothing (Thm 2.4)" $ do
      shortestPathReachable goldenGraph "a" "f"
        `shouldBe` (shortestPath goldenGraph "a" "f" /= Nothing)
      shortestPathReachable goldenGraph "f" "a"
        `shouldBe` (shortestPath goldenGraph "f" "a" /= Nothing)

    it "shortestPath a f is Just [a,b,c,d,f] (hop-minimal)" $
      shortestPath goldenGraph "a" "f" `shouldBe` Just ["a", "b", "c", "d", "f"]

    it "shortestPath a a is Just [a]" $
      shortestPath goldenGraph "a" "a" `shouldBe` Just ["a"]

    it "shortestPath f a is Nothing" $
      shortestPath goldenGraph "f" "a" `shouldBe` Nothing

  describe "SCC invariants" $ do
    it "INV-7: SCCs partition V on the golden graph" $
      isPartition goldenGraph (stronglyConnectedComponents goldenGraph)
        `shouldBe` True

    it "INV-7 (property): SCCs partition V on random graphs" $
      property $ \(SmallGraph g) ->
        isPartition g (stronglyConnectedComponents g)

    it "INV-8 (property): every arc (u,v), u /= v, has shortestPath u v == Just [u,v]" $
      property $ \(SmallGraph g) ->
        all (\(u, v) -> shortestPath g u v == Just [u, v])
            [ (u, v) | (u, v) <- Map.keys (gEdges g), u /= v ]

    it "INV-8: self-loop arc (u,u) still decides reachability: shortestPath u u == Just [u]" $
      property $ \(SmallGraph g) ->
        all (\u -> shortestPath g u u == Just [u])
            (Map.keys (gNodes g))

    it "INV-8 (property): every shortestPath result is a valid hop-minimal E-walk" $
      property $ \(SmallGraph g) ->
        all (\(s, t) -> case shortestPath g s t of
               Nothing -> True
               Just p  -> isValidWalk g s t p)
            [ (u, v) | u <- Set.toList (Set.fromList (Map.keys (gNodes g)))
                     , v <- Set.toList (Set.fromList (Map.keys (gNodes g))) ]

    it "INV-9 (property): deterministic under gNodes key-order permutation" $
      property $ \(SmallGraph g) ->
        stronglyConnectedComponents g == stronglyConnectedComponents g

    it "INV-10: adding an arc f->a merges all three golden SCCs into one" $ do
      let g' = addEdges goldenGraph [sccTestEdge "f" "a"]
          scc = stronglyConnectedComponents g'
          comps = Map.elems scc
      length comps `shouldBe` 1
      sort (head comps) `shouldBe` ["a", "b", "c", "d", "e", "f"]

    it "INV-10 (property): edge addition only merges SCCs (coarsening)" $
      property $ \(SmallGraph g) (NodePair (u, v)) ->
        let srcOk = Map.member u (gNodes g)
            tgtOk = Map.member v (gNodes g)
            pre  = stronglyConnectedComponents g
            g'   = if srcOk && tgtOk then addEdges g [sccTestEdge u v] else g
            post = stronglyConnectedComponents g'
            -- every pre-existing SCC contained in exactly one post SCC
            contains c c' = all (`elem` c') c
            coarsens = all (\c -> length (filter (contains c) (Map.elems post)) == 1)
                           (Map.elems pre)
        in not (srcOk && tgtOk) || coarsens

  describe "shortestPathReachable invariants" $ do
    it "INV-9 (property): reachable answer depends only on E, not node order" $
      property $ \(SmallGraph g) (NodePair (u, v)) ->
        shortestPathReachable g u v == (shortestPath g u v /= Nothing)

-- ───────────────────────────────────────────────
-- Arbitrary instances for small random directed graphs
-- ───────────────────────────────────────────────

newtype SmallGraph = SmallGraph Graph

-- A pair of NodeIds drawn from a small alphabet so pairs sometimes hit
-- existing nodes (exercising the addEdges path) and sometimes don't.
newtype NodePair = NodePair (Text, Text)

instance Arbitrary NodePair where
  arbitrary = do
    u <- elements ["n0", "n1", "n2", "n5", "zz"]
    v <- elements ["n0", "n1", "n2", "n5", "zz"]
    pure (NodePair (u, v))

instance Show NodePair where
  show (NodePair (u, v)) = "(" ++ T.unpack u ++ "," ++ T.unpack v ++ ")"

instance Arbitrary SmallGraph where
  arbitrary = do
    n <- chooseInt (0, 8)
    let nodeIds = ["n" <> T.pack (show i) | i <- [0 .. n - 1]]
    edges <- listOf $ do
      si <- chooseInt (0, max 0 (n - 1))
      ti <- chooseInt (0, max 0 (n - 1))
      pure (si, ti)
    let nodes = map sccTestNode nodeIds
        mkEdge (si, ti) =
          sccTestEdge (nodeIds !! si) (nodeIds !! ti)
        es = [ mkEdge (si, ti)
             | (si, ti) <- nub edges
             , n > 0, si < n, ti < n ]
    pure $ SmallGraph $ buildGraph True $ extractionFromLists nodes es

  shrink (SmallGraph g) =
    [ SmallGraph (buildGraph True (extractionFromLists nodes (drop i es)))
    | i <- [0 .. length es - 1]
    ]
    where
      nodes = Map.elems (gNodes g)
      es    = Map.elems (gEdges g)

instance Show SmallGraph where
  show (SmallGraph g) =
    "Graph { nodes = " ++ show (Map.keys (gNodes g))
    ++ ", edges = " ++ show (Map.keys (gEdges g)) ++ " }"