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
,   nodeSource = Nothing
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

  describe "condensation (§2.3, cond)" $ do
    it "cond is the chain {a,b,c}->0 -> {d,e}->1 -> {f}->2 (golden)" $ do
      cond goldenGraph `shouldBe` Map.fromList [(0, [1]), (1, [2]), (2, [])]

    it "cond (golden): no self-arc and the sink maps to []" $ do
      let arcs = Map.toList (cond goldenGraph)
      all (\(c, cs) -> c `notElem` cs) arcs `shouldBe` True
      Map.lookup 2 (cond goldenGraph) `shouldBe` Just []

    it "cond (property): every SCC is a vertex of the condensation" $ do
      property $ \(SmallGraph g) ->
        length (Map.elems (stronglyConnectedComponents g))
          == length (Map.keys (cond g))

    it "cond (property): no component id points to itself" $ do
      property $ \(SmallGraph g) ->
        all (\(c, cs) -> c `notElem` cs) (Map.toList (cond g))

    it "cond (property): the condensation is acyclic (Thm 2.3)" $ do
      property $ \(SmallGraph g) -> condAcyclic (cond g)

  describe "directed acyclicity (§2.5, INV-C5, isAcyclicDirected)" $ do
    it "isAcyclicDirected: golden graph has cycles -> False" $ do
      isAcyclicDirected goldenGraph `shouldBe` False

    it "isAcyclicDirected: a pure path DAG -> True" $ do
      isAcyclicDirected dagPath `shouldBe` True

    it "isAcyclicDirected: a self-loop is a cycle in a one-node SCC -> False" $ do
      isAcyclicDirected selfLoop `shouldBe` False

    it "isAcyclicDirected (property): True iff every SCC is a singleton and no self-loop" $ do
      property $ \(SmallGraph g) ->
        let allSingleton = all ((<= 1) . length) (Map.elems (stronglyConnectedComponents g))
            noSelfLoop   = all (\(u, v) -> u /= v) (Map.keys (gEdges g))
        in isAcyclicDirected g == (allSingleton && noSelfLoop)

  describe "undirected cycle detection (§2.3, Thm 2.3, INV-C2, hasCycleUndirected)" $ do
    it "hasCycleUndirected: directed triangle F1 -> True" $
      hasCycleUndirected triF1 `shouldBe` True

    it "hasCycleUndirected: directed DAG F2 -> True while isAcyclicDirected == True" $ do
      hasCycleUndirected dagF2 `shouldBe` True
      isAcyclicDirected dagF2 `shouldBe` True

    it "hasCycleUndirected: self-loop F3 -> True (INV-C4)" $
      hasCycleUndirected selfLoop `shouldBe` True

    it "hasCycleUndirected: reciprocal pair a->b, b->a -> False (AF-3)" $
      hasCycleUndirected reciprocalF `shouldBe` False

    it "hasCycleUndirected: directed path F7 -> False (a tree)" $
      hasCycleUndirected dagPath `shouldBe` False

    it "hasCycleUndirected (property): INV-C2 soundness — self-loop or q >= 1" $
      property $ \(SmallGraph g) ->
        let arcs    = Map.keys (gEdges g)
            selfLoops = any (\(u, v) -> u == v) arcs
            simpleA   = Set.fromList [ (min u v, max u v) | (u, v) <- arcs, u /= v ]
            n         = Map.size (gNodes g)
            c         = length (connectedComponents g)
            q         = Set.size simpleA - n + c
        in hasCycleUndirected g == (selfLoops || q >= 1)

    it "hasCycleUndirected (property): adding an arc to a cyclic graph keeps it cyclic (monotonic)" $
      property $ \(SmallGraph g) (NodePair (u, v)) ->
        let srcOk = Map.member u (gNodes g)
            tgtOk = Map.member v (gNodes g)
            g' = if srcOk && tgtOk then addEdges g [sccTestEdge u v] else g
        in hasCycleUndirected g ==> hasCycleUndirected g'

  describe "isolated nodes (§2.5, INV-C3, isolatedNodes)" $ do
    it "isolatedNodes: {a,b} connected, {c} isolated" $ do
      let g = buildGraph True $ extractionFromLists
                   (map sccTestNode ["a", "b", "c"])
                   [ sccTestEdge "a" "b" ]
      isolatedNodes g `shouldBe` ["c"]

    it "isolatedNodes (property): each isolated node forms a singleton component" $ do
      property $ \(SmallGraph g) ->
        let comps = Map.elems (connectedComponents g)
        in all (\n -> any (== [n]) comps) (isolatedNodes g)

  describe "connectivity decomposition (§2.6, connectedComponents / weaklyConnectedComponents)" $ do
    it "connectedComponents: golden graph weakly connected -> one class of all six" $ do
      connectedComponents goldenGraph `shouldBe` Map.fromList [(0, ["a", "b", "c", "d", "e", "f"])]

    it "weaklyConnectedComponents equals connectedComponents (undirected support)" $ do
      property $ \(SmallGraph g) -> weaklyConnectedComponents g == connectedComponents g

    it "connectedComponents (property): partitions V (INV-C1)" $ do
      property $ \(SmallGraph g) -> isPartition g (connectedComponents g)

    it "INV-C6 (golden): {a},{b},{c} SCCs inside one WCC {a,b,c} (C_w < C_s)" $ do
      let g = buildGraph True $ extractionFromLists
                 (map sccTestNode ["a", "b", "c"])
                 [ sccTestEdge "a" "b"
                 , sccTestEdge "b" "c" ]
          scc = Map.elems (stronglyConnectedComponents g)
          wcc = Map.elems (connectedComponents g)
      length scc `shouldBe` 3
      length wcc `shouldBe` 1

    it "INV-C6 (property): each SCC lies in exactly one connectedComponent (coarsening)" $ do
      property $ \(SmallGraph g) ->
        let sccs   = Map.elems (stronglyConnectedComponents g)
            comps  = Map.elems (connectedComponents g)
            subset a b = all (`elem` b) a
            inExactlyOne s = length (filter (\c -> subset s c) comps) == 1
        in all inExactlyOne sccs

-- A directed path DAG: a -> b -> c.
dagPath :: Graph
dagPath =
  buildGraph True $ extractionFromLists
    (map sccTestNode ["a", "b", "c"])
    [ sccTestEdge "a" "b"
    , sccTestEdge "b" "c"
    ]

-- A single node with a self-loop: one SCC, but a directed cycle.
selfLoop :: Graph
selfLoop =
  buildGraph True $ extractionFromLists
    (map sccTestNode ["a"])
    [ sccTestEdge "a" "a" ]

-- Fixture F1 — single directed triangle.
triF1 :: Graph
triF1 =
  buildGraph True $ extractionFromLists
    (map sccTestNode ["a", "b", "c"])
    [ sccTestEdge "a" "b"
    , sccTestEdge "b" "c"
    , sccTestEdge "c" "a"
    ]

-- Fixture F2 — directed DAG whose undirected support has a cycle.
dagF2 :: Graph
dagF2 =
  buildGraph True $ extractionFromLists
    (map sccTestNode ["a", "b", "c", "d"])
    [ sccTestEdge "a" "b"
    , sccTestEdge "a" "c"
    , sccTestEdge "b" "d"
    , sccTestEdge "c" "d"
    ]

-- Two nodes with reciprocal arcs: collapses to one undirected edge (AF-3).
reciprocalF :: Graph
reciprocalF =
  buildGraph True $ extractionFromLists
    (map sccTestNode ["a", "b"])
    [ sccTestEdge "a" "b"
    , sccTestEdge "b" "a"
    ]

-- Kahn's algorithm over a successor map: repeatedly drop nodes with zero
-- in-degree over the induced remaining subgraph; all nodes removable => acyclic.
condAcyclic :: Map.Map Int [Int] -> Bool
condAcyclic m
  | Map.null m = True
  | otherwise =
      let indeg r v = length [ () | u <- r, v `elem` Map.findWithDefault [] u m ]
          zeros r   = [ u | u <- r, indeg r u == 0 ]
      in case zeros (Map.keys m) of
           []  -> False
           zs  -> condAcyclic (Map.filterWithKey (\k _ -> k `notElem` zs) m)

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