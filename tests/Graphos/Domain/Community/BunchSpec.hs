{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Bunch file-level MDG fold and hill-climbing search (openspec
-- @bunch-search-based-modularization@, spec @bunch-clustering@).
module Graphos.Domain.Community.BunchSpec (spec) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (ShortText, fromText)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Community.MQ (Weighting(..))
import Graphos.Domain.Community.Bunch
import Graphos.Domain.Community.Landscape (RandomGraphSpec(..), generateRandomMDG)
import Graphos.Domain.Random (mkGen)

spec :: Spec
spec = do
  foldSpec
  searchSpec
  realProjectsSpec

-- ───────────────────────────────────────────────
-- Search
-- ───────────────────────────────────────────────

searchSpec :: Spec
searchSpec = describe "bunchSearch (hill climbing over partitions)" $ do
  let seeds = [0 .. 31] :: [Int]
      cfg = defaultBunchConfig
      -- two 5-cliques joined by one edge: MQ = 2 · (2·10 / (2·10 + 1))
      twoCliquesMQ = 2 * (20 / 21) :: Double

  describe "two cliques" $ do
    it "converges to the two cliques for every seed in 0..31" $
      mapM_ (\s -> do
        let r = bunchSearch cfg (fromIntegral s) twoCliques
        abs (brFinalMQ r - twoCliquesMQ) `shouldSatisfy` (< 1e-9)
        brPartition r `shouldBe` [[0 .. 4], [5 .. 9]]
        brFinalClusters r `shouldBe` 2) seeds

    it "MQ never decreases across iterations without annealing" $
      mapM_ (\s -> do
        let t = brMQTrace (bunchSearch cfg (fromIntegral s) twoCliques)
        and (zipWith (<=) t (drop 1 t)) `shouldBe` True) seeds

    it "the random start is the first recorded point and is worse or equal" $
      mapM_ (\s -> do
        let r = bunchSearch cfg (fromIntegral s) twoCliques
        brStartMQ r `shouldSatisfy` (<= brFinalMQ r + 1e-12)
        brStartClusters r `shouldSatisfy` (\k -> k >= 1 && k <= 10)) seeds

  describe "threshold η" $ do
    it "η ∈ {0, 50, 100} reach the same MQ from the same seed" $
      mapM_ (\s -> do
        let mqs = [ brFinalMQ (bunchSearch cfg { bcThreshold = eta } (fromIntegral s) twoCliques)
                  | eta <- [0, 50, 100] ]
        mapM_ (\mq -> abs (mq - twoCliquesMQ) `shouldSatisfy` (< 1e-9)) mqs) [0 .. 7 :: Int]

    it "counts MQ evaluations and iterations" $ do
      let r = bunchSearch cfg 3 twoCliques
      brEvaluations r `shouldSatisfy` (> 0)
      brIterations r `shouldSatisfy` (> 0)
      brIterations r `shouldSatisfy` (< bcMaxIterations cfg)

  describe "building blocks" $ do
    it "open a cluster that no single move can: two disjoint edges from one cluster" $ do
      let m = mdgFromPairs (files 4) [((0, 1), 1), ((2, 3), 1)]
          r = bunchSearchFrom cfg 0 (VU.fromList [0, 0, 0, 0]) m
      brFinalMQ r `shouldBe` 2
      brPartition r `shouldBe` [[0, 1], [2, 3]]
      brStartMQ r `shouldBe` 1

  describe "simulated annealing" $ do
    it "is off by default: no non-improving candidate is accepted" $
      mapM_ (\s -> brAnnealingAccepts (bunchSearch cfg (fromIntegral s) twoCliques) `shouldBe` 0) seeds

    it "with T0 = 100, α = 0.99 the search terminates and reports its acceptances" $
      mapM_ (\s -> do
        let r = bunchSearch cfg { bcAnnealing = Just (100, 0.99) } (fromIntegral s) twoCliques
        brIterations r `shouldSatisfy` (<= bcMaxIterations cfg)
        brAnnealingAccepts r `shouldSatisfy` (>= 0)
        brFinalMQ r `shouldSatisfy` (\mq -> mq >= 0 && mq <= 10)) [0 .. 7 :: Int]

  describe "population and determinism" $ do
    it "bunchCluster is reproducible and its best run has the highest MQ, lowest seed on ties" $ do
      let res = bunchCluster cfg { bcPopulation = 6, bcSeed = 10 } twoCliques
      res `shouldBe` bunchCluster cfg { bcPopulation = 6, bcSeed = 10 } twoCliques
      map brSeed (bresRuns res) `shouldBe` [10 .. 15]
      let best = bresBest res
          top = [ brSeed r | r <- bresRuns res, brFinalMQ r >= brFinalMQ best - 1e-12 ]
      brSeed best `shouldBe` minimum top

    it "the same graph in another node order gives the same partition" $ do
      let nodes = [ nodeIn "z.ts" "z", nodeIn "m.ts" "m", nodeIn "a.ts" "a", nodeIn "b.ts" "b" ]
          edges = [ edge Calls "z" "m", edge Calls "a" "b", edge Calls "m" "a" ]
          m1 = foldToFileMDG UnitWeights (buildGraph False (extractionFromLists nodes edges))
          m2 = foldToFileMDG UnitWeights (buildGraph False (extractionFromLists (reverse nodes) (reverse edges)))
      bunchCluster cfg m1 `shouldBe` bunchCluster cfg m2

  describe "incremental MQ (QuickCheck)" $ do
    it "the running MQ after every move equals the full recompute within 1e-9" $
      property $ forAll genMDGAndMoves $ \(m, assign0raw, moves) ->
        -- The search state compresses cluster ids to [0, k); work in that
        -- label space so a move's target id means the same cluster on both sides.
        let assign0 = compactIds assign0raw
            incremental = replayMoves m assign0 moves
            states = drop 1 (scanl (\a (i, b) -> a VU.// [(i, b)]) assign0 moves)
            full = map (mqOfAssignment m) states
        in and (zipWith (\x y -> abs (x - y) < 1e-9) incremental full)

    it "randomPartition draws n ids in [0, n) and is deterministic" $
      property $ \(NonNegative n0) (seed :: Int) ->
        let n = n0 `mod` 30
            (a, _) = randomPartition n (mkGen (fromIntegral seed))
        in VU.length a == n && VU.all (\c -> c >= 0 && c < max 1 n) a
           && fst (randomPartition n (mkGen (fromIntegral seed))) == a

  it "canonicalPartition orders clusters by smallest member" $
    canonicalPartition (VU.fromList [1, 0, 1, 2]) `shouldBe` [[0, 2], [1], [3]]

  describe "scale check (task 3.6)" $ do
    it "terminates under the iteration cap on a 200-file random MDG" $ do
      let m = generateRandomMDG (RndSpec 200 0.05) 0
          r = bunchSearch cfg 0 m
      brIterations r `shouldSatisfy` (< bcMaxIterations cfg)
      brFinalMQ r `shouldSatisfy` (> 0)
      brFinalClusters r `shouldSatisfy` (\k -> k >= 1 && k <= 200)

-- | Files f0..f4 and f5..f9 as two 5-cliques, bridged by the edge 4–5.
twoCliques :: FileMDG
twoCliques = mdgFromPairs (files 10)
  ([ ((i, j), 1) | i <- [0 .. 4], j <- [i + 1 .. 4] ]
   ++ [ ((i, j), 1) | i <- [5 .. 9], j <- [i + 1 .. 9] ]
   ++ [ ((4, 5), 1) ])

files :: Int -> [ShortText]
files n = [ fromText (T.pack ("f" ++ show i)) | i <- [0 .. n - 1] ]

genMDGAndMoves :: Gen (FileMDG, VU.Vector Int, [(Int, Int)])
genMDGAndMoves = do
  n <- choose (1, 10 :: Int)
  e <- choose (0, 20 :: Int)
  pairs <- vectorOf e ((,) <$> choose (0, n - 1) <*> choose (0, n - 1))
  ws <- vectorOf e (elements [1, 2, 3 :: Double])
  let m = mdgFromPairs (files n) (zip pairs ws)
  k <- choose (1, n)
  assign0 <- VU.fromList <$> vectorOf n (choose (0, k - 1))
  nMoves <- choose (0, 12 :: Int)
  moves <- vectorOf nMoves ((,) <$> choose (0, n - 1) <*> choose (0, n - 1))
  pure (m, assign0, moves)

-- | Relabel cluster ids to 0..k-1 in ascending order of the original ids.
compactIds :: VU.Vector Int -> VU.Vector Int
compactIds v =
  let tbl = Map.fromList (zip (sort (nub (VU.toList v))) [0 ..])
  in VU.map (\c -> Map.findWithDefault 0 c tbl) v

-- ───────────────────────────────────────────────
-- Fold
-- ───────────────────────────────────────────────

foldSpec :: Spec
foldSpec = describe "foldToFileMDG" $ do
  describe "symbol edges fold to one file pair" $ do
    -- functions in a.ts call functions in b.ts three times, b.ts imports a.ts
    let nodes = [ nodeIn "a.ts" "a.f", nodeIn "a.ts" "a.h", nodeIn "b.ts" "b.g", nodeIn "b.ts" "b.mod" ]
        edges = [ edge Calls "a.f" "b.g", edge Calls "a.h" "b.g", edge Calls "a.f" "b.g2"
                , edge Imports "b.mod" "a.f" ]
        g = buildGraph False (extractionFromLists (nodes ++ [nodeIn "b.ts" "b.g2"]) edges)

    it "unit weights: one edge {a.ts, b.ts} of weight 1" $ do
      let m = foldToFileMDG UnitWeights g
      V.toList (mdgFiles m) `shouldBe` [fromText "a.ts", fromText "b.ts"]
      mdgEdgeCount m `shouldBe` 1
      VU.toList (mdgEdgeU m) `shouldBe` [0]
      VU.toList (mdgEdgeV m) `shouldBe` [1]
      VU.toList (mdgEdgeW m) `shouldBe` [1]

    it "edge weights: the pair weighs the sum of its four edges" $ do
      let m = foldToFileMDG EdgeWeights g
      VU.toList (mdgEdgeW m) `shouldBe` [4]
      mdgTotalWeight m `shouldBe` 4

    it "adjacency is symmetric and parallel to its weights" $ do
      let m = foldToFileMDG EdgeWeights g
      VU.toList (mdgAdj m V.! 0) `shouldBe` [1]
      VU.toList (mdgAdj m V.! 1) `shouldBe` [0]
      VU.toList (mdgAdjW m V.! 0) `shouldBe` [4]

  it "contains-only graph has the files as nodes and no edges" $ do
    let nodes = [ nodeIn "a.ts" "a", nodeIn "a.ts" "a.f", nodeIn "b.ts" "b" ]
        g = buildGraph False (extractionFromLists nodes [edge Contains "a" "a.f"])
        m = foldToFileMDG UnitWeights g
    mdgFileCount m `shouldBe` 2
    mdgEdgeCount m `shouldBe` 0
    V.toList (mdgAdj m) `shouldBe` [VU.empty, VU.empty]

  it "edges inside one file are dropped (no self-pair)" $ do
    let g = buildGraph False (extractionFromLists [nodeIn "a.ts" "a.f", nodeIn "a.ts" "a.g"] [edge Calls "a.f" "a.g"])
    mdgEdgeCount (foldToFileMDG UnitWeights g) `shouldBe` 0

  it "node order does not matter: files are indexed by ascending path" $ do
    let nodes = [ nodeIn "z.ts" "z", nodeIn "m.ts" "m", nodeIn "a.ts" "a" ]
        edges = [ edge Calls "z" "a", edge Calls "m" "a" ]
        g1 = buildGraph False (extractionFromLists nodes edges)
        g2 = buildGraph False (extractionFromLists (reverse nodes) (reverse edges))
        m1 = foldToFileMDG UnitWeights g1
    m1 `shouldBe` foldToFileMDG UnitWeights g2
    V.toList (mdgFiles m1) `shouldBe` map fromText ["a.ts", "m.ts", "z.ts"]
    -- pairs ascending: (a,m) then (a,z)
    VU.toList (mdgEdgeU m1) `shouldBe` [0, 0]
    VU.toList (mdgEdgeV m1) `shouldBe` [1, 2]
    VU.toList (mdgAdj m1 V.! 0) `shouldBe` [1, 2]

-- ───────────────────────────────────────────────
-- Fixtures
-- ───────────────────────────────────────────────

nodeIn :: Text -> Text -> Node
nodeIn file nid = Node
  { nodeId = nid
  , nodeLabel = fromText nid
  , nodeFileType = CodeFile
  , nodeSourceFile = fromText file
  , nodeSource = Nothing
  , nodeLineStart = Just 1
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodePresentBits = 0
  }

edge :: Relation -> Text -> Text -> Edge
edge rel u v = Edge
  { edgeId = EdgeId (u <> "->" <> v)
  , edgeSource = u
  , edgeTarget = v
  , edgeRelation = rel
  , edgeWeight = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra = Nothing
  }

-- ───────────────────────────────────────────────
-- Real repositories (openspec bunch-real-projects D2, D3)
-- ───────────────────────────────────────────────

realProjectsSpec :: Spec
realProjectsSpec = do
  describe "foldToFileMDG relation allowlist (D2)" $ do
    it "ignores inferred, depends_on and documents edges" $ do
      let nodes = [ nodeIn "a.ts" "a", nodeIn "b.ts" "b", nodeIn "c.ts" "c", nodeIn "README.md" "readme" ]
          edges = [ edge DependsOn "a" "b", edge Inferred "b" "a", edge Documents "readme" "a", edge Imports "a" "c" ]
          m = foldToFileMDG UnitWeights (buildGraph False (extractionFromLists nodes edges))
          pairs = [ (mdgFiles m V.! (mdgEdgeU m VU.! e), mdgFiles m V.! (mdgEdgeV m VU.! e)) | e <- [0 .. mdgEdgeCount m - 1] ]
      pairs `shouldBe` [("a.ts", "c.ts")]

    it "folds only the configured relations and never a forbidden one" $ do
      let nodes = [ nodeIn "a.ts" "a", nodeIn "b.ts" "b", nodeIn "c.ts" "c" ]
          g = buildGraph False (extractionFromLists nodes [ edge Imports "a" "b", edge Calls "b" "c", edge DependsOn "a" "c" ])
      mdgEdgeCount (foldToFileMDGWith [Calls] UnitWeights g) `shouldBe` 1
      mdgEdgeCount (foldToFileMDGWith [DependsOn] UnitWeights g) `shouldBe` 0
      mdgEdgeCount (foldToFileMDGWith [Imports, Calls, DependsOn] UnitWeights g) `shouldBe` 2

  describe "splitBySubtree (D3)" $ do
    it "splits by directory and keeps root files together" $ do
      let paths = [ "src/a/" ++ show i ++ ".ts" | i <- [1 .. 5 :: Int] ]
               ++ [ "src/b/" ++ show i ++ ".ts" | i <- [1 .. 5 :: Int] ] ++ ["x.ts", "y.ts"]
      splitBySubtree 6 paths `shouldBe` [[0 .. 4], [5 .. 9], [10, 11]]

    it "cuts a single directory into consecutive chunks" $
      splitBySubtree 4 [ "lib/" ++ [c] ++ ".ts" | c <- ['a' .. 'j'] ] `shouldBe` [[0 .. 3], [4 .. 7], [8, 9]]

    it "chunks partition the input and respect the maximum" $
      property $ forAll genPaths $ \(maxN, paths) ->
        let chunks = splitBySubtree maxN paths
        in sort (concat chunks) == [0 .. length paths - 1] && all ((<= max 1 maxN) . length) chunks
           && not (any null chunks)

    it "is a deterministic function of the path list" $
      property $ forAll genPaths $ \(maxN, paths) -> splitBySubtree maxN paths == splitBySubtree maxN paths

  describe "refinePartition (D3)" $ do
    it "never lowers MQ and returns a partition of the same files" $
      property $ forAll genMDGAndMoves $ \(m, assign0, _) ->
        let (assign1, _, passes) = refinePartition m assign0
        in VU.length assign1 == VU.length assign0
           && mqOfAssignment m assign1 >= mqOfAssignment m assign0 - 1e-9
           && passes <= maxRefinePasses

    it "is idempotent on its own output" $
      property $ forAll genMDGAndMoves $ \(m, assign0, _) ->
        let (assign1, _, _) = refinePartition m assign0
            (_, moves2, _) = refinePartition m assign1
        in moves2 == 0

    it "absorbs a file left alone next to its clique" $ do
      let start = VU.fromList (10 : replicate 4 0 ++ replicate 5 1)   -- file 0 isolated from clique {1..4}
          (assign1, moves, _) = refinePartition twoCliques start
      canonicalPartition assign1 `shouldBe` [[0 .. 4], [5 .. 9]]
      moves `shouldSatisfy` (>= 1)

  describe "bunchClusterPartitioned (D3)" $ do
    let cfg = defaultBunchConfig { bcPopulation = 2 }
    it "runs one search when the MDG fits one run, and refinement changes nothing" $ do
      let r = bunchClusterPartitioned 500 cfg twoCliques
      prRuns r `shouldBe` [10]
      prPartition r `shouldBe` [[0 .. 4], [5 .. 9]]
      prMoves r `shouldBe` 0

    it "splits by sub-tree above the per-run size and recovers both cliques" $ do
      let m = mdgFromPairs (map fromText ([ T.pack ("a/" ++ show i ++ ".ts") | i <- [0 .. 4 :: Int] ]
                                       ++ [ T.pack ("b/" ++ show i ++ ".ts") | i <- [0 .. 4 :: Int] ]))
                           (mdgPairs twoCliques)
          r = bunchClusterPartitioned 5 cfg m
      prRuns r `shouldBe` [5, 5]
      prPartition r `shouldBe` [[0 .. 4], [5 .. 9]]

    it "absorbs a cross-sub-tree singleton (all its imports in another sub-tree)" $ do
      -- a/0..3 form a clique; c/x.ts imports all of them and sits alone in its sub-tree.
      let fs = map fromText ([ T.pack ("a/" ++ show i ++ ".ts") | i <- [0 .. 3 :: Int] ] ++ ["c/x.ts"])
          m = mdgFromPairs fs ([ ((i, j), 1) | i <- [0 .. 3], j <- [i + 1 .. 3] ] ++ [ ((i, 4), 1) | i <- [0 .. 3] ])
          r = bunchClusterPartitioned 4 cfg m
      prRuns r `shouldBe` [4, 1]
      prPartition r `shouldBe` [[0 .. 4]]
      prMoves r `shouldSatisfy` (>= 1)
      prMQ r `shouldSatisfy` (> mqOfAssignment m (VU.fromList [0, 0, 0, 0, 1]))

    it "is deterministic for a fixed seed" $ do
      let m = mdgFromPairs (map fromText ([ T.pack ("a/" ++ show i ++ ".ts") | i <- [0 .. 4 :: Int] ]
                                       ++ [ T.pack ("b/" ++ show i ++ ".ts") | i <- [0 .. 4 :: Int] ]))
                           (mdgPairs twoCliques)
      bunchClusterPartitioned 3 cfg m `shouldBe` bunchClusterPartitioned 3 cfg m

-- | The weighted pairs of an MDG, to rebuild it under other file names.
mdgPairs :: FileMDG -> [((Int, Int), Double)]
mdgPairs m = [ ((mdgEdgeU m VU.! e, mdgEdgeV m VU.! e), mdgEdgeW m VU.! e) | e <- [0 .. mdgEdgeCount m - 1] ]

-- | A per-run maximum and an ascending, duplicate-free list of nested paths.
genPaths :: Gen (Int, [FilePath])
genPaths = do
  maxN <- choose (1, 12 :: Int)
  k <- choose (0, 60 :: Int)
  ps <- vectorOf k $ do
    depth <- choose (0, 3 :: Int)
    dirs <- vectorOf depth (elements ["src", "lib", "a", "b", "c"])
    f <- elements [ [c] ++ ".ts" | c <- ['a' .. 'h'] ]
    pure (concatMap (++ "/") dirs ++ f)
  pure (maxN, sort (nub ps))
