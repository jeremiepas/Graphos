{-# LANGUAGE OverloadedStrings #-}
-- | clustering-evaluation-and-selection, group 2: the file dependency matrix
-- (the Bunch MDG with edge weights), distances, single linkage (Prim) and
-- average / complete linkage (cached nearest neighbours), against naive
-- references.
module Graphos.Domain.Community.AgglomerativeSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Data.List (sort, minimumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Community.Bunch (FileMDG(..), mdgFromPairs, mdgFileCount, defaultMDGRelations)
import Graphos.Domain.Community.Agglomerative

-- ───────────────────────────────────────────────
-- Fixtures
-- ───────────────────────────────────────────────

node :: Text -> Text -> Node
node file nid = Node
  { nodeId = nid, nodeLabel = fromText nid, nodeFileType = CodeFile
  , nodeSourceFile = fromText file, nodeSource = Nothing
  , nodeLineStart = Just 1, nodeLineEnd = Nothing, nodeSignature = Nothing
  , nodeCommunityId = Nothing, nodeKind = Nothing, nodeDegree = Nothing
  , nodeIsBridge = Nothing, nodeExtra = Nothing, nodePresentBits = 0 }

edge :: Relation -> Text -> Text -> Edge
edge r u v = Edge (EdgeId (u <> "->" <> v)) u v r 1.0 (Confidence 1.0) Nothing

-- | An MDG on files 0..n-1 from weighted pairs.
mdg :: Int -> [((Int, Int), Double)] -> FileMDG
mdg n = mdgFromPairs [ fromText (fileName i) | i <- [0 .. n - 1] ]
  where fileName i = "f" <> showT i

showT :: Int -> Text
showT = T.pack . show

row :: FileMDG -> Int -> (VU.Vector Int, VU.Vector Double)
row m i = (mdgAdj m V.! i, mdgAdjW m V.! i)

-- | Random MDG with 1..30 files and small integer weights.
genMDG :: Gen FileMDG
genMDG = do
  n <- choose (1, 30)
  let pairs = [ (i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]
  chosen <- sublistOf pairs
  ws <- vectorOf (length chosen) (choose (1, 3 :: Int))
  pure (mdg n [ (p, fromIntegral w) | (p, w) <- zip chosen ws ])

-- ───────────────────────────────────────────────
-- Naive references (O(F³)): merge the globally closest pair by
-- (distance, cluster id, cluster id) until K clusters remain
-- ───────────────────────────────────────────────

-- | Linkage-free naive merge driven by a cluster-distance function.
naiveMerge :: Int -> ([Int] -> [Int] -> [Int] -> Double -> Double -> Double)
           -> (Int -> Int -> Double) -> Int -> [[Int]]
naiveMerge n _ _ _ | n == 0 = []
naiveMerge n upd d0 k = go (Map.fromList [ (i, [i]) | i <- [0 .. n - 1] ]) dist0
  where
    dist0 = Map.fromList [ ((i, j), d0 i j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]
    go cls dm
      | Map.size cls <= max 1 k = sort (map sort (Map.elems cls))
      | otherwise =
          let ((a, b), _) = minimumBy (comparing (\((x, y), v) -> (v, x, y))) (Map.toList dm)
              ca = cls Map.! a; cb = cls Map.! b
              others = [ c | c <- Map.keys cls, c /= a, c /= b ]
              look x y = dm Map.! (min x y, max x y)
              dm' = Map.fromList
                ([ ((min a c, max a c), upd ca cb (cls Map.! c) (look a c) (look b c)) | c <- others ]
                 ++ [ (p, v) | (p@(x, y), v) <- Map.toList dm
                             , x /= a, x /= b, y /= a, y /= b ])
              cls' = Map.insert a (ca ++ cb) (Map.delete b cls)
          in go cls' dm'

lanceWilliams :: Linkage -> [Int] -> [Int] -> [Int] -> Double -> Double -> Double
lanceWilliams link ca cb _ dac dbc = case link of
  Single   -> min dac dbc
  Complete -> max dac dbc
  Average  -> (fromIntegral (length ca) * dac + fromIntegral (length cb) * dbc)
                / fromIntegral (length ca + length cb)

spec :: Spec
spec = do
  describe "2.1 file dependency matrix = the Bunch MDG with edge weights" $ do
    it "symbol edges fold to files: W[a][b] = W[b][a] = 2" $ do
      let g = buildGraph False $ extractionFromLists
                [node "a.ts" "f", node "b.ts" "g", node "b.ts" "h"]
                [edge Calls "f" "g", edge Calls "f" "h"]
          m = fileMatrix defaultMDGRelations g
      (VU.toList (mdgAdj m V.! 0), VU.toList (mdgAdjW m V.! 0)) `shouldBe` ([1], [2])
      (VU.toList (mdgAdj m V.! 1), VU.toList (mdgAdjW m V.! 1)) `shouldBe` ([0], [2])
    it "contains edges are ignored: W is zero" $ do
      let g = buildGraph False $ extractionFromLists
                [node "a.ts" "a", node "a.ts" "a.f", node "b.ts" "b"] [edge Contains "a" "a.f"]
          m = fileMatrix defaultMDGRelations g
      all VU.null (V.toList (mdgAdj m)) `shouldBe` True
    it "packages are not files: an import of zod adds no row and no pair" $ do
      let zod = (node "external:zod" "zod") { nodeKind = Just (fromText "External"), nodeLineStart = Nothing }
          g = buildGraph False $ extractionFromLists
                [node "a.ts" "a", node "b.ts" "b", zod]
                [edge Imports "a" "zod", edge Imports "b" "zod"]
          m = fileMatrix defaultMDGRelations g
      (mdgFileCount m, all VU.null (V.toList (mdgAdj m))) `shouldBe` (2, True)

  describe "2.2 distances" $ do
    -- rows: 0 = (w01 = 3, w02 = 4), 1 = (w10 = 3), 2 = (w20 = 4); 3 has no edge
    let m = mdg 4 [((0, 1), 3), ((0, 2), 4)]
        dd k i j = rowDistance k (row m i) (row m j)
    -- rows over 4 files: 0 = (0,3,4,0), 1 = (3,0,0,0), 2 = (4,0,0,0), 3 = 0
    it "euclidean on the hand-computed fixture" $ do
      dd Euclidean 0 3 `shouldBe` 5
      dd Euclidean 1 2 `shouldBe` 1
      dd Euclidean 0 1 `shouldBe` sqrt 34
    it "manhattan on the hand-computed fixture" $ do
      dd Manhattan 0 3 `shouldBe` 7
      dd Manhattan 1 2 `shouldBe` 1              -- rows (3,0,0,0) vs (4,0,0,0)
    it "cosine: parallel rows are at 0, a zero row is at 1" $ do
      dd Cosine 1 2 `shouldBe` 0
      dd Cosine 0 3 `shouldBe` 1
      dd Cosine 3 3 `shouldBe` 1
    it "every distance is symmetric and non-negative" $ forAll genMDG $ \g ->
      let n = mdgFileCount g
      in conjoin [ rowDistance k (row g i) (row g j) == rowDistance k (row g j) (row g i)
                     .&&. rowDistance k (row g i) (row g j) >= 0
                 | k <- [minBound .. maxBound], i <- [0 .. n - 1], j <- [0 .. n - 1] ]

  describe "2.3 single linkage" $ do
    let twoGroups = mdg 4 [((0, 1), 5), ((2, 3), 5)]
    it "two obvious groups: every distance gives {{a,b},{c,d}}" $
      [ agglomerate k Single 2 twoGroups | k <- [minBound .. maxBound] ]
        `shouldBe` replicate 3 [[0, 1], [2, 3]]
    it "exact cluster count: F = 20, q = 5 gives 4 clusters, for every linkage" $ do
      let chain = mdg 20 [ ((i, i + 1), 1) | i <- [0 .. 18] ]
      clusterCountFor 20 5 `shouldBe` 4
      [ length (agglomerate Cosine l (clusterCountFor 20 5) chain) | l <- [minBound .. maxBound] ]
        `shouldBe` [4, 4, 4]
    it "a file's feature row carries its largest weight on the diagonal" $ do
      let m = mdg 3 [((0, 1), 5), ((1, 2), 2)]
      (VU.toList (fst (featureRow m 1)), VU.toList (snd (featureRow m 1))) `shouldBe` ([0, 1, 2], [5, 5, 2])
      featureRow (mdg 2 []) 0 `shouldBe` (VU.empty, VU.empty)
    it "equals the naive single-linkage merge" $ forAll genMDG $ \g -> forAll (choose (1, 30)) $ \k ->
      conjoin [ agglomerate dist Single k g
                  === naiveMerge (mdgFileCount g) (lanceWilliams Single)
                        (\i j -> rowDistance dist (featureRow g i) (featureRow g j)) k
              | dist <- [minBound .. maxBound] ]

  describe "2.4 average and complete linkage" $ do
    it "two obvious groups for every distance and linkage" $
      [ agglomerate k l 2 (mdg 4 [((0, 1), 5), ((2, 3), 5)])
      | k <- [minBound .. maxBound], l <- [Average, Complete] ]
        `shouldBe` replicate 6 [[0, 1], [2, 3]]
    it "matches the naive O(F^3) reference on random graphs, F <= 30" $
      forAll genMDG $ \g -> forAll (choose (1, 30)) $ \k ->
        conjoin [ agglomerate dist link k g
                    === naiveMerge (mdgFileCount g) (lanceWilliams link)
                          (\i j -> rowDistance dist (featureRow g i) (featureRow g j)) k
                | dist <- [minBound .. maxBound], link <- [Average, Complete] ]
    it "the Lance-Williams recurrence equals the definition (mean / max of pairwise distances)" $
      forAll genMDG $ \g ->
        let n = mdgFileCount g
            d i j = rowDistance Manhattan (row g i) (row g j)
            half = n `div` 2
            ca = [0 .. half - 1]; cb = [half .. n - 2]; c = [n - 1]
            direct f xs ys = f [ d x y | x <- xs, y <- ys ]
            mean xs = sum xs / fromIntegral (length xs)
        in n >= 3 && not (null ca) && not (null cb) ==>
             let dac = direct mean ca c; dbc = direct mean cb c
                 mac = direct maximum ca c; mbc = direct maximum cb c
             in abs (lanceWilliams Average ca cb c dac dbc - direct mean (ca ++ cb) c) < 1e-9
                  .&&. lanceWilliams Complete ca cb c mac mbc === direct maximum (ca ++ cb) c
    it "is deterministic" $ forAll genMDG $ \g ->
      agglomerate Cosine Average 3 g === agglomerate Cosine Average 3 g
    it "returns exactly min K F clusters" $ forAll genMDG $ \g -> forAll (choose (1, 40)) $ \k ->
      conjoin [ length (agglomerate Euclidean l k g) === min k (mdgFileCount g)
              | l <- [minBound .. maxBound] ]
