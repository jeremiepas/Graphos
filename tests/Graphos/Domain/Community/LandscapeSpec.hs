{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Search landscape (openspec @bunch-search-based-modularization@, spec
-- @search-landscape-evaluation@): records, similarity distribution, basins,
-- random-partition baseline, control MDG generators.
module Graphos.Domain.Community.LandscapeSpec (spec) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Short (ShortText, fromText)
import qualified Data.Vector.Unboxed as VU
import Test.Hspec
import Test.QuickCheck ()

import Graphos.Domain.Community.Landscape
import Graphos.Domain.Community.Bunch
import Graphos.Domain.Random (mkGen)

spec :: Spec
spec = do
  landscapeSpec
  similaritySpec
  basinSpec
  baselineSpec
  generatorSpec

-- ───────────────────────────────────────────────
-- Fixtures
-- ───────────────────────────────────────────────

-- | Files f0..f4 and f5..f9 as two 5-cliques, bridged by the edge 4–5.
twoCliques :: FileMDG
twoCliques = mdgFromPairs (files 10)
  ([ ((i, j), 1) | i <- [0 .. 4], j <- [i + 1 .. 4] ]
   ++ [ ((i, j), 1) | i <- [5 .. 9], j <- [i + 1 .. 9] ]
   ++ [ ((4, 5), 1) ])

files :: Int -> [ShortText]
files n = [ fromText (T.pack ("f" ++ show i)) | i <- [0 .. n - 1] ]

cliquesMQ :: Double
cliquesMQ = 2 * (20 / 21)

-- ───────────────────────────────────────────────
-- Structural landscape (4.1)
-- ───────────────────────────────────────────────

landscapeSpec :: Spec
landscapeSpec = describe "runLandscape" $ do
  it "emits n records with seeds s..s+n-1 sorted by final MQ ascending" $ do
    let res = runLandscape twoCliques defaultBunchConfig 3 20
    length (lrRuns res) `shouldBe` 20
    sort (map rrSeed (lrRuns res)) `shouldBe` [3 .. 22]
    let mqs = map rrFinalMQ (lrRuns res)
    mqs `shouldBe` sort mqs

  it "every run converged to the cliques: all records at the fixture MQ" $ do
    let res = runLandscape twoCliques defaultBunchConfig 0 16
    mapM_ (\r -> abs (rrFinalMQ r - cliquesMQ) `shouldSatisfy` (< 1e-9)) (lrRuns res)
    mapM_ (\r -> rrFinalClusters r `shouldBe` 2) (lrRuns res)

  it "is a deterministic function of (mdg, cfg, seed, n)" $ do
    runLandscape twoCliques defaultBunchConfig 5 8
      `shouldBe` runLandscape twoCliques defaultBunchConfig 5 8

-- ───────────────────────────────────────────────
-- Similarity distribution (4.2)
-- ───────────────────────────────────────────────

similaritySpec :: Spec
similaritySpec = describe "similarityDistribution" $ do
  let distOf assigns = similarityDistribution twoCliques assigns
      oneCluster = VU.replicate 10 0
      cliques    = VU.fromList [0, 0, 0, 0, 0, 1, 1, 1, 1, 1]

  it "shares sum to 100" $ do
    let d = distOf [cliques, oneCluster, cliques]
    sum (map (similarityShare d) [minBound .. maxBound]) `shouldSatisfy` (\s -> abs (s - 100) < 1e-9)

  it "N identical partitions put every edge in Zero or High, Low = Medium = 0" $ do
    let d = distOf (replicate 5 cliques)
    similarityShare d BucketLow `shouldBe` 0
    similarityShare d BucketMedium `shouldBe` 0
    similarityShare d BucketZero + similarityShare d BucketHigh `shouldSatisfy` (\s -> abs (s - 100) < 1e-9)

  it "one all-intra partition repeated gives 100% High" $ do
    let d = similarityDistribution twoCliques (replicate 7 oneCluster)
    similarityShare d BucketHigh `shouldSatisfy` (\s -> abs (s - 100) < 1e-9)

  it "alternating two partitions sends the differing edges to Medium" $ do
    -- a: cliques; b: {0,5}, {1,6}, {2,7}, {3,8}, {4,9}. Of the 21 MDG edges,
    -- the 20 clique edges are intra in a only (b pairs no adjacent files and
    -- the bridge 4–5 is split in both) → P = 1/2 → Medium; the rest → Zero.
    let b = VU.fromList [0, 1, 2, 3, 4, 0, 1, 2, 3, 4]
        d = distOf [cliques, b]
    similarityShare d BucketMedium `shouldSatisfy` (\s -> abs (s - 100 * 20 / 21) < 1e-9)
    similarityShare d BucketZero `shouldSatisfy` (\s -> abs (s - 100 * 1 / 21) < 1e-9)

-- ───────────────────────────────────────────────
-- Basins (4.2)
-- ───────────────────────────────────────────────

basinSpec :: Spec
basinSpec = describe "groupBasins" $ do
  let rec mq k = RunRecord
        { rrSeed = 0, rrStartMQ = 0, rrStartClusters = 1
        , rrFinalMQ = mq, rrFinalClusters = k, rrIntraFraction = 0
        , rrEvaluations = 0, rrIterations = 0, rrAnnealingAccepts = 0 }

  it "bins MQ 1.80–1.84 and 2.30–2.31 into two basins with 60% and 40%" $ do
    let low  = [ rec (1.80 + fromIntegral i * 0.0005) 5 | i <- [0 .. 59 :: Int] ]
        high = [ rec (2.30 + fromIntegral i * 0.00025) 9 | i <- [0 .. 39 :: Int] ]
        basins = groupBasins 0.05 (low ++ high)
    length basins `shouldBe` 2
    map basinRuns basins `shouldBe` [60, 40]
    map basinShare basins `shouldBe` [60, 40]
    case basins of
      [l, h] -> do
        basinMQRange l `shouldSatisfy` (\(lo, hi) -> abs (lo - 1.80) < 1e-9 && abs (hi - 1.8295) < 1e-6)
        basinClusterRange l `shouldBe` (5, 5)
        basinClusterRange h `shouldBe` (9, 9)
      _ -> expectationFailure "expected exactly two basins"

  it "drops bins below the minimum share" $ do
    let basins = groupBasins 0.05 [rec 1.0 1, rec 1.01 1, rec 2.0 2, rec 2.0 2, rec 2.0 2]
        -- 2/5 = 40% at 2.00, 1/5 = 20% at 1.00/1.05 bins → both listed? No:
        -- 1.00 and 1.01 bin to 1.00 (one basin, 40%); 2.00 → 40%… 3/5 = 60%.
    map basinRuns basins `shouldBe` [2, 3]

  it "empty input gives no basins" $
    groupBasins 0.05 [] `shouldBe` []

-- ───────────────────────────────────────────────
-- Random-partition baseline (4.3)
-- ───────────────────────────────────────────────

baselineSpec :: Spec
baselineSpec = describe "randomBaseline" $ do
  it "is below the converged MQ on the two-clique fixture for seeds 0..31" $
    mapM_ (\s -> do
      let res = runLandscapeOpts twoCliques defaultBunchConfig
                  (fromIntegral s) defaultLandscapeOptions
                    { loRuns = 8, loRandomPartitions = 100 }
          minRunMQ = minimum (map rrFinalMQ (lrRuns res))
      rbMax (lrBaseline res) `shouldSatisfy` (< minRunMQ)) [0 .. 31 :: Int]

  it "min ≤ median ≤ max and is deterministic" $ do
    let b1 = randomBaseline twoCliques 50 (mkGen 7)
        b2 = randomBaseline twoCliques 50 (mkGen 7)
    b1 `shouldBe` b2
    rbMin b1 `shouldSatisfy` (<= rbMedian b1)
    rbMedian b1 `shouldSatisfy` (<= rbMax b1)
    rbCount b1 `shouldBe` 50

-- ───────────────────────────────────────────────
-- Control MDG generators (4.3)
-- ───────────────────────────────────────────────

generatorSpec :: Spec
generatorSpec = describe "random control MDGs" $ do
  it "emits exactly floor(D·N(N-1)/2) distinct pairs" $ do
    mdgEdgeCount (generateRandomMDG (RndSpec 100 0.05) 0) `shouldBe` 247
    mdgEdgeCount (generateRandomMDG (RndSpec 100 0.5) 0) `shouldBe` 2475
    mdgEdgeCount (generateRandomMDG (RndSpec 100 0.05) 1) `shouldBe` 247

  it "is seeded: the same spec and seed give the same edge set" $ do
    let a = generateRandomMDG (RndSpec 100 0.05) 1
        b = generateRandomMDG (RndSpec 100 0.05) 1
    (VU.toList (mdgEdgeU a), VU.toList (mdgEdgeV a))
      `shouldBe` (VU.toList (mdgEdgeU b), VU.toList (mdgEdgeV b))

  it "bip draws only cross pairs and saturates when the count exceeds them" $ do
    -- bip:100:0.25 → ⌊0.25·4950⌋ = 1237 cross pairs (the cross space holds 2500)
    let g = generateRandomMDG (BipSpec 100 0.25) 3
    mdgEdgeCount g `shouldBe` 1237
    all (\(u, v) -> (u < 50 && v >= 50) || (v < 50 && u >= 50))
        (zip (VU.toList (mdgEdgeU g)) (VU.toList (mdgEdgeV g))) `shouldBe` True
    -- bip:100:0.75 nominally asks 3,712 edges but only 2,500 cross pairs exist:
    -- the generator saturates to the complete bipartite graph (recorded as a
    -- spec inconsistency in the change's tasks).
    mdgEdgeCount (generateRandomMDG (BipSpec 100 0.75) 0) `shouldBe` 2500

  it "parses rnd:N:D / bip:N:D and rejects malformed input" $ do
    parseRandomGraphSpec "rnd:100:0.05" `shouldBe` Right (RndSpec 100 0.05)
    parseRandomGraphSpec "bip:100:0.75" `shouldBe` Right (BipSpec 100 0.75)
    parseRandomGraphSpec "BIP:50:0.5" `shouldBe` Right (BipSpec 50 0.5)
    parseRandomGraphSpec "rnd:100:1.5" `shouldSatisfy` isLeft
    parseRandomGraphSpec "foo:10:0.5" `shouldSatisfy` isLeft
    parseRandomGraphSpec "rnd:10" `shouldSatisfy` isLeft
    renderRandomGraphSpec (RndSpec 100 0.05) `shouldBe` "rnd:100:0.05"

  it "counts distinct pairs (no duplicates, no self pairs)" $ do
    let g = generateRandomMDG (RndSpec 60 0.3) 42
        pairs = zip (VU.toList (mdgEdgeU g)) (VU.toList (mdgEdgeV g))
    length pairs `shouldBe` length (dedup pairs)
    all (uncurry (<)) pairs `shouldBe` True
  where
    isLeft :: Either a b -> Bool
    isLeft (Left _) = True
    isLeft _        = False
    dedup = Map.keys . Map.fromList . (\ps -> [ (p, ()) | p <- ps ])
