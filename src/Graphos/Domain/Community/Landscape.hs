{-# LANGUAGE StrictData #-}
-- | The Bunch search landscape (Mitchell & Mancoridis 2008 §4), Domain side
-- (openspec change @bunch-search-based-modularization@, capability
-- @search-landscape-evaluation@).
--
-- The paper evaluates the search by running it many times from independent
-- random starts and reading the result as a landscape rather than as one
-- partition:
--
--   * a __structural record per run__ — start and final MQ, start and final
--     cluster count, intra-edge fraction, MQ evaluations, iterations,
--     annealing acceptances — with runs reported sorted by final MQ
--     ascending (Figs. 9–10);
--   * an __edge similarity distribution__ — for every MDG edge the share of
--     runs in which its endpoints share a cluster, bucketed Zero (@P = 0@),
--     Low (@0 < P ≤ 0.10@), Medium (@0.10 < P < 0.75@) and High (@P ≥ 0.75@),
--     each bucket reported as a percentage of @|E|@ (Table 4). The four
--     shares sum to 100%;
--   * __basins of attraction__ — runs grouped by final MQ in 0.05 bins; the
--     report lists every bin holding at least 5% of the runs with its MQ and
--     cluster-count ranges (the paper's "one or two basins");
--   * a __random-partition baseline__ — @M@ random partitions scored with MQ,
--     reporting min / median / max (Fig. 7), so a run's MQ can be read
--     against what a random partition scores.
--
-- The module also generates the paper's random control MDGs: @rnd:N:D@ picks
-- @⌊D·N(N−1)/2⌋@ distinct pairs uniformly without replacement from the
-- complete graph, @bip:N:D@ the same count of pairs drawn only across two
-- halves of @⌊N/2⌋@ and @⌈N/2⌉@ nodes (saturating to the full bipartite
-- pair set when the requested count exceeds it). Both are seeded and
-- deterministic.
module Graphos.Domain.Community.Landscape
  ( -- * Landscape
    RunRecord(..)
  , LandscapeResult(..)
  , runLandscape
  , runLandscapeOpts
  , LandscapeOptions(..)
  , defaultLandscapeOptions
    -- * Edge similarity distribution
  , SimilarityBucket(..)
  , bucketShare
  , SimilarityDistribution(..)
  , similarityDistribution
  , similarityShare
    -- * Basins of attraction
  , Basin(..)
  , groupBasins
    -- * Random-partition baseline
  , RandomBaseline(..)
  , randomBaseline
    -- * Random control MDGs
  , RandomGraphSpec(..)
  , parseRandomGraphSpec
  , renderRandomGraphSpec
  , randomSpecEdgeCount
  , generateRandomMDG
  ) where

import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (double, decimal)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import Numeric (showFFloat)

import Graphos.Domain.Community.Bunch
  ( BunchConfig(..)
  , BunchRun(..)
  , FileMDG(..)
  , assignmentOf
  , bunchSearch
  , mdgEdgeCount
  , mdgEdgeU
  , mdgEdgeV
  , mdgFileCount
  , mdgFromPairs
  , mqOfAssignment
  , randomPartition
  )
import Graphos.Domain.Random (Gen, mkGen, nextWord64)
import Data.Text.Short (ShortText, fromText)

-- ───────────────────────────────────────────────
-- Structural landscape
-- ───────────────────────────────────────────────

-- | The structural record of one search run (design D6). The partition
-- itself is not kept — the similarity distribution is computed from the
-- partitions while the runs are still in hand.
data RunRecord = RunRecord
  { rrSeed             :: Word64
  , rrStartMQ          :: Double
  , rrStartClusters    :: Int
  , rrFinalMQ          :: Double
  , rrFinalClusters    :: Int
  , rrIntraFraction    :: Double
  , rrEvaluations      :: Int  -- ^ MQ evaluations (candidates examined)
  , rrIterations       :: Int
  , rrAnnealingAccepts :: Int  -- ^ non-improving candidates accepted
  } deriving (Eq, Show)

runRecordOf :: BunchRun -> RunRecord
runRecordOf r = RunRecord
  { rrSeed             = brSeed r
  , rrStartMQ          = brStartMQ r
  , rrStartClusters    = brStartClusters r
  , rrFinalMQ          = brFinalMQ r
  , rrFinalClusters    = brFinalClusters r
  , rrIntraFraction    = brIntraFraction r
  , rrEvaluations      = brEvaluations r
  , rrIterations       = brIterations r
  , rrAnnealingAccepts = brAnnealingAccepts r
  }

-- | Knobs of a landscape evaluation beyond the search configuration.
data LandscapeOptions = LandscapeOptions
  { loRuns            :: Int    -- ^ N independent runs, seeds @s..s+N-1@ (default 100)
  , loRandomPartitions :: Int   -- ^ M baseline partitions (default 1,000)
  , loBasinMinShare   :: Double -- ^ minimum share of runs for a basin to be listed (default 0.05)
  } deriving (Eq, Show)

defaultLandscapeOptions :: LandscapeOptions
defaultLandscapeOptions = LandscapeOptions
  { loRuns             = 100
  , loRandomPartitions = 1000
  , loBasinMinShare    = 0.05
  }

-- | The full landscape of @N@ runs: records sorted by final MQ, the edge
-- similarity distribution, the listed basins and the random-partition
-- baseline.
data LandscapeResult = LandscapeResult
  { lrRuns       :: [RunRecord]              -- ^ sorted by final MQ ascending
  , lrSimilarity :: SimilarityDistribution
  , lrBasins     :: [Basin]                  -- ^ groups holding ≥ loBasinMinShare of runs
  , lrBaseline   :: RandomBaseline
  } deriving (Eq, Show)

-- | Run the Bunch search @n@ times with seeds @seed..seed+n-1@ (the paper's
-- 100 independent restarts) and read the landscape off the runs. Run @r@ of
-- the baseline uses draws from a generator split off @seed@, so the baseline
-- is independent of the runs and reproducible.
runLandscape :: FileMDG -> BunchConfig -> Word64 -> Int -> LandscapeResult
runLandscape m cfg seed n =
  runLandscapeOpts m cfg seed defaultLandscapeOptions { loRuns = n }

-- | 'runLandscape' with explicit options.
runLandscapeOpts :: FileMDG -> BunchConfig -> Word64 -> LandscapeOptions -> LandscapeResult
runLandscapeOpts m cfg seed opts =
  let n     = max 0 (loRuns opts)
      runs  = [ bunchSearch cfg (seed + fromIntegral r) m | r <- [0 .. n - 1] ]
      sorted = sortOn rrFinalMQ (map runRecordOf runs)

      sim   = similarityDistribution m (map partitionAssign runs)
      basins = groupBasins (loBasinMinShare opts) sorted

      (_, baseGen) = nextWord64 (mkGen seed)  -- split the stream from the runs' seeds
      baseline = randomBaseline m (max 0 (loRandomPartitions opts)) baseGen
  in LandscapeResult
       { lrRuns       = sorted
       , lrSimilarity = sim
       , lrBasins     = basins
       , lrBaseline   = baseline
       }
  where
    partitionAssign r = assignmentOf (mdgFileCount m) (brPartition r)

-- ───────────────────────────────────────────────
-- Edge similarity distribution (Table 4)
-- ───────────────────────────────────────────────

-- | The four similarity buckets of Table 4.
data SimilarityBucket
  = BucketZero    -- ^ @P = 0@: never intra-cluster
  | BucketLow     -- ^ @0 < P ≤ 0.10@
  | BucketMedium  -- ^ @0.10 < P < 0.75@
  | BucketHigh    -- ^ @P ≥ 0.75@
  deriving (Eq, Ord, Enum, Bounded, Show)

allBuckets :: [SimilarityBucket]
allBuckets = [minBound .. maxBound]

-- | Bucket of a per-edge share @P = C_uv / N@.
bucketShare :: Double -> SimilarityBucket
bucketShare p
  | p <= 0    = BucketZero
  | p <= 0.10 = BucketLow
  | p < 0.75  = BucketMedium
  | otherwise = BucketHigh

-- | The similarity distribution over the MDG's edges: per bucket the number
-- of edges and (derived) the share of @|E|@ in percent. 'simShares' sums to
-- 100 up to floating error and 'simSharesSum' is exactly 100 by construction
-- of the per-bucket complements — see 'similarityDistribution'.
data SimilarityDistribution = SimilarityDistribution
  { simEdges  :: Int                        -- ^ |E| scored edges
  , simRuns   :: Int                        -- ^ N runs
  , simCounts :: Map.Map SimilarityBucket Int   -- ^ edges per bucket
  } deriving (Eq, Show)

-- | Share of |E| in a bucket, in percent.
similarityShare :: SimilarityDistribution -> SimilarityBucket -> Double
similarityShare d b =
  let total = simEdges d
  in if total <= 0 then 0
     else 100 * fromIntegral (Map.findWithDefault 0 b (simCounts d)) / fromIntegral total

-- | Classify every MDG edge by the share of runs in which its endpoints
-- share a cluster. @assigns@ are the per-run assignment vectors (cluster id
-- per file; only equality matters).
similarityDistribution :: FileMDG -> [VU.Vector Int] -> SimilarityDistribution
similarityDistribution m assigns =
  let e = mdgEdgeCount m
      perEdge =
        [ length [ () | a <- assigns
                       , VU.length a > max u v
                       , VU.unsafeIndex a u == VU.unsafeIndex a v ]
        | e' <- [0 .. e - 1]
        , let u = mdgEdgeU m VU.! e'
        , let v = mdgEdgeV m VU.! e'
        ]
      n = length assigns
      counts = Map.fromList
        [ (b, length [ () | c <- perEdge, bucketShare (frac c) == b ])
        | b <- allBuckets ]
      frac c = if n <= 0 then 0 else fromIntegral c / fromIntegral n
  in SimilarityDistribution
       { simEdges  = e
       , simRuns   = n
       , simCounts = counts
       }

-- ───────────────────────────────────────────────
-- Basins of attraction
-- ───────────────────────────────────────────────

-- | One basin: the runs whose final MQ falls in the same 0.05 bin.
data Basin = Basin
  { basinKey          :: Double        -- ^ bin's lower bound, a multiple of 0.05
  , basinRuns         :: Int
  , basinShare        :: Double        -- ^ share of all runs, in percent
  , basinMQRange      :: (Double, Double)
  , basinClusterRange :: (Int, Int)
  } deriving (Eq, Show)

-- | Group runs by final MQ rounded down to a 0.05 bin and keep the bins
-- holding at least @minShare@ of the runs (default 5%), ascending by bin.
-- The bin is the floor at 0.05 — the paper's Fig. 9/10 bins — so a group
-- 1.80–1.84 stays one basin.
groupBasins :: Double -> [RunRecord] -> [Basin]
groupBasins minShare records =
  let total = length records
      binOf r = fromIntegral (floor (rrFinalMQ r * 20) :: Integer) / 20
      grouped = Map.toAscList
        (Map.fromListWith (flip (++)) [ (binOf r, [r]) | r <- records ])
  in [ Basin
        { basinKey          = k
        , basinRuns         = length rs
        , basinShare        = if total <= 0 then 0 else 100 * fromIntegral (length rs) / fromIntegral total
        , basinMQRange      = (rrFinalMQ (firstOf rs), rrFinalMQ (lastOf rs))
        , basinClusterRange = (rrFinalClusters (firstOf rs), rrFinalClusters (lastOf rs))
        }
     | (k, rs) <- grouped
     , not (null rs)
     , fromIntegral (length rs) / max 1 (fromIntegral total) >= minShare
     ]
  where
    -- Total helpers over the by-construction non-empty groups.
    firstOf (x : _) = x
    firstOf []      = error "groupBasins: empty group"
    lastOf (x : xs) = walk x xs
      where walk a []       = a
            walk _ (y : ys) = walk y ys
    lastOf [] = error "groupBasins: empty group"

-- ───────────────────────────────────────────────
-- Random-partition baseline (Fig. 7)
-- ───────────────────────────────────────────────

-- | Min / median / max MQ over @M@ uniformly random partitions.
data RandomBaseline = RandomBaseline
  { rbCount  :: Int
  , rbMin    :: Double
  , rbMedian :: Double
  , rbMax    :: Double
  } deriving (Eq, Show)

-- | Score @m@ random partitions of the MDG (the paper's 1,000) with MQ.
-- The generator is threaded, so the baseline is a deterministic function of
-- @(m, gen)@. The median of an even count averages the two middle values.
randomBaseline :: FileMDG -> Int -> Gen -> RandomBaseline
randomBaseline m mParts gen0
  | mParts <= 0 = RandomBaseline 0 0 0 0
  | otherwise =
      let go 0 _ acc = reverse acc
          go i g acc =
            let (assign, g') = randomPartition (mdgFileCount m) g
            in go (i - 1 :: Int) g' (mqOfAssignment m assign : acc)
          (vals) = go mParts gen0 []
          sorted = sort vals
          medianOf xs =
            let len = length xs
                mid = len `div` 2
            in if odd len then at mid xs
               else (at (mid - 1) xs + at mid xs) / 2
          at i ys = ys !! i
      in case sorted of
           [] -> RandomBaseline 0 0 0 0
           _  -> RandomBaseline
                   { rbCount  = length sorted
                   , rbMin    = firstOf sorted
                   , rbMedian = medianOf sorted
                   , rbMax    = lastOf sorted
                   }
  where
    firstOf (x : _) = x
    firstOf []      = error "randomBaseline: empty"
    lastOf (x : xs) = walk x xs
      where walk a []       = a
            walk _ (y : ys) = walk y ys
    lastOf [] = error "randomBaseline: empty"

-- ───────────────────────────────────────────────
-- Random control MDGs (Table 3)
-- ───────────────────────────────────────────────

-- | The paper's random and bipartite control MDGs: @rnd:N:D@ and @bip:N:D@.
data RandomGraphSpec
  = RndSpec Int Double -- ^ N nodes, density D over the complete graph
  | BipSpec Int Double -- ^ N nodes, density D over the complete graph, edges across two halves only
  deriving (Eq, Show)

-- | Parse @rnd:N:D@ / @bip:N:D@ (family name case-insensitive).
parseRandomGraphSpec :: Text -> Either String RandomGraphSpec
parseRandomGraphSpec t =
  case T.splitOn ":" (T.strip t) of
    [fam, nT, dT] -> do
      n <- readInt nT
      d <- readDouble dT
      case T.toLower fam of
        "rnd" | n >= 0, d >= 0, d <= 1 -> Right (RndSpec n d)
              | otherwise -> Left "rnd:N:D needs N >= 0 and D in [0,1]"
        "bip" | n >= 0, d >= 0, d <= 1 -> Right (BipSpec n d)
              | otherwise -> Left "bip:N:D needs N >= 0 and D in [0,1]"
        other -> Left ("Unknown random-graph family: " ++ T.unpack other
                        ++ " (expected rnd or bip)")
    _ -> Left ("Expected rnd:N:D or bip:N:D, got: " ++ T.unpack t)
  where
    readInt x = case decimal (T.strip x) of
      Right (v, rest) | T.null (T.strip rest) -> Right v
      _ -> Left ("Not an integer: " ++ T.unpack x)
    readDouble x = case double (T.strip x) of
      Right (v, rest) | T.null (T.strip rest) -> Right v
      _ -> Left ("Not a number: " ++ T.unpack x)

-- | Render a spec back to its @fam:N:D@ form (plain decimal, no exponent
-- notation).
renderRandomGraphSpec :: RandomGraphSpec -> Text
renderRandomGraphSpec spec = T.pack
  ( prefix ++ show n ++ ":" ++ density )
  where
    (prefix, n, d) = case spec of
      RndSpec k x -> ("rnd:", k, x)
      BipSpec k x -> ("bip:", k, x)
    density = stripZeros (showFFloat (Just 6) d "")
    stripZeros = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse

-- | The requested edge count @⌊D·N(N−1)/2⌋@ (the paper's Table 3 counts:
-- @rnd:100:0.05@ → 247, @rnd:100:0.5@ → 2,475, @bip:100:0.75@ → 3,712).
randomSpecEdgeCount :: RandomGraphSpec -> Int
randomSpecEdgeCount spec = case spec of
  RndSpec n d -> count n d
  BipSpec n d -> count n d
  where
    count n d = floor (d * fromIntegral (n * (n - 1)) / 2 :: Double)

-- | Generate a control MDG: node names @f0..f(N-1)@, unit-weight edges drawn
-- uniformly without replacement from the family's candidate pairs. Each
-- candidate pair receives one independent 64-bit key from the seeded
-- SplitMix64 stream and the requested count takes the smallest keys — a
-- uniform sample without replacement that is a deterministic function of
-- the spec and the seed.
--
-- @bip@ draws only from the cross pairs of the two halves
-- @⌊N/2⌋@ and @⌈N/2⌉@; when the requested count exceeds the cross pairs
-- available the MDG saturates to the complete bipartite graph (a bipartite
-- graph on 100 nodes has at most 2,500 cross pairs, so @bip:100:0.75@'s
-- nominal 3,712 cannot exist — the generator yields all 2,500).
generateRandomMDG :: RandomGraphSpec -> Word64 -> FileMDG
generateRandomMDG spec seed =
  let n = case spec of RndSpec k _ -> k; BipSpec k _ -> k
      files = [ fromText (T.pack ("f" ++ show i)) | i <- [0 .. n - 1] ] :: [ShortText]
      half  = n `div` 2
      candidates = case spec of
        RndSpec _ _ -> [ (i, j) | i <- [0 .. n - 1], j <- [i + 1 .. n - 1] ]
        BipSpec _ _ -> [ (i, j) | i <- [0 .. half - 1], j <- [half .. n - 1] ]
      requested = randomSpecEdgeCount spec
      m = min requested (length candidates)
      keys = keysOf (length candidates)
      keyed = zip keys candidates
      -- Sort by key (ties by pair order — keys are effectively unique), take m.
      chosen = take m (sortOn (\(k, p) -> (k, p)) keyed)
  in mdgFromPairs files [ (p, 1) | (_, p) <- chosen ]
  where
    keysOf cnt =
      let go 0 _ acc = reverse acc
          go i g acc = let (w, g') = nextWord64 g in go (i - 1 :: Int) g' (w : acc)
      in go cnt (mkGen seed) []
