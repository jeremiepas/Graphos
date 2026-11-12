{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
-- | Search-landscape orchestration (openspec change
-- @bunch-search-based-modularization@, capability
-- @search-landscape-evaluation@, design D6).
--
-- "Graphos.Domain.Community.Landscape" runs the seeded Bunch search N times
-- over a file MDG and reduces the runs to the structural landscape, the edge
-- similarity distribution, the basins and the random-partition baseline.
-- This module adds the two pieces the paper reads next to the landscape:
--
--   * the __Leiden point__ — the build's Leiden partition folded to the file
--     level by majority community and scored in the same coordinates (MQ,
--     cluster count, intra-edge fraction), so the deterministic partition
--     sits next to Bunch's basins; present only when the graph carries
--     communities (a @--no-cluster@ build has none);
--   * the __control MDGs__ @rnd:N:D@ / @bip:N:D@ — when one is requested the
--     landscape runs on the generated MDG instead of a graph file, and the
--     Leiden point is absent (a random control carries no communities).
module Graphos.UseCase.ClusterEval
  ( LandscapeInput(..)
  , defaultLandscapeInput
  , LeidenPoint(..)
  , LandscapeReport(..)
  , landscapeReport
  , foldCommunitiesToFiles
  , landscapeSummary
  , landscapeJSON
  ) where

import Data.Aeson (Value, object, (.=))

import Data.List (nub, sort, sortOn)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import Numeric (showFFloat)

import Graphos.Domain.Types (Node(..))
import Graphos.Domain.Graph (Graph, gNodes)
import Graphos.Domain.Community.Bunch
  ( BunchConfig(..)
  , defaultBunchConfig
  , FileMDG(..)
  , foldToFileMDG
  , mdgFileCount
  , mdgTotalWeight
  , intraOfAssignment
  , mqOfAssignment
  )
import Graphos.Domain.Community.Landscape
import Graphos.Domain.Community.MQ (Weighting(..))

-- | Everything one landscape evaluation needs.
data LandscapeInput = LandscapeInput
  { liBunch            :: BunchConfig          -- ^ the search configuration
  , liSeed             :: Word64               -- ^ first seed; run r uses @s+r@
  , liRuns             :: Int                  -- ^ N (default 100, the paper's count)
  , liRandomPartitions :: Int                  -- ^ M baseline partitions (default 1,000)
  , liWeighted         :: Bool                 -- ^ fold the file MDG with edge weights
  , liControl          :: Maybe RandomGraphSpec -- ^ run on a generated MDG instead
  , liBasinMinShare    :: Double               -- ^ basin listing threshold (default 5%)
  } deriving (Eq, Show)

defaultLandscapeInput :: LandscapeInput
defaultLandscapeInput = LandscapeInput
  { liBunch            = defaultBunchConfig
  , liSeed             = 0
  , liRuns             = 100
  , liRandomPartitions = 1000
  , liWeighted         = False
  , liControl          = Nothing
  , liBasinMinShare    = 0.05
  }

-- | The Leiden partition folded to files, in the landscape's coordinates.
data LeidenPoint = LeidenPoint
  { lpMQ            :: Double
  , lpClusters      :: Int
  , lpIntraFraction :: Double
  } deriving (Eq, Show)

-- | The evaluated landscape plus the optional Leiden point.
data LandscapeReport = LandscapeReport
  { lrmdg    :: FileMDG
  , lrResult :: LandscapeResult
  , lrLeiden :: Maybe LeidenPoint
  } deriving (Eq, Show)

-- | Run the evaluation. With a control spec the MDG is generated and the
-- graph is not needed; otherwise the graph's file MDG is folded with the
-- input's weighting and, when the graph carries communities, the Leiden
-- point is scored in the same coordinates.
landscapeReport :: LandscapeInput -> Maybe Graph -> LandscapeReport
landscapeReport input mGraph = case liControl input of
  Just spec -> report (generateRandomMDG spec (liSeed input)) Nothing
  Nothing ->
    let g = fromMaybe
              (error "landscapeReport: no graph given and no control MDG") mGraph
        mdg = foldToFileMDG weighting g
        point = if hasCommunities g
                  then Just (foldCommunitiesToFiles mdg g)
                  else Nothing
    in report mdg point
  where
    weighting = if liWeighted input then EdgeWeights else UnitWeights
    report mdg point = LandscapeReport
      { lrmdg = mdg
      , lrResult = runLandscapeOpts mdg (liBunch input) (liSeed input)
                     defaultLandscapeOptions
                       { loRuns = liRuns input
                       , loRandomPartitions = liRandomPartitions input
                       , loBasinMinShare = liBasinMinShare input
                       }
      , lrLeiden = point
      }
    hasCommunities g = any (isJust . nodeCommunityId) (Map.elems (gNodes g))

-- | Fold the graph's node communities to the file level by majority
-- community (ties to the smallest community id) and score the resulting
-- file partition with MQ on the MDG. Files whose nodes carry no community
-- become singletons — a full clustered build covers every node, so this
-- only trims the degenerate partial-coverage case.
foldCommunitiesToFiles :: FileMDG -> Graph -> LeidenPoint
foldCommunitiesToFiles mdg g =
  let files = V.toList (mdgFiles mdg)
      nodeComm = Map.fromList
        [ (nodeId n, c) | n <- Map.elems (gNodes g), Just c <- [nodeCommunityId n] ]
      nodesByFile = Map.fromListWith (flip (++))
        [ (nodeSourceFile n, [nodeId n]) | n <- Map.elems (gNodes g) ]
      fileComm = Map.fromList
        [ (f, majority cids)
        | f <- files
        , let cids = [ c | nid <- Map.findWithDefault [] f nodesByFile
                     , Just c <- [Map.lookup nid nodeComm] ] ]
      majority [] = Nothing
      majority cs = case sortOn (\(c, cnt) -> (Down cnt, c))
                           (Map.toList (Map.fromListWith (+) [ (c, 1 :: Int) | c <- cs ])) of
        ((c, _) : _) -> Just c
        []           -> Nothing

      grouped = sort (nub [ c | Just c <- Map.elems fileComm ])
      idxOf = Map.fromList (zip grouped [0 :: Int ..])
      singletons = zip [length grouped ..]
                       [ f | f <- files, case fileComm Map.! f of Nothing -> True; _ -> False ]
      singletonIdx = Map.fromList [ (f, i) | (i, f) <- singletons ]
      fullAssign = VU.fromList
        [ case fileComm Map.! f of
            Just c  -> idxOf Map.! c
            Nothing -> singletonIdx Map.! f
        | f <- files ]
      totalW = mdgTotalWeight mdg
  in LeidenPoint
       { lpMQ            = mqOfAssignment mdg fullAssign
       , lpClusters      = length grouped + length singletons
       , lpIntraFraction = if totalW <= 0
                             then 0
                             else intraOfAssignment mdg fullAssign / totalW
       }

-- | Markdown summary of a landscape: basins table, similarity table,
-- random-partition baseline and the Leiden point when present.
landscapeSummary :: LandscapeReport -> Text
landscapeSummary rep = T.unlines $
  [ "## Search landscape", ""
  , "Basins of attraction (final MQ binned at 0.05; groups with ≥ 5% of runs):", ""
  ]
  ++ basinsTable (lrBasins result)
  ++ [ "", "Edge similarity distribution (share of runs whose endpoints share a cluster):", "" ]
  ++ similarityTable (lrSimilarity result)
  ++ [ "", baselineLine (lrBaseline result) ]
  ++ leidenLines (lrLeiden rep)
  where
    result = lrResult rep

basinsTable :: [Basin] -> [Text]
basinsTable [] = ["_no basin holds 5% of the runs_"]
basinsTable bs =
  ["| MQ bin | Runs | Share | MQ range | Clusters |"
  , "|--------|------|-------|----------|----------|"]
  ++ [ T.pack ("| " ++ showDouble (basinKey b) ++ " | " ++ show (basinRuns b)
        ++ " | " ++ showDouble (basinShare b) ++ "% | "
        ++ showDouble (fst (basinMQRange b)) ++ "–" ++ showDouble (snd (basinMQRange b))
        ++ " | " ++ show (fst (basinClusterRange b)) ++ "–" ++ show (snd (basinClusterRange b))
        ++ " |")
     | b <- bs ]

similarityTable :: SimilarityDistribution -> [Text]
similarityTable d =
  ["| Bucket | P_uv | Edges | Share of \\|E\\| |"
  , "|--------|------|-------|---------------|"]
  ++ [ T.pack ("| " ++ name ++ " | " ++ range ++ " | " ++ show (countOf b)
        ++ " | " ++ showDouble (similarityShare d b) ++ "% |")
     | (b, name, range) <- buckets ]
  where
    buckets =
      [ (BucketZero,   "Zero",   "P = 0")
      , (BucketLow,    "Low",    "0 < P ≤ 0.10")
      , (BucketMedium, "Medium", "0.10 < P < 0.75")
      , (BucketHigh,   "High",   "P ≥ 0.75")
      ]
    countOf b = Map.findWithDefault 0 b (simCounts d)

baselineLine :: RandomBaseline -> Text
baselineLine b = T.pack
  ("Random-partition baseline (" ++ show (rbCount b) ++ " partitions): "
   ++ "min " ++ showDouble (rbMin b) ++ ", median " ++ showDouble (rbMedian b)
   ++ ", max " ++ showDouble (rbMax b))

leidenLines :: Maybe LeidenPoint -> [Text]
leidenLines Nothing = []
leidenLines (Just p) =
  [ "", T.pack ("Leiden point (partition folded to files by majority community): MQ "
      ++ showDouble (lpMQ p) ++ ", " ++ show (lpClusters p) ++ " clusters, intra-edge fraction "
      ++ showDouble (lpIntraFraction p)) ]

-- | Fixed-notation double rendering for the summary tables.
showDouble :: Double -> String
showDouble x = showFFloat (Just 4) x ""

-- | The landscape as one JSON object (@landscape.json@ and @--json@):
-- @config@, @mdg@, @runs@ (sorted by final MQ), @similarity@, @basins@,
-- @random_baseline@ and, when the graph carries communities, @leiden@.
landscapeJSON :: LandscapeInput -> LandscapeReport -> Value
landscapeJSON input rep = object $
  [ "config" .= object
      [ "seed"              .= liSeed input
      , "runs"              .= liRuns input
      , "random_partitions" .= liRandomPartitions input
      , "weighted"          .= liWeighted input
      , "random_graph"      .= fmap renderRandomGraphSpec (liControl input)
      , "basin_min_share"   .= liBasinMinShare input
      , "threshold"         .= bcThreshold (liBunch input)
      , "annealing"         .= fmap (\(t0, a) -> object ["t0" .= t0, "alpha" .= a]) (bcAnnealing (liBunch input))
      , "max_iterations"    .= bcMaxIterations (liBunch input)
      ]
  , "mdg" .= object
      [ "files" .= mdgFileCount mdg
      , "edges" .= VU.length (mdgEdgeU mdg)
      ]
  , "runs" .= map runJSON (lrRuns res)
  , "similarity" .= object
      ( [ "edges" .= simEdges sim, "runs" .= simRuns sim ]
        ++ [ bucketKey b .= object
               [ "edges"   .= Map.findWithDefault 0 b (simCounts sim)
               , "percent" .= similarityShare sim b ]
           | b <- [minBound .. maxBound] ] )
  , "basins" .= map basinJSON (lrBasins res)
  , "random_baseline" .= object
      [ "count"  .= rbCount bl
      , "min"    .= rbMin bl
      , "median" .= rbMedian bl
      , "max"    .= rbMax bl
      ]
  ]
  ++ [ "leiden" .= object
         [ "mq"             .= lpMQ p
         , "clusters"       .= lpClusters p
         , "intra_fraction" .= lpIntraFraction p ]
     | Just p <- [lrLeiden rep] ]
  where
    mdg = lrmdg rep
    res = lrResult rep
    sim = lrSimilarity res
    bl  = lrBaseline res
    bucketKey b = case b of
      BucketZero   -> "zero"
      BucketLow    -> "low"
      BucketMedium -> "medium"
      BucketHigh   -> "high"
    runJSON r = object
      [ "seed"              .= rrSeed r
      , "start_mq"          .= rrStartMQ r
      , "start_clusters"    .= rrStartClusters r
      , "final_mq"          .= rrFinalMQ r
      , "final_clusters"    .= rrFinalClusters r
      , "intra_fraction"    .= rrIntraFraction r
      , "evaluations"       .= rrEvaluations r
      , "iterations"        .= rrIterations r
      , "annealing_accepts" .= rrAnnealingAccepts r
      ]
    basinJSON b = object
      [ "mq_bin"        .= basinKey b
      , "runs"          .= basinRuns b
      , "percent"       .= basinShare b
      , "mq_range"      .= [fst (basinMQRange b), snd (basinMQRange b)]
      , "cluster_range" .= [fst (basinClusterRange b), snd (basinClusterRange b)]
      ]
