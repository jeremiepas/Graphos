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
    -- * Score / compare / sweep (clustering-evaluation-and-selection)
  , ScoreResult(..)
  , CompareResult(..)
  , SweepOptions(..)
  , defaultSweepOptions
  , SweepRow(..)
  , SweepResult(..)
  , filePartition
  , partitionOfCommunityMap
  , directoryTruthFor
  , multiRefTruth
  , scoreGraph
  , compareGraphs
  , sweepGraph
  , scoreJSON
  , compareJSON
  , sweepJSON
  , sweepMarkdown
  , sweepReport
  ) where

import Data.Aeson (Value, object, (.=))

import Data.List (nub, sort, sortOn)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Short as TS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import Numeric (showFFloat)

import Graphos.Domain.Types
  ( Node(..), NodeId, CommunityMap, ClusterQuality(..)
  , ClusterEvalRow(..), ClusterEvalReport(..)
  )
import Graphos.Domain.Graph (Graph, gNodes)
import Graphos.Domain.Community (Resolution(..), defaultResolution)
import Graphos.Domain.Community.Partition
  ( Partition
  , fromAssignment
  , groups
  , meetAll
  , directoryTruth
  , mojoFM
  , stability
  , MoJoResult(..)
  )
import Graphos.Domain.Community.Bunch (mdgFileSet)
import Graphos.Domain.Community.Agglomerative (Distance(..), Linkage(..))
import Graphos.UseCase.Cluster
  ( ClusterAlgorithm(..)
  , BunchSettings(..)
  , AgglomerativeSettings(..)
  , defaultBunchSettings
  , defaultAgglomerativeSettings
  , runClustering
  )
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

-- ───────────────────────────────────────────────
-- Score / compare / sweep (clustering-evaluation-and-selection)
-- ───────────────────────────────────────────────

-- | Result of scoring one graph's stored partition against a ground truth.
data ScoreResult = ScoreResult
  { srScore       :: !Double  -- ^ MoJoFM in [0, 100]
  , srMno         :: !Int
  , srMaxMno      :: !Int
  , srCommon      :: !Int
  , srDropped     :: !Int     -- ^ nodes dropped from either side
  , srTruthGroups :: !Int
  , srClusters    :: !Int
  } deriving (Eq, Show)

-- | Stability of two builds' file partitions, both directions.
data CompareResult = CompareResult
  { crAB :: !MoJoResult  -- ^ old → new
  , crBA :: !MoJoResult  -- ^ new → old
  } deriving (Eq, Show)

-- | One configuration of the sweep.
data SweepRow = SweepRow
  { swConfig     :: !Text
  , swMojofm     :: !Double
  , swClusters   :: !Int
  , swSingletons :: !Int
  , swModularity :: !Double
  , swPartition  :: !(Partition Text)
  , swCommunities :: !CommunityMap
  , swAlgorithm  :: !ClusterAlgorithm
  } deriving (Eq, Show)

-- | Everything one sweep needs. The truth is computed by the caller so the
-- CLI can choose directory depth, multi-ref meet or an explicit file.
data SweepOptions = SweepOptions
  { soTruth         :: !(Partition Text)
  , soTruthLabel    :: !Text
  , soResolution    :: !Resolution
  , soBunch         :: !BunchSettings
  , soAgglomerative :: !AgglomerativeSettings
  , soDivisors      :: ![Int]
  } deriving (Eq, Show)

defaultSweepOptions :: Partition Text -> SweepOptions
defaultSweepOptions truth = SweepOptions
  { soTruth         = truth
  , soTruthLabel    = "directory depth 2"
  , soResolution    = defaultResolution
  , soBunch         = defaultBunchSettings
  , soAgglomerative = defaultAgglomerativeSettings
  , soDivisors      = [5, 7, 10, 15, 20, 25]
  }

-- | The ranked sweep plus the winning partition.
data SweepResult = SweepResult
  { swRows       :: ![SweepRow]  -- ^ best first: MoJoFM desc, singletons, name
  , swSelected   :: !SweepRow
  , swTruthGroups :: !Int
  , swTruthLabel :: !Text
  , swSkipped    :: ![Text]
  } deriving (Eq, Show)

-- | Fold a graph's stored node communities to files by majority community
-- (ties to the smallest community id). Files with no community on any of
-- their nodes are absent from the partition, so a partially clustered graph
-- reports them as dropped.
filePartition :: Graph -> Partition Text
filePartition g = partitionOfNodes g
  [ (nid, c) | n <- Map.elems (gNodes g), Just c <- [nodeCommunityId n], let nid = nodeId n ]

-- | Fold an explicit community map to files, keeping the same file set and
-- majority rule as 'filePartition'.
partitionOfCommunityMap :: Graph -> CommunityMap -> Partition Text
partitionOfCommunityMap g cm =
  partitionOfNodes g [ (nid, cid) | (cid, nids) <- Map.toList cm, nid <- nids ]

partitionOfNodes :: Graph -> [(NodeId, Int)] -> Partition Text
partitionOfNodes g pairs =
  let files = mdgFileSet g
      commOf = Map.fromList pairs
      nodesByFile = Map.fromListWith (flip (++))
        [ (nodeSourceFile n, [nodeId n]) | n <- Map.elems (gNodes g) ]
      fileLabel f =
        let cs = [ c | nid <- Map.findWithDefault [] f nodesByFile
                 , Just c <- [Map.lookup nid commOf] ]
        in case sortOn (\x -> (Down (snd x), fst x))
                 (Map.toList (Map.fromListWith (+) [ (c, 1 :: Int) | c <- cs ])) of
             ((c, _) : _) -> Just (T.pack ("c" ++ show c))
             []           -> Nothing
  in fromAssignment (Map.fromList
       [ (T.pack (TS.toString f), lbl)
       | f <- Set.toAscList files, Just lbl <- [fileLabel f] ])

-- | Directory ground truth of one graph's files at depth @d@.
directoryTruthFor :: Int -> Graph -> Partition Text
directoryTruthFor d g = directoryTruth d (map (T.pack . TS.toString) (Set.toAscList (mdgFileSet g)))

-- | Multi-ref ground truth (design D2): the meet of the per-graph directory
-- partitions, with the number of files dropped because some graph lacks them.
multiRefTruth :: Int -> [Graph] -> (Partition Text, Int)
multiRefTruth d gs = meetAll (map (directoryTruthFor d) gs)

-- | Score a graph's stored communities against a truth partition.
scoreGraph :: Graph -> Partition Text -> ScoreResult
scoreGraph g truth =
  let p = filePartition g
      r = mojoFM p truth
  in ScoreResult
       { srScore       = mjScore r
       , srMno         = mjMno r
       , srMaxMno      = mjMaxMno r
       , srCommon      = mjCommon r
       , srDropped     = mjDroppedA r + mjDroppedB r
       , srTruthGroups = length (groups truth)
       , srClusters    = length (groups p)
       }

-- | Stability of two graphs' file partitions.
compareGraphs :: Graph -> Graph -> CompareResult
compareGraphs ga gb =
  let (ab, ba) = stability (filePartition ga) (filePartition gb)
  in CompareResult ab ba

-- | Run Leiden, Bunch and the full agglomerative grid, score each against
-- the truth, and rank by MoJoFM descending, then fewer singletons, then
-- configuration name. A candidate skipped above its file cap is omitted and
-- the reason recorded in 'swSkipped'; Leiden is always present.
sweepGraph :: SweepOptions -> Graph -> SweepResult
sweepGraph opts g =
  let files = Set.size (mdgFileSet g)
      aggs = [ (distanceNameT d, linkageNameT l, q)
             | d <- [Euclidean, Manhattan, Cosine]
             , l <- [Single, Average, Complete]
             , q <- soDivisors opts ]
      aggSkip
        | files > asMaxFiles (soAgglomerative opts) =
            Just (T.pack ("agglomerative: " ++ show files ++ " files exceed --agglomerative-max-files "
                          ++ show (asMaxFiles (soAgglomerative opts)) ++ "; Leiden ran instead"))
        | otherwise = Nothing
      bunchSkip
        | files > bsMaxFiles (soBunch opts) =
            Just (T.pack ("bunch: " ++ show files ++ " files exceed --bunch-max-files "
                          ++ show (bsMaxFiles (soBunch opts)) ++ "; Leiden ran instead"))
        | otherwise = Nothing
      leidenRow = rowFor "leiden" Leiden
      bunchRows = [ rowFor "bunch" (BunchAlgorithm (soBunch opts)) | isNothing bunchSkip ]
      aggRows = [ rowFor (T.pack (distance ++ "_" ++ linkage ++ "_" ++ show q))
                    (AgglomerativeAlgorithm (soAgglomerative opts)
                       { asDistance = d, asLinkage = l, asDivisor = q })
                | isNothing aggSkip
                , (distance, linkage, q) <- aggs
                , let (d, l) = lookupPair distance linkage ]
      rows = sortOn (\r -> (Down (swMojofm r), swSingletons r, swConfig r))
               (leidenRow : bunchRows ++ aggRows)
      skipped = [ m | Just m <- [bunchSkip, aggSkip] ]
  in case rows of
       [] -> SweepResult [] leidenRow (length (groups (soTruth opts))) (soTruthLabel opts) skipped
       (best : _) -> SweepResult rows best (length (groups (soTruth opts))) (soTruthLabel opts) skipped
  where
    rowFor name algo =
      let (comm, _, q) = runClustering algo (soResolution opts) g
          p = partitionOfCommunityMap g comm
          mojofm = mjScore (mojoFM p (soTruth opts))
          singles = length [ xs | xs <- groups p, length xs == 1 ]
      in SweepRow name mojofm (cqClusters q) singles (cqModularity q) p comm algo
    distanceNameT Euclidean = "euclidean"
    distanceNameT Manhattan = "manhattan"
    distanceNameT Cosine    = "cosine"
    linkageNameT Single   = "single"
    linkageNameT Average  = "average"
    linkageNameT Complete = "complete"
    lookupPair distance linkage = case (distance, linkage) of
      ("euclidean", "single")   -> (Euclidean, Single)
      ("euclidean", "average")  -> (Euclidean, Average)
      ("euclidean", "complete") -> (Euclidean, Complete)
      ("manhattan", "single")   -> (Manhattan, Single)
      ("manhattan", "average")  -> (Manhattan, Average)
      ("manhattan", "complete") -> (Manhattan, Complete)
      ("cosine", "single")      -> (Cosine, Single)
      ("cosine", "average")     -> (Cosine, Average)
      _                         -> (Cosine, Complete)
    isNothing Nothing = True
    isNothing (Just _) = False

-- | JSON for @clustereval score --json@.
scoreJSON :: ScoreResult -> Value
scoreJSON r = object
  [ "mojofm"      .= srScore r
  , "mno"         .= srMno r
  , "max_mno"     .= srMaxMno r
  , "common"      .= srCommon r
  , "dropped"     .= srDropped r
  , "truth_groups" .= srTruthGroups r
  , "clusters"    .= srClusters r
  ]

-- | JSON for @clustereval compare --json@.
compareJSON :: CompareResult -> Value
compareJSON r = object
  [ "stability_ab" .= mjScore (crAB r)
  , "stability_ba" .= mjScore (crBA r)
  , "mojofm_ab"    .= mjScore (crAB r)
  , "mojofm_ba"    .= mjScore (crBA r)
  , "mno_ab"       .= mjMno (crAB r)
  , "mno_ba"       .= mjMno (crBA r)
  , "common_ab"    .= mjCommon (crAB r)
  , "common_ba"    .= mjCommon (crBA r)
  ]

-- | JSON for @clustereval sweep --json@ (also @cluster_eval.json@).
sweepJSON :: SweepResult -> Value
sweepJSON r = object
  [ "truth"        .= swTruthLabel r
  , "truth_groups" .= swTruthGroups r
  , "selected"     .= swConfig (swSelected r)
  , "rows"         .= map rowJSON (swRows r)
  , "skipped"      .= swSkipped r
  ]
  where
    rowJSON x = object
      [ "config"     .= swConfig x
      , "mojofm"     .= swMojofm x
      , "clusters"   .= swClusters x
      , "singletons" .= swSingletons x
      , "modularity" .= swModularity x
      ]

-- | The markdown ranking section appended to @GRAPH_REPORT.md@ by an
-- @auto@ build.
sweepMarkdown :: SweepResult -> Text
sweepMarkdown r = T.unlines $
  [ "## Configuration sweep", ""
  , T.pack ("Ground truth: " ++ T.unpack (swTruthLabel r) ++ " (" ++ show (swTruthGroups r) ++ " groups)")
  , ""
  , "| Rank | Configuration | MoJoFM | Clusters | Singletons | Modularity |"
  , "|------|---------------|--------|----------|------------|------------|"
  ]
  ++ [ T.pack ("| " ++ show i ++ " | " ++ T.unpack (swConfig x) ++ " | "
        ++ showDouble2 (swMojofm x) ++ " | " ++ show (swClusters x) ++ " | "
        ++ show (swSingletons x) ++ " | " ++ showDouble2 (swModularity x) ++ " |")
     | (i, x) <- zip [1 :: Int ..] (swRows r) ]
  ++ [ "" , T.pack ("Selected: " ++ T.unpack (swConfig (swSelected r))) ]
  ++ [ T.pack ("Skipped: " ++ T.unpack s) | s <- swSkipped r ]

showDouble2 :: Double -> String
showDouble2 x = showFFloat (Just 2) x ""

-- | Domain report carried by the analysis record and written to
-- @cluster_eval.json@.
sweepReport :: SweepResult -> ClusterEvalReport
sweepReport r = ClusterEvalReport
  { cerTruth       = swTruthLabel r
  , cerTruthGroups = swTruthGroups r
  , cerRows        = [ ClusterEvalRow (swConfig x) (swMojofm x) (swClusters x)
                         (swSingletons x) (swModularity x)
                     | x <- swRows r ]
  , cerSelected    = swConfig (swSelected r)
  , cerSkipped     = swSkipped r
  }
