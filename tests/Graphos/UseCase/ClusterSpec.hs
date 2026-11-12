{-# LANGUAGE OverloadedStrings #-}
-- | Algorithm dispatch in the cluster stage (openspec
-- @bunch-search-based-modularization@, specs @community-detection@ and
-- @bunch-clustering@).
module Graphos.UseCase.ClusterSpec (spec) where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Short (fromText)
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph)
import Graphos.Domain.Community (Resolution(..), MergeStrategy(..), modularity, capCommunitySizes)
import Graphos.Domain.Community.Bunch (BunchConfig(..), defaultBunchConfig, foldToFileMDG, mdgFileCount, mdgEdgeCount, bunchCluster, BunchResult(..), BunchRun(..))
import Graphos.Domain.Community.MQ (Weighting(..))
import Graphos.UseCase.Cluster
import Graphos.Domain.Config.Clustering (ClusteringConfig(..), ClusteringAlgorithmName(..), BunchFileConfig(..), defaultClusteringConfig, defaultBunchFileConfig)
import Graphos.Domain.Community.MQ (modularizationQuality, mqValue)

spec :: Spec
spec = do
  leidenSpec
  bunchSpec
  capSpec
  configSpec
  realProjectsSpec

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

edgeOf :: Relation -> Text -> Text -> Edge
edgeOf rel u v = Edge
  { edgeId = EdgeId (u <> "->" <> v)
  , edgeSource = u
  , edgeTarget = v
  , edgeRelation = rel
  , edgeWeight = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra = Nothing
  }

-- | Three files with a chain a.ts – b.ts – c.ts and two symbols per file.
chainGraph :: Graph
chainGraph = buildGraph False $ extractionFromLists
  [ nodeIn "a.ts" "a.f", nodeIn "a.ts" "a.h"
  , nodeIn "b.ts" "b.g", nodeIn "b.ts" "b.mod"
  , nodeIn "c.ts" "c.p", nodeIn "c.ts" "c.q" ]
  [ edgeOf Calls "a.f" "b.g", edgeOf Calls "a.h" "b.mod"
  , edgeOf Imports "b.g" "c.p", edgeOf Calls "c.q" "b.g" ]

res :: Resolution
res = Resolution { resGamma = 1.0, resMinSize = 1, resMergeInto = MergeToNeighbor, resMaxIterations = 50 }

-- ───────────────────────────────────────────────
-- Leiden unchanged (5.1)
-- ───────────────────────────────────────────────

leidenSpec :: Spec
leidenSpec = describe "runClustering Leiden" $
  it "is the pre-existing clusterGraphWithResolution result, byte for byte" $ do
    let (comm, coh, q) = runClustering Leiden res chainGraph
        (comm0, coh0) = clusterGraphWithResolution chainGraph res
    comm `shouldBe` comm0
    coh `shouldBe` coh0
    cqAlgorithm q `shouldBe` "leiden"
    cqSkip q `shouldBe` Nothing
    cqClusters q `shouldBe` Map.size comm
    cqModularity q `shouldBe` modularity chainGraph comm0

-- ───────────────────────────────────────────────
-- Bunch push-back (5.1)
-- ───────────────────────────────────────────────

bunchSpec :: Spec
bunchSpec = describe "runClustering (BunchAlgorithm …)" $ do
  let settings = defaultBunchSettings { bsConfig = defaultBunchConfig { bcSeed = 1 } }

  it "gives every symbol its file's cluster" $ do
    let (comm, _coh, q) = runClustering (BunchAlgorithm settings) res chainGraph
        commOf :: Text -> [CommunityId]
        commOf nid = [ cid | (cid, members) <- Map.toList comm, nid `elem` members ]
        clusters :: [(Text, [CommunityId])]
        clusters = [ (f, commOf nid)
                   | (f, nid) <- [ ("a.ts", "a.f"), ("a.ts", "a.h")
                                  , ("b.ts", "b.g"), ("b.ts", "b.mod")
                                  , ("c.ts", "c.p"), ("c.ts", "c.q") ] ]
    cqAlgorithm q `shouldBe` "bunch"
    cqSkip q `shouldBe` Nothing
    -- both symbols of a file land in the same community
    mapM_ (\(_, cs) -> length cs `shouldBe` 1) clusters

  it "is deterministic and covers every node exactly once" $ do
    let (comm, _, _) = runClustering (BunchAlgorithm settings) res chainGraph
        (comm', _, _) = runClustering (BunchAlgorithm settings) res chainGraph
    comm `shouldBe` comm'
    sort (concatMap snd (Map.toList comm)) `shouldBe` sort ["a.f", "a.h", "b.g", "b.mod", "c.p", "c.q"]

  it "matches the file-level search result pushed to symbols" $ do
    let (comm, _, q) = runClustering (BunchAlgorithm settings) res chainGraph
        m = foldToFileMDG UnitWeights chainGraph
        best = bresBest (bunchCluster (bsConfig settings) m)
        symbolFiles :: [(Text, Text)]
        symbolFiles = [ ("a.f", "a.ts"), ("a.h", "a.ts")
                      , ("b.g", "b.ts"), ("b.mod", "b.ts")
                      , ("c.p", "c.ts"), ("c.q", "c.ts") ]
        filesOf :: CommunityId -> [Text]
        filesOf cid = sort [ f | (cid', members) <- Map.toList comm
                           , cid' == cid, nid <- members, (nid', f) <- symbolFiles
                           , nid == nid' ]
    (mdgFileCount m, mdgEdgeCount m) `shouldBe` (3, 2)
    -- the quality record is the search's own score of the pushed-back partition
    cqMQ q `shouldBe` brFinalMQ best
    cqClusters q `shouldBe` brFinalClusters best
    -- no community is empty and none exceeds the file count
    mapM_ (\cid -> filesOf cid `shouldSatisfy` (not . null)) (Map.keys comm)
    Map.size comm `shouldSatisfy` (\k -> k >= 1 && k <= 3)

-- ───────────────────────────────────────────────
-- File cap (5.1)
-- ───────────────────────────────────────────────

capSpec :: Spec
capSpec = describe "file cap" $ do
  let settings = defaultBunchSettings { bsMaxFiles = 2 }  -- the chain has 3 files

  it "skips Bunch above the cap and returns Leiden's partition with a skip entry" $ do
    let (comm, coh, q) = runClustering (BunchAlgorithm settings) res chainGraph
        (comm0, coh0) = clusterGraphWithResolution chainGraph res
    comm `shouldBe` comm0
    coh `shouldBe` coh0
    cqAlgorithm q `shouldBe` "leiden"
    cqSkip q `shouldBe` Just (BunchSkip 3 2)

  it "at exactly the cap Bunch runs" $ do
    let settingsAtCap = settings { bsMaxFiles = 3 }
        (_, _, q) = runClustering (BunchAlgorithm settingsAtCap) res chainGraph
    cqAlgorithm q `shouldBe` "bunch"
    cqSkip q `shouldBe` Nothing

-- ───────────────────────────────────────────────
-- Config bridge and report/export agreement (6.1, 6.3)
-- ───────────────────────────────────────────────

configSpec :: Spec
configSpec = describe "clusterAlgorithmFromConfig / partitionQuality" $ do
  it "maps the default config to Leiden" $
    clusterAlgorithmFromConfig defaultClusteringConfig `shouldBe` Leiden

  it "maps clustering.algorithm bunch and its block to BunchSettings" $
    clusterAlgorithmFromConfig defaultClusteringConfig
      { ccAlgorithm = AlgoBunch, ccSeed = 7
      , ccBunch = defaultBunchFileConfig
          { bfPopulation = 3, bfThreshold = 50, bfAnnealing = Just (100, 0.99)
          , bfMaxIterations = 42, bfMaxFiles = 9, bfWeighted = True } }
      `shouldBe` BunchAlgorithm BunchSettings
        { bsConfig = defaultBunchConfig
            { bcSeed = 7, bcPopulation = 3, bcThreshold = 50
            , bcAnnealing = Just (100, 0.99), bcMaxIterations = 42 }
        , bsMaxFiles = 9, bsMaxFilesPerRun = 500
        , bsRelations = [Imports, Calls, References, Extends, Implements], bsWeighted = True }

  it "scores the partition it is given: MQ equals modularizationQuality of those communities" $ do
    let (comm, _, q0) = runClustering (BunchAlgorithm defaultBunchSettings) res chainGraph
        q = partitionQuality (cqAlgorithm q0) chainGraph comm (cqSkip q0)
    cqMQ q `shouldBe` mqValue (modularizationQuality chainGraph comm)
    cqAlgorithm q `shouldBe` "bunch"
    q `shouldBe` q0 { cqBunchRuns = Nothing }

-- ───────────────────────────────────────────────
-- Real repositories (openspec bunch-real-projects D1, D2, D3)
-- ───────────────────────────────────────────────

-- | Two 3-file directories, each a chain, joined by one import.
twoDirGraph :: Graph
twoDirGraph = buildGraph False $ extractionFromLists
  [ nodeIn "a/1.ts" "a1", nodeIn "a/2.ts" "a2", nodeIn "a/3.ts" "a3"
  , nodeIn "b/1.ts" "b1", nodeIn "b/2.ts" "b2", nodeIn "b/3.ts" "b3" ]
  [ edgeOf Imports "a1" "a2", edgeOf Imports "a2" "a3", edgeOf Imports "a1" "a3"
  , edgeOf Imports "b1" "b2", edgeOf Imports "b2" "b3", edgeOf Imports "b1" "b3"
  , edgeOf Imports "a3" "b1" ]

-- | 'twoDirGraph' after edge inference: depends_on edges across the directories.
enrichedTwoDir :: Graph
enrichedTwoDir = buildGraph False $ extractionFromLists
  [ nodeIn "a/1.ts" "a1", nodeIn "a/2.ts" "a2", nodeIn "a/3.ts" "a3"
  , nodeIn "b/1.ts" "b1", nodeIn "b/2.ts" "b2", nodeIn "b/3.ts" "b3" ]
  [ edgeOf Imports "a1" "a2", edgeOf Imports "a2" "a3", edgeOf Imports "a1" "a3"
  , edgeOf Imports "b1" "b2", edgeOf Imports "b2" "b3", edgeOf Imports "b1" "b3"
  , edgeOf Imports "a3" "b1"
  , edgeOf DependsOn "a1" "b2", edgeOf DependsOn "a2" "b3", edgeOf DependsOn "a3" "b2" ]

realProjectsSpec :: Spec
realProjectsSpec = do
  let bunch = BunchAlgorithm defaultBunchSettings { bsConfig = defaultBunchConfig { bcSeed = 1, bcPopulation = 2 } }

  describe "capPartition (D1)" $ do
    it "never splits a Bunch partition, whatever the cap" $ do
      let (comm, _, q) = runClustering bunch res twoDirGraph
      cqAlgorithm q `shouldBe` "bunch"
      capPartition q True 0.01 twoDirGraph comm `shouldBe` comm
      Map.size comm `shouldSatisfy` (< 6)        -- fewer modules than files

    it "caps a Leiden partition, including Leiden run above the Bunch ceiling" $ do
      let skipped = BunchAlgorithm defaultBunchSettings { bsMaxFiles = 2 }
          (comm, _, q) = runClustering skipped res twoDirGraph
      cqAlgorithm q `shouldBe` "leiden"
      capPartition q True 0.2 twoDirGraph comm `shouldBe` capCommunitySizes twoDirGraph comm 0.2
      capPartition q False 0.2 twoDirGraph comm `shouldBe` comm

  describe "stepFiveClustering (D2)" $ do
    it "reuses the Step-4 Bunch partition on the inference-enriched graph" $ do
      let step4@(comm4, _, _) = runClustering bunch res twoDirGraph
          (comm5, _, q5) = stepFiveClustering bunch res step4 enrichedTwoDir
      comm5 `shouldBe` comm4
      cqAlgorithm q5 `shouldBe` "bunch"
      cqBunchRuns q5 `shouldSatisfy` (/= Nothing)

    it "the enriched graph folds to the same MDG (inferred relations excluded)" $
      foldToFileMDG UnitWeights enrichedTwoDir `shouldBe` foldToFileMDG UnitWeights twoDirGraph

    it "re-clusters with Leiden" $ do
      let step4 = runClustering Leiden res twoDirGraph
          (comm5, _, _) = stepFiveClustering Leiden res step4 enrichedTwoDir
          (commL, _, _) = runClustering Leiden res enrichedTwoDir
      comm5 `shouldBe` commL

  describe "partitioned dispatch (D3)" $ do
    it "splits by sub-tree above the per-run size instead of falling back to Leiden" $ do
      let split = BunchAlgorithm defaultBunchSettings
                    { bsConfig = defaultBunchConfig { bcSeed = 1, bcPopulation = 2 }, bsMaxFilesPerRun = 3 }
          (comm, _, q) = runClustering split res twoDirGraph
      cqAlgorithm q `shouldBe` "bunch"
      cqSkip q `shouldBe` Nothing
      fmap briRuns (cqBunchRuns q) `shouldBe` Just [3, 3]
      sort (concatMap snd (Map.toList comm)) `shouldBe` sort ["a1", "a2", "a3", "b1", "b2", "b3"]

    it "keeps the hard ceiling: above bsMaxFiles Leiden runs with a skip entry" $ do
      let (_, _, q) = runClustering (BunchAlgorithm defaultBunchSettings { bsMaxFiles = 5 }) res twoDirGraph
      cqSkip q `shouldBe` Just (BunchSkip 6 5)
