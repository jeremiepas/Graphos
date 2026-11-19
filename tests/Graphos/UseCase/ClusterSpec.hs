{-# LANGUAGE OverloadedStrings #-}
-- | Algorithm dispatch in the cluster stage (openspec
-- @bunch-search-based-modularization@, specs @community-detection@ and
-- @bunch-clustering@).
module Graphos.UseCase.ClusterSpec (spec) where

import Data.List (sort, nub)
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText, toText)
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph, gNodes, gEdges)
import Graphos.Domain.Community (Resolution(..), MergeStrategy(..), modularity, capCommunitySizes, undirectedView)
import Graphos.Domain.Community.Bunch (BunchConfig(..), defaultBunchConfig, foldToFileMDG, mdgFileCount, mdgEdgeCount, bunchCluster, BunchResult(..), BunchRun(..))
import Graphos.Domain.Community.MQ (Weighting(..))
import Graphos.Domain.Community.Agglomerative (Distance(..), Linkage(..))
import Graphos.UseCase.Cluster
import Graphos.Domain.Config.Clustering
  ( ClusteringConfig(..), ClusteringAlgorithmName(..), BunchFileConfig(..)
  , AgglomerativeFileConfig(..), LinkageName(..), DistanceName(..)
  , defaultClusteringConfig, defaultBunchFileConfig, defaultAgglomerativeFileConfig
  )
import Graphos.Domain.Community.MQ (modularizationQuality, mqValue)

spec :: Spec
spec = do
  leidenSpec
  bunchSpec
  capSpec
  configSpec
  agglomerativeSpec
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

  it "placeholder nodes join the community of their importers, and are covered" $ do
    -- two triangles of files joined by one edge; package `p` is imported only
    -- from the first triangle, package `q` by nobody
    let tri xs = [ edgeOf Imports u v | (u, i) <- zip xs [0 :: Int ..], (v, j) <- zip xs [0 ..], i < j ]
        files = ["a1", "a2", "a3", "b1", "b2", "b3"]
        ph f k = (nodeIn ("external:" <> f) f) { nodeKind = Just (fromText k), nodeLineStart = Nothing }
        nodes = [ nodeIn (f <> ".ts") f | f <- files ] ++ [ph "p" "External", ph "q" "External"]
        edges = tri ["a1", "a2", "a3"] ++ tri ["b1", "b2", "b3"] ++ [edgeOf Imports "a3" "b1"]
                ++ [ edgeOf Imports f "p" | f <- ["a1", "a2", "a3"] ]
        g = buildGraph False (extractionFromLists nodes edges)
        (comm, _, _) = runClustering (BunchAlgorithm settings) res g
        cidOf nid = [ cid | (cid, ms) <- Map.toList comm, nid `elem` ms ]
    sort (concatMap snd (Map.toList comm)) `shouldBe` sort (files ++ ["p", "q"])
    cidOf "p" `shouldBe` cidOf "a1"
    cidOf "a1" `shouldNotBe` cidOf "b1"
    -- unreferenced `q` gets a community of its own
    length (cidOf "q") `shouldBe` 1
    cidOf "q" `shouldNotBe` cidOf "a1"
    cidOf "q" `shouldNotBe` cidOf "b1"

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
    -- chainGraph only has allowlisted relations, so scoring the whole graph or
    -- the MDG relations gives the same figures
    (cqMQ q, cqModularity q, cqIntraFraction q, cqClusters q)
      `shouldBe` (cqMQ q0, cqModularity q0, cqIntraFraction q0, cqClusters q0)

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

-- | A @--directed@ fixture whose report modularity used to leave [-1/2, 1]:
-- two directed triangles joined by one arc, with two pairs also stored in the
-- reverse orientation (modularity-undirected-view).
directedFixture :: Graph
directedFixture = buildGraph True $ extractionFromLists
  [ nodeIn "a/1.ts" "a1", nodeIn "a/2.ts" "a2", nodeIn "a/3.ts" "a3"
  , nodeIn "b/1.ts" "b1", nodeIn "b/2.ts" "b2", nodeIn "b/3.ts" "b3" ]
  [ edgeOf Imports "a1" "a2", edgeOf Imports "a2" "a3", edgeOf Imports "a3" "a1"
  , edgeOf Imports "b1" "b2", edgeOf Imports "b2" "b3", edgeOf Imports "b3" "b1"
  , edgeOf Imports "a3" "b1"
  , edgeOf Calls "a2" "a1", edgeOf Calls "b2" "b1" ]

realProjectsSpec :: Spec
realProjectsSpec = do
  describe "report modularity on a --directed build (modularity-undirected-view 2.3)" $
    forM_ [("leiden", Leiden), ("bunch", BunchAlgorithm defaultBunchSettings { bsConfig = defaultBunchConfig { bcSeed = 1 } })] $ \(name, algo) ->
      it (name ++ ": cqModularity is in [-1/2, 1], equals the undirected score, and finalComm covers every node") $ do
        let (rawComm, _, rawQ) = runClustering algo res directedFixture
            finalComm = capPartition rawQ True 0.05 directedFixture rawComm
            q = partitionQuality (cqAlgorithm rawQ) directedFixture finalComm (cqSkip rawQ)
            members = concat (Map.elems finalComm)
        sort members `shouldBe` sort (Map.keys (gNodes directedFixture))
        length members `shouldBe` length (nub members)
        cqModularity q `shouldSatisfy` (\m -> m >= -0.5 && m <= 1)
        cqModularity q `shouldBe` modularity (undirectedView directedFixture) finalComm

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

    it "scores the reused partition over the MDG relations, not the inferred edges" $ do
      let step4@(comm4, _, q4) = runClustering bunch res twoDirGraph
          (_, _, q5) = stepFiveClustering bunch res step4 enrichedTwoDir
          wholeGraphMQ = mqValue (modularizationQuality enrichedTwoDir comm4)
      cqMQ q5 `shouldBe` cqMQ q4
      cqModularity q5 `shouldBe` cqModularity q4
      cqScoredRelations q5 `shouldBe` Just ["imports", "calls", "references", "extends", "implements"]
      -- the depends_on edges would change the score if they were counted
      wholeGraphMQ `shouldSatisfy` (/= cqMQ q5)

    it "bunchPartitionQuality equals modularizationQuality of the restricted graph" $ do
      let (comm, _, _) = runClustering bunch res twoDirGraph
          q = bunchPartitionQuality [Imports] enrichedTwoDir comm
      cqMQ q `shouldBe` mqValue (modularizationQuality (restrictToRelations [Imports] enrichedTwoDir) comm)
      cqScoredRelations q `shouldBe` Just ["imports"]

    it "inferred code→doc `references` edges change neither the MDG nor the score" $ do
      -- semantic/bounded edge inference links code to doc headers with
      -- `References` (an allowlisted relation); a doc file is not an MDG file
      let doc f nid = (nodeIn f nid) { nodeFileType = DocFile }
          withDocs = buildGraph False $ extractionFromLists
            (Map.elems (gNodes twoDirGraph) ++ [doc "specs/x.md" "x.h1", doc "specs/y.md" "y.h1"])
            (Map.elems (gEdges twoDirGraph)
              ++ [ edgeOf References c d | c <- ["a1", "a2", "b3"], d <- ["x.h1", "y.h1"] ])
          step4@(comm4, _, q4) = runClustering bunch res twoDirGraph
          (comm5, _, q5) = stepFiveClustering bunch res step4 withDocs
      foldToFileMDG UnitWeights withDocs `shouldBe` foldToFileMDG UnitWeights twoDirGraph
      comm5 `shouldBe` comm4
      (cqMQ q5, cqModularity q5, cqIntraFraction q5) `shouldBe` (cqMQ q4, cqModularity q4, cqIntraFraction q4)

    it "doc files left out of the MDG join the module they document, or one of their own" $ do
      let doc f nid = (nodeIn f nid) { nodeFileType = DocFile }
          g = buildGraph False $ extractionFromLists
            (Map.elems (gNodes twoDirGraph) ++ [doc "docs/a.md" "da", doc "docs/lone.md" "dl"])
            (Map.elems (gEdges twoDirGraph) ++ [edgeOf Documents "da" "a1", edgeOf Documents "da" "a2"])
          (comm, _, _) = runClustering bunch res g
          cidOf nid = [ cid | (cid, ms) <- Map.toList comm, nid `elem` ms ]
      cidOf "da" `shouldBe` cidOf "a1"
      length (cidOf "dl") `shouldBe` 1
      cidOf "dl" `shouldNotBe` cidOf "a1"
      cidOf "dl" `shouldNotBe` cidOf "b1"

    it "Leiden quality still covers every edge" $ do
      let (_, _, q) = runClustering Leiden res twoDirGraph
      cqScoredRelations q `shouldBe` Nothing

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

-- ───────────────────────────────────────────────
-- Agglomerative dispatch and the file cap (2.5, 3.2)
-- ───────────────────────────────────────────────

agglomerativeSpec :: Spec
agglomerativeSpec = do
  describe "runClustering (AgglomerativeAlgorithm ...)" $ do
    it "clusters files and pushes the file partition to every symbol node" $ do
      let settings = defaultAgglomerativeSettings
            { asDistance = Cosine, asLinkage = Single, asDivisor = 1 }
          (comm, _, q) = runClustering (AgglomerativeAlgorithm settings) res twoDirGraph
          files = [ "a/1.ts", "a/2.ts", "a/3.ts", "b/1.ts", "b/2.ts", "b/3.ts" ]
          fileComm f = [ cid | (cid, nids) <- Map.toList comm
                             , nid <- nids
                             , Just nd <- [Map.lookup nid (gNodes twoDirGraph)]
                             , let file = T.unpack (toText (nodeSourceFile nd))
                             , file == f ]
      cqAlgorithm q `shouldBe` "agglomerative"
      sort (concat (Map.elems comm)) `shouldBe` sort (Map.keys (gNodes twoDirGraph))
      -- every symbol of a file shares one cluster
      map (length . nub . fileComm) files `shouldBe` replicate 6 1

    it "skips above --agglomerative-max-files and reports the cap, running Leiden" $ do
      let settings = defaultAgglomerativeSettings { asMaxFiles = 2 }
          (comm, _, q) = runClustering (AgglomerativeAlgorithm settings) res twoDirGraph
      cqAlgorithm q `shouldBe` "leiden"
      cqSkip q `shouldBe` Just (BunchSkip 6 2)
      comm `shouldBe` fst (clusterGraphWithResolution twoDirGraph res)

    it "is deterministic: same graph, same configuration, same partition" $ do
      let settings = defaultAgglomerativeSettings { asDistance = Euclidean, asLinkage = Average, asDivisor = 1 }
      fst3 (runClustering (AgglomerativeAlgorithm settings) res twoDirGraph)
        `shouldBe` fst3 (runClustering (AgglomerativeAlgorithm settings) res twoDirGraph)

  describe "clusterAlgorithmFromConfig (agglomerative and auto)" $ do
    it "maps the agglomerative block to AgglomerativeSettings" $
      clusterAlgorithmFromConfig defaultClusteringConfig
        { ccAlgorithm = AlgoAgglomerative
        , ccAgglomerative = defaultAgglomerativeFileConfig
            { afDistance = DistanceManhattan, afLinkage = LinkageAverage, afDivisor = 10, afMaxFiles = 123 } }
        `shouldBe` AgglomerativeAlgorithm defaultAgglomerativeSettings
          { asDistance = Manhattan, asLinkage = Average, asDivisor = 10, asMaxFiles = 123 }

    it "maps auto to AutoAlgorithm carrying both algorithm families" $ do
      let cfg = defaultClusteringConfig { ccAlgorithm = AlgoAuto, ccSeed = 3 }
      case clusterAlgorithmFromConfig cfg of
        AutoAlgorithm au -> do
          bsConfig (auBunch au) `shouldBe` defaultBunchConfig { bcSeed = 3 }
          auAgglomerative au `shouldBe` defaultAgglomerativeSettings
        other -> expectationFailure ("expected AutoAlgorithm, got " ++ show other)

  where
    fst3 (a, _, _) = a
