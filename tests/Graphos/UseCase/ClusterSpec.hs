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
import Graphos.Domain.Community (Resolution(..), MergeStrategy(..), modularity)
import Graphos.Domain.Community.Bunch (BunchConfig(..), defaultBunchConfig, foldToFileMDG, mdgFileCount, mdgEdgeCount, bunchCluster, BunchResult(..), BunchRun(..))
import Graphos.Domain.Community.MQ (Weighting(..))
import Graphos.UseCase.Cluster

spec :: Spec
spec = do
  leidenSpec
  bunchSpec
  capSpec

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
