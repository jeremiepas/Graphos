{-# LANGUAGE OverloadedStrings #-}
-- | @--cluster-only@ persists its result (checkpoint-and-cluster-only-controls).
--
-- The cluster-only entry point loads @graph.checkpoint.json@, re-runs
-- clustering and analysis, and must write the clustered @graph.json@ (with
-- @metadata.graphos.communities@ filled) and, under @--community-graph@,
-- @community_graph.json@. It previously computed the communities, logged
-- their count and exited without writing anything.
module Graphos.UseCase.ClusterOnlySpec (spec) where

import Data.Aeson (Value(..), decode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text.Short (fromText)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Config (defaultOtelConfig)
import Graphos.Infrastructure.Logging (LogLevel(..))
import Graphos.Infrastructure.Observability.SDK (initObservability, ObservabilityEnv(..))
import Graphos.Infrastructure.Wiring (productionAppEnv)
import qualified Graphos.Infrastructure.Export.JSON as ExportJSON
import Graphos.UseCase.Pipeline (runClusterOnlyPipeline)

spec :: Spec
spec = describe "runClusterOnlyPipeline" $
  it "writes graph.json with communities and community_graph.json from the checkpoint" $
    withSystemTempDirectory "graphos-cluster-only" $ \out -> do
      let graph = buildGraph True (extractionFromLists fixtureNodes fixtureEdges)
          graphPath = out </> "graph.json"
          checkpointPath = out </> "graph.checkpoint.json"
      ExportJSON.saveCheckpoint graph checkpointPath (T.pack out)
      obsEnv <- initObservability LogError defaultOtelConfig Nothing (out </> "traces")
      let appEnv = productionAppEnv (otelLogEnv obsEnv) obsEnv
          config = defaultConfig
            { cfgOutputDir = out
            , cfgInputPath = out
            , cfgDirected = True
            , cfgNoViz = True
            , cfgCommunityGraph = True
            , cfgClusterOnly = True
            }
      result <- runClusterOnlyPipeline appEnv config
      result `shouldSatisfy` either (const False) (> 0)
      doesFileExist graphPath `shouldReturn` True
      doesFileExist (out </> "community_graph.json") `shouldReturn` True
      -- The checkpoint is kept so clustering can be re-run with other settings.
      doesFileExist checkpointPath `shouldReturn` True
      bs <- BSL.readFile graphPath
      communityCount (decode bs) `shouldSatisfy` (> 0)

-- | Number of entries under @graph.metadata.graphos.communities@.
communityCount :: Maybe Value -> Int
communityCount mv = maybe 0 size $ do
  Object top <- mv
  Object g   <- KM.lookup (Key.fromText "graph") top
  Object md  <- KM.lookup (Key.fromText "metadata") g
  Object gm  <- KM.lookup (Key.fromText "graphos") md
  KM.lookup (Key.fromText "communities") gm
  where
    size (Object km) = KM.size km
    size _           = 0

-- | Two files, each a module containing four symbols, linked by one import.
fixtureNodes :: [Node]
fixtureNodes =
  [ node nid sf | (nid, sf) <- [("a", "src/a.ts"), ("b", "src/b.ts")] ]
  ++ [ node (m <> "_" <> T.pack (show i)) sf
     | (m, sf) <- [("a", "src/a.ts"), ("b", "src/b.ts")], i <- [1 .. 4 :: Int] ]
  where
    node nid sf = Node nid (fromText nid) CodeFile (fromText sf) Nothing
      (Just 1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

fixtureEdges :: [Edge]
fixtureEdges =
  [ edge m (m <> "_" <> T.pack (show i)) Contains | m <- ["a", "b"], i <- [1 .. 4 :: Int] ]
  ++ [ edge "a" "b" Imports ]
  where
    edge s t r = Edge (EdgeId (s <> "->" <> t)) s t r 1.0 (Confidence 1.0) Nothing
