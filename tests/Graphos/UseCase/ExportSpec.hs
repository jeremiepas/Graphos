module Graphos.UseCase.ExportSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.IORef (newIORef, readIORef, modifyIORef)
import System.IO.Temp (withSystemTempDirectory)

import Graphos.Domain.Types hiding (PushMode(..))
import Graphos.Domain.Graph (buildGraph)
import Graphos.UseCase.Port.ExportPort
import Graphos.UseCase.Export (exportAll)

spec :: Spec
spec = describe "exportAll" $ do
  it "passes mLabels to epExportHTML" $ do
    withSystemTempDirectory "graphos-export-test" $ \tmpDir -> do
      ref <- newIORef Nothing
      let g = buildGraph False (extractionFromLists [] [])
          analysis = Analysis Map.empty Map.empty [] [] []
          detection = Detection 0 0 True Nothing Map.empty
          labels = Just (Map.fromList [(1 :: Int, T.pack "Auth")])
          port = ExportPort
            { epExportHTML = \_g _analysis mLbl _path -> modifyIORef ref (const (Just mLbl))
            , epExportObsidian = \_ _ _ -> pure ()
            , epExportReport = \_ _ -> pure ()
            , epExportCypher = \_ _ -> pure ()
            , epExportMemgraphCypher = \_ _ -> pure ()
            , epPushToNeo4jFull = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToNeo4jSubgraph = \_ _ _ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToNeo4jCommunity = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushEdgeRepair = \_ _ _ _ -> pure ("", 0, 0)
            , epPushToMemgraphFull = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToMemgraphSubgraph = \_ _ _ _ _ _ _ _ -> pure ("", 0, 0)
            , epPushToMemgraphCommunity = \_ _ _ _ _ _ -> pure ("", 0, 0)
            , epOpenIncrementalWriter = \_ -> pure undefined
            , epWriteNodes = \_ _ -> pure ()
            , epWriteEdges = \_ _ -> pure ()
            , epWriteCommunities = \_ _ -> pure ()
            , epWriteCohesion = \_ _ -> pure ()
            , epWriteGodNodes = \_ _ -> pure ()
            , epWriteAnalysisTail = \_ _ -> pure ()
            , epWriteCommunityAggregates = \_ _ -> pure ()
            , epWriteCompositions = \_ _ -> pure ()
            , epFlushWriter = \_ -> pure ()
            , epCloseWriter = \_ -> pure ()
            , epExportCommunityGraph = \_ _ _ -> pure ()
            , epSaveCheckpoint = \_ _ -> pure ()
            , epExportAll = undefined
            }
          config = defaultConfig { cfgOutputDir = tmpDir, cfgNoViz = False }
      _ <- exportAll port g analysis config detection labels
      recorded <- readIORef ref
      recorded `shouldBe` Just labels
