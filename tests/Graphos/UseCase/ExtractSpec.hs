module Graphos.UseCase.ExtractSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map

import Graphos.Domain.Types
import Graphos.UseCase.Extract (resolveGranularity)

spec :: Spec
spec = do
  describe "Extraction" $ do
    it "emptyExtraction has zero nodes and edges" $ do
      Map.size (extractionNodes emptyExtraction) `shouldBe` 0
      Map.size (extractionEdges emptyExtraction) `shouldBe` 0

  describe "resolveGranularity (resolution order)" $ do
    let gcfgWithJson = defaultGraphosConfig  -- .json has a file-level override by default
        gcfgGlobalFile = defaultGraphosConfig { gcGranularity = GranularityFile }

    it "CLI flag wins over per-extension and global config" $ do
      resolveGranularity (Just GranularityFine) gcfgWithJson ".json" `shouldBe` GranularityFine
      resolveGranularity (Just GranularityFine) gcfgGlobalFile ".ts" `shouldBe` GranularityFine

    it "per-extension override wins over global" $ do
      resolveGranularity Nothing gcfgGlobalFile ".json" `shouldBe` GranularityFile
      resolveGranularity Nothing defaultGraphosConfig ".json" `shouldBe` GranularityFile

    it "global config applies when no CLI or per-extension override" $ do
      resolveGranularity Nothing gcfgGlobalFile ".ts" `shouldBe` GranularityFile

    it "built-in default (function) applies when nothing is set" $ do
      resolveGranularity Nothing defaultGraphosConfig ".ts" `shouldBe` GranularityFunction
      resolveGranularity Nothing defaultGraphosConfig ".unknown" `shouldBe` GranularityFunction
