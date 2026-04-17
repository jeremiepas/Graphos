module Graphos.UseCase.PipelineSpec where

import Test.Hspec

import Graphos.Domain.Types

spec :: Spec
spec = do
  describe "PipelineConfig" $ do
    it "has sensible defaults" $ do
      let cfg = defaultConfig
      cfgInputPath cfg `shouldBe` "."
      cfgOutputDir cfg `shouldBe` "graphos-out"
      cfgDirected cfg `shouldBe` False
      cfgNoViz cfg `shouldBe` False