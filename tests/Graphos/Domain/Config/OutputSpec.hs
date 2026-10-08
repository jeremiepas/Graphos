-- | Output-directory resolution tests (multi-source-graphs 2.2).
module Graphos.Domain.Config.OutputSpec where

import Test.Hspec

import Graphos.Domain.Config
    ( GraphosConfig(..)
    , defaultGraphosConfig
    , effectiveOutputDir
    )
import Graphos.Domain.Config.Output (defaultOutputDirName, resolveOutputDir)

spec :: Spec
spec = do
  describe "resolveOutputDir (CLI -o wins over config output:)" $ do
    it "CLI -o given wins over the configured output key" $
      resolveOutputDir True "cli-out" (Just "my-graph-out") `shouldBe` "cli-out"

    it "CLI -o given wins even when it repeats the default name" $
      resolveOutputDir True "graphos-out" (Just "my-graph-out")
        `shouldBe` "graphos-out"

    it "no CLI -o uses the configured output key" $
      resolveOutputDir False "graphos-out" (Just "my-graph-out")
        `shouldBe` "my-graph-out"

    it "no CLI -o and no config key falls back to the canonical default" $
      resolveOutputDir False "graphos-out" Nothing `shouldBe` "graphos-out"

    it "the canonical default is graphos-out" $
      defaultOutputDirName `shouldBe` "graphos-out"

  describe "effectiveOutputDir (loaded config → output directory)" $ do
    it "defaults to graphos-out when gcOutput is Nothing" $
      effectiveOutputDir defaultGraphosConfig `shouldBe` "graphos-out"

    it "uses the configured output key when set" $
      effectiveOutputDir defaultGraphosConfig { gcOutput = Just "my-graph-out" }
        `shouldBe` "my-graph-out"