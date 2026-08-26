{-# LANGUAGE OverloadedStrings #-}
module Graphos.Domain.ConfigSpec where

import Test.Hspec
import Data.Aeson (decode, encode, eitherDecode)
import qualified Data.Map.Strict as Map

import Graphos.Domain.Config

spec :: Spec
spec = do
  describe "Granularity JSON round-trip" $ do
    it "round-trips fine" $
      decode (encode GranularityFine) `shouldBe` Just GranularityFine
    it "round-trips function" $
      decode (encode GranularityFunction) `shouldBe` Just GranularityFunction
    it "round-trips file" $
      decode (encode GranularityFile) `shouldBe` Just GranularityFile
    it "serializes to the documented strings" $ do
      encode GranularityFine `shouldBe` "\"fine\""
      encode GranularityFunction `shouldBe` "\"function\""
      encode GranularityFile `shouldBe` "\"file\""

    it "rejects unknown levels naming the allowed values" $ do
      case eitherDecode "\"statement\"" :: Either String Granularity of
        Left err -> err `shouldContain` "fine, function, or file"
        Right _  -> expectationFailure "expected parse failure for unknown granularity"

  describe "Granularity defaults" $ do
    it "built-in default is function level" $
      defaultGranularity `shouldBe` GranularityFunction

    it "default global config uses the built-in default" $
      gcGranularity defaultGraphosConfig `shouldBe` GranularityFunction

    it "JSON extension defaults to file granularity" $
      (ecGranularity =<< Map.lookup ".json" defaultExtractors) `shouldBe` Just GranularityFile

    it "code extensions have no per-extension override by default" $ do
      (ecGranularity =<< Map.lookup ".ts" defaultExtractors) `shouldBe` Nothing
      (ecGranularity =<< Map.lookup ".hs" defaultExtractors) `shouldBe` Nothing

  describe "Granularity config merging" $ do
    it "project granularity overrides global when non-default" $ do
      let global = defaultGraphosConfig { gcGranularity = GranularityFile }
          project = defaultGraphosConfig { gcGranularity = GranularityFine }
      gcGranularity (mergeGraphosConfig global project) `shouldBe` GranularityFine

    it "global granularity wins when project is at the default" $ do
      let global = defaultGraphosConfig { gcGranularity = GranularityFile }
          project = defaultGraphosConfig
      gcGranularity (mergeGraphosConfig global project) `shouldBe` GranularityFile

  describe "SemanticEdgesConfig JSON" $ do
    it "round-trips the default" $
      decode (encode defaultSemanticEdgesConfig) `shouldBe` Just defaultSemanticEdgesConfig

    it "round-trips a custom config" $ do
      let cfg = SemanticEdgesConfig False 10 0.7
      decode (encode cfg) `shouldBe` Just cfg

    it "serializes to snake_case keys" $ do
      encode defaultSemanticEdgesConfig `shouldBe`
        "{\"enabled\":true,\"max_fan_out\":50,\"threshold\":0.5}"

    it "parses explicit values" $ do
      decode "{\"enabled\":false,\"max_fan_out\":10,\"threshold\":0.7}" `shouldBe`
        Just (SemanticEdgesConfig False 10 0.7)

    it "defaults a missing section to enabled/50/0.5" $ do
      decode "{}" `shouldBe` Just defaultSemanticEdgesConfig

    it "fills partial keys with defaults" $ do
      decode "{\"enabled\":false}" `shouldBe` Just (SemanticEdgesConfig False 50 0.5)
