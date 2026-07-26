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
