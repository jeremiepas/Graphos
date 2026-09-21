{-# LANGUAGE OverloadedStrings #-}
module Graphos.Domain.ConfigSpec where

import Test.Hspec
import Data.Aeson (decode, encode, eitherDecode)
import qualified Data.Map.Strict as Map

import Graphos.Domain.Config

spec :: Spec
spec = do
  describe "EmbeddingConfig batchSize/concurrency (streaming-embeddings)" $ do
    it "defaults batchSize to 64 and concurrency to 1 when keys are absent" $ do
      decode "{\"enabled\": true}" `shouldBe` Just
        (defaultEmbeddingConfig { embEnabled = True })

    it "parses explicit batchSize and concurrency values" $ do
      let cfg = defaultEmbeddingConfig { embBatchSize = 128, embConcurrency = 4 }
      decode "{\"batchSize\": 128, \"concurrency\": 4}" `shouldBe` Just cfg

    it "validation accepts the defaults" $
      validateEmbeddingConfig defaultEmbeddingConfig `shouldBe` Right ()

    it "validation accepts explicit valid values" $
      validateEmbeddingConfig defaultEmbeddingConfig { embBatchSize = 1, embConcurrency = 8 }
        `shouldBe` Right ()

    it "validation rejects batchSize below 1, naming the key" $
      validateEmbeddingConfig defaultEmbeddingConfig { embBatchSize = 0 }
        `shouldBe` Left ("embedding.batchSize", 0)

    it "validation rejects concurrency below 1, naming the key" $
      validateEmbeddingConfig defaultEmbeddingConfig { embConcurrency = 0 }
        `shouldBe` Left ("embedding.concurrency", 0)

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
      let cfg = SemanticEdgesConfig False 10 0.7 6 [".hs", ".rs"]
      decode (encode cfg) `shouldBe` Just cfg

    it "serializes to snake_case keys" $ do
      encode defaultSemanticEdgesConfig `shouldBe`
        "{\"enabled\":true,\"max_fan_out\":50,\"threshold\":0.5,\"min_ident_length\":4,\"path_extensions\":[\".ts\",\".tsx\",\".js\",\".jsx\",\".mjs\",\".cjs\",\".hs\",\".lhs\",\".rs\",\".py\",\".pyx\",\".go\",\".java\",\".kt\",\".scala\",\".sc\",\".kts\",\".rb\",\".php\",\".cpp\",\".cc\",\".cxx\",\".c++\",\".hpp\",\".hh\",\".h\",\".cs\",\".swift\",\".m\",\".mm\",\".ml\",\".mli\",\".ex\",\".exs\",\".erl\",\".clj\",\".sh\",\".bash\",\".yaml\",\".yml\",\".toml\",\".json\",\".md\",\".markdown\"]}"

    it "parses explicit values" $ do
      decode "{\"enabled\":false,\"max_fan_out\":10,\"threshold\":0.7}" `shouldBe`
        Just (SemanticEdgesConfig False 10 0.7 4 defaultPathExtensions)

    it "defaults a missing section to enabled/50/0.5" $ do
      decode "{}" `shouldBe` Just defaultSemanticEdgesConfig

    it "fills partial keys with defaults" $ do
      decode "{\"enabled\":false}" `shouldBe` Just (SemanticEdgesConfig False 50 0.5 4 defaultPathExtensions)
