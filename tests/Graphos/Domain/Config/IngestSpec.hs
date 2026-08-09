{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Domain.Config.IngestSpec where

import Test.Hspec
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Yaml as Yaml

import Graphos.Domain.Config
import Graphos.Domain.Config.Ingest
import Graphos.Domain.Types.Pipeline (FileCategory(..))
import Graphos.UseCase.Ingest (resolveEmbedForCategory, resolveGranularityForCategory)

spec :: Spec
spec = do
  describe "IngestConfig defaults" $ do
    it "has embed False for backward compatibility" $
      icEmbed defaultIngestConfig `shouldBe` False

    it "has merge True by default" $
      icMerge defaultIngestConfig `shouldBe` True

    it "has deduplicate True by default" $
      icDeduplicate defaultIngestConfig `shouldBe` True

    it "uses ingest-optimized cluster defaults" $ do
      icResolution defaultIngestConfig `shouldBe` 0.8
      icMinCommSize defaultIngestConfig `shouldBe` 2
      icMaxLeidenIter defaultIngestConfig `shouldBe` 20

  describe "IngestConfig JSON round-trip" $ do
    it "round-trips full config" $ do
      let cfg = defaultIngestConfig
            { icEmbed = True
            , icEmbedModel = Just "nomic-embed-text"
            , icEmbedDimension = Just 768
            , icResolution = 0.7
            }
      decode (encode cfg) `shouldBe` Just cfg

    it "round-trips with per-category overrides" $ do
      let cfg = defaultIngestConfig
            { icCategories = defaultIngestCategories
                { icatCode = Just defaultIngestCategoryConfig { iccEmbed = Just True }
                , icatImage = Just defaultIngestCategoryConfig { iccEmbed = Just False }
                }
            }
      decode (encode cfg) `shouldBe` Just cfg

  describe "IngestConfig YAML parsing" $ do
    it "parses an empty ingest section as defaults" $ do
      let yaml = "ingest: {}" :: BS8.ByteString
      case Yaml.decodeEither' yaml of
        Left err -> expectationFailure $ show err
        Right (cfg :: IngestConfig) -> do
          icEmbed cfg `shouldBe` False
          icMerge cfg `shouldBe` True
          icDeduplicate cfg `shouldBe` True

    it "parses all ingest fields" $ do
      let yaml = BS8.unlines
            [ "embed: true"
            , "embed_model: nomic-embed-text"
            , "embed_dimension: 768"
            , "merge: false"
            , "deduplicate: false"
            , "resolution: 0.7"
            , "min_comm_size: 3"
            , "max_leiden_iter: 10"
            , "index_path: custom-index.json"
            , "url:"
            , "  timeout: 60"
            , "  user_agent: custom-agent"
            , "  retry: 3"
            , "categories:"
            , "  code:"
            , "    embed: true"
            , "    granularity: fine"
            ]
      case Yaml.decodeEither' yaml of
        Left err -> expectationFailure $ show err
        Right (cfg :: IngestConfig) -> do
          icEmbed cfg `shouldBe` True
          icEmbedModel cfg `shouldBe` Just "nomic-embed-text"
          icEmbedDimension cfg `shouldBe` Just 768
          icMerge cfg `shouldBe` False
          icDeduplicate cfg `shouldBe` False
          icResolution cfg `shouldBe` 0.7
          icMinCommSize cfg `shouldBe` 3
          icMaxLeidenIter cfg `shouldBe` 10
          icIndexPath cfg `shouldBe` "custom-index.json"
          iucTimeout (icUrl cfg) `shouldBe` 60
          iucUserAgent (icUrl cfg) `shouldBe` "custom-agent"
          iucRetry (icUrl cfg) `shouldBe` 3
          resolveEmbedForCategory False (icCategories cfg) CodeFiles `shouldBe` True
          resolveGranularityForCategory GranularityFunction (icCategories cfg) CodeFiles `shouldBe` GranularityFine

  describe "mergeIngestConfig" $ do
    it "project overrides global when non-default" $ do
      let global = defaultIngestConfig { icEmbed = True }
          project = defaultIngestConfig { icEmbed = False }
      icEmbed (mergeIngestConfig global project) `shouldBe` False

    it "project always wins over global regardless of value" $ do
      let global = defaultIngestConfig { icEmbed = True }
          project = defaultIngestConfig
      icEmbed (mergeIngestConfig global project) `shouldBe` False

    it "merges Maybe fields with project override" $ do
      let global = defaultIngestConfig { icEmbedModel = Just "global-model" }
          project = defaultIngestConfig { icEmbedModel = Just "project-model" }
      icEmbedModel (mergeIngestConfig global project) `shouldBe` Just "project-model"

    it "falls back to global when project Maybe is Nothing" $ do
      let global = defaultIngestConfig { icEmbedModel = Just "global-model" }
          project = defaultIngestConfig
      icEmbedModel (mergeIngestConfig global project) `shouldBe` Just "global-model"

  describe "mergeGraphosConfig ingest" $ do
    it "threads gcIngest through merge" $ do
      let global = defaultGraphosConfig { gcIngest = defaultIngestConfig { icEmbed = True } }
          project = defaultGraphosConfig { gcIngest = defaultIngestConfig { icEmbed = False } }
      icEmbed (gcIngest (mergeGraphosConfig global project)) `shouldBe` False

  describe "Category resolution" $ do
    it "inherits top-level when category config is absent" $ do
      resolveEmbedForCategory True defaultIngestCategories CodeFiles `shouldBe` True
      resolveEmbedForCategory False defaultIngestCategories DocFiles `shouldBe` False

    it "overrides top-level when category sets explicit value" $ do
      let cats = defaultIngestCategories { icatCode = Just defaultIngestCategoryConfig { iccEmbed = Just True } }
      resolveEmbedForCategory False cats CodeFiles `shouldBe` True

    it "inherits when category config has Nothing field" $ do
      let cats = defaultIngestCategories { icatCode = Just defaultIngestCategoryConfig { iccEmbed = Nothing } }
      resolveEmbedForCategory True cats CodeFiles `shouldBe` True
      resolveEmbedForCategory False cats CodeFiles `shouldBe` False

    it "granularity inherits and overrides correctly" $ do
      let cats = defaultIngestCategories
            { icatDoc = Just defaultIngestCategoryConfig { iccGranularity = Just GranularityFile }
            }
      resolveGranularityForCategory GranularityFunction cats DocFiles `shouldBe` GranularityFile
      resolveGranularityForCategory GranularityFunction cats CodeFiles `shouldBe` GranularityFunction
