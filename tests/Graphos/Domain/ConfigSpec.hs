{-# LANGUAGE OverloadedStrings #-}
module Graphos.Domain.ConfigSpec where

import Test.Hspec
import Data.Aeson (decode, encode, eitherDecode, eitherDecodeStrict', Value(..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Graphos.Domain.Config
import Graphos.Domain.Config.Clustering
import Graphos.Domain.Types.Edge (Relation(..))
import Graphos.Domain.Types.Graph (Extraction(..), ExtractionError(..), emptyExtraction)

spec :: Spec
spec = do
  describe "EmbeddingConfig batchSize/concurrency (streaming-embeddings)" $ do
    it "defaults batchSize to 64 and concurrency to 1 when keys are absent" $ do
      decode "{\"enabled\": true}" `shouldBe` Just
        (defaultEmbeddingConfig { embEnabled = True })

    it "parses explicit batchSize and concurrency values" $ do
      let cfg = defaultEmbeddingConfig { embBatchSize = 128, embConcurrency = 4 }
      decode "{\"batchSize\": 128, \"concurrency\": 4}" `shouldBe` Just cfg

    it "defaults maxTokens to 512 when the key is absent" $
      decode "{\"enabled\": true}" `shouldBe` Just
        (defaultEmbeddingConfig { embEnabled = True })

    it "reflects the maxTokens default value" $
      embMaxTokens defaultEmbeddingConfig `shouldBe` 512

    it "parses an explicit maxTokens value" $ do
      let cfg = defaultEmbeddingConfig { embMaxTokens = 256 }
      decode "{\"maxTokens\": 256}" `shouldBe` Just cfg

    it "parses maxTokens 0 (truncation disabled)" $ do
      let cfg = defaultEmbeddingConfig { embMaxTokens = 0 }
      decode "{\"maxTokens\": 0}" `shouldBe` Just cfg

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

  describe "EmbeddingConfig prefixes (lfm-embedding-optimization 1.2)" $ do
    it "defaults docPrefix/queryPrefix to empty when keys are absent" $
      decode "{\"enabled\": true}" `shouldBe` Just
        (defaultEmbeddingConfig { embEnabled = True })

    it "parses explicit docPrefix and queryPrefix values" $ do
      let cfg = defaultEmbeddingConfig
                  { embDocPrefix = "document: ", embQueryPrefix = "query: " }
      decode "{\"docPrefix\": \"document: \", \"queryPrefix\": \"query: \"}"
        `shouldBe` Just cfg

    it "parses maxTokens 0 with model-default limit semantics" $ do
      let cfg = defaultEmbeddingConfig { embMaxTokens = 0 }
      decode "{\"maxTokens\": 0}" `shouldBe` Just cfg

    it "validation rejects maxTokens below 0, naming the key" $
      validateEmbeddingConfig defaultEmbeddingConfig { embMaxTokens = -1 }
        `shouldBe` Left ("embedding.maxTokens", -1)

  describe "EmbeddingConfig streaming (lfm-embedding-optimization 3.3)" $ do
    it "defaults streaming to true when the key is absent" $
      decode "{\"enabled\": true}" `shouldBe` Just
        (defaultEmbeddingConfig { embEnabled = True })

    it "parses an explicit streaming: false" $ do
      let cfg = defaultEmbeddingConfig { embStreaming = False }
      decode "{\"streaming\": false}" `shouldBe` Just cfg

    it "parses an explicit streaming: true" $ do
      let cfg = defaultEmbeddingConfig { embStreaming = True }
      decode "{\"streaming\": true}" `shouldBe` Just cfg

  describe "EmbeddingConfig defaults (lfm-embedding-optimization 6.1)" $ do
    it "defaults the model to the LFM2.5 GGUF reference" $
      embModel defaultEmbeddingConfig
        `shouldBe` "hf.co/LiquidAI/LFM2.5-Embedding-350M-GGUF:Q4_K_M"

    it "defaults concurrency to the benchmark-pinned 1" $
      embConcurrency defaultEmbeddingConfig `shouldBe` 1

    it "explicit nomic config still parses unchanged (AC: explicit nomic unchanged)" $ do
      let cfg = defaultEmbeddingConfig { embModel = "nomic-embed-text" }
      decode "{\"model\": \"nomic-embed-text\"}" `shouldBe` Just cfg

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
      -- Key order in the encoded object is HashMap-accidental (aeson's KeyMap);
      -- assert the shape through a decoded map instead of raw bytes.
      case decode (encode defaultSemanticEdgesConfig) of
        Just (Object km) -> do
          KeyMap.keys km `shouldMatchList`
            [ "enabled", "max_fan_out", "threshold", "min_ident_length", "path_extensions" ]
        _ -> expectationFailure "expected a decodable object"

    it "parses explicit values" $ do
      decode "{\"enabled\":false,\"max_fan_out\":10,\"threshold\":0.7}" `shouldBe`
        Just (SemanticEdgesConfig False 10 0.7 4 defaultPathExtensions)

    it "defaults a missing section to enabled/50/0.5" $ do
      decode "{}" `shouldBe` Just defaultSemanticEdgesConfig

    it "fills partial keys with defaults" $ do
      decode "{\"enabled\":false}" `shouldBe` Just (SemanticEdgesConfig False 50 0.5 4 defaultPathExtensions)

  describe "ClusteringConfig algorithm keys (bunch-search-based-modularization 6.1)" $ do
    it "defaults to leiden, seed 0 and the default Bunch block" $
      decode "{}" `shouldBe` Just defaultClusteringConfig

    it "decodes clustering.algorithm, clustering.seed and clustering.bunch.*" $
      decode "{\"algorithm\":\"bunch\",\"seed\":3,\"bunch\":{\"population\":4,\"threshold\":50,\
             \\"annealing\":{\"enabled\":true,\"t0\":50,\"alpha\":0.9},\"max_iterations\":200,\
             \\"max_files\":1000,\"max_files_per_run\":300,\"relations\":[\"imports\",\"calls\"],\"weighted\":true}}"
        `shouldBe` Just defaultClusteringConfig
          { ccAlgorithm = AlgoBunch, ccSeed = 3
          , ccBunch = defaultBunchFileConfig
              { bfPopulation = 4, bfThreshold = 50, bfAnnealing = Just (50, 0.9), bfMaxIterations = 200
              , bfMaxFiles = 1000, bfMaxFilesPerRun = 300, bfRelations = [Imports, Calls], bfWeighted = True } }

    it "defaults to the real-projects limits and the code-dependency allowlist (bunch-real-projects)" $ do
      bfMaxFiles defaultBunchFileConfig `shouldBe` 50000
      bfMaxFilesPerRun defaultBunchFileConfig `shouldBe` 500
      bfRelations defaultBunchFileConfig `shouldBe` [Imports, Calls, References, Extends, Implements]

    it "rejects an unknown relation name in clustering.bunch.relations" $
      (eitherDecode "{\"bunch\":{\"relations\":[\"imports\",\"frobs\"]}}" :: Either String ClusteringConfig)
        `shouldSatisfy` either (const True) (const False)

    it "validates the allowlist and the per-run size" $ do
      let withB f = defaultClusteringConfig { ccBunch = f defaultBunchFileConfig }
          rejected = either (const True) (const False)
      validClusteringConfig (withB (\b -> b { bfRelations = [Imports, DependsOn] })) `shouldSatisfy` rejected
      validClusteringConfig (withB (\b -> b { bfRelations = [Documents] })) `shouldSatisfy` rejected
      validClusteringConfig (withB (\b -> b { bfRelations = [] })) `shouldSatisfy` rejected
      validClusteringConfig (withB (\b -> b { bfMaxFilesPerRun = 1 })) `shouldSatisfy` rejected
      validClusteringConfig (withB (\b -> b { bfRelations = [Imports, Extends] })) `shouldSatisfy` either (const False) (const True)

    it "accepts camelCase multi-word keys and defaults the annealing schedule" $
      fmap ccBunch (decode "{\"bunch\":{\"maxIterations\":7,\"maxFiles\":9,\"annealing\":{\"enabled\":true}}}")
        `shouldBe` Just defaultBunchFileConfig
          { bfMaxIterations = 7, bfMaxFiles = 9, bfAnnealing = Just defaultAnnealing }

    it "rejects an unknown algorithm" $
      (eitherDecode "{\"algorithm\":\"louvain\"}" :: Either String ClusteringConfig)
        `shouldSatisfy` either (const True) (const False)

    it "lets the CLI override the config (threshold 50 in yaml, --bunch-threshold 0)" $ do
      let yaml = defaultClusteringConfig { ccBunch = defaultBunchFileConfig { bfThreshold = 50 } }
          eff  = applyClusteringOverrides noClusteringOverrides { covThreshold = Just 0 } yaml
      bfThreshold (ccBunch eff) `shouldBe` 0
      bfThreshold (ccBunch (applyClusteringOverrides noClusteringOverrides yaml)) `shouldBe` 50

    it "validates the Bunch ranges" $ do
      let withB f = defaultClusteringConfig { ccBunch = f defaultBunchFileConfig }
      validClusteringConfig (withB (\b -> b { bfThreshold = 101 })) `shouldSatisfy` either (const True) (const False)
      validClusteringConfig (withB (\b -> b { bfPopulation = 0 })) `shouldSatisfy` either (const True) (const False)
      validClusteringConfig (withB (\b -> b { bfAnnealing = Just (100, 1) })) `shouldSatisfy` either (const True) (const False)
      validClusteringConfig defaultClusteringConfig `shouldBe` Right defaultClusteringConfig

    it "merges a non-default project algorithm over the global one" $ do
      let project = defaultClusteringConfig { ccAlgorithm = AlgoBunch }
          global  = defaultClusteringConfig { ccSeed = 5 }
          merged  = mergeClusteringConfig project global
      (ccAlgorithm merged, ccSeed merged) `shouldBe` (AlgoBunch, 5)

  describe "custom extractors domain schema (avi-1766 1.1-1.4)" $ do
    it "maps the command extractor mode to and from JSON" $ do
      (decode "\"command\"" :: Maybe ExtractorMode) `shouldBe` Just ExtractCommand
      encode ExtractCommand `shouldBe` "\"command\""
      (decode "\"tree-sitter\"" :: Maybe ExtractorMode) `shouldBe` Just ExtractTreeSitter

    it "parses a command entry with argv, batch and timeout" $
      (eitherDecode "{\"mode\":\"command\",\"command\":[\"perl-graphos-extract\",\"{file}\"],\"batch\":true,\"timeout\":30}"
        :: Either String ExtractorConfig)
        `shouldBe` Right ExtractorConfig
          { ecMode = ExtractCommand
          , ecGrammar = Nothing
          , ecLanguageId = Nothing
          , ecGranularity = Nothing
          , ecCommand = Just ["perl-graphos-extract", "{file}"]
          , ecBatch = Just True
          , ecTimeoutSeconds = Just 30
          , ecGrammarPath = Nothing
          , ecQueries = Nothing
          }

    it "parses runtime-grammar fields from snake_case keys" $
      fmap (\ec -> (ecGrammarPath ec, ecQueries ec))
        (decode "{\"mode\":\"tree-sitter\",\"grammar_path\":\"php.so\",\"queries\":\"q/\"}"
          :: Maybe ExtractorConfig)
        `shouldBe` Just (Just "php.so", Just "q/")

    it "round-trips an extractor entry through encode/decode" $ do
      let ec = ExtractorConfig ExtractTreeSitter (Just "php") Nothing Nothing
                 Nothing Nothing Nothing (Just "php.so") (Just "q/")
      (decode (encode ec) :: Maybe ExtractorConfig) `shouldBe` Just ec

    it "maps every resolve kind to and from JSON" $ do
      let kinds :: [(ResolveKind, String)]
          kinds = [ (ResolvePackage, "package"), (ResolvePSR4, "psr4")
                  , (ResolveModulePath, "module-path"), (ResolvePathPrefix, "path-prefix") ]
      mapM_ (\(k, s) -> do
               encode k `shouldBe` encode s
               (decode (encode k) :: Maybe ResolveKind) `shouldBe` Just k) kinds

    it "parses a package resolve entry with roots" $
      (eitherDecode "{\"kind\":\"package\",\"roots\":[\"src/main/java\"]}" :: Either String ResolveConfig)
        `shouldBe` Right (ResolveConfig ResolvePackage ["src/main/java"] Nothing)

    it "rejects an unknown resolve kind" $
      (eitherDecode "{\"kind\":\"frobnicate\"}" :: Either String ResolveConfig)
        `shouldSatisfy` either (const True) (const False)

    it "carries extraction errors on an Extraction and round-trips them" $ do
      let err = ExtractionError "a.pl" "command" "malformed JSON"
          ex = emptyExtraction { extractionErrors = [err] }
      (decode (encode ex) :: Maybe Extraction) `shouldBe` Just ex

    it "defaults extraction errors to [] when the key is absent" $
      (decode "{\"nodes\":{},\"edges\":{}}" :: Maybe Extraction)
        `shouldBe` Just emptyExtraction

  describe "custom extractors config validation (avi-1766 2.1-2.2)" $ do
    it "rejects a command entry without command, naming extension and field" $ do
      let errs = sectionErrors "{\"extractors\":{\".x\":{\"mode\":\"command\"}}}"
      errs `shouldSatisfy` (not . null)
      concat errs `shouldContain` "\".x\""
      concat errs `shouldContain` "command"

    it "rejects an empty command argv" $
      sectionErrors "{\"extractors\":{\".x\":{\"mode\":\"command\",\"command\":[]}}}"
        `shouldSatisfy` (not . null)

    it "rejects an empty argv element" $
      sectionErrors "{\"extractors\":{\".x\":{\"mode\":\"command\",\"command\":[\"extract\",\"\"]}}}"
        `shouldSatisfy` (not . null)

    it "rejects tree-sitter grammar_path under mode command" $ do
      let errs = sectionErrors
            "{\"extractors\":{\".x\":{\"mode\":\"command\",\"command\":[\"e\"],\"grammar_path\":\"g.so\"}}}"
      errs `shouldSatisfy` (not . null)
      concat errs `shouldContain` "grammar_path"

    it "rejects a tree-sitter entry without grammar or grammar_path" $ do
      let errs = sectionErrors "{\"extractors\":{\".x\":{\"mode\":\"tree-sitter\"}}}"
      errs `shouldSatisfy` (not . null)
      concat errs `shouldContain` "grammar"

    it "rejects a command field under mode tree-sitter" $ do
      let errs = sectionErrors
            "{\"extractors\":{\".x\":{\"mode\":\"tree-sitter\",\"grammar\":\"perl\",\"command\":[\"e\"]}}}"
      concat errs `shouldContain` "command"

    it "rejects a non-positive command timeout" $ do
      let errs = sectionErrors
            "{\"extractors\":{\".x\":{\"mode\":\"command\",\"command\":[\"e\"],\"timeout\":0}}}"
      concat errs `shouldContain` "timeout"

    it "accepts a valid command entry" $
      sectionErrors
        "{\"extractors\":{\".pl\":{\"mode\":\"command\",\"command\":[\"perl-graphos-extract\",\"{file}\"],\"batch\":true,\"timeout\":30}}}"
        `shouldBe` []

    it "accepts a runtime tree-sitter entry" $
      sectionErrors
        "{\"extractors\":{\".php\":{\"mode\":\"tree-sitter\",\"grammar_path\":\"php.so\",\"queries\":\"q/\"}}}"
        `shouldBe` []

    it "rejects an unknown extractor mode naming extension and mode" $ do
      let errs = sectionErrors "{\"extractors\":{\".x\":{\"mode\":\"frobnicate\"}}}"
      errs `shouldSatisfy` (not . null)
      concat errs `shouldContain` ".x"
      concat errs `shouldContain` "frobnicate"

    it "rejects an unknown resolve kind naming extension and kind" $ do
      let errs = sectionErrors "{\"resolve\":{\".x\":{\"kind\":\"frobnicate\"}}}"
      errs `shouldSatisfy` (not . null)
      concat errs `shouldContain` ".x"
      concat errs `shouldContain` "frobnicate"

    it "rejects a package resolve entry without roots" $ do
      let errs = sectionErrors "{\"resolve\":{\".java\":{\"kind\":\"package\"}}}"
      concat errs `shouldContain` "roots"

    it "rejects a module-path resolve entry without source_of_truth" $ do
      let errs = sectionErrors "{\"resolve\":{\".kt\":{\"kind\":\"module-path\"}}}"
      concat errs `shouldContain` "source_of_truth"

    it "accepts valid resolve entries for every kind" $
      sectionErrors
        "{\"resolve\":{\
        \\".java\":{\"kind\":\"package\",\"roots\":[\"src/main/java\"]},\
        \\".php\":{\"kind\":\"psr4\",\"roots\":[\"src\"]},\
        \\".kt\":{\"kind\":\"module-path\",\"source_of_truth\":\"settings.gradle\"},\
        \\".go\":{\"kind\":\"path-prefix\"}}}"
        `shouldBe` []

    it "accepts a config without extractors/resolve sections" $
      sectionErrors "{}" `shouldBe` []

    it "reports every offending entry, not just the first" $ do
      let errs = sectionErrors
            "{\"extractors\":{\".a\":{\"mode\":\"command\"},\".b\":{\"mode\":\"tree-sitter\"}}}"
      length errs `shouldBe` 2

    it "accepts every built-in extractor through the typed validator" $
      mapM_ (\(ext, ec) -> validateExtractorEntry ext ec `shouldBe` Right ())
            (Map.toList defaultExtractors)

-- | Errors the loader would report for a raw config document.
sectionErrors :: BS.ByteString -> [String]
sectionErrors json =
  case eitherDecodeStrict' json :: Either String Value of
    Right v -> configSectionErrors v
    Left e  -> ["<invalid test JSON: " ++ e ++ ">"]
