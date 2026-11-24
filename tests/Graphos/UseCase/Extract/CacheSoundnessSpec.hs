{-# LANGUAGE OverloadedStrings #-}
-- | Cache soundness for custom extractors (custom-extractors 5.5).
--
-- INV-CACHE-SOUND says the extraction cache key must factor through every
-- extraction-affecting configuration value. This suite pins the part of that
-- invariant the custom-extractor work adds:
--
--   * command argv / batch / timeout changes invalidate the fingerprint;
--   * the runtime grammar library and its @.scm@ queries are fingerprinted by
--     on-disk size+mtime, so replacing either invalidates it;
--   * declarative @resolve:@ entries participate;
--   * identical configs fingerprint identically;
--   * and a config change actually turns a saved extraction into a cache miss
--     (no stale cache is served).
--
-- It also pins the dispatch split used by the same step: command-mode files get
-- their own route and runtime-grammar files stay on the tree-sitter route.
module Graphos.UseCase.Extract.CacheSoundnessSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import Graphos.Domain.Config (ResolveConfig(..), ResolveKind(..))
import Graphos.Domain.Types
  ( ExtractorConfig(..)
  , ExtractorMode(..)
  , GraphosConfig(..)
  , PipelineConfig(..)
  , defaultConfig
  , defaultGraphosConfig
  , extractionFromLists
  )
import Graphos.Domain.Graph (makeStubNode)
import Graphos.Infrastructure.Wiring (productionFileSystemPort)
import Graphos.UseCase.Extract.Core
  ( extractionConfigFingerprint, partitionByExtractor )
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..))

-- ───────────────────────────────────────────────
-- Fixtures
-- ───────────────────────────────────────────────

commandExtractor :: [String] -> ExtractorConfig
commandExtractor argv = ExtractorConfig
  { ecMode           = ExtractCommand
  , ecGrammar        = Nothing
  , ecLanguageId     = Nothing
  , ecGranularity    = Nothing
  , ecCommand        = Just argv
  , ecBatch          = Just False
  , ecTimeoutSeconds = Just 60
  , ecGrammarPath    = Nothing
  , ecQueries        = Nothing
  }

runtimeGrammarExtractor :: FilePath -> Maybe FilePath -> ExtractorConfig
runtimeGrammarExtractor grammarPath queries = ExtractorConfig
  { ecMode           = ExtractTreeSitter
  , ecGrammar        = Just "php"
  , ecLanguageId     = Just "php"
  , ecGranularity    = Nothing
  , ecCommand        = Nothing
  , ecBatch          = Nothing
  , ecTimeoutSeconds = Nothing
  , ecGrammarPath    = Just grammarPath
  , ecQueries        = queries
  }

withExtractor :: String -> ExtractorConfig -> GraphosConfig -> GraphosConfig
withExtractor ext ec g = g { gcExtractors = Map.insert ext ec (gcExtractors g) }

withCommandExtractor :: [String] -> GraphosConfig
withCommandExtractor argv = withExtractor ".pl" (commandExtractor argv) defaultGraphosConfig

pipeline :: GraphosConfig -> PipelineConfig
pipeline g = defaultConfig
  { cfgGraphosConfig = g
  , cfgAllowCommandExtractors = True
  }

fingerprintOf :: PipelineConfig -> IO T.Text
fingerprintOf = extractionConfigFingerprint

shouldDiffer :: T.Text -> T.Text -> Expectation
shouldDiffer a b = a `shouldSatisfy` (/= b)

-- ───────────────────────────────────────────────
-- Spec
-- ───────────────────────────────────────────────

spec :: Spec
spec = describe "extraction cache soundness (custom-extractors 5.5)" $ do

  it "identical configs fingerprint identically" $ do
    let cfg = pipeline (withCommandExtractor ["perl-extract", "{file}"])
    fp1 <- fingerprintOf cfg
    fp2 <- fingerprintOf cfg
    fp1 `shouldBe` fp2

  it "changing a command template invalidates the fingerprint" $ do
    fp1 <- fingerprintOf (pipeline (withCommandExtractor ["perl-extract", "{file}"]))
    fp2 <- fingerprintOf (pipeline (withCommandExtractor ["perl-extract-v2", "{file}"]))
    fp1 `shouldDiffer` fp2

  it "changing batch or timeout invalidates the fingerprint" $ do
    let base  = pipeline (withCommandExtractor ["perl-extract", "{files}"])
        batch = base
          { cfgGraphosConfig = withExtractor ".pl"
              ((commandExtractor ["perl-extract", "{files}"]) { ecBatch = Just True })
              (cfgGraphosConfig base) }
        slow  = base
          { cfgGraphosConfig = withExtractor ".pl"
              ((commandExtractor ["perl-extract", "{files}"]) { ecTimeoutSeconds = Just 5 })
              (cfgGraphosConfig base) }
    fpBase  <- fingerprintOf base
    fpBatch <- fingerprintOf batch
    fpSlow  <- fingerprintOf slow
    fpBase `shouldDiffer` fpBatch
    fpBase `shouldDiffer` fpSlow

  it "changing the command gate invalidates the fingerprint" $ do
    let cfg  = pipeline (withCommandExtractor ["perl-extract", "{file}"])
        off  = cfg { cfgAllowCommandExtractors = False }
    fpOn  <- fingerprintOf cfg
    fpOff <- fingerprintOf off
    fpOn `shouldDiffer` fpOff

  it "replacing a runtime grammar library invalidates the fingerprint" $
    withSystemTempDirectory "graphos-grammar" $ \dir -> do
      let lib = dir </> "tree-sitter-php.so"
          cfg = pipeline (withExtractor ".php"
                  (runtimeGrammarExtractor lib Nothing) defaultGraphosConfig)
      writeFile lib "ABI-9"
      fp1 <- fingerprintOf cfg
      writeFile lib "ABI-9-extended"   -- different size: stamp must change
      fp2 <- fingerprintOf cfg
      fp1 `shouldDiffer` fp2

  it "adding a runtime query file invalidates the fingerprint" $
    withSystemTempDirectory "graphos-queries" $ \dir -> do
      let lib  = dir </> "tree-sitter-php.so"
          qdir = dir </> "queries"
          cfg  = pipeline (withExtractor ".php"
                  (runtimeGrammarExtractor lib (Just qdir)) defaultGraphosConfig)
      writeFile lib "ABI-9"
      createDirectoryIfMissing True qdir
      fpEmpty <- fingerprintOf cfg
      writeFile (qdir </> "definitions.scm") "(function_definition) @definition.function"
      fpWithDefs <- fingerprintOf cfg
      fpEmpty `shouldDiffer` fpWithDefs

  it "changing a resolve entry invalidates the fingerprint" $ do
    let empty = pipeline defaultGraphosConfig
        withJava = pipeline (defaultGraphosConfig
          { gcResolve = Map.fromList
              [ (".java", ResolveConfig ResolvePackage ["src/main/java"] Nothing) ] })
        withKotlin = pipeline (defaultGraphosConfig
          { gcResolve = Map.fromList
              [ (".java", ResolveConfig ResolvePackage ["src/kotlin"] Nothing) ] })
    fpEmpty    <- fingerprintOf empty
    fpJava     <- fingerprintOf withJava
    fpKotlin   <- fingerprintOf withKotlin
    fpEmpty `shouldDiffer` fpJava
    fpJava  `shouldDiffer` fpKotlin

  it "does not serve a cache entry saved under a different config" $
    withSystemTempDirectory "graphos-cache" $ \dir -> do
      let src = dir </> "x.pl"
          out = dir </> "out"
      writeFile src "print 1;\n"
      fpA <- fingerprintOf (pipeline (withCommandExtractor ["perl-extract", "{file}"]))
      fpB <- fingerprintOf (pipeline (withCommandExtractor ["perl-extract", "{file}", "--v2"]))
      let ext = extractionFromLists [makeStubNode src] []
      fspSaveCachedExtraction productionFileSystemPort fpA src ext out
      hitSame <- fspLoadCachedExtraction productionFileSystemPort fpA src out
      hitOther <- fspLoadCachedExtraction productionFileSystemPort fpB src out
      hitSame `shouldSatisfy` (/= Nothing)
      hitOther `shouldBe` Nothing

  describe "dispatch" $ do
    it "routes command-mode files to their own list" $ do
      let cfg = pipeline (withCommandExtractor ["perl-extract", "{file}"])
          (ts, lsp, cmd, stubbed) =
            partitionByExtractor cfg ["a.pl", "b.py", "c.weird"]
      cmd     `shouldBe` ["a.pl"]
      ts      `shouldBe` ["b.py"]
      lsp     `shouldBe` []
      stubbed `shouldBe` ["c.weird"]

    it "keeps runtime-grammar files on the tree-sitter route" $ do
      let cfg = pipeline (withExtractor ".php"
                  (runtimeGrammarExtractor "/opt/lib/tree-sitter-php.so" Nothing)
                  defaultGraphosConfig)
          (ts, lsp, cmd, stubbed) = partitionByExtractor cfg ["a.php"]
      ts      `shouldBe` ["a.php"]
      lsp     `shouldBe` []
      cmd     `shouldBe` []
      stubbed `shouldBe` []
