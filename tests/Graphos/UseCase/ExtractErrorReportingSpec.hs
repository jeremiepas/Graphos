{-# LANGUAGE OverloadedStrings #-}
-- | Per-file extraction error recording and reporting (custom-extractors 7.1–7.3).
--
-- The unit cases pin the isolation helper and the roll-up used by the log and
-- the report. The integration case runs the real @extractAll@ orchestration on
-- a fixture with one succeeding and one failing command extractor: the build
-- must complete, the failing file must be stub-extracted, and its error must be
-- recorded on the merged 'Extraction'.
module Graphos.UseCase.ExtractErrorReportingSpec (spec) where

import Control.Exception (ErrorCall(..), throwIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Short (toText)
import System.Directory
  ( createDirectoryIfMissing
  , getPermissions
  , setPermissions
  , executable
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Config (defaultOtelConfig)
import Graphos.Domain.Graph (buildGraph, makeStubNode)
import Graphos.Infrastructure.Logging (LogLevel(..))
import Graphos.Infrastructure.Observability.SDK (initObservability, ObservabilityEnv(..))
import Graphos.Infrastructure.Wiring (productionAppEnvWithConfig)
import Graphos.UseCase.Extract.Core
  ( extractionErrorSummaryLines
  , extractAll
  , isolateFileExtraction
  , summariseExtractionErrors
  )
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.UseCase.Report (generateReport)

-- | Logging sink: the assertions are on the returned values, not on stderr.
silentLog :: LoggingPort
silentLog = LoggingPort
  { lpLogTrace = const (pure ())
  , lpLogDebug = const (pure ())
  , lpLogInfo  = const (pure ())
  , lpLogWarn  = const (pure ())
  , lpLogError = const (pure ())
  }

stubExtraction :: FilePath -> Extraction
stubExtraction fp = extractionFromLists [makeStubNode fp] []

-- | Write an executable POSIX shell script used as an extractor argv[0].
writeScript :: FilePath -> String -> String -> IO FilePath
writeScript root name body = do
  let path = root </> name
  writeFile path ("#!/bin/sh\n" ++ body ++ "\n")
  perms <- getPermissions path
  setPermissions path perms { executable = True }
  pure path

commandConfig :: [String] -> ExtractorConfig
commandConfig argv = ExtractorConfig
  { ecMode           = ExtractCommand
  , ecGrammar        = Nothing
  , ecLanguageId     = Nothing
  , ecGranularity    = Nothing
  , ecCommand        = Just argv
  , ecBatch          = Nothing
  , ecTimeoutSeconds = Just 10
  , ecGrammarPath    = Nothing
  , ecQueries        = Nothing
  }

analysis0 :: Analysis
analysis0 = Analysis
  { analysisCommunities      = Map.empty
  , analysisNullModel        = DefaultNullModel
  , analysisCohesion         = Map.empty
  , analysisGodNodes         = []
  , analysisSurprises        = []
  , analysisQuestions        = []
  , analysisArticulation     = []
  , analysisBccCount         = 0
  , analysisPartitionQuality = Nothing
  , analysisClusterEval      = Nothing
  }

spec :: Spec
spec = do
  describe "isolateFileExtraction (7.1)" $ do
    it "records a throwing file as a stub with the extractor's error" $ do
      ex <- isolateFileExtraction silentLog "a.pl" "command"
              (throwIO (ErrorCall "extractor exploded"))
      Map.size (extractionNodes ex) `shouldBe` 1
      map (T.unpack . toText . nodeSourceFile) (Map.elems (extractionNodes ex))
        `shouldBe` ["a.pl"]
      case extractionErrors ex of
        [err] -> do
          eeFile err `shouldBe` "a.pl"
          eeExtractor err `shouldBe` "command"
          T.isInfixOf "extractor exploded" (eeMessage err) `shouldBe` True
        other -> expectationFailure ("expected exactly one recorded error, got " ++ show other)

    it "passes a successful extraction through untouched" $ do
      let ok = stubExtraction "a.pl"
      ex <- isolateFileExtraction silentLog "a.pl" "command" (pure ok)
      ex `shouldBe` ok
      extractionErrors ex `shouldBe` []

  describe "summariseExtractionErrors (7.2)" $ do
    it "counts per extractor, most frequent first, ties alphabetical" $ do
      let errs =
            [ ExtractionError "a.pl" "command" "boom"
            , ExtractionError "b.pl" "command" "boom"
            , ExtractionError "c.ts"  "typescript" "parse failed"
            , ExtractionError "d.go"  "lsp" "disconnected"
            , ExtractionError "e.go"  "lsp" "disconnected"
            , ExtractionError "f.go"  "lsp" "disconnected"
            ]
      summariseExtractionErrors errs
        `shouldBe` [("lsp", 3), ("command", 2), ("typescript", 1)]

    it "renders a bounded summary with per-extractor counts" $ do
      let errs = [ ExtractionError ("f" ++ show i ++ ".pl") "command" "boom"
                 | i <- [1 .. 7 :: Int] ]
          lines' = extractionErrorSummaryLines errs
      lines' `shouldSatisfy` (not . null)
      lines' `shouldSatisfy` (any (T.isInfixOf "7 file(s) recorded extraction errors (command: 7)"))
      length lines' `shouldBe` 1 + 5 + 1  -- summary + 5 details + "and N more"
      lines' `shouldSatisfy` (any (T.isInfixOf "… and 2 more"))

    it "renders nothing for a clean extraction" $
      extractionErrorSummaryLines [] `shouldBe` []

  describe "generateReport (7.2)" $ do
    it "summarises per-extractor errors in a dedicated section" $ do
      let g = buildGraph False (stubExtraction "a.pl")
          errs = [ ExtractionError "a.pl" "command" "exited with code 3"
                 , ExtractionError "b.pl" "command" "malformed JSON"
                 ]
          detection = Detection 0 0 True Nothing Map.empty Map.empty emptyExclusionCounts
          report = generateReport g analysis0 defaultConfig detection errs Nothing
      report `shouldSatisfy` T.isInfixOf "## Extraction Errors"
      report `shouldSatisfy` T.isInfixOf "Files with extraction errors: 2 (command 2)"
      report `shouldSatisfy` T.isInfixOf "| a.pl | command | exited with code 3 |"

    it "says so when every extractor succeeded" $ do
      let g = buildGraph False (stubExtraction "a.pl")
          detection = Detection 0 0 True Nothing Map.empty Map.empty emptyExclusionCounts
          report = generateReport g analysis0 defaultConfig detection [] Nothing
      report `shouldSatisfy` T.isInfixOf "## Extraction Errors"
      report `shouldSatisfy` T.isInfixOf "No extraction errors recorded."

  describe "extractAll integration (7.3)" $ do
    it "completes with one failing extractor and records its error" $
      withSystemTempDirectory "graphos-extract-errors" $ \root -> do
        let out = root </> "out"
        createDirectoryIfMissing True out
        goodScript <- writeScript root "good.sh" $
          "printf '%s' '{\"nodes\":[{\"id\":\"fn:ok\",\"label\":\"ok\"}],\"edges\":[]}'"
        badScript <- writeScript root "bad.sh" "exit 3"
        let goodFile = root </> "good.pl"
            badFile  = root </> "bad.pm"
        writeFile goodFile "sub ok { 1 }\n"
        writeFile badFile  "sub bad { 2 }\n"
        let gcfg = defaultGraphosConfig
              { gcExtractors = Map.union
                  (Map.fromList
                    [ (".pl", commandConfig [goodScript, "{file}"])
                    , (".pm", commandConfig [badScript,  "{file}"])
                    ])
                  (gcExtractors defaultGraphosConfig)
              }
            config = defaultConfig
              { cfgInputPath = root
              , cfgOutputDir = out
              , cfgGraphosConfig = gcfg
              , cfgAllowCommandExtractors = True
              , cfgFresh = True
              , cfgThreads = 1
              , cfgNoViz = True
              }
            detection = Detection
              { detectionTotalFiles = 2
              , detectionTotalWords = 0
              , detectionNeedsGraph = True
              , detectionWarning = Nothing
              , detectionFiles = Map.singleton CodeFiles [goodFile, badFile]
              , detectionClassification = Map.empty
              , detectionExclusions = emptyExclusionCounts
              }
        obsEnv <- initObservability LogError defaultOtelConfig Nothing (root </> "traces")
        let appEnv = productionAppEnvWithConfig (otelLogEnv obsEnv) obsEnv gcfg
        ex <- extractAll appEnv config detection Nothing
        -- The failing extractor did not abort the build: both files are in the
        -- merged extraction (the failing one as its stub).
        let sources = map (T.unpack . toText . nodeSourceFile) (Map.elems (extractionNodes ex))
        sources `shouldContain` [goodFile]
        sources `shouldContain` [badFile]
        map eeFile (extractionErrors ex) `shouldBe` [badFile]
        map eeExtractor (extractionErrors ex) `shouldBe` ["command"]
        summariseExtractionErrors (extractionErrors ex) `shouldBe` [("command", 1)]
        extractionErrorSummaryLines (extractionErrors ex)
          `shouldSatisfy` (not . null)
