{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Observability.SDKSpec where

import Test.Hspec
import Control.Monad (forM_)
import Control.Exception (catch, SomeException)
import System.Directory (doesDirectoryExist, doesFileExist, getTemporaryDirectory, listDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO (readFile)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Graphos.Infrastructure.Observability.SDK

-- | Remove a directory if it exists, ignoring errors if it doesn't.
cleanDir :: FilePath -> IO ()
cleanDir dir = catch (removeDirectoryRecursive dir) (\(_ :: SomeException) -> pure ())

spec :: Spec
spec = do
  describe "Debug trace directory creation" $ do
    it "does not create a directory when tracing is disabled" $ do
      tmp <- mkTempPath "disabled"
      cleanDir tmp
      env <- newDebugTraceEnv False tmp
      flushDebugTrace env
      doesDirectoryExist tmp `shouldReturn` False

    it "does not create a directory when tracing is enabled but no events were emitted" $ do
      tmp <- mkTempPath "empty"
      cleanDir tmp
      env <- newDebugTraceEnv True tmp
      flushDebugTrace env
      doesDirectoryExist tmp `shouldReturn` False

    it "creates the directory and a JSONL file when events were emitted" $ do
      tmp <- mkTempPath "events"
      cleanDir tmp
      env <- newDebugTraceEnv True tmp
      debugTraceEvent env "test_event" Map.empty
      flushDebugTrace env
      doesDirectoryExist tmp `shouldReturn` True
      files <- listDirectory tmp
      length files `shouldBe` 1
      let file = NE.head $ NE.fromList files
      doesFileExist (tmp </> file) `shouldReturn` True
      file `shouldSatisfy` (\f -> ".jsonl" `T.isSuffixOf` T.pack f)
      -- Clean up after test
      cleanDir tmp

  describe "Span store (bounded, keep last N)" $ do
    it "retains only the last 1000 spans when more than 10000 are inserted" $ do
      st <- newSpanStore 1000
      now <- getCurrentTime
      forM_ [1..10000] $ \i ->
        insertSpan st (SpanRecord (T.pack ("span_" ++ show i)) now now Map.empty)
      spans <- readSpans st
      length spans `shouldBe` 1000
      map srName spans `shouldBe`
        [T.pack ("span_" ++ show i) | i <- [9001..10000]]

    it "does not grow beyond capacity when inserting a huge number of spans" $ do
      st <- newSpanStore 1000
      now <- getCurrentTime
      forM_ [1..100000] $ \i ->
        insertSpan st (SpanRecord (T.pack ("s_" ++ show i)) now now Map.empty)
      spans <- readSpans st
      length spans `shouldSatisfy` (\n -> n <= 1000)

  describe "Debug trace buffer (bounded)" $ do
    it "keeps in-memory buffer bounded below the cap while persisting all events" $ do
      tmp <- mkTempPath "bound"
      cleanDir tmp
      env <- newDebugTraceEnvAt True tmp defaultDebugTraceCapacity
      forM_ [1..20000] $ \i ->
        debugTraceEvent env (T.pack ("evt_" ++ show i)) Map.empty
      n <- debugBufferLen env
      n `shouldSatisfy` (\x -> x <= defaultDebugTraceCapacity)
      flushDebugTrace env
      files <- listDirectory tmp
      totalLines <- sum <$> mapM (\f -> countLines (tmp </> f)) files
      totalLines `shouldSatisfy` (\t -> t >= 20000)
      cleanDir tmp

    it "never lets the in-memory buffer exceed the configured capacity" $ do
      tmp <- mkTempPath "cap"
      cleanDir tmp
      env <- newDebugTraceEnvAt True tmp 500
      forM_ [1..5000] $ \i ->
        debugTraceEvent env (T.pack ("e_" ++ show i)) Map.empty
      n <- debugBufferLen env
      n `shouldSatisfy` (\x -> x <= 500)
      cleanDir tmp

  describe "Histogram aggregation (O(1) per metric)" $ do
    it "renders a valid single-observation histogram" $ do
      store <- newMetricsStore
      observeHistogram store "hist_single" 0.5
      out <- renderPrometheusMetrics store
      out `shouldSatisfy` (\s -> "# TYPE hist_single histogram" `T.isInfixOf` s)
      out `shouldSatisfy` (\s -> "hist_single_count 1" `T.isInfixOf` s)
      out `shouldSatisfy` (\s -> "hist_single_bucket{le=\"0.5\"} 1" `T.isInfixOf` s)

    it "aggregates 100k observations into a constant-size metric" $ do
      store <- newMetricsStore
      forM_ [1..100000] $ \i ->
        observeHistogram store "hist_scale" (realToFrac i :: Double)
      out <- renderPrometheusMetrics store
      out `shouldSatisfy` (\s -> "hist_scale_count 100000" `T.isInfixOf` s)

  describe "Prometheus rendering (valid output)" $ do
    it "renders counters, gauges, and histograms together" $ do
      store <- newMetricsStore
      incCounter store "req_total" 42
      setGauge store "mem_bytes" 1024.0
      observeHistogram store "latency_s" 2.5
      out <- renderPrometheusMetrics store
      out `shouldSatisfy` (\s -> "req_total 42" `T.isInfixOf` s)
      out `shouldSatisfy` (\s -> "mem_bytes 1024.0" `T.isInfixOf` s)
      out `shouldSatisfy` (\s -> "latency_s_count 1" `T.isInfixOf` s)

countLines :: FilePath -> IO Int
countLines f = length . lines <$> readFile f

mkTempPath :: String -> IO FilePath
mkTempPath tag = do
  base <- getTemporaryDirectory
  pure $ base </> ("graphos-sdk-test-" ++ tag)
