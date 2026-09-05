{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Observability via the official hs-opentelemetry-sdk.
--
-- Replaces the custom OTLP implementation (Observability.hs) with the
-- standards-compliant OpenTelemetry SDK. Provides the same 'ObservabilityEnv'
-- interface so the Domain/UseCase layers are unchanged.
--
-- Key improvements over the custom implementation:
--   * Spec-compliant OTLP protobuf/JSON encoding (no hand-rolled JSON)
--   * W3C trace context propagation (traceparent/tracestate headers)
--   * Environment variable configuration (OTEL_EXPORTER_OTLP_ENDPOINT, etc.)
--   * Built-in batch processor with proper flush on shutdown
--   * Log bridge: console logs are also shipped as OTLP log records
--   * Proper error reporting (no silently swallowed HTTP errors)
--
-- Usage:
--   graphos . --otel                                        # OTel with defaults
--   graphos . --otel --otel-endpoint http://localhost:14319  # Docker Compose endpoint
--   OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:14319 graphos . --otel  # Via env var
module Graphos.Infrastructure.Observability.SDK
  ( -- * Initialization
    initObservability
  , shutdownObservability
  , ObservabilityEnv(..)

    -- * Tracing (delegates to hs-opentelemetry-sdk)
  , module OpenTelemetry.Trace

    -- * Metrics
  , MetricsStore
  , newMetricsStore
  , CounterName
  , GaugeName
  , HistogramName
  , HistogramAgg(..)
  , incCounter
  , decCounter
  , setGauge
  , observeHistogram
  , readCounter
  , readGauge
  , renderPrometheusMetrics

    -- * Debug tracing (Graphos-specific, not OTel)
  , DebugTraceEnv
  , newDebugTraceEnv
  , newDebugTraceEnvAt
  , debugTraceEvent
  , debugTraceSpan
  , flushDebugTrace
  , debugBufferLen
  , defaultDebugTraceCapacity

    -- * Bounded span store (Graphos-specific, not OTel)
  , SpanStore(..)
  , SpanRecord(..)
  , newSpanStore
  , insertSpan
  , readSpans

    -- * OTLP config (simplified: most fields now come from OTEL_* env vars)
  , OtelConfig(..)
  , defaultOtelConfig

    -- * Prometheus endpoint
  , startMetricsServer
  ) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar (MVar, newMVar, swapMVar, modifyMVar, readMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (void, when)
import System.Timeout (timeout)
import System.IO (hPutStrLn, stderr)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime(..), getCurrentTime, diffUTCTime)
import Data.Time (formatTime, defaultTimeLocale)
import Network.HTTP.Types (status200, hContentType)
import Network.Wai (pathInfo, responseLBS)
import Network.Wai.Handler.Warp (runSettings, setPort, setHost, defaultSettings, setBeforeMainLoop)
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv, unsetEnv)

-- hs-opentelemetry-sdk imports
import OpenTelemetry.Trace
  ( Tracer
  , TracerProvider
  , Span
  , SpanKind(..)
  , SpanContext
  , inSpan
  , inSpan'
  , inSpan''
  , defaultSpanArguments
  , SpanArguments(..)
  , NewLink(..)
  , NewEvent(..)
  , addAttribute
  , addAttributes
  , addEvent
  , setStatus
  , SpanStatus(..)
  , toAttribute
  , makeTracer
  , tracerOptions
  , initializeGlobalTracerProvider
  , shutdownTracerProvider
  , getGlobalTracerProvider
  )
import OpenTelemetry.Internal.Common.Types (InstrumentationLibrary(..))
import OpenTelemetry.Attributes (emptyAttributes)

import Graphos.Infrastructure.Logging (LogLevel(..), LogEnv, defaultLogEnv, logInfo, enableOtlpLogShipping, flushOtlpLogs)

-- ───────────────────────────────────────────────
-- Configuration
-- ───────────────────────────────────────────────

import Graphos.Domain.Config (OtelConfig(..), defaultOtelConfig)

-- ───────────────────────────────────────────────
-- Observability environment
-- ───────────────────────────────────────────────

-- | Full observability environment passed through the pipeline.
data ObservabilityEnv = ObservabilityEnv
  { otelTracer     :: Tracer              -- ^ From hs-opentelemetry-sdk
  , otelMetrics    :: MetricsStore        -- ^ Graphos metrics (Prometheus format)
  , otelConfig     :: OtelConfig
  , otelDebugTrace :: DebugTraceEnv       -- ^ Graphos-specific JSONL trace
  , otelSpans      :: SpanStore          -- ^ Bounded in-memory span store (keep last N)
  , otelLogEnv     :: LogEnv              -- ^ Console logging + OTLP log bridge
  , otelProvider   :: Maybe TracerProvider -- ^ SDK provider (for shutdown), Nothing if disabled
  , otelServerThread :: Maybe (Async ())   -- ^ Metrics server thread (for clean shutdown)
  }

-- ───────────────────────────────────────────────
-- Metrics (Prometheus-compatible, unchanged from custom impl)
-- ───────────────────────────────────────────────

type CounterName = Text
type GaugeName = Text
type HistogramName = Text

-- | Predefined Prometheus histogram buckets (seconds).
histogramBuckets :: [Double]
histogramBuckets = [0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]

-- | Aggregated histogram state — O(1) memory per metric regardless of observation count.
data HistogramAgg = HistogramAgg
  { haCount    :: !Int64
  , haSum      :: !Double
  , haMin      :: !(Maybe Double)
  , haMax      :: !(Maybe Double)
  , haBuckets  :: !(Map Double Int64)  -- ^ cumulative bucket count: upper bound -> count
  }

emptyHistogramAgg :: HistogramAgg
emptyHistogramAgg = HistogramAgg
  { haCount = 0
  , haSum = 0.0
  , haMin = Nothing
  , haMax = Nothing
  , haBuckets = Map.empty
  }

updateHistogramAgg :: Double -> HistogramAgg -> HistogramAgg
updateHistogramAgg val agg =
  let newMin = case haMin agg of
        Nothing -> Just val
        Just mn  -> Just (min mn val)
      newMax = case haMax agg of
        Nothing -> Just val
        Just mx  -> Just (max mx val)
      -- Increment all cumulative buckets whose upper bound >= val
      bucketInc = Map.fromList [(b, 1) | b <- histogramBuckets, val <= b]
  in agg
    { haCount = haCount agg + 1
    , haSum   = haSum agg + val
    , haMin   = newMin
    , haMax   = newMax
    , haBuckets = Map.unionWith (+) (haBuckets agg) bucketInc
    }

-- | Atomic metrics store with counters, gauges, and histograms.
-- Retained from custom implementation for Prometheus /metrics endpoint.
-- Phase 2 will migrate this to SDK MeterProvider + OTLP metrics push.
data MetricsStore = MetricsStore
  { msCounters   :: IORef (Map CounterName Int64)
  , msGauges     :: IORef (Map GaugeName Double)
  , msHistograms :: IORef (Map HistogramName HistogramAgg)
  }

newMetricsStore :: IO MetricsStore
newMetricsStore = MetricsStore
  <$> newIORef Map.empty
  <*> newIORef Map.empty
  <*> newIORef Map.empty

incCounter :: MetricsStore -> CounterName -> Int64 -> IO ()
incCounter ms name delta = atomicModifyIORef' (msCounters ms) $ \m ->
  let v = Map.findWithDefault 0 name m
  in (Map.insert name (v + delta) m, ())

decCounter :: MetricsStore -> CounterName -> Int64 -> IO ()
decCounter ms name delta = incCounter ms name (-delta)

setGauge :: MetricsStore -> GaugeName -> Double -> IO ()
setGauge ms name val = atomicModifyIORef' (msGauges ms) $ \m ->
  (Map.insert name val m, ())

observeHistogram :: MetricsStore -> HistogramName -> Double -> IO ()
observeHistogram ms name val = atomicModifyIORef' (msHistograms ms) $ \m ->
  let existing = Map.findWithDefault emptyHistogramAgg name m
  in (Map.insert name (updateHistogramAgg val existing) m, ())

readCounter :: MetricsStore -> CounterName -> IO Int64
readCounter ms name = Map.findWithDefault 0 name <$> readIORef (msCounters ms)

readGauge :: MetricsStore -> GaugeName -> IO Double
readGauge ms name = Map.findWithDefault 0 name <$> readIORef (msGauges ms)

-- | Render all metrics in Prometheus exposition format.
renderPrometheusMetrics :: MetricsStore -> IO Text
renderPrometheusMetrics ms = do
  counters <- readIORef (msCounters ms)
  gauges <- readIORef (msGauges ms)
  histograms <- readIORef (msHistograms ms)
  let counterLines = concatMap renderCounter (Map.toList counters)
      gaugeLines = concatMap renderGauge (Map.toList gauges)
      histLines = concatMap renderHistogram (Map.toList histograms)
  pure $ T.unlines $ counterLines ++ gaugeLines ++ histLines
  where
    renderCounter (name, val) =
      [ "# HELP " <> name <> " Total " <> name
      , "# TYPE " <> name <> " counter"
      , name <> " " <> T.pack (show val)
      , ""
      ]
    renderGauge (name, val) =
      [ "# HELP " <> name <> " Current " <> name
      , "# TYPE " <> name <> " gauge"
      , name <> " " <> T.pack (show val)
      , ""
      ]
    renderHistogram (name, agg) =
      let cnt = haCount agg
          sum_ = haSum agg
          bucketLines = map (renderBucket name agg) histogramBuckets
          infLine = name <> "_bucket{le=\"+Inf\"} " <> T.pack (show cnt)
      in [ "# HELP " <> name <> " Histogram " <> name
         , "# TYPE " <> name <> " histogram"
         ]
         ++ bucketLines
         ++ [infLine]
         ++ [name <> "_count " <> T.pack (show cnt)]
         ++ [name <> "_sum " <> T.pack (show sum_)]
         ++ [""]
    renderBucket name agg le =
      let cnt = Map.findWithDefault 0 le (haBuckets agg)
      in name <> "_bucket{le=\"" <> T.pack (show le) <> "\"} " <> T.pack (show cnt)

-- ───────────────────────────────────────────────
-- Debug tracing (structured JSON events, Graphos-specific)
-- ───────────────────────────────────────────────

-- | Default number of spans retained by a freshly created span store.
defaultSpanCapacity :: Int
defaultSpanCapacity = 1000

-- | Default maximum number of debug trace events held in memory before
-- flushing to disk.
defaultDebugTraceCapacity :: Int
defaultDebugTraceCapacity = 10000

-- | A recorded span: a timed, attributed unit of work.
data SpanRecord
  = SpanRecord
      { srName    :: !Text            -- ^ Logical span name
      , srStart   :: !UTCTime         -- ^ Span start
      , srEnd     :: !UTCTime         -- ^ Span end
      , srAttrs   :: !(Map Text Text) -- ^ Free-form attributes
      }

-- | Bounded in-memory ring of recently recorded spans. Keeps at most the last
-- 'spanCapacity' records, evicting the oldest when the cap is reached, so span
-- accumulation during long pipeline runs cannot grow memory without bound.
data SpanStore
  = SpanStore
      { spanCapacity :: !Int            -- ^ Maximum retained spans
      , spanBuffer   :: MVar [SpanRecord] -- ^ Live ring buffer of spans
      }

-- | Create a bounded span store retaining at most the given number of spans.
newSpanStore :: Int -> IO SpanStore
newSpanStore cap = SpanStore cap <$> newMVar []

-- | Record a span, evicting the oldest spans if the capacity is exceeded.
insertSpan :: SpanStore -> SpanRecord -> IO ()
insertSpan st rec = modifyMVar (spanBuffer st) $ \buf ->
  let buf' = buf ++ [rec]
  in if length buf' > spanCapacity st
       then pure (drop (length buf' - spanCapacity st) buf', ())
       else pure (buf', ())

-- | Snapshot of the currently retained spans (oldest first).
readSpans :: SpanStore -> IO [SpanRecord]
readSpans st = readMVar (spanBuffer st)

-- | Debug trace environment for local structured event logging.
-- Retained from custom implementation — this writes JSONL files for
-- offline analysis, not OTLP.
-- Both the debug-event buffer and the span store are bounded, so neither can
-- grow memory without bound during long pipeline runs.
data DebugTraceEnv = DebugTraceEnv
  { dtEnabled  :: Bool
  , dtPath     :: FilePath
  , dtCapacity :: !Int              -- ^ Max debug events held in memory before flush
  , dtSpanCap  :: !Int              -- ^ Max spans retained by the span store (<= 0 disables)
   , dtSpans    :: SpanStore         -- ^ Bounded in-memory span store (keep last N)
   , dtBuffer   :: MVar [Text]
   , dtSeq      :: IORef Integer     -- ^ Monotonic counter for unique flush filenames
   }

-- | Create a debug trace environment with default capacities.
newDebugTraceEnv :: Bool -> FilePath -> IO DebugTraceEnv
newDebugTraceEnv p q = newDebugTraceEnvAt p q defaultDebugTraceCapacity

-- | Create a debug trace environment with an explicit in-memory debug-event
-- buffer capacity. The span store defaults to 'defaultSpanCapacity'.
newDebugTraceEnvAt :: Bool -> FilePath -> Int -> IO DebugTraceEnv
newDebugTraceEnvAt enabled tracePath eventCap = do
  spans <- newSpanStore defaultSpanCapacity
  buf   <- newMVar []
  seq0 <- newIORef 0
  pure (DebugTraceEnv enabled tracePath eventCap defaultSpanCapacity spans buf seq0)

-- | Current number of debug events held in memory (test / introspection helper).
debugBufferLen :: DebugTraceEnv -> IO Int
debugBufferLen env = length <$> readMVar (dtBuffer env)

-- | Emit a structured debug trace event.
-- When the in-memory buffer reaches capacity, events are flushed to disk automatically.
debugTraceEvent :: DebugTraceEnv -> Text -> Map Text Text -> IO ()
debugTraceEvent env name attrs
  | not (dtEnabled env) = pure ()
  | otherwise = do
      now <- getCurrentTime
      let ts = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%3qZ" now
          attrsJson = T.intercalate "," $ map (\(k,v) -> "\"" <> k <> "\":\"" <> v <> "\"") (Map.toList attrs)
          line = "{\"timestamp\":\"" <> ts <> "\",\"event\":\"" <> name <> "\"," <> attrsJson <> "}"
      modifyMVar (dtBuffer env) $ \buf ->
        if length buf >= dtCapacity env
          then do
            -- Buffer is full: write all buffered events to disk, clear the
            -- buffer, then add only the new event so in-memory size stays bounded.
            flushEventsToDisk env buf
            pure ([line], ())
          else pure (buf ++ [line], ())

-- | Record a span start/end as a trace event.
debugTraceSpan :: DebugTraceEnv -> Text -> UTCTime -> UTCTime -> Map Text Text -> IO ()
debugTraceSpan env name start end attrs
  | not (dtEnabled env) = pure ()
  | otherwise = do
      let dur = realToFrac (diffUTCTime end start) :: Double
      debugTraceEvent env ("span_" <> name) (Map.insert "duration_s" (T.pack $ show dur) attrs)
      when (dtSpanCap env > 0) $
        insertSpan (dtSpans env) (SpanRecord name start end attrs)

-- | Write buffered events to a JSONL file on disk.
-- Creates the trace directory if needed. Errors are silently ignored to avoid
-- disrupting the main application flow.
flushEventsToDisk :: DebugTraceEnv -> [Text] -> IO ()
flushEventsToDisk env events
  | null events = pure ()
  | otherwise = do
       createDirectoryIfMissing True (dtPath env) `catch` (\(_ :: SomeException) -> pure ())
       seqNum <- atomicModifyIORef' (dtSeq env) $ \n -> (n + 1, n + 1)
       let filename = "trace_" ++ show seqNum ++ ".jsonl"
           filepath = dtPath env ++ "/" ++ filename
       TIO.writeFile filepath (T.unlines events) `catch` (\(_ :: SomeException) -> pure ())

-- | Flush buffered trace events to disk.
-- The trace directory is created lazily here, only when tracing is enabled and
-- there are buffered events to write. This guarantees that a traces/ folder
-- exists if and only if a trace JSONL file was actually produced.
flushDebugTrace :: DebugTraceEnv -> IO ()
flushDebugTrace env
  | not (dtEnabled env) = pure ()
  | otherwise = do
      events <- swapMVar (dtBuffer env) []
      case events of
        [] -> pure ()
        _  -> do
           createDirectoryIfMissing True (dtPath env)
           seqNum <- atomicModifyIORef' (dtSeq env) $ \n -> (n + 1, n + 1)
           let filename = "trace_" ++ show seqNum ++ ".jsonl"
               filepath = dtPath env ++ "/" ++ filename
           TIO.writeFile filepath (T.unlines events) `catch` (\(_ :: SomeException) -> pure ())

-- ───────────────────────────────────────────────
-- Prometheus metrics HTTP endpoint
-- ───────────────────────────────────────────────

-- | Start an HTTP server exposing /metrics in Prometheus format.
startMetricsServer :: MetricsStore -> Int -> IO ()
startMetricsServer ms listenPort = do
  env <- defaultLogEnv LogInfo
  let app req respond =
        case pathInfo req of
          ["metrics"] -> do
            metricsText <- renderPrometheusMetrics ms
            respond $ responseLBS status200
              [(hContentType, "text/plain; version=0.0.4; charset=utf-8")]
              (BSL.fromStrict $ TE.encodeUtf8 metricsText)
          _ -> respond $ responseLBS status200 [] "Graphos observability server"
  let settings = setPort listenPort
               $ setHost "0.0.0.0"
               $ setBeforeMainLoop (logInfo env $ T.pack $ "Metrics server listening on :" ++ show listenPort ++ "/metrics")
               $ defaultSettings
  runSettings settings app

-- ───────────────────────────────────────────────
-- Initialization
-- ───────────────────────────────────────────────

-- | Initialize full observability stack using hs-opentelemetry-sdk.
--
-- When 'otelEnabled' is True:
--   * Sets OTEL_EXPORTER_OTLP_ENDPOINT from 'otelEndpoint' (if non-empty)
--   * Sets OTEL_SERVICE_NAME from 'otelServiceName'
--   * Calls 'initializeGlobalTracerProvider' to set up the SDK
--   * Creates a 'Tracer' for "graphos" instrumentation
--   * Enables OTLP log shipping via the Log bridge
--   * Forks the Prometheus metrics server if 'metricsPort' is Just
--
-- When 'otelEnabled' is False:
--   * No OTLP export, no tracing provider
--   * Still creates metrics store and debug trace
initObservability :: LogLevel -> OtelConfig -> Maybe Int -> FilePath -> IO ObservabilityEnv
initObservability logLevel otelCfg metricsPort debugDir = do
  logEnv <- defaultLogEnv logLevel
  metrics <- newMetricsStore
  debugTrace <- newDebugTraceEnv (logLevel <= LogDebug) debugDir

  mProvider <- if otelEnabled otelCfg
    then do
      -- Set OTEL_* env vars from CLI flags (so the SDK picks them up)
      case otelEndpoint otelCfg of
        "" -> pure ()  -- use existing env var or SDK default
        ep -> setEnv "OTEL_EXPORTER_OTLP_ENDPOINT" ep
      setEnv "OTEL_SERVICE_NAME" (otelServiceName otelCfg)

      logInfo logEnv "Initializing OpenTelemetry SDK..."

      -- Initialize the global TracerProvider (reads OTEL_* env vars)
      provider <- initializeGlobalTracerProvider
      logInfo logEnv "OpenTelemetry SDK initialized (OTLP exporter active)"

      -- Enable log shipping to OTLP Collector → Loki
      -- The log bridge ships all logs at or above the current level
      enableOtlpLogShipping logEnv (otelLogsEndpoint otelCfg) (otelServiceName otelCfg)
      logInfo logEnv "OTLP log shipping enabled"

      pure (Just provider)
    else do
      -- Clean up any leftover env vars when --otel is off
      unsetEnv "OTEL_EXPORTER_OTLP_ENDPOINT" `catch` (\(_ :: SomeException) -> pure ())
      unsetEnv "OTEL_SERVICE_NAME" `catch` (\(_ :: SomeException) -> pure ())
      pure Nothing

  -- Get the tracer from the provider (or a no-op tracer if disabled)
  let instrLib = InstrumentationLibrary
        { libraryName = T.pack (otelServiceName otelCfg)
        , libraryVersion = "0.1.0"
        , librarySchemaUrl = ""
        , libraryAttributes = emptyAttributes
        }
  tracer <- case mProvider of
    Just provider -> pure $ makeTracer provider instrLib tracerOptions
    Nothing       -> do
      -- When OTel is disabled, get the global provider (which defaults to no-op)
      provider <- getGlobalTracerProvider
      pure $ makeTracer provider instrLib tracerOptions

  -- Start Prometheus metrics server if requested
  serverThread <- case metricsPort of
    Just p -> do
      logInfo logEnv $ T.pack $ "Starting Prometheus metrics server on :" ++ show p ++ "/metrics"
      Just <$> async (startMetricsServer metrics p)
    Nothing -> pure Nothing

  pure ObservabilityEnv
    { otelTracer = tracer
    , otelMetrics = metrics
    , otelConfig = otelCfg
    , otelDebugTrace = debugTrace
    , otelSpans = dtSpans debugTrace
    , otelLogEnv = logEnv
    , otelProvider = mProvider
    , otelServerThread = serverThread
    }

-- | Shut down observability: cancel metrics server, flush debug traces, OTLP logs, and SDK provider.
-- Each sub-cleanup is wrapped in an independent 5-second timeout with exception
-- catching so that one component hanging (e.g. OTLP collector unavailable) does
-- not prevent the others from running.
shutdownObservability :: ObservabilityEnv -> IO ()
shutdownObservability env = do
  let logEnv = otelLogEnv env
      componentTimeout :: IO () -> String -> IO ()
      componentTimeout action label = do
        result <- timeout 5000000 (action `catch` \(e :: SomeException) -> do
          hPutStrLn stderr $ "[graphos] WARNING: " ++ label ++ " threw exception: " ++ show e)
        case result of
          Nothing -> hPutStrLn stderr $ "[graphos] WARNING: " ++ label ++ " timed out after 5s"
          Just () -> pure ()
  case otelServerThread env of
    Just thread -> do
      logInfo logEnv "Shutting down metrics server..."
      componentTimeout (cancel thread) "metrics server shutdown"
      logInfo logEnv "Metrics server shut down."
    Nothing -> pure ()
  componentTimeout (flushDebugTrace (otelDebugTrace env)) "debug trace flush"
  componentTimeout (flushOtlpLogs logEnv) "OTLP logs flush"
  case otelProvider env of
    Just provider -> do
      logInfo logEnv "Shutting down OpenTelemetry SDK..."
      componentTimeout (void $ shutdownTracerProvider provider (Just 5000)) "OTLP SDK shutdown"
      logInfo logEnv "OpenTelemetry SDK shut down (spans flushed)"
    Nothing -> pure ()
