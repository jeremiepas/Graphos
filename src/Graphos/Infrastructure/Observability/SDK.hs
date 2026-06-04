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
  , CounterName
  , GaugeName
  , HistogramName
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
  , debugTraceEvent
  , debugTraceSpan
  , flushDebugTrace

    -- * OTLP config (simplified: most fields now come from OTEL_* env vars)
  , OtelConfig(..)
  , defaultOtelConfig

    -- * Prometheus endpoint
  , startMetricsServer
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, swapMVar, modifyMVar_)
import Control.Exception (SomeException, catch)
-- import Control.Monad (when, forever)  -- removed: unused, was causing -Werror=unused-imports
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Int (Int64)
import Data.List (sort)
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

-- | OpenTelemetry configuration (simplified from custom version).
-- Most settings now come from OTEL_* environment variables:
--   * OTEL_EXPORTER_OTLP_ENDPOINT  → endpoint (default: http://localhost:4318)
--   * OTEL_SERVICE_NAME             → service name (default: graphos)
--   * OTEL_RESOURCE_ATTRIBUTES      → service.version, etc.
--   * OTEL_BSP_SCHEDULE_DELAY       → export interval (default: 5000ms)
--   * OTEL_SDK_DISABLED             → disable SDK (default: false)
data OtelConfig = OtelConfig
  { otelEnabled        :: Bool
  , otelEndpoint       :: String    -- ^ CLI --otel-endpoint override (empty = use env var)
  , otelServiceName    :: String    -- ^ CLI --otel-service-name override
  , otelLogsEndpoint   :: String    -- ^ OTLP logs endpoint (for log bridge)
  } deriving (Eq, Show)

defaultOtelConfig :: OtelConfig
defaultOtelConfig = OtelConfig
  { otelEnabled        = False
  , otelEndpoint       = ""
  , otelServiceName    = "graphos"
  , otelLogsEndpoint   = "http://localhost:4318/v1/logs"
  }

-- ───────────────────────────────────────────────
-- Observability environment
-- ───────────────────────────────────────────────

-- | Full observability environment passed through the pipeline.
data ObservabilityEnv = ObservabilityEnv
  { otelTracer     :: Tracer              -- ^ From hs-opentelemetry-sdk
  , otelMetrics    :: MetricsStore        -- ^ Graphos metrics (Prometheus format)
  , otelConfig     :: OtelConfig
  , otelDebugTrace :: DebugTraceEnv       -- ^ Graphos-specific JSONL trace
  , otelLogEnv     :: LogEnv              -- ^ Console logging + OTLP log bridge
  , otelProvider   :: Maybe TracerProvider -- ^ SDK provider (for shutdown), Nothing if disabled
  }

-- ───────────────────────────────────────────────
-- Metrics (Prometheus-compatible, unchanged from custom impl)
-- ───────────────────────────────────────────────

type CounterName = Text
type GaugeName = Text
type HistogramName = Text

-- | Atomic metrics store with counters, gauges, and histograms.
-- Retained from custom implementation for Prometheus /metrics endpoint.
-- Phase 2 will migrate this to SDK MeterProvider + OTLP metrics push.
data MetricsStore = MetricsStore
  { msCounters   :: IORef (Map CounterName Int64)
  , msGauges     :: IORef (Map GaugeName Double)
  , msHistograms :: IORef (Map HistogramName [Double])
  }

newMetricsStore :: IO MetricsStore
newMetricsStore = MetricsStore <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef Map.empty

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
  let existing = Map.findWithDefault [] name m
  in (Map.insert name (val : existing) m, ())

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
    renderHistogram (name, vals) =
      let sorted = sort vals
          cnt = length sorted
          sum_ = sum sorted
          buckets = [0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0] :: [Double]
          bucketLines = map (renderBucket name sorted) buckets
          infLine = name <> "_bucket{le=\"+Inf\"} " <> T.pack (show cnt)
      in [ "# HELP " <> name <> " Histogram " <> name
         , "# TYPE " <> name <> " histogram"
         ]
         ++ bucketLines
         ++ [infLine]
         ++ [name <> "_count " <> T.pack (show cnt)]
         ++ [name <> "_sum " <> T.pack (show sum_)]
         ++ [""]
    renderBucket name vals le =
      let cnt = length $ filter (<= le) vals
      in name <> "_bucket{le=\"" <> T.pack (show le) <> "\"} " <> T.pack (show cnt)

-- ───────────────────────────────────────────────
-- Debug tracing (structured JSON events, Graphos-specific)
-- ───────────────────────────────────────────────

-- | Debug trace environment for local structured event logging.
-- Retained from custom implementation — this writes JSONL files for
-- offline analysis, not OTLP.
data DebugTraceEnv = DebugTraceEnv
  { dtEnabled :: Bool
  , dtPath    :: FilePath
  , dtBuffer  :: MVar [Text]
  }

newDebugTraceEnv :: Bool -> FilePath -> IO DebugTraceEnv
newDebugTraceEnv enabled tracePath = do
  createDirectoryIfMissing True tracePath
  DebugTraceEnv enabled tracePath <$> newMVar []

-- | Emit a structured debug trace event.
debugTraceEvent :: DebugTraceEnv -> Text -> Map Text Text -> IO ()
debugTraceEvent env name attrs
  | not (dtEnabled env) = pure ()
  | otherwise = do
      now <- getCurrentTime
      let ts = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%3qZ" now
          attrsJson = T.intercalate "," $ map (\(k,v) -> "\"" <> k <> "\":\"" <> v <> "\"") (Map.toList attrs)
          line = "{\"timestamp\":\"" <> ts <> "\",\"event\":\"" <> name <> "\"," <> attrsJson <> "}"
      modifyMVar_ (dtBuffer env) (\buf -> pure $ buf ++ [line])

-- | Record a span start/end as a trace event.
debugTraceSpan :: DebugTraceEnv -> Text -> UTCTime -> UTCTime -> Map Text Text -> IO ()
debugTraceSpan env name start end attrs
  | not (dtEnabled env) = pure ()
  | otherwise = do
      let dur = realToFrac (diffUTCTime end start) :: Double
      debugTraceEvent env ("span_" <> name) (Map.insert "duration_s" (T.pack $ show dur) attrs)

-- | Flush buffered trace events to disk.
flushDebugTrace :: DebugTraceEnv -> IO ()
flushDebugTrace env
  | not (dtEnabled env) = pure ()
  | otherwise = do
      events <- swapMVar (dtBuffer env) []
      case events of
        [] -> pure ()
        _  -> do
          now <- getCurrentTime
          let filename = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" now ++ ".jsonl"
              filepath = dtPath env ++ "/" ++ filename
          TIO.writeFile filepath (T.unlines events)

-- ───────────────────────────────────────────────
-- Prometheus metrics HTTP endpoint
-- ───────────────────────────────────────────────

-- | Start an HTTP server exposing /metrics in Prometheus format.
startMetricsServer :: MetricsStore -> Int -> IO ()
startMetricsServer ms listenPort = do
  env <- defaultLogEnv LevelInfo
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
  debugTrace <- newDebugTraceEnv (logLevel >= LevelDebug) debugDir

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
  case metricsPort of
    Just p -> do
      logInfo logEnv $ T.pack $ "Starting Prometheus metrics server on :" ++ show p ++ "/metrics"
      _ <- forkIO $ startMetricsServer metrics p
      pure ()
    Nothing -> pure ()

  pure ObservabilityEnv
    { otelTracer = tracer
    , otelMetrics = metrics
    , otelConfig = otelCfg
    , otelDebugTrace = debugTrace
    , otelLogEnv = logEnv
    , otelProvider = mProvider
    }

-- | Shut down observability: flush debug traces, OTLP logs, and SDK provider.
shutdownObservability :: ObservabilityEnv -> IO ()
shutdownObservability env = do
  flushDebugTrace (otelDebugTrace env)
  flushOtlpLogs (otelLogEnv env)
  -- Shutdown the global TracerProvider (flushes all buffered spans)
  case otelProvider env of
    Just provider -> do
      _ <- shutdownTracerProvider provider (Just 5000)
      logInfo (otelLogEnv env) "OpenTelemetry SDK shut down (spans flushed)"
    Nothing -> pure ()

