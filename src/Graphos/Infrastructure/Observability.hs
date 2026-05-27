-- | Observability infrastructure: tracing, metrics, and debug instrumentation.
--
-- Provides:
--   * OpenTelemetry-compatible tracing (spans with timing and attributes)
--   * Prometheus-style metrics (counters, gauges, histograms)
--   * A debug trace log for detailed pipeline introspection
--   * OTLP HTTP export for traces and metrics (via http-conduit)
--   * A built-in Prometheus metrics HTTP endpoint (--metrics)
--
-- Architecture:
--   * 'Tracer' manages in-memory span recording with atomic ID generation
--   * 'MetricsStore' holds atomic counters/gauges/histograms in IORef maps
--   * 'DebugTraceEnv' writes structured JSON events for offline analysis
--   * The OTLP exporter sends spans and metrics to an OpenTelemetry Collector
--   * The Prometheus endpoint serves /metrics in the standard exposition format
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Observability
  ( -- * Initialization
    initObservability
  , shutdownObservability
  , ObservabilityEnv(..)

    -- * Tracing
  , Tracer
  , Span(..)
  , SpanKind(..)
  , SpanStatus(..)
  , withSpan
  , withSpan_
  , addSpanEvent
  , addSpanAttribute
  , setSpanStatus

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

    -- * Debug tracing
  , DebugTraceEnv
  , newDebugTraceEnv
  , debugTraceEvent
  , debugTraceSpan
  , flushDebugTrace

    -- * OTLP export
  , OtelConfig(..)
  , defaultOtelConfig
  , exportTracesOTLP
  , exportMetricsOTLP

    -- * Prometheus endpoint
  , startMetricsServer
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, swapMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (when, unless, forever)
import Data.Bits (shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', atomicModifyIORef')
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
import Data.Time (formatTime, defaultTimeLocale, fromGregorian)
import Data.Word (Word64, Word8)
import Network.HTTP.Client
  ( newManager
  , defaultManagerSettings
  , parseRequest
  , httpLbs
  , RequestBody(..)
  , Request(..)
  , method
  , requestHeaders
  , requestBody
  )
import Network.HTTP.Types (status200, hContentType, methodPost)
import Network.Wai (pathInfo, responseLBS)
import Network.Wai.Handler.Warp (runSettings, setPort, setHost, defaultSettings, setBeforeMainLoop)
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)

import Graphos.Infrastructure.Logging (LogLevel(..), LogEnv, defaultLogEnv, logInfo, logDebug, enableOtlpLogShipping, flushOtlpLogs)

-- ───────────────────────────────────────────────
-- Configuration
-- ───────────────────────────────────────────────

-- | OpenTelemetry configuration for tracing and metrics export.
data OtelConfig = OtelConfig
  { otelTracesEndpoint  :: String
  , otelMetricsEndpoint :: String
  , otelLogsEndpoint    :: String
  , otelServiceName     :: String
  , otelServiceVersion  :: String
  , otelExportInterval  :: Int
  , otelEnabled         :: Bool
  } deriving (Eq, Show)

defaultOtelConfig :: OtelConfig
defaultOtelConfig = OtelConfig
  { otelTracesEndpoint  = "http://localhost:4318/v1/traces"
  , otelMetricsEndpoint = "http://localhost:4318/v1/metrics"
  , otelLogsEndpoint     = "http://localhost:4318/v1/logs"
  , otelServiceName     = "graphos"
  , otelServiceVersion  = "0.1.0"
  , otelExportInterval  = 15
  , otelEnabled         = False
  }

-- | Full observability environment passed through the pipeline.
data ObservabilityEnv = ObservabilityEnv
  { otelTracer     :: Tracer
  , otelMetrics    :: MetricsStore
  , otelConfig     :: OtelConfig
  , otelDebugTrace :: DebugTraceEnv
  , otelLogEnv     :: LogEnv
  }

-- ───────────────────────────────────────────────
-- Tracing
-- ───────────────────────────────────────────────

-- | Kind of span.
data SpanKind = SpanServer | SpanClient | SpanInternal | SpanProducer | SpanConsumer
  deriving (Eq, Show)

-- | Span status.
data SpanStatus = SpanOK | SpanError Text
  deriving (Eq, Show)

-- | A traced span with timing, attributes, and events.
data Span = Span
  { spanName       :: Text
  , spanKind       :: SpanKind
  , spanStart      :: UTCTime
  , spanEnd        :: Maybe UTCTime
  , spanStatus     :: SpanStatus
  , spanAttributes :: Map Text Text
  , spanEvents     :: [(UTCTime, Text, Map Text Text)]
  , spanParentId   :: Maybe Word64
  , spanTraceId    :: Word64
  , spanSpanId     :: Word64
  } deriving (Eq, Show)

-- | Tracer holds the configuration and completed spans.
data Tracer = Tracer
  { _tracerServiceName :: String
  , _tracerServiceVer  :: String
  , tracerSpans       :: IORef [Span]
  , tracerNextId      :: IORef Word64
  , tracerLogEnv      :: LogEnv
  }

-- | Create a new tracer.
newTracer :: String -> String -> LogEnv -> IO Tracer
newTracer name ver logEnv = Tracer name ver <$> newIORef [] <*> newIORef 1 <*> pure logEnv

-- | Run an action inside a new span. The span is recorded when done.
withSpan :: Tracer -> Text -> SpanKind -> Map Text Text -> IO a -> IO a
withSpan tracer name kind attrs action = do
  start <- getCurrentTime
  traceId <- atomicModifyIORef' (tracerNextId tracer) (\n -> (n + 1, n))
  spanId <- atomicModifyIORef' (tracerNextId tracer) (\n -> (n + 1, n))
  let span0 = Span
        { spanName = name
        , spanKind = kind
        , spanStart = start
        , spanEnd = Nothing
        , spanStatus = SpanOK
        , spanAttributes = attrs
        , spanEvents = []
        , spanParentId = Nothing
        , spanTraceId = traceId
        , spanSpanId = spanId
        }
  result <- (Right <$> action) `catch` (\(e :: SomeException) -> pure $ Left e)
  end <- getCurrentTime
  let status = case result of
        Right _ -> SpanOK
        Left e -> SpanError (T.pack $ show e)
      finalized = span0 { spanEnd = Just end, spanStatus = status }
  modifyIORef' (tracerSpans tracer) (finalized :)
  let dur = realToFrac (diffUTCTime end start) :: Double
  logDebug (tracerLogEnv tracer) $ "[trace] " <> name <> " (" <> T.pack (show dur) <> "s)"
  case result of
    Right r -> pure r
    Left e -> error (show e)

-- | withSpan without attributes.
withSpan_ :: Tracer -> Text -> SpanKind -> IO a -> IO a
withSpan_ tracer name kind = withSpan tracer name kind Map.empty

-- | Add an event to the most recent span (best-effort).
addSpanEvent :: Tracer -> Text -> Map Text Text -> IO ()
addSpanEvent tracer name attrs = do
  now <- getCurrentTime
  modifyIORef' (tracerSpans tracer) $ \case
    [] -> []
    (s:ss) -> s { spanEvents = spanEvents s ++ [(now, name, attrs)] } : ss

-- | Add an attribute to the most recent span (best-effort).
addSpanAttribute :: Tracer -> Text -> Text -> IO ()
addSpanAttribute tracer k v = do
  modifyIORef' (tracerSpans tracer) $ \case
    [] -> []
    (s:ss) -> s { spanAttributes = Map.insert k v (spanAttributes s) } : ss

-- | Set the status of the most recent span (best-effort).
setSpanStatus :: Tracer -> SpanStatus -> IO ()
setSpanStatus tracer status = do
  modifyIORef' (tracerSpans tracer) $ \case
    [] -> []
    (s:ss) -> s { spanStatus = status } : ss

-- ───────────────────────────────────────────────
-- Metrics (Prometheus-compatible)
-- ───────────────────────────────────────────────

type CounterName = Text
type GaugeName = Text
type HistogramName = Text

-- | Atomic metrics store with counters, gauges, and histograms.
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
-- Debug tracing (structured JSON events)
-- ───────────────────────────────────────────────

-- | Debug trace environment for local structured event logging.
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
-- OTLP HTTP export (traces + metrics)
-- ───────────────────────────────────────────────

-- | Export completed spans as OTLP JSON to the traces endpoint.
exportTracesOTLP :: OtelConfig -> [Span] -> IO ()
exportTracesOTLP config spans = unless (null spans) $ do
  let body = encodeOTLPTraces config spans
  _ <- httpPost (otelTracesEndpoint config) body
  pure ()

-- | Export metrics as OTLP JSON to the metrics endpoint.
-- Uses current nanosecond timestamps so Prometheus/Collector accepts them.
exportMetricsOTLP :: OtelConfig -> MetricsStore -> IO ()
exportMetricsOTLP config ms = do
  counters <- readIORef (msCounters ms)
  gauges <- readIORef (msGauges ms)
  histograms <- readIORef (msHistograms ms)
  unless (Map.null counters && Map.null gauges && Map.null histograms) $ do
    now <- getCurrentTime
    let ts = T.pack $ show (utcToNano now)
        body = encodeOTLPMetrics config counters gauges histograms ts
    _ <- httpPost (otelMetricsEndpoint config) body
    pure ()

-- ───────────────────────────────────────────────
-- OTLP JSON encoding
-- ───────────────────────────────────────────────

encodeOTLPTraces :: OtelConfig -> [Span] -> BSL.ByteString
encodeOTLPTraces config spans = BSL.fromStrict $ TE.encodeUtf8 $ T.concat
  [ "{\"resourceSpans\":[{\"resource\":{\"attributes\":["
  , "{\"key\":\"service.name\",\"value\":{\"stringValue\":\"" <> T.pack (otelServiceName config) <> "\"}},"
  , "{\"key\":\"service.version\",\"value\":{\"stringValue\":\"" <> T.pack (otelServiceVersion config) <> "\"}}]},"
  , "\"scopeSpans\":[{\"scope\":{\"name\":\"graphos\"},\"spans\":["
  , T.intercalate "," (map encodeSpan spans)
  , "]}]}]}"
  ]

encodeSpan :: Span -> Text
encodeSpan s = T.concat
  [ "{\"traceId\":\"" <> formatTraceId (spanTraceId s) <> "\","
  , "\"spanId\":\"" <> formatSpanId (spanSpanId s) <> "\","
  , "\"name\":\"" <> escapeJson (spanName s) <> "\","
  , "\"kind\":" <> kindNum (spanKind s) <> ","
  , "\"startTimeUnixNano\":\"" <> formatTimeNano (spanStart s) <> "\","
  , case spanEnd s of
      Just end -> "\"endTimeUnixNano\":\"" <> formatTimeNano end <> "\","
      Nothing -> ""
  , "\"status\":{\"code\":" <> statusNum (spanStatus s)
  , case spanStatus s of
      SpanError msg -> ",\"message\":\"" <> escapeJson msg <> "\"}"
      SpanOK -> "}"
  , case spanParentId s of
      Just pid -> ",\"parentSpanId\":\"" <> formatSpanId pid <> "\""
      Nothing -> ""
  , ",\"attributes\":[" <> T.intercalate "," (map attrEntry (Map.toList (spanAttributes s))) <> "]}"
  ]
  where
    kindNum SpanServer = "1"
    kindNum SpanClient = "2"
    kindNum SpanInternal = "3"
    kindNum SpanProducer = "4"
    kindNum SpanConsumer = "5"
    statusNum SpanOK = "0"
    statusNum (SpanError _) = "1"
    attrEntry (k,v) = "{\"key\":\"" <> escapeJson k <> "\",\"value\":{\"stringValue\":\"" <> escapeJson v <> "\"}}"
    -- OTLP requires 128-bit (32 hex char) trace IDs and 64-bit (16 hex char) span IDs.
    -- Word64 is 64-bit (16 hex chars). We use:
    --   traceId: zero-prefix + 16 hex = 32 hex chars (128-bit)
    --   spanId:  16 hex chars (64-bit)
    formatTraceId n = T.replicate 16 "0" <> T.pack (concatMap (padHex . showHexInt) (wordToBytes n))
    formatSpanId n = T.pack (concatMap (padHex . showHexInt) (wordToBytes n))
    padHex h = replicate (2 - length h) '0' ++ h
    showHexInt :: Word8 -> String
    showHexInt b0 = case quotRem b0 16 of
      (q, r) -> (if q > 0 then showHexInt q else "") ++ intToDigitStr (fromIntegral r :: Int)
    intToDigitStr r
      | r < 10 = show r
      | r == 10 = "a"
      | r == 11 = "b"
      | r == 12 = "c"
      | r == 13 = "d"
      | r == 14 = "e"
      | otherwise = "f"
    wordToBytes :: Word64 -> [Word8]
    wordToBytes w = [ fromIntegral (w `shiftR` 56 .&. 0xFF)
                    , fromIntegral (w `shiftR` 48 .&. 0xFF)
                    , fromIntegral (w `shiftR` 40 .&. 0xFF)
                    , fromIntegral (w `shiftR` 32 .&. 0xFF)
                    , fromIntegral (w `shiftR` 24 .&. 0xFF)
                    , fromIntegral (w `shiftR` 16 .&. 0xFF)
                    , fromIntegral (w `shiftR` 8 .&. 0xFF)
                    , fromIntegral (w .&. 0xFF)
                    ]
    formatTimeNano t = T.pack $ show (utcToNano t)

utcToNano :: UTCTime -> Integer
utcToNano t = floor (diffUTCTime t unixEpoch * 1e9)

-- | Unix epoch (1970-01-01 00:00:00 UTC) as a UTCTime value.
unixEpoch :: UTCTime
unixEpoch = UTCTime (fromGregorian 1970 1 1) 0

encodeOTLPMetrics :: OtelConfig -> Map CounterName Int64 -> Map GaugeName Double -> Map HistogramName [Double] -> Text -> BSL.ByteString
encodeOTLPMetrics config counters gauges histograms ts = BSL.fromStrict $ TE.encodeUtf8 $ T.concat
  [ "{\"resourceMetrics\":[{\"resource\":{\"attributes\":["
  , "{\"key\":\"service.name\",\"value\":{\"stringValue\":\"" <> T.pack (otelServiceName config) <> "\"}},"
  , "{\"key\":\"service.version\",\"value\":{\"stringValue\":\"" <> T.pack (otelServiceVersion config) <> "\"}}]},"
  , "\"scopeMetrics\":[{\"scope\":{\"name\":\"graphos\"},\"metrics\":["
  , T.intercalate "," (counterMs ++ gaugeMs ++ histMs)
  , "]}]}]}"
  ]
  where
    counterMs = map encodeCounterMetric (Map.toList counters)
    gaugeMs = map encodeGaugeMetric (Map.toList gauges)
    histMs = map encodeHistogramMetric (Map.toList histograms)

    encodeCounterMetric (name, val) = T.concat
      [ "{\"name\":\"" <> name <> "\",\"description\":\"Total " <> name <> "\",\"unit\":\"1\","
      , "\"sum\":{\"dataPoints\":[{\"asInt\":\"" <> T.pack (show val) <> "\""
      , ",\"startTimeUnixNano\":\"" <> ts <> "\",\"timeUnixNano\":\"" <> ts <> "\"}]"
      , ",\"aggregationTemporality\":2,\"isMonotonic\":true}}"
      ]
    encodeGaugeMetric (name, val) = T.concat
      [ "{\"name\":\"" <> name <> "\",\"description\":\"Current " <> name <> "\",\"unit\":\"1\","
      , "\"gauge\":{\"dataPoints\":[{\"asDouble\":\"" <> T.pack (show val) <> "\""
      , ",\"startTimeUnixNano\":\"" <> ts <> "\",\"timeUnixNano\":\"" <> ts <> "\"}]}}"
      ]
    encodeHistogramMetric (name, vals) = T.concat
      [ "{\"name\":\"" <> name <> "\",\"description\":\"Histogram " <> name <> "\",\"unit\":\"s\","
      , "\"histogram\":{\"dataPoints\":[{\"bucketCounts\":["
      , T.intercalate "," (map (T.pack . show . countInBucket vals) standardBuckets)
      , "],\"explicitBounds\":[" <> T.intercalate "," (map (T.pack . show) standardBuckets) <> "]"
      , ",\"count\":\"" <> T.pack (show (length vals)) <> "\""
      , ",\"sum\":\"" <> T.pack (show (sum vals)) <> "\""
      , ",\"startTimeUnixNano\":\"" <> ts <> "\",\"timeUnixNano\":\"" <> ts <> "\"}]}}"
      ]
    countInBucket vals bound = length $ filter (<= bound) vals
    standardBuckets = [0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]

-- ───────────────────────────────────────────────
-- HTTP POST (best-effort, uses http-conduit)
-- ───────────────────────────────────────────────

httpPost :: String -> BSL.ByteString -> IO ()
httpPost url body = do
  mgr <- newManager defaultManagerSettings
  req <- parseRequest url
  let req' = req { method = methodPost
                 , requestHeaders = [("Content-Type", "application/json")]
                 , requestBody = RequestBodyLBS body
                 }
  result <- (Right <$> httpLbs req' mgr) `catch` (\(e :: SomeException) -> pure $ Left $ show e)
  case result of
    Right _   -> pure ()
    Left err  -> hPutStrLn stderr $ "[graphos] WARNING: OTLP POST to " ++ url ++ " failed: " ++ err

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

-- | Initialize full observability stack.
-- Creates tracer, metrics store, debug trace, and optionally starts:
--   * OTLP background exporter (if otelEnabled)
--   * Structured JSON log shipping to OTLP Collector → Loki (if otelEnabled && logLevel >= LevelTrace)
--   * Prometheus metrics server (if metricsPort is Just)
initObservability :: LogLevel -> OtelConfig -> Maybe Int -> FilePath -> IO ObservabilityEnv
initObservability logLevel otelCfg metricsPort debugDir = do
  logEnv <- defaultLogEnv logLevel
  tracer <- newTracer (otelServiceName otelCfg) (otelServiceVersion otelCfg) logEnv
  metrics <- newMetricsStore
  debugTrace <- newDebugTraceEnv (logLevel >= LevelDebug) debugDir

  when (otelEnabled otelCfg) $ do
    logInfo logEnv "Starting OTLP metrics exporter..."
    -- Enable structured JSON log shipping to OTLP Collector → Loki
    -- When --otel is active, all logs at or above the current log level are shipped.
    -- Previously this was gated behind --debug (LevelTrace), which meant --otel alone
    -- produced no logs in Grafana — a common source of confusion.
    enableOtlpLogShipping logEnv (otelLogsEndpoint otelCfg) (otelServiceName otelCfg)
    _ <- forkIO $ forever $ do
      threadDelay (otelExportInterval otelCfg * 1000000)
      spans <- readIORef (tracerSpans tracer)
      unless (null spans) $ exportTracesOTLP otelCfg spans
      exportMetricsOTLP otelCfg metrics
    pure ()

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
    }

-- | Shut down observability: flush debug traces, OTLP logs, and export remaining spans.
shutdownObservability :: ObservabilityEnv -> IO ()
shutdownObservability env = do
  flushDebugTrace (otelDebugTrace env)
  flushOtlpLogs (otelLogEnv env)
  when (otelEnabled (otelConfig env)) $ do
    spans <- readIORef (tracerSpans (otelTracer env))
    unless (null spans) $ exportTracesOTLP (otelConfig env) spans
    exportMetricsOTLP (otelConfig env) (otelMetrics env)

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

escapeJson :: Text -> Text
escapeJson = T.concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = T.singleton c