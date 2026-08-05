-- | Observability configuration types.
-- ObservabilityConfig, OtelConfig, and their defaults.
-- Pure data types — no IO.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config.Observability
  ( ObservabilityConfig(..)
  , defaultObservabilityConfig
  , OtelConfig(..)
  , defaultOtelConfig
  , mergeObservabilityConfig
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, withObject, (.:?), (.!=))
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import Graphos.Domain.Config.Extraction (lowerFirst)
import GHC.Generics (Generic)

-- ───────────────────────────────────────────────
-- Observability Configuration
-- ───────────────────────────────────────────────

-- | Configuration for tracing, metrics, and debug instrumentation.
-- All fields are optional in graphos.yaml — missing values fall back to defaults.
--
-- CLI flags (--otel, --metrics, --debug-trace) override these values.
data ObservabilityConfig = ObservabilityConfig
   { obsEnabled        :: Bool     -- ^ Enable OpenTelemetry trace/metric export
   , obsEndpoint       :: String   -- ^ OTLP endpoint base URL (e.g. "http://localhost:14319")
   , obsMetricsPort    :: Int      -- ^ Prometheus metrics server port (0 = disabled)
   , obsServiceName    :: String   -- ^ Service name for spans
   , obsServiceVersion :: String   -- ^ Service version for spans
   , obsExportInterval :: Int      -- ^ Metrics export interval in seconds
   , obsDebugTraceDir  :: String   -- ^ Directory for debug trace JSONL files ("" = disabled)
   , obsDebug          :: Bool     -- ^ Enable debug mode: TRACE logs + structured log shipping to Loki
   } deriving (Eq, Show, Generic)

instance ToJSON ObservabilityConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }

instance FromJSON ObservabilityConfig where
  parseJSON = withObject "ObservabilityConfig" $ \v -> ObservabilityConfig
    <$> v .:? "enabled"         .!= False
    <*> v .:? "endpoint"        .!= "http://localhost:4318"
    <*> v .:? "metricsPort"     .!= 0
    <*> v .:? "serviceName"     .!= "graphos"
    <*> v .:? "serviceVersion"  .!= "0.1.0"
    <*> v .:? "exportInterval" .!= 15
    <*> v .:? "debugTraceDir"  .!= ""
    <*> v .:? "debug"           .!= False

defaultObservabilityConfig :: ObservabilityConfig
defaultObservabilityConfig = ObservabilityConfig
  { obsEnabled        = False
  , obsEndpoint       = "http://localhost:4318"
  , obsMetricsPort    = 0
  , obsServiceName    = "graphos"
  , obsServiceVersion = "0.1.0"
  , obsExportInterval = 15
  , obsDebugTraceDir  = ""
  , obsDebug          = False
  }

-- | Runtime OpenTelemetry configuration derived from CLI flags.
-- Kept in Domain because it is pure configuration data (no IO), though it is
-- consumed by 'Graphos.Infrastructure.Observability.SDK'.
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

-- | Merge two ObservabilityConfig values: project overrides global.
-- A field in project is considered "explicit" if it differs from the default.
mergeObservabilityConfig :: ObservabilityConfig -> ObservabilityConfig -> ObservabilityConfig
mergeObservabilityConfig global project = ObservabilityConfig
  { obsEnabled        = if obsEnabled project /= obsEnabled defaultObservabilityConfig
                           then obsEnabled project
                           else obsEnabled global
  , obsEndpoint        = if obsEndpoint project /= obsEndpoint defaultObservabilityConfig
                           then obsEndpoint project
                           else obsEndpoint global
  , obsMetricsPort     = if obsMetricsPort project /= obsMetricsPort defaultObservabilityConfig
                           then obsMetricsPort project
                           else obsMetricsPort global
  , obsServiceName     = if obsServiceName project /= obsServiceName defaultObservabilityConfig
                           then obsServiceName project
                           else obsServiceName global
  , obsServiceVersion  = if obsServiceVersion project /= obsServiceVersion defaultObservabilityConfig
                           then obsServiceVersion project
                           else obsServiceVersion global
  , obsExportInterval  = if obsExportInterval project /= obsExportInterval defaultObservabilityConfig
                           then obsExportInterval project
                           else obsExportInterval global
  , obsDebugTraceDir   = if obsDebugTraceDir project /= obsDebugTraceDir defaultObservabilityConfig
                           then obsDebugTraceDir project
                           else obsDebugTraceDir global
  , obsDebug            = if obsDebug project /= obsDebug defaultObservabilityConfig
                           then obsDebug project
                           else obsDebug global
  }