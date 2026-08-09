-- | Port interface for observability operations (tracing, metrics, debug tracing).
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Only Domain types appear in signatures.
module Graphos.UseCase.Port.ObservabilityPort
  ( -- * Observability port
    ObservabilityPort(..)
    -- * Time types
  , StartTime(..)
  , EndTime(..)
  ) where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Graphos.Domain.Config (OtelConfig(..))
import Graphos.Domain.Logging (LogEnv)

-- | Record-of-functions port for observability.
-- Infrastructure.Wiring provides the concrete implementation using
-- hs-opentelemetry-sdk, IORef MetricsStore, and debug trace.
data ObservabilityPort = ObservabilityPort
  { opLogEnv :: LogEnv
    -- ^ Access to the logging environment (LogEnv).
    --   Provides log functions via LoggingPort without importing Infrastructure.
  , opInitObservability :: OtelConfig -> Maybe Int -> FilePath -> IO ()
    -- ^ Initialize observability from config and CLI flags.
    --   Returns an opaque handle that the pipeline carries.
  , opShutdownObservability :: IO ()
    -- ^ Shut down observability (flush traces, metrics, debug trace).
  , opIncCounter :: Text -> Int64 -> IO ()
    -- ^ Record a metric counter increment.
  , opSetGauge :: Text -> Double -> IO ()
    -- ^ Record a metric gauge value.
  , opRecordHistogram :: Text -> Double -> IO ()
    -- ^ Record a metric histogram value.
  , opTraceEvent :: Text -> [(Text, Text)] -> IO ()
    -- ^ Write a debug trace event.
  , opDebugTraceSpan :: Text -> StartTime -> EndTime -> Map Text Text -> IO ()
    -- ^ Record a debug trace span with timing.
  }

-- | Opaque start/end time types for debug trace spans.
newtype StartTime = StartTime UTCTime deriving (Eq, Show)
newtype EndTime = EndTime UTCTime deriving (Eq, Show)
