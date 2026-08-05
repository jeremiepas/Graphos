-- | Port interface for observability operations (tracing, metrics, debug tracing).
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Only Domain types appear in signatures.
module Graphos.UseCase.Port.ObservabilityPort
  ( -- * Observability port
    ObservabilityPort(..)
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Graphos.Domain.Config (OtelConfig)

-- | Record-of-functions port for observability.
-- Infrastructure.Wiring provides the concrete implementation using
-- hs-opentelemetry-sdk, IORef MetricsStore, and debug trace.
data ObservabilityPort = ObservabilityPort
  { -- | Initialize observability from config and CLI flags.
    --   Returns an opaque handle that the pipeline carries.
    opInitObservability :: OtelConfig -> Maybe Int -> FilePath -> IO ()
    -- | Shut down observability (flush traces, metrics, debug trace).
  , opShutdownObservability :: IO ()
    -- | Record a metric counter increment.
  , opIncCounter :: Text -> Int64 -> IO ()
    -- | Record a metric gauge value.
  , opSetGauge :: Text -> Double -> IO ()
    -- | Write a debug trace event.
  , opTraceEvent :: Text -> [(Text, Text)] -> IO ()
    }