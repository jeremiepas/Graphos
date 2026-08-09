-- | Logging domain types: log levels, log environment, and OTLP config.
-- Pure data types — no IO. Used by both Domain and Infrastructure layers.
module Graphos.Domain.Logging
  ( -- * Log levels
    LogLevel(..)
  , logLevelToInt
  , logLevelFromInt
  , logLevelToOtlpSeverity
    -- * Log environment
  , OtlpLogConfig(..)
  , LogEnv(..)
  ) where

import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef)
import Data.Text (Text)

-- ───────────────────────────────────────────────
-- Log levels
-- ───────────────────────────────────────────────

-- | Log severity level.
-- Matching LoggingPort for consistency across layers.
data LogLevel
  = LogTrace   -- ^ Everything, including internal tracing (--debug)
  | LogDebug   -- ^ Debug + info + warnings + errors (--verbose)
  | LogInfo    -- ^ Info + warnings + errors (default)
  | LogWarn    -- ^ Warnings + errors
  | LogError   -- ^ Errors only (always shown)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Convert log level to integer (for comparison)
logLevelToInt :: LogLevel -> Int
logLevelToInt LogTrace = 4
logLevelToInt LogDebug = 3
logLevelToInt LogInfo  = 2
logLevelToInt LogWarn  = 1
logLevelToInt LogError = 0

-- | Parse log level from an integer (0=error, 1=warn, etc.)
logLevelFromInt :: Int -> LogLevel
logLevelFromInt n
  | n <= 0    = LogError
  | n == 1    = LogWarn
  | n == 2    = LogInfo
  | n == 3    = LogDebug
  | otherwise = LogTrace

-- | Map log level to OTLP severity number
logLevelToOtlpSeverity :: LogLevel -> Int
logLevelToOtlpSeverity LogError = 17  -- ERROR
logLevelToOtlpSeverity LogWarn  = 13  -- WARN
logLevelToOtlpSeverity LogInfo  = 9   -- INFO
logLevelToOtlpSeverity LogDebug = 5   -- DEBUG
logLevelToOtlpSeverity LogTrace = 1   -- TRACE

-- ───────────────────────────────────────────────
-- Log environment
-- ───────────────────────────────────────────────

-- | OTLP log shipping config (set when --otel --debug are both active)
data OtlpLogConfig = OtlpLogConfig
  { olcEndpoint     :: String    -- ^ OTLP HTTP endpoint (e.g. "http://localhost:4318/v1/logs")
  , olcServiceName  :: String    -- ^ Service name for resource attributes
  , olcBuffer       :: MVar [Text]  -- ^ Buffer of pending OTLP log records
  , olcBatchSize    :: Int       -- ^ Flush after this many records (1 = immediate flush)
  }

-- | Logging environment, threaded through the application
data LogEnv = LogEnv
  { leLevel       :: IORef LogLevel       -- ^ Current log level (mutable for runtime adjustment)
  , lePrefix      :: Text                 -- ^ Module/component prefix
  , leOtlpConfig  :: IORef (Maybe OtlpLogConfig)  -- ^ OTLP log shipping (Nothing = disabled)
  , leTraceId     :: IORef (Maybe Text)   -- ^ Current trace ID for log correlation
  }
