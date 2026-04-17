-- | Logging infrastructure with severity levels.
-- Supports: ERROR, WARN, INFO (default), DEBUG, TRACE
-- Controlled via --verbose and --debug CLI flags.
module Graphos.Infrastructure.Logging
  ( -- * Log levels
    LogLevel(..)
  , logLevelToInt
  , logLevelFromInt
    -- * Logging monad
  , LogEnv(..)
  , defaultLogEnv
  , runWithLog
    -- * Log functions
  , logError
  , logWarn
  , logInfo
  , logDebug
  , logTrace
    -- * Convenience
  , withTiming
  , withTimingDebug
  ) where

import Data.IORef (IORef, newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale, diffUTCTime)
import System.IO (hFlush, stdout, stderr, hPutStrLn)

-- ───────────────────────────────────────────────
-- Log levels
-- ───────────────────────────────────────────────

-- | Log severity level
data LogLevel
  = LevelError   -- ^ Errors only (always shown)
  | LevelWarn    -- ^ Warnings + errors
  | LevelInfo    -- ^ Info + warnings + errors (default)
  | LevelDebug   -- ^ Debug + info + warnings + errors (--verbose)
  | LevelTrace   -- ^ Everything, including internal tracing (--debug)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Convert log level to integer (for comparison)
logLevelToInt :: LogLevel -> Int
logLevelToInt LevelError = 0
logLevelToInt LevelWarn  = 1
logLevelToInt LevelInfo  = 2
logLevelToInt LevelDebug = 3
logLevelToInt LevelTrace = 4

-- | Parse log level from an integer (0=error, 1=warn, etc.)
logLevelFromInt :: Int -> LogLevel
logLevelFromInt n
  | n <= 0    = LevelError
  | n == 1    = LevelWarn
  | n == 2    = LevelInfo
  | n == 3    = LevelDebug
  | otherwise = LevelTrace

-- ───────────────────────────────────────────────
-- Log environment
-- ───────────────────────────────────────────────

-- | Logging environment, threaded through the application
data LogEnv = LogEnv
  { leLevel :: IORef LogLevel   -- ^ Current log level (mutable for runtime adjustment)
  , lePrefix :: Text            -- ^ Module/component prefix
  }

-- | Create a default log environment at the given level
defaultLogEnv :: LogLevel -> IO LogEnv
defaultLogEnv level = do
  ref <- newIORef level
  pure LogEnv { leLevel = ref, lePrefix = "graphos" }

-- | Run an action with a log environment
runWithLog :: LogLevel -> (LogEnv -> IO a) -> IO a
runWithLog level action = do
  env <- defaultLogEnv level
  action env

-- ───────────────────────────────────────────────
-- Core logging
-- ───────────────────────────────────────────────

-- | Log a message at the specified level
logMessage :: LogEnv -> LogLevel -> Text -> IO ()
logMessage env level msg = do
  currentLevel <- readIORef (leLevel env)
  if logLevelToInt level <= logLevelToInt currentLevel
    then do
      timestamp <- getCurrentTime
      let ts = formatTime defaultTimeLocale "%H:%M:%S" timestamp
          prefix = lePrefix env
          levelTag = case level of
            LevelError -> "ERROR"
            LevelWarn  -> " WARN"
            LevelInfo  -> " INFO"
            LevelDebug -> "DEBUG"
            LevelTrace -> "TRACE"
          line = "[" ++ ts ++ "] [" ++ levelTag ++ "] [" ++ T.unpack prefix ++ "] " ++ T.unpack msg
      if level == LevelError
        then hPutStrLn stderr line
        else putStrLn line
      hFlush stdout
    else pure ()

-- | Log an error message (always shown)
logError :: LogEnv -> Text -> IO ()
logError env = logMessage env LevelError

-- | Log a warning message
logWarn :: LogEnv -> Text -> IO ()
logWarn env = logMessage env LevelWarn

-- | Log an info message (default level)
logInfo :: LogEnv -> Text -> IO ()
logInfo env = logMessage env LevelInfo

-- | Log a debug message (shown with --verbose)
logDebug :: LogEnv -> Text -> IO ()
logDebug env = logMessage env LevelDebug

-- | Log a trace message (shown with --debug)
logTrace :: LogEnv -> Text -> IO ()
logTrace env = logMessage env LevelTrace

-- ───────────────────────────────────────────────
-- Timing helpers
-- ───────────────────────────────────────────────

-- | Time an action and log at INFO level
withTiming :: LogEnv -> Text -> IO a -> IO a
withTiming env label action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  let elapsed = show (diffTimeSec end start) ++ "s"
  logInfo env $ label <> " completed in " <> T.pack elapsed
  pure result
  where
    diffTimeSec t2 t1 = realToFrac (diffUTCTime t2 t1) :: Double
withTimingDebug :: LogEnv -> Text -> IO a -> IO a
withTimingDebug env label action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  let elapsed = show (diffTimeSec end start) ++ "s"
  logDebug env $ label <> " completed in " <> T.pack elapsed
  pure result
  where
    diffTimeSec t2 t1 = realToFrac (diffUTCTime t2 t1) :: Double
