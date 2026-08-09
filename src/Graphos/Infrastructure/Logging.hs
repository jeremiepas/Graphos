{-# LANGUAGE ScopedTypeVariables #-}
-- | Logging infrastructure with severity levels and OTLP log shipping.
-- Supports: ERROR, WARN, INFO (default), DEBUG, TRACE
-- Controlled via --verbose and --debug CLI flags.
--
-- When OTLP log export is enabled (--otel --debug), every log line is also
-- shipped as a structured JSON OTLP log record to the OpenTelemetry Collector,
-- which forwards to Loki for Grafana visualization.
module Graphos.Infrastructure.Logging
  ( -- * Log levels (re-exported from Domain.Logging for backward compat)
    LogLevel(..)
  , logLevelToInt
  , logLevelFromInt
  , logLevelToOtlpSeverity
    -- * Log environment (re-exported from Domain.Logging for backward compat)
  , OtlpLogConfig(..)
  , LogEnv(..)
    -- * Logging monad
  , defaultLogEnv
  , runWithLog
    -- * Log functions
  , logError
  , logWarn
  , logInfo
  , logDebug
  , logTrace
    -- * OTLP log shipping
  , enableOtlpLogShipping
  , setLogTraceContext
  , flushOtlpLogs
    -- * Convenience
  , withTiming
  , withTimingDebug
  ) where

import Graphos.Domain.Logging
  ( LogLevel(..)
  , logLevelToInt
  , logLevelFromInt
  , logLevelToOtlpSeverity
  , OtlpLogConfig(..)
  , LogEnv(..)
  )

import Control.Concurrent.MVar (newMVar, modifyMVar_, tryTakeMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (when, unless)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime(..), getCurrentTime, formatTime, defaultTimeLocale, diffUTCTime, fromGregorian)
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
import System.IO (hFlush, stdout, stderr, hPutStrLn)

-- ───────────────────────────────────────────────
-- Log environment
-- ───────────────────────────────────────────────
-- (LogLevel, OtlpLogConfig, LogEnv are re-exported from Domain.Logging)

-- | Create a default log environment at the given level
defaultLogEnv :: LogLevel -> IO LogEnv
defaultLogEnv level = do
  ref <- newIORef level
  otlpRef <- newIORef Nothing
  traceIdRef <- newIORef Nothing
  pure LogEnv { leLevel = ref, lePrefix = "graphos", leOtlpConfig = otlpRef, leTraceId = traceIdRef }

-- | Run an action with a log environment
runWithLog :: LogLevel -> (LogEnv -> IO a) -> IO a
runWithLog level action = do
  env <- defaultLogEnv level
  action env

-- ───────────────────────────────────────────────
-- OTLP log shipping
-- ───────────────────────────────────────────────

-- | Enable OTLP log shipping. Only call when --otel --debug are both active.
enableOtlpLogShipping :: LogEnv -> String -> String -> IO ()
enableOtlpLogShipping env endpoint serviceName = do
  buffer <- newMVar []
  let cfg = OtlpLogConfig
        { olcEndpoint = endpoint
        , olcServiceName = serviceName
        , olcBuffer = buffer
        , olcBatchSize = 1  -- immediate flush fordev; increase for production
        }
  writeIORef (leOtlpConfig env) (Just cfg)

-- | Set/unset the current trace ID for log-trace correlation.
setLogTraceContext :: LogEnv -> Maybe Text -> IO ()
setLogTraceContext env tid = writeIORef (leTraceId env) tid

-- | Ship a log record to OTLP Collector (best-effort, non-blocking)
shipLogToOtlp :: LogEnv -> LogLevel -> UTCTime -> Text -> IO ()
shipLogToOtlp env level timestamp msg = do
  mCfg <- readIORef (leOtlpConfig env)
  case mCfg of
    Nothing -> pure ()
    Just cfg -> do
      mTraceId <- readIORef (leTraceId env)
      let ts = T.pack $ show (floor (diffUTCTime timestamp unixEpoch * 1e9) :: Integer)
          severityNum = logLevelToOtlpSeverity level
          levelStr = T.toUpper . T.pack $ drop 5 (show level)
          traceAttr = case mTraceId of
            Just tid -> ",{\"key\":\"trace_id\",\"value\":{\"stringValue\":\"" <> escapeJson tid <> "\"}}"
            Nothing -> ""
          record = T.concat
            [ "{\"timeUnixNano\":\"" <> ts <> "\""
            , ",\"severityNumber\":" <> T.pack (show severityNum)
            , ",\"severityText\":\"" <> levelStr <> "\""
            , ",\"body\":{\"stringValue\":\"" <> escapeJson msg <> "\"}"
            , ",\"attributes\":["
            , "{\"key\":\"service.name\",\"value\":{\"stringValue\":\"" <> T.pack (olcServiceName cfg) <> "\"}}"
            , ",{\"key\":\"level\",\"value\":{\"stringValue\":\"" <> levelStr <> "\"}}"
            , traceAttr
            , "]}"
            ]
      modifyMVar_ (olcBuffer cfg) $ \buf -> do
        let newBuf = buf ++ [record]
        -- Flush when batch is full
        when (length newBuf >= olcBatchSize cfg) $ flushOtlpLogBuffer cfg newBuf
        pure $ if length newBuf >= olcBatchSize cfg then [] else newBuf

-- | Flush buffered OTLP log records to the Collector
flushOtlpLogBuffer :: OtlpLogConfig -> [Text] -> IO ()
flushOtlpLogBuffer _cfg [] = pure ()
flushOtlpLogBuffer cfg records = do
  let body = BSL.fromStrict $ TE.encodeUtf8 $ T.concat
        [ "{\"resourceLogs\":[{\"resource\":{\"attributes\":["
        , "{\"key\":\"service.name\",\"value\":{\"stringValue\":\"" <> T.pack (olcServiceName cfg) <> "\"}}"
        , "]},\"scopeLogs\":[{\"scope\":{\"name\":\"graphos\"},\"logRecords\":["
        , T.intercalate "," records
        , "]}]}]}"
        ]
  _ <- httpPostLog (olcEndpoint cfg) body
  pure ()

-- | HTTP POST for log shipping (best-effort, logs warnings on failure)
httpPostLog :: String -> BSL.ByteString -> IO ()
httpPostLog url body = do
  mgr <- newManager defaultManagerSettings
  req <- parseRequest url
  let req' = req { method = "POST"
                 , requestHeaders = [("Content-Type", "application/json")]
                 , requestBody = RequestBodyLBS body
                 }
  result <- (Right <$> httpLbs req' mgr) `catch` (\(e :: SomeException) -> pure $ Left $ show e)
  case result of
    Right _   -> pure ()
    Left err  -> hPutStrLn stderr $ "[graphos] WARNING: OTLP log POST to " ++ url ++ " failed: " ++ err

-- | Flush remaining buffered logs (call at shutdown)
flushOtlpLogs :: LogEnv -> IO ()
flushOtlpLogs env = do
  mCfg <- readIORef (leOtlpConfig env)
  case mCfg of
    Nothing -> pure ()
    Just cfg -> do
      remaining <- tryTakeMVar (olcBuffer cfg)
      case remaining of
        Just records -> unless (null records) $ flushOtlpLogBuffer cfg records
        Nothing     -> pure ()

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
            LogError -> "ERROR"
            LogWarn  -> " WARN"
            LogInfo  -> " INFO"
            LogDebug -> "DEBUG"
            LogTrace -> "TRACE"
          line = "[" ++ ts ++ "] [" ++ levelTag ++ "] [" ++ T.unpack prefix ++ "] " ++ T.unpack msg
      if level == LogError
        then hPutStrLn stderr line
        else putStrLn line
      hFlush stdout
      -- Ship to OTLP if enabled
      shipLogToOtlp env level timestamp msg
    else pure ()

-- | Log an error message (always shown)
logError :: LogEnv -> Text -> IO ()
logError env = logMessage env LogError

-- | Log a warning message
logWarn :: LogEnv -> Text -> IO ()
logWarn env = logMessage env LogWarn

-- | Log an info message (default level)
logInfo :: LogEnv -> Text -> IO ()
logInfo env = logMessage env LogInfo

-- | Log a debug message (shown with --verbose)
logDebug :: LogEnv -> Text -> IO ()
logDebug env = logMessage env LogDebug

-- | Log a trace message (shown with --debug)
logTrace :: LogEnv -> Text -> IO ()
logTrace env = logMessage env LogTrace

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

-- ───────────────────────────────────────────────
-- JSON escaping
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

-- | Unix epoch (1970-01-01) as a UTCTime value for nanosecond timestamp calculation.
unixEpoch :: UTCTime
unixEpoch = UTCTime (fromGregorian 1970 1 1) 0