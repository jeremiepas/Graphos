-- | Port interface for logging operations.
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Only Domain types appear in signatures.
module Graphos.UseCase.Port.LoggingPort
  ( -- * Logging port
    LoggingPort(..)
  , LogLevel(..)
  ) where

import Data.Text (Text)

-- | Log levels, matching Infrastructure.Logging.
data LogLevel = LogTrace | LogDebug | LogInfo | LogWarn | LogError
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Record-of-functions port for logging.
data LoggingPort = LoggingPort
  { lpLogTrace :: Text -> IO ()
  , lpLogDebug :: Text -> IO ()
  , lpLogInfo  :: Text -> IO ()
  , lpLogWarn  :: Text -> IO ()
  , lpLogError :: Text -> IO ()
  }