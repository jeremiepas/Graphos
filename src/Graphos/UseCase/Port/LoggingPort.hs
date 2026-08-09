-- | Port interface for logging operations.
-- Record-of-functions that decouples UseCase from Infrastructure.
-- Only Domain types appear in signatures.
module Graphos.UseCase.Port.LoggingPort
  ( -- * Logging port
    LoggingPort(..)
    -- * Log levels (re-exported from Domain.Logging)
  , LogLevel(..)
  ) where

import Data.Text (Text)
import Graphos.Domain.Logging (LogLevel(..))

-- | Record-of-functions port for logging.
data LoggingPort = LoggingPort
  { lpLogTrace :: Text -> IO ()
  , lpLogDebug :: Text -> IO ()
  , lpLogInfo  :: Text -> IO ()
  , lpLogWarn  :: Text -> IO ()
  , lpLogError :: Text -> IO ()
  }