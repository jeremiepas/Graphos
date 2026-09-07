-- | Opaque handle for incremental JSON writers.
-- Defined in Domain so that UseCase ports can refer to it without
-- importing Infrastructure concrete types.
module Graphos.Domain.Types.Writer
  ( IncrementalWriter(..)
  ) where

import Data.IORef (IORef)
import System.IO (Handle)

-- | Opaque handle to an incremental JSON writer.
-- Infrastructure implementations attach a real handle and state;
-- UseCase only passes the handle through port methods.
--
-- @iwTmpPath@/@iwTarget@ carry the atomic-write bookkeeping: the writer
-- streams into a temp file and the target path it will be renamed over at
-- commit time. Both are @Nothing@ for non-atomic writers (e.g. stdout).
data IncrementalWriter = IncrementalWriter
  { iwHandle  :: Handle
  , iwFirst   :: IORef Bool
  , iwTmpPath :: Maybe FilePath
  , iwTarget  :: Maybe FilePath
  }