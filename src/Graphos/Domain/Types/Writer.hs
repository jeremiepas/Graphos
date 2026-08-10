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
data IncrementalWriter = IncrementalWriter
  { iwHandle :: Handle
  , iwFirst  :: IORef Bool
  }
