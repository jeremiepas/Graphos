-- | Port for reading the source file behind a node (function-source-access).
-- Record-of-functions that keeps the disk access of @graphos source@ in
-- Infrastructure; the range selection is pure and lives in
-- "Graphos.UseCase.Source".
module Graphos.UseCase.Port.SourceReaderPort
  ( SourceReaderPort(..)
  ) where

import Data.Text (Text)

newtype SourceReaderPort = SourceReaderPort
  { -- | The lines of a file, without line terminators; 'Nothing' when the
    -- file does not exist or cannot be read.
    srpReadLines :: FilePath -> IO (Maybe [Text])
  }
