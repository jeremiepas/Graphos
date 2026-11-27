-- | Reads the lines of a source file for @graphos source@
-- (function-source-access). Bytes are decoded as UTF-8, invalid sequences
-- replaced, and a trailing carriage return is dropped from each line.
module Graphos.Infrastructure.FileSystem.SourceReader
  ( readSourceLines
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (doesFileExist)

-- | The lines of a file; 'Nothing' when it does not exist or cannot be read.
readSourceLines :: FilePath -> IO (Maybe [Text])
readSourceLines path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      r <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      pure $ case r of
        Left _      -> Nothing
        Right bytes -> Just (map dropCR (T.lines (decodeUtf8With lenientDecode bytes)))
  where
    dropCR l = maybe l id (T.stripSuffix (T.pack "\r") l)
