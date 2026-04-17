-- | Manifest saving - track file mtimes for incremental updates
module Graphos.Infrastructure.FileSystem.Manifest
  ( saveManifest
  , loadManifest
  , ManifestEntry(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), object, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, createDirectoryIfMissing)

-- | Manifest entry - file path and its modification time
data ManifestEntry = ManifestEntry
  { mePath    :: FilePath
  , meMtime   :: Text  -- ISO8601 string
  , meHash    :: Text  -- Content hash for change detection
  } deriving (Eq, Show)

instance ToJSON ManifestEntry where
  toJSON e = object
    [ "path"  .= mePath e
    , "mtime" .= meMtime e
    , "hash"  .= meHash e
    ]

instance FromJSON ManifestEntry where
  parseJSON = withObject "ManifestEntry" $ \v -> do
    path  <- v .: "path"
    mtime <- v .: "mtime"
    hash  <- v .: "hash"
    pure ManifestEntry { mePath = path, meMtime = mtime, meHash = hash }

-- | Save manifest to graphos-out/manifest.json
saveManifest :: [ManifestEntry] -> FilePath -> IO ()
saveManifest entries root = do
  let path = root ++ "/graphos-out/manifest.json"
  createDirectoryIfMissing True (root ++ "/graphos-out")
  BSL.writeFile path (encode entries)

-- | Load manifest from graphos-out/manifest.json
loadManifest :: FilePath -> IO (Either Text [ManifestEntry])
loadManifest root = do
  let path = root ++ "/graphos-out/manifest.json"
  exists <- doesFileExist path
  if not exists
    then pure (Right [])
    else do
      bs <- BSL.readFile path
      case eitherDecode bs of
        Left err  -> pure (Left (T.pack err))
        Right entries -> pure (Right entries)