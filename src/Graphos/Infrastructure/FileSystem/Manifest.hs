-- | Manifest saving - track file mtimes for incremental updates
module Graphos.Infrastructure.FileSystem.Manifest
  ( saveManifest
  , loadManifest
  , saveManifestIn
  , loadManifestIn
  , manifestPathIn
  , ManifestEntry(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), object, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Graphos.Domain.Config (defaultOutputDirName)
import Graphos.Infrastructure.FileSystem.AtomicWrite (writeFileAtomic)

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

-- | Manifest path under the effective pipeline output directory
-- (multi-source-graphs 2.2): @<outDir>/manifest.json@.
manifestPathIn :: FilePath -> FilePath
manifestPathIn outDir = outDir </> "manifest.json"

-- | Save the manifest under the effective output directory (atomic).
saveManifestIn :: [ManifestEntry] -> FilePath -> IO ()
saveManifestIn entries outDir =
  writeFileAtomic (manifestPathIn outDir) (encode entries)

-- | Load the manifest from the effective output directory.
loadManifestIn :: FilePath -> IO (Either Text [ManifestEntry])
loadManifestIn outDir = loadAt (manifestPathIn outDir)

-- | Legacy root-based manifest path (pre-change project-root convention
-- @<root>/graphos-out/manifest.json@) — retained for compatibility.
manifestPath :: FilePath -> FilePath
manifestPath root = root </> defaultOutputDirName </> "manifest.json"

-- | Save manifest to graphos-out/manifest.json (atomic)
saveManifest :: [ManifestEntry] -> FilePath -> IO ()
saveManifest entries root = saveManifestIn entries (root </> defaultOutputDirName)

-- | Load manifest from graphos-out/manifest.json
loadManifest :: FilePath -> IO (Either Text [ManifestEntry])
loadManifest root = loadAt (manifestPath root)

loadAt :: FilePath -> IO (Either Text [ManifestEntry])
loadAt path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right [])
    else do
      bs <- BSL.readFile path
      case eitherDecode bs of
        Left err  -> pure (Left (T.pack err))
        Right entries -> pure (Right entries)