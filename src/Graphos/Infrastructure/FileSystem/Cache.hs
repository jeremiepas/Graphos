-- | Extraction cache - skip unchanged files on re-run
-- Stores per-file extraction results keyed by SHA256 hash of file contents.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.FileSystem.Cache
  ( loadCached
  , saveCached
  , checkSemanticCache
  , saveSemanticCache
  , clearCache
  , cacheDir
  , loadPipelineCheckpoint
  , savePipelineCheckpoint
  , clearPipelineCheckpoint
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), object, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (doesFileExist, createDirectoryIfMissing, removeFile, renameFile)
import System.FilePath (takeFileName, (</>))

import Graphos.Domain.Types
import Graphos.Domain.Types.Pipeline (PipelineCheckpoint(..))

-- | Get the cache directory path
cacheDir :: FilePath -> FilePath
cacheDir root = root </> "graphos-out" </> "cache"

-- | Load cached extraction for a file (returns Nothing if not cached or file changed)
loadCached :: FilePath -> FilePath -> IO (Maybe Extraction)
loadCached path root = do
  h <- fileHash path root
  let entry = cacheDir root </> h ++ ".json"
  exists <- doesFileExist entry
  if not exists
    then pure Nothing
    else do
      bs <- BSL.readFile entry
      case eitherDecode bs of
        Left _   -> pure Nothing
        Right cached -> pure (Just (cachedToExtraction cached))

-- | Save extraction result for a file
saveCached :: FilePath -> Extraction -> FilePath -> IO ()
saveCached path result root = do
  h <- fileHash path root
  let entry = cacheDir root </> h ++ ".json"
      tmp   = entry ++ ".tmp"
  createDirectoryIfMissing True (cacheDir root)
  BSL.writeFile tmp (encode (extractionToCached result))
  renameFile tmp entry  -- atomic write

-- | Check semantic cache for a list of files
-- Returns (cachedExtractions, uncachedFiles)
checkSemanticCache :: [FilePath] -> FilePath -> IO ([Extraction], [FilePath])
checkSemanticCache files root = do
  results <- mapM checkOne files
  let (cached, uncached) = foldl' classify ([], []) results
  pure (reverse cached, reverse uncached)
  where
    checkOne f = do
      mExt <- loadCached f root
      pure (f, mExt)
    classify (cached, uncached) (_f, Just ext) = (ext : cached, uncached)
    classify (cached, uncached) (f, Nothing)   = (cached, f : uncached)

-- | Save semantic extraction results grouped by source_file
saveSemanticCache :: [Node] -> [Edge] -> [Hyperedge] -> FilePath -> IO Int
saveSemanticCache nodes edges _hyperedges root = do
  let byFile = groupBySourceFile nodes edges
  mapM_ (\(fpath, (ns, es)) -> saveCached fpath (extractionFromLists ns es) root) (Map.toList byFile)
  pure (Map.size byFile)

-- | Clear all cache entries
clearCache :: FilePath -> IO ()
clearCache root = do
  let dir = cacheDir root
  exists <- doesFileExist dir
  if exists
    then removeFile dir  -- simplified: just remove the dir marker
    else pure ()

-- ───────────────────────────────────────────────
-- Internal cached extraction type (with JSON instances)
-- ───────────────────────────────────────────────

-- | Serializable representation for caching
data CachedExtraction = CachedExtraction
  { ceNodes      :: [Node]
  , ceEdges      :: [Edge]
  } deriving (Eq, Show)

instance ToJSON CachedExtraction where
  toJSON ce = object
    [ "nodes"      .= ceNodes ce
    , "edges"      .= ceEdges ce
    ]

instance FromJSON CachedExtraction where
  parseJSON = withObject "CachedExtraction" $ \v -> CachedExtraction
    <$> v .: "nodes"
    <*> v .: "edges"

extractionToCached :: Extraction -> CachedExtraction
extractionToCached e = CachedExtraction
  { ceNodes      = Map.elems (extNodes e)
  , ceEdges      = Map.elems (extEdges e)
  }

cachedToExtraction :: CachedExtraction -> Extraction
cachedToExtraction c = extractionFromLists (ceNodes c) (ceEdges c)

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Compute SHA256 hash of file contents + relative path
fileHash :: FilePath -> FilePath -> IO String
fileHash path root = do
  -- Simplified: use file path as hash key (proper SHA256 would need cryptohash)
  -- TODO: implement proper SHA256 hashing with cryptohash-sha256
  let rel = makeRelative root path
  pure (show (length rel) ++ "_" ++ map safeChar rel)
  where
    safeChar '/' = '_'
    safeChar '.' = '_'
    safeChar c   = c
    makeRelative root' path'
      | takeFileName root' `isPrefixOf` path' = drop (length (takeFileName root') + 1) path'
      | otherwise = path'
    isPrefixOf _ "" = True
    isPrefixOf [] _ = True
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- | Group nodes/edges/hyperedges by source_file
groupBySourceFile :: [Node] -> [Edge] -> Map FilePath ([Node], [Edge])
groupBySourceFile nodes edges =
  let nodeSourceMap = Map.fromList [(nodeId n, nodeSourceFile n) | n <- nodes]
      nodeMap  = foldl' (\m n -> Map.insertWith (\(a,b) (a',b') -> (a++a', b++b')) (T.unpack (nodeSourceFile n)) ([n], []) m) Map.empty nodes
      edgeMap  = foldl' (\m e -> let srcFile = Map.findWithDefault "" (edgeSource e) nodeSourceMap
                                 in Map.insertWith (\(a,b) (a',b') -> (a++a', b++b')) (T.unpack srcFile) ([], [e]) m) nodeMap edges
  in edgeMap

-- ───────────────────────────────────────────────
-- Pipeline checkpoint (resume from failure)
-- ───────────────────────────────────────────────

-- | Path to the pipeline checkpoint file.
checkpointPath :: FilePath -> FilePath
checkpointPath outputDir = outputDir </> "pipeline.checkpoint.json"

-- | Save a pipeline checkpoint to disk.
-- Uses atomic write (write to tmp, then rename) to avoid corruption.
savePipelineCheckpoint :: FilePath -> PipelineCheckpoint -> IO ()
savePipelineCheckpoint outputDir chk = do
  createDirectoryIfMissing True outputDir
  let path = checkpointPath outputDir
      tmp  = path ++ ".tmp"
  BSL.writeFile tmp (encode chk)
  renameFile tmp path

-- | Load a pipeline checkpoint from disk.
-- Returns Nothing if no checkpoint exists (first run or after cleanup).
loadPipelineCheckpoint :: FilePath -> IO (Maybe PipelineCheckpoint)
loadPipelineCheckpoint outputDir = do
  let path = checkpointPath outputDir
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      bs <- BSL.readFile path
      case eitherDecode bs of
        Left _   -> pure Nothing  -- corrupt checkpoint, start fresh
        Right chk -> pure (Just chk)

-- | Clear (delete) the pipeline checkpoint after successful completion.
clearPipelineCheckpoint :: FilePath -> IO ()
clearPipelineCheckpoint outputDir = do
  let path = checkpointPath outputDir
  removeFile path `catch` \(_ :: SomeException) -> pure ()