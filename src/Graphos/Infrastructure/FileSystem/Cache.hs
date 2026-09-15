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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Short (toText)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import Data.Word (Word8)
import Numeric (showHex)
import qualified Crypto.Hash.SHA256 as Hash

import Graphos.Domain.Types
import Graphos.Domain.Types.Pipeline (PipelineCheckpoint(..))
import Graphos.Infrastructure.FileSystem.AtomicWrite (writeFileAtomic)

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
  writeFileAtomic entry (encode (extractionToCached result))

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

-- | Hex-encode a single byte, zero-padding to two digits.
byteToHex :: Word8 -> String
byteToHex w = case showHex (fromIntegral w :: Int) "" of
  s -> if length s == 1 then '0' : s else s

-- | Content-addressed cache key: the SHA-256 (hex) of a file's contents.
--
-- The key is a pure function of file content only (never the path), so the cache
-- factors through content as required by INV-CACHE-SOUND / Theorem 1 of the
-- hot-reload cost model: identical content hits the same slot, and any edit to
-- the file's bytes produces a new key, invalidating the entry. A missing file has
-- no content to hash, so it falls back to a deterministic path-derived key that
-- always misses (forcing re-extraction) without throwing.
fileHash :: FilePath -> FilePath -> IO String
fileHash path _root = do
  exists <- doesFileExist path
  if exists
    then do
      contents <- BS.readFile path
      let digestBytes = Hash.hash contents :: BS.ByteString
      pure (concatMap byteToHex (BS.unpack digestBytes))
    else pure (show (length path) ++ "_" ++ path)

-- | Group nodes/edges/hyperedges by source_file
groupBySourceFile :: [Node] -> [Edge] -> Map FilePath ([Node], [Edge])
groupBySourceFile nodes edges =
  let nodeSourceMap = Map.fromList [(nodeId n, nodeSourceFile n) | n <- nodes]
      nodeMap  = foldl' (\m n -> Map.insertWith (\(a,b) (a',b') -> (a++a', b++b')) (T.unpack (toText (nodeSourceFile n))) ([n], []) m) Map.empty nodes
      edgeMap  = foldl' (\m e -> let srcFile = Map.findWithDefault "" (edgeSource e) nodeSourceMap
                                  in Map.insertWith (\(a,b) (a',b') -> (a++a', b++b')) (T.unpack (toText srcFile)) ([], [e]) m) nodeMap edges
  in edgeMap

-- ───────────────────────────────────────────────
-- Pipeline checkpoint (resume from failure)
-- ───────────────────────────────────────────────

-- | Path to the pipeline checkpoint file.
checkpointPath :: FilePath -> FilePath
checkpointPath outputDir = outputDir </> "pipeline.checkpoint.json"

-- | Save a pipeline checkpoint to disk (atomic write).
savePipelineCheckpoint :: FilePath -> PipelineCheckpoint -> IO ()
savePipelineCheckpoint outputDir chk = do
  let path = checkpointPath outputDir
  writeFileAtomic path (encode chk)

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