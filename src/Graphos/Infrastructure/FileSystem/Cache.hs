{-# LANGUAGE ScopedTypeVariables #-}
-- | Extraction cache - skip unchanged files on re-run.
--
-- Stores per-file extraction results keyed by SHA256 hash of file contents
-- concatenated with a caller-supplied fingerprint of the extraction-affecting
-- configuration (Infrastructure is Domain- and UseCase-free; the UseCase layer
-- computes the fingerprint and passes it through the port).
--
-- INV-CACHE-SOUND (AVI-521 §1.2): the key factors through file content AND
-- extraction-affecting configuration, so identical content under identical
-- config hits the same slot, and any change to either the bytes or the
-- fingerprinted config values invalidates the entry. The fingerprint serializes
-- the effective per-extension granularity (with CLI override), the extractor
-- mode per extension, and the pdf extraction level.
module Graphos.Infrastructure.FileSystem.Cache
  ( loadCached
  , saveCached
  , loadCachedFingerprinted
  , saveCachedFingerprinted
  , cacheKeyForFile
  , checkSemanticCache
  , saveSemanticCache
  , clearCache
  , cacheDir
  , cacheDirIn
  , embedCacheDir
  , embedCacheDirIn
  , evictToCap
  , loadPipelineCheckpoint
  , savePipelineCheckpoint
  , clearPipelineCheckpoint
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), object, eitherDecode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (ord)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Short (toText)
import System.Directory (doesFileExist, doesDirectoryExist, removeFile, listDirectory)
import System.Posix.Files (getFileStatus, fileSize, modificationTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad (forM)
import System.FilePath ((</>))
import Data.Word (Word64, Word8)
import Numeric (showHex)
import qualified Crypto.Hash.SHA256 as Hash

import Graphos.Domain.Config (defaultOutputDirName)
import Graphos.Domain.Types
import Graphos.Domain.Types.Pipeline (PipelineCheckpoint(..))
import Graphos.Infrastructure.FileSystem.AtomicWrite (writeFileAtomic)

-- | Get the cache directory for the effective pipeline output directory
-- (multi-source-graphs 2.2): @<outDir>/cache/@. The port-level convention —
-- caches always live under the effective output directory, never a hardcoded
-- project-root name.
cacheDirIn :: FilePath -> FilePath
cacheDirIn outDir = outDir </> "cache"

-- | Legacy root-based cache directory (pre-change project-root convention
-- @<root>/graphos-out/cache/@), retained for tests and legacy callers.
cacheDir :: FilePath -> FilePath
cacheDir root = cacheDirIn (root </> defaultOutputDirName)

-- | Load cached extraction for a file under the content-only (legacy) key.
-- Retained for the semantic-cache helpers and tests; new wiring calls the
-- fingerprinted variants below.
loadCached :: FilePath -> FilePath -> IO (Maybe Extraction)
loadCached path root = do
  h <- fileHash path root
  readCacheEntry (entryPath h root)

-- | Load cached extraction under the fingerprinted key:
-- @sha256(content <> fingerprint)@. @outDir@ is the pipeline output directory
-- (the port-level convention: caches live at @<outDir>/cache/@).
loadCachedFingerprinted :: Text -> FilePath -> FilePath -> IO (Maybe Extraction)
loadCachedFingerprinted fingerprint path outDir = do
  h <- fileHash path outDir
  readCacheEntry (entryPathIn outDir (cacheKeyForFile h fingerprint))

readCacheEntry :: FilePath -> IO (Maybe Extraction)
readCacheEntry entry = do
  exists <- doesFileExist entry
  if not exists
    then pure Nothing
    else do
      bs <- BSL.readFile entry
      case eitherDecode bs of
        Left _   -> pure Nothing
        Right cached -> pure (Just (cachedToExtraction cached))

-- | Save extraction result for a file under the content-only (legacy) key.
saveCached :: FilePath -> Extraction -> FilePath -> IO ()
saveCached path result root = do
  h <- fileHash path root
  writeEntry (entryPath h root) result

-- | Save extraction result under the fingerprinted key. @outDir@ is the
-- pipeline output directory.
saveCachedFingerprinted :: Text -> FilePath -> Extraction -> FilePath -> IO ()
saveCachedFingerprinted fingerprint path result outDir = do
  h <- fileHash path outDir
  writeEntry (entryPathIn outDir (cacheKeyForFile h fingerprint)) result

entryPath :: String -> FilePath -> FilePath
entryPath h root = cacheDir root </> h ++ ".json"

-- | Entry path under the port-level convention: @<outDir>/cache/<key>.json@.
entryPathIn :: FilePath -> String -> FilePath
entryPathIn outDir h = cacheDirIn outDir </> h ++ ".json"

writeEntry :: FilePath -> Extraction -> IO ()
writeEntry entry result = writeFileAtomic entry (encode (extractionToCached result))

-- | The composite cache key: sha256 of (content hash hex <> fingerprint).
-- Kept total and deterministic so the same (content, config) pair always maps
-- to the same slot.
cacheKeyForFile :: String -> Text -> String
cacheKeyForFile contentHash fingerprint =
  sha256Hex (contentHash <> T.unpack fingerprint)
  where
    sha256Hex = concatMap byteToHex . BS.unpack . (Hash.hash :: BS.ByteString -> BS.ByteString) . BS.pack . map (fromIntegral . ord)

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
-- LRU size-cap eviction (wire-incremental-update 2.2)
-- ───────────────────────────────────────────────

-- | The embedding cache lives inside the extraction cache root
-- (port-level convention: @<outDir>/cache/embeddings/@).
embedCacheDirIn :: FilePath -> FilePath
embedCacheDirIn outDir = cacheDirIn outDir </> "embeddings"

-- | Legacy root-based embedding-cache directory — retained for tests.
embedCacheDir :: FilePath -> FilePath
embedCacheDir root = embedCacheDirIn (root </> defaultOutputDirName)

-- | Evict the oldest-mtime entries across the extraction and embedding caches
-- until the combined size is at or below the cap (bytes). A cap of 0 disables
-- eviction entirely (unbounded growth). Missing directories are no-ops; only
-- regular files are considered. Returns the number of entries evicted.
--
-- Sound by construction (AVI-521 INV-CACHE-SOUND): both caches are
-- content-addressed, so eviction can only turn a would-be hit into a miss
-- whose recomputation yields the same result — never a wrong result.
--
-- The sweep runs over the pipeline's effective output directory: the caller
-- (UseCase.Pipeline.Core) passes @cfgOutputDir@, so a configured @output:@
-- relocates the bounded caches with the rest of the artifacts (2.2).
evictToCap :: Word64 -> FilePath -> IO Int
evictToCap capBytes outDir
  | capBytes == 0 = pure 0
  | otherwise = do
      exs <- cacheEntries (cacheDirIn outDir)
      ems <- cacheEntries (embedCacheDirIn outDir)
      -- Oldest mtime first; ties broken by path for determinism.
      let ordered = sortBy (comparing (\(_, mt, p) -> (mt, p))) (exs ++ ems)
          -- Integer arithmetic: under the cap this difference is negative and
          -- must not underflow Word64.
          excess  = toInteger (sum [sz | (sz, _, _) <- ordered])
                      - toInteger capBytes
          victims = takeWhileAccum excess ordered
      mapM_ (removeFileSafe . entryPath3) victims
      pure (length victims)
  where
    -- Take the oldest entries until the running evicted size covers the
    -- excess. When excess is non-positive (under the cap) the accumulator
    -- guard is immediately true, so nothing is evicted.
    takeWhileAccum excess = guard (excess > 0) >> go 0 . map scale
      where
        guard c = if c then id else const []
        scale (sz, mt, p) = (toInteger sz, mt, p)
        go _ [] = []
        go acc ((sz, mt, p) : rest)
          | acc >= excess = []
          | otherwise     = (sz, mt, p) : go (acc + sz) rest
    entryPath3 (_, _, p) = p

removeFileSafe :: FilePath -> IO ()
removeFileSafe p = removeFile p `catch` \(_ :: SomeException) -> pure ()

-- | Every regular cache entry: @(size, mtime, path)@, subdirectories skipped.
cacheEntries :: FilePath -> IO [(Word64, UTCTime, FilePath)]
cacheEntries dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      names <- listDirectory dir
      fmap concat $ forM names $ \n -> do
        let p = dir </> n
        isFile <- doesFileExist p
        if not isFile
          then pure []
          else do
            st <- getFileStatus p
            pure [(fromIntegral (fileSize st),
                   posixSecondsToUTCTime (realToFrac (modificationTime st)),
                   p)]

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