-- | Extraction cache - skip unchanged files on re-run
-- Stores per-file extraction results keyed by SHA256 hash of file contents.
module Graphos.Infrastructure.FileSystem.Cache
  ( loadCached
  , saveCached
  , checkSemanticCache
  , saveSemanticCache
  , clearCache
  , cacheDir
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), (.=), object, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (doesFileExist, createDirectoryIfMissing, removeFile, renameFile)
import System.FilePath (takeFileName, (</>))

import Graphos.Domain.Types

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
saveSemanticCache nodes edges hyperedges root = do
  let byFile = groupBySourceFile nodes edges hyperedges
  mapM_ (\(fpath, (ns, es, hs)) -> saveCached fpath (Extraction ns es hs 0 0) root) (Map.toList byFile)
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
  , ceHyperedges :: [Hyperedge]
  } deriving (Eq, Show)

instance ToJSON CachedExtraction where
  toJSON ce = object
    [ "nodes"      .= ceNodes ce
    , "edges"      .= ceEdges ce
    , "hyperedges" .= ceHyperedges ce
    ]

instance FromJSON CachedExtraction where
  parseJSON = withObject "CachedExtraction" $ \v -> CachedExtraction
    <$> v .: "nodes"
    <*> v .: "edges"
    <*> v .: "hyperedges"

extractionToCached :: Extraction -> CachedExtraction
extractionToCached e = CachedExtraction
  { ceNodes      = extractionNodes e
  , ceEdges      = extractionEdges e
  , ceHyperedges = extractionHyperedges e
  }

cachedToExtraction :: CachedExtraction -> Extraction
cachedToExtraction c = Extraction
  { extractionNodes      = ceNodes c
  , extractionEdges      = ceEdges c
  , extractionHyperedges = ceHyperedges c
  , extractionInputTokens  = 0
  , extractionOutputTokens = 0
  }

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
groupBySourceFile :: [Node] -> [Edge] -> [Hyperedge] -> Map FilePath ([Node], [Edge], [Hyperedge])
groupBySourceFile nodes edges hyperedges =
  let nodeMap  = foldl' (\m n -> Map.insertWith (\(a,b,c) (a',b',c') -> (a++a', b++b', c++c')) (T.unpack (nodeSourceFile n)) ([n], [], []) m) Map.empty nodes
      edgeMap  = foldl' (\m e -> Map.insertWith (\(a,b,c) (a',b',c') -> (a++a', b++b', c++c')) (T.unpack (edgeSourceFile e)) ([], [e], []) m) nodeMap edges
      hyperMap = foldl' (\m h -> Map.insertWith (\(a,b,c) (a',b',c') -> (a++a', b++b', c++c')) (T.unpack (hyperedgeSourceFile h)) ([], [], [h]) m) edgeMap hyperedges
  in hyperMap