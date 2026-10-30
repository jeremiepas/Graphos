-- | File watcher for --watch mode
-- Uses fsnotify to recursively watch a directory for file changes.
--
-- * Single-root: callback receives [(Nothing, absolutePath)]
-- * Multi-root: callback receives [(Just sourceName, relativePath), ...]
-- * Events are debounced: rapid successive changes are batched into a single callback.
-- * Events are attributed to sources by longest-prefix ownership BEFORE coalescing.
-- * Debounce operates on the union of events across all watchers.
--
-- Example usage (single-root):
-- >
-- > watchDirectory "/path/to/project" onChange defaultGraphosWatchConfig shutdownVar
--
-- Example usage (multi-root):
-- >
-- > let sources = [SourceConfig "src1" "/project/src1" [], SourceConfig "src2" "/project/src2" []]
-- > watchSources sources onChange defaultGraphosWatchConfig shutdownVar
module Graphos.Infrastructure.FileSystem.Watcher
  ( watchDirectory
  , watchSources
  , GraphosWatchConfig(..)
  , defaultGraphosWatchConfig
  , SourcePath
  , recordChanged
  , recordChangedForSource
  , attributeEvent
  , resolveWatchPaths
  , startsWithBase
  , startsWith
  ) where


import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (void, when)
import Data.List (maximumBy, isPrefixOf)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Graphos.Domain.Config.Source (SourceConfig(..))
import System.FilePath ((</>))
import System.FSNotify (Event(..), withManager, watchTree)

-- | A source-qualified path: (sourceName, relativePath).
-- sourceName is Nothing for single-root, Just name for multi-root.
type SourcePath = (Maybe T.Text, FilePath)

-- | Does @prefix@ occur at the very start of @path@?
startsWith :: FilePath -> FilePath -> Bool
startsWith path prefix = prefix `isPrefixOf` path

-- | Is @path@ equal to, or contained within, the directory @base@?
startsWithBase :: FilePath -> FilePath -> Bool
startsWithBase path base =
  path == base || (base `isPrefixOf` path && '/' == path !! length base)

-- | Strip the @base@ prefix (and a following slash) from @path@.
relativize :: FilePath -> FilePath -> FilePath
relativize base path =
  case drop (length base) path of
    ('/') : rest -> rest
    other        -> other

-- | The source whose directory is the longest prefix of @path@, if any.
longestMatch :: [SourceConfig] -> FilePath -> Maybe (FilePath, T.Text)
longestMatch sources path =
  let matches = [ (scPath s, scName s) | s <- sources, startsWithBase path (scPath s) ]
  in if null matches
       then Nothing
       else Just (maximumBy (comparing (\(b, _) -> length b)) matches)

-- | Attribute a filesystem event to exactly one source by longest-prefix.
--
-- Returns @(Just sourceName, relativePath)@ for the owning source, or
-- @(Nothing, path)@ when no source owns it. The relative path is expressed
-- relative to the owning source's directory.
attributeEvent :: [SourceConfig] -> FilePath -> (Maybe T.Text, FilePath)
attributeEvent sources path =
  case longestMatch sources path of
    Just (base, name) -> (Just name, relativize base path)
    Nothing           -> (Nothing, path)

-- | Re-assemble openable absolute paths from a batch of watcher pairs.
--
-- The multi-root watcher carries source-relative paths (see 'watchSources');
-- the application layer joins each relative path back onto its owning
-- source's configured root so the downstream reader — which opens the file
-- with 'Data.ByteString.readFile' — receives an absolute, canonicalizable
-- path rather than a bare relative component. This is the inverse of the
-- attribution done in 'attributeEvent', and keeps the callback contract at
-- [(Maybe sourceName, relativePath)] intact per the multi-root spec.
--
-- A pair with no owning source ('Nothing'), or whose source name is absent
-- from @sources@, is returned unchanged: such a path arrives straight from
-- fsnotify already absolute, so there is nothing to join.
resolveWatchPaths :: [SourceConfig] -> [(Maybe T.Text, FilePath)] -> [FilePath]
resolveWatchPaths sources = map resolveOne
  where
    rootMap = Map.fromList [(scName s, scPath s) | s <- sources]
    resolveOne (Just name, rel) =
      case Map.lookup name rootMap of
        Just root -> root </> rel
        Nothing   -> rel
    resolveOne (_, rel) = rel


-- | Watch configuration
data GraphosWatchConfig = GraphosWatchConfig
  { gwcDebounceDelay :: Int  -- ^ Debounce delay in milliseconds (default: 200)
  , gwcBatchSize     :: Int  -- ^ Max files to batch in one callback (default: 100)
  } deriving (Eq, Show)

-- | Default watch configuration
defaultGraphosWatchConfig :: GraphosWatchConfig
defaultGraphosWatchConfig = GraphosWatchConfig
  { gwcDebounceDelay = 200
  , gwcBatchSize     = 100
  }

-- | Watch a directory for file changes (single-root mode).
-- Callback receives [(sourceName, path)] where sourceName is Nothing.
-- This is the original entry point; multi-root uses watchSources instead.
watchDirectory :: FilePath -> ([(Maybe T.Text, FilePath)] -> IO ()) -> GraphosWatchConfig -> MVar () -> IO ()
watchDirectory dir onChange config shutdownVar = do
  -- State for debouncing
  changedFiles <- newMVar (Set.empty :: Set SourcePath)
  let debounceDelay = gwcDebounceDelay config
  
  -- Start the watcher in a bracket to ensure cleanup
  withManager $ \mgr -> do
    -- Watch the directory tree
    void $ watchTree mgr dir (const True) $ \event -> do
      case event of
        Added path _ _    -> recordChanged changedFiles (Nothing, path)
        Modified path _ _  -> recordChanged changedFiles (Nothing, path)
        Removed path _ _  -> recordChanged changedFiles (Nothing, path)
        _                 -> pure ()  -- Ignore other events
    
    -- Periodically flush the debounced changes
    let flushLoop = do
          threadDelay (debounceDelay * 1000)  -- microseconds
          files <- takeMVar changedFiles
          when (not (Set.null files)) $ do
            onChange (Set.toList files)
          putMVar changedFiles (Set.empty :: Set SourcePath)
          flushLoop
    
    -- Run flush loop and wait for shutdown
    void $ race flushLoop (takeMVar shutdownVar)

-- | Record a file change in the debounce set
recordChanged :: MVar (Set SourcePath) -> SourcePath -> IO ()
recordChanged changedFiles path = do
  current <- takeMVar changedFiles
  putMVar changedFiles (Set.insert path current)

-- | Watch multiple source directories with source attribution.
-- For each source, watches its directory and events are attributed to the source
-- with the longest-prefix match. Events are debounced across all sources.
--
-- The callback receives [(Maybe sourceName, relativePath), ...] where:
--   - sourceName is Just name when the path belongs to that source
--   - sourceName is Nothing when no source matches (fallback attribution)
watchSources :: [SourceConfig] -> ([(Maybe T.Text, FilePath)] -> IO ()) -> GraphosWatchConfig -> MVar () -> IO ()
watchSources sourceConfigs onChange config shutdownVar = do
  -- State for debouncing -- tracks (sourceName, relativePath) pairs
  changedFiles <- newMVar (Set.empty :: Set SourcePath)
  let debounceDelay = gwcDebounceDelay config
  
  -- Start the watcher in a bracket to ensure cleanup
  withManager $ \mgr -> do
    -- Watch each source directory
    mapM_ (\sourceConfig -> do
      let sourceDir = scPath sourceConfig
      void $ watchTree mgr sourceDir (const True) $ \event -> do
        case event of
          Added path _ _    -> emitForSource sourceConfigs changedFiles sourceConfig path
          Modified path _ _  -> emitForSource sourceConfigs changedFiles sourceConfig path
          Removed path _ _  -> emitForSource sourceConfigs changedFiles sourceConfig path
          _                 -> pure ()  -- Ignore other events
      ) sourceConfigs
    
    -- Periodically flush the debounced changes
    let flushLoop = do
          threadDelay (debounceDelay * 1000)  -- microseconds
          files <- takeMVar changedFiles
          when (not (Set.null files)) $ do
            onChange (Set.toList files)
          putMVar changedFiles (Set.empty :: Set SourcePath)
          flushLoop
    
    -- Run flush loop and wait for shutdown
    void $ race flushLoop (takeMVar shutdownVar)

-- | Record a file change attributed to a specific source
recordChangedForSource :: MVar (Set SourcePath) -> Maybe T.Text -> FilePath -> IO ()
recordChangedForSource changedFiles sourceName path = do
  let relativePath = path
  let sourcePath = (sourceName, relativePath)
  current <- takeMVar changedFiles
  putMVar changedFiles (Set.insert sourcePath current)

-- | Record a single event only under the source that owns it by longest-prefix.
--
-- Because fsnotify reports a change in a nested source directory to *every*
-- enclosing watcher, we re-attribute each event globally and record it under
-- exactly one source (the innermost). This keeps nested changes from being
-- reported twice (once per enclosing source).
emitForSource :: [SourceConfig] -> MVar (Set SourcePath) -> SourceConfig -> FilePath -> IO ()
emitForSource sources changedFiles thisConfig path =
  let (maybeName, relPath) = attributeEvent sources path
  in when (maybeName == Just (scName thisConfig)) $
       recordChangedForSource changedFiles maybeName relPath