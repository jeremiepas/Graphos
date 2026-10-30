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
  ) where


import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (void, when)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Graphos.Domain.Config.Source (SourceConfig(..))
import System.FSNotify (Event(..), withManager, watchTree)

-- | A source-qualified path: (sourceName, relativePath).
-- sourceName is Nothing for single-root, Just name for multi-root.
type SourcePath = (Maybe T.Text, FilePath)

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
          Added path _ _    -> recordChangedForSource changedFiles (Just (scName sourceConfig)) path
          Modified path _ _  -> recordChangedForSource changedFiles (Just (scName sourceConfig)) path
          Removed path _ _  -> recordChangedForSource changedFiles (Just (scName sourceConfig)) path
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