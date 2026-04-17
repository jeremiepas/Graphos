-- | File watcher for --watch mode
-- Uses fsnotify to recursively watch a directory for file changes.
module Graphos.Infrastructure.FileSystem.Watcher
  ( watchDirectory
  , GraphosWatchConfig(..)
  , defaultGraphosWatchConfig
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (SomeException(..), catch)
import Control.Monad (void, when)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FSNotify (Event(..), defaultConfig, withManager, watchTree)

-- | Watch a directory for file changes (recursive).
--
-- * @dir@ — Directory to watch
-- * @onChange@ — Callback invoked with changed file paths (debounced)
-- * @config@ — Optional watch configuration (debounce delay, etc.)
--
-- This function blocks until a shutdown signal is received or an error occurs.
-- It watches for create, modify, and delete events recursively.
-- Events are debounced: rapid successive changes are batched into a single callback.
--
-- Example usage:
--
-- > watchDirectory "/path/to/project" \path -> do
-- >   putStrLn $ "Detected change: " ++ path
-- >   runPipeline updateConfig
-- >   pure ()
--
-- To stop watching, send a signal to the returned MVar.
watchDirectory :: FilePath -> (FilePath -> IO ()) -> GraphosWatchConfig -> MVar () -> IO ()
watchDirectory dir onChange config shutdownVar = do
  -- State for debouncing
  changedFiles <- newMVar Set.empty
  let debounceDelay = gwcDebounceDelay config
  
  -- Start the watcher in a bracket to ensure cleanup
  withManager $ \mgr -> do
    -- Watch the directory tree
    void $ watchTree mgr dir (const True) $ \event -> do
      case event of
        Added path _ _    -> recordChange changedFiles path
        Modified path _ _  -> recordChange changedFiles path
        Removed path _ _  -> recordChange changedFiles path
        _                 -> pure ()  -- Ignore other events
    
    -- Periodically flush the debounced changes
    let flushLoop = do
          threadDelay (debounceDelay * 1000)  -- microseconds
          files <- takeMVar changedFiles
          when (not (Set.null files)) $ do
            onChange (T.unpack (T.intercalate ", " (map T.pack (Set.toList files))))
          putMVar changedFiles Set.empty
          flushLoop
    
    -- Run flush loop and wait for shutdown
    void $ race flushLoop (takeMVar shutdownVar)

-- | Record a file change in the debounce set
recordChange :: MVar (Set FilePath) -> FilePath -> IO ()
recordChange changedFiles path = do
  current <- takeMVar changedFiles
  putMVar changedFiles (Set.insert path current)

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
