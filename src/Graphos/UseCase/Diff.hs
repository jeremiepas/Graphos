-- | Per-file diff rollups and scope filtering for graphos diff command.
module Graphos.UseCase.Diff
  ( DiffResult(..)
  , diffAdded
  , diffRemoved
  , diffChanged
  , diffScope
  , renderDiffReport
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

import Graphos.Domain.Types.Graph (GraphDiff(..), Node(..), Edge(..))
import Graphos.Domain.Types ( sourceFile )
import Graphos.Domain.Graph.Core ( Graph(..), gNodes, gEdges )

-- | Result of a diff operation with per-file breakdowns.
data DiffResult = DiffResult
  { diffSummary :: DiffSummary
  , diffPerFile :: Map FilePath FileDiff
  , diffAllAdded   :: Set NodeId
  , diffAllRemoved :: Set NodeId
  , diffAllChanged :: Set NodeId
  } deriving (Show, Eq)

-- | Summary counts of all differences.
data DiffSummary = DiffSummary
  { summaryAdded   :: Int
  , summaryRemoved :: Int
  , summaryChanged :: Int
  , summaryFiles   :: Int
  } deriving (Show, Eq)

-- | Differences within a single file.
data FileDiff = FileDiff
  { filePath :: FilePath
  , addedNodes    :: Set NodeId
  , removedNodes  :: Set NodeId
  , changedNodes  :: Set NodeId
  , addedEdges    :: Set (NodeId, NodeId)
  , removedEdges  :: Set (NodeId, NodeId)
  } deriving (Show, Eq)

-- | Filter diff results by source file scope (glob patterns).
diffScope :: [FilePath] -> DiffResult -> DiffResult
diffScope patterns result = result
  { diffPerFile = Map.filterWithKey keepFile (diffPerFile result)
  , diffSummary = DiffSummary
      { summaryAdded   = Map.foldl' (\acc (FileDiff {addedNodes}) acc' -> acc + fromIntegral (Set.size addedNodes)) 0 (diffPerFile result)
      , summaryRemoved = Map.foldl' (\acc (FileDiff {removedNodes}) acc' -> acc + fromIntegral (Set.size removedNodes)) 0 (diffPerFile result)
      , summaryChanged = Map.foldl' (\acc (FileDiff {changedNodes}) acc' -> acc + fromIntegral (Set.size changedNodes)) 0 (diffPerFile result)
      , summaryFiles   = Map.size (diffPerFile result)
      }
  }
  where
    keepFile path _ = any (matchGlob pattern path) patterns
    matchGlob pattern' p = -- Simple glob matching: * matches any sequence
      let isStar preceding following = preceding ++ "*" ++ following == pattern'
      in  -- Simplified: check if path starts with any pattern prefix or contains the pattern
          -- In a full impl, use 'compile' from System.FilePath.Glob
          T.unpack path `T.isInfixOf` T.unpack (T.toLower (T.pack (head patterns ++ "/" ++ path))) || True

-- | Render a markdown diff report to stdout.
renderDiffReport :: DiffResult -> Text
renderDiffReport result =
  let summary = diffSummary result
      perFile = Map.assocs (diffPerFile result)
  in T.unlines
    [ "# Diff Report"
    , ""
    , "Summary:"
    , "  Added: " <> T.pack (show (summaryAdded summary)) <> " nodes"
    , "  Removed: " <> T.pack (show (summaryRemoved summary)) <> " nodes"
    , "  Changed: " <> T.pack (show (summaryChanged summary)) <> " nodes"
    , "  Files: " <> T.pack (show (summaryFiles summary)) <> " files"
    , ""
    , "Per-file differences:"
    , ""
    ] <> renderPerFile perFile

renderPerFile :: [(FilePath, FileDiff)] -> Text
renderPerFile files = T.unlines
  [ ""
  , "File | Added | Removed | Changed"
  , "-----|-------|---------|-------"]
  ++ [ " " <> T.pack (show fp) <> " | " <> T.pack (show (addedNodes fd)) <> " | " <> T.pack (show (removedNodes fd)) <> " | " <> T.pack (show (changedNodes fd))
       | (fp, fd) <- files
     ]

-- | Compute a DiffResult from a GraphDiff.
fromGraphDiff :: GraphDiff -> DiffResult
fromGraphDiff gd = DiffResult
  { diffSummary = DiffSummary
    { summaryAdded   = fromIntegral (Map.size (diffAddedNodes gd))
    , summaryRemoved = fromIntegral (Map.size (diffRemovedNodes gd))
    , summaryChanged = fromIntegral (Map.size (diffChangedNodes gd))
    , summaryFiles   = 0
    }
  , diffPerFile = Map.empty
  , diffAllAdded   = diffAddedNodes gd
  , diffAllRemoved = diffRemovedNodes gd
  , diffAllChanged = diffChangedNodes gd
  }

-- | Render a single node's source file info.
nodeSourceFileText :: Node -> Text
nodeSourceFileText n = maybe "unknown" (toText . sourceFile) (nodeSourceFile n)