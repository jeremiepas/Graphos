-- | Persistence for Cypher mutations: write the mutated graph back to
-- the loaded graph.json, with a timestamped backup of the previous file.
--
-- The document keeps the loaded file's schema_version (via the shared
-- incremental writer) and carries over the loaded derived sections
-- (communities, cohesion, god_nodes, community_labels,
-- community_aggregates); node degrees are recomputed from the mutated
-- edges and the adjacency/hash rebuilt by the caller (Eval's
-- rebuildAdjacency runs before persistence).
--
-- The re-extraction pipeline overwrites graph.json on the next run —
-- mutations are not replayed (documented caveat, surfaced to the user).
module Graphos.Infrastructure.Export.PersistMutation
  ( persistMutatedGraph
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (copyFile, doesFileExist)

import Graphos.Domain.Graph (Graph, gNodes, gEdges)
import Graphos.Domain.Graph.Mutation (rebuildAdjacency)
import Graphos.UseCase.Load (LoadResult(..))
import Graphos.Infrastructure.Export.IncrementalJSON
  ( openWriter
  , closeWriter
  , writeNodes
  , writeEdges
  , writeCommunities
  , writeCohesion
  , writeGodNodes
  , writeAnalysisTail
  , writeCommunityAggregates
  , writeCompositions
  )

-- | Persist the mutated graph to the original graph.json path. Returns
-- the backup path on success.
persistMutatedGraph :: FilePath -> LoadResult -> Graph -> IO (Either Text FilePath)
persistMutatedGraph path lr mutated = do
  backupPath <- timestampedBackupPath path
  exists <- doesFileExist path
  if exists
    then copyFile path backupPath
    else pure ()
  errOrUnit <- writeDoc
  case errOrUnit of
    Left err -> pure (Left err)
    Right () -> pure (Right backupPath)
  where
    mutated' = rebuildAdjacency mutated
    writeDoc = do
      let nodes = Map.elems (gNodes mutated')
          edges = Map.elems (gEdges mutated')
      iw <- openWriter path
      writeNodes iw nodes
      writeEdges iw edges
      writeCommunities iw (lrCommunities lr)
      writeCohesion iw (lrCohesion lr)
      writeGodNodes iw (lrGodNodes lr)
      writeAnalysisTail iw (Just (lrCommunityLabels lr))
      writeCommunityAggregates iw (lrCommunityAggregates lr)
      writeCompositions iw (lrCompositions lr)
      closeWriter iw
      pure (Right ())

-- | A backup path like @graph.json.bak-20260904T120000Z@.
timestampedBackupPath :: FilePath -> IO FilePath
timestampedBackupPath path = do
  now <- getCurrentTime
  let stamp = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" now
  pure (path ++ ".bak-" ++ stamp)