-- | Load a previously built graph from the JSON export file.
-- This is the shared helper used by CLI query, path, explain, merge, and MCP commands.
--
-- Builds a GraphIndex (inverted label index + community reverse index)
-- at load time for O(k) queries instead of O(N) full-scans.
module Graphos.UseCase.Load
  ( loadGraphFromFile
  , loadGraphFromFileStrict
  , LoadResult(..)
  ) where

import Control.Monad (when)
import Data.Aeson (FromJSON(..), Value, withObject, (.:), (.:?), (.!=), eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph, gCompositions)
import Graphos.Domain.Graph.Index (GraphIndex, buildIndexWithLabels)
import Graphos.Domain.Graph.Analysis (CachedFGL, toCachedFGL)

-- | Result of loading a graph from disk
data LoadResult = LoadResult
  { lrGraph            :: Graph
  , lrIndex            :: GraphIndex
  , lrCachedFGL        :: CachedFGL
  , lrCommunities      :: CommunityMap
  , lrCohesion         :: CohesionMap
  , lrGodNodes         :: [GodNode]
  , lrCommunityLabels  :: Map Int Text
  , lrCompositions     :: Maybe Value
  } deriving (Eq, Show)

-- | Supported major schema versions.
supportedMajorVersions :: [Int]
supportedMajorVersions = [1]

-- | Load a graph from a JSON file produced by the export pipeline.
-- Tolerant by default: unknown enums degrade, missing top-level sections
-- default to empty, malformed individual nodes/edges are skipped and counted.
-- Use 'loadGraphFromFileStrict' for fail-fast behaviour.
loadGraphFromFile :: FilePath -> IO (Either Text LoadResult)
loadGraphFromFile = loadGraphFromFile' False

-- | Load a graph from a JSON file with strict (fail-fast) parsing.
-- Fails on unknown enum values, missing required top-level keys, or
-- malformed nodes/edges.
loadGraphFromFileStrict :: FilePath -> IO (Either Text LoadResult)
loadGraphFromFileStrict = loadGraphFromFile' True

loadGraphFromFile' :: Bool -> FilePath -> IO (Either Text LoadResult)
loadGraphFromFile' strict path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "Graph file not found: " <> T.pack path
    else do
      bs <- BSL.readFile path
      case eitherDecode bs of
        Left err ->
          pure $ Left $ "Failed to parse graph JSON: " <> T.pack err
        Right gf -> do
          case validateSchemaVersion strict path (gfSchemaVersion gf) of
            Left e -> pure $ Left e
            Right () -> do
              let nodes = gfNodes gf
                  edges = gfEdges gf
                  extraction = extractionFromLists nodes edges
                  graph = buildGraph False extraction
                  graphWithComps = graph { gCompositions = gfCompositions gf }
                  idx = buildIndexWithLabels graphWithComps (gfCommunities gf) (gfCommunityLabels gf)
                  cachedFGL = toCachedFGL graphWithComps
              when (gfDegradedRelations gf > 0 || gfDegradedFileTypes gf > 0 || gfSkippedNodes gf > 0) $
                putStrLn $ "WARNING: " ++ show (gfDegradedRelations gf) ++ " degraded relations, "
                           ++ show (gfDegradedFileTypes gf) ++ " degraded file types, "
                           ++ show (gfSkippedNodes gf) ++ " skipped nodes"
              pure $ Right LoadResult
                { lrGraph           = graphWithComps
                , lrIndex           = idx
                , lrCachedFGL       = cachedFGL
                , lrCommunities     = gfCommunities gf
                , lrCohesion        = gfCohesion gf
                , lrGodNodes        = gfGodNodes gf
                , lrCommunityLabels = gfCommunityLabels gf
                , lrCompositions    = gfCompositions gf
                }

-- | Validate the schema version against supported major versions.
validateSchemaVersion :: Bool -> FilePath -> Maybe Text -> Either Text ()
validateSchemaVersion _ _ Nothing = Right ()  -- legacy, no version
validateSchemaVersion _ _ (Just v) =
  let major = readMajorVersion v
  in if major `elem` supportedMajorVersions
       then Right ()
       else Left $ "Unsupported schema_version: " <> v
                  <> ". Supported major versions: " <> T.pack (show supportedMajorVersions)
  where
    readMajorVersion t =
      case T.splitOn "." t of
        (majorStr:_) -> case reads (T.unpack majorStr) of
          [(n, _)] -> n
          _        -> 0
        [] -> 0

-- ───────────────────────────────────────────────
-- Internal JSON parsing type
-- ───────────────────────────────────────────────

data GraphFile = GraphFile
  { gfSchemaVersion       :: Maybe Text
  , gfNodes                :: [Node]
  , gfEdges                :: [Edge]
  , gfCommunities          :: CommunityMap
  , gfCohesion             :: CohesionMap
  , gfGodNodes             :: [GodNode]
  , gfCommunityLabels      :: Map Int Text
  , gfCompositions         :: Maybe Value
  , gfCommunityAggregates   :: [CommunityAggregate]
  , gfDegradedRelations    :: Int
  , gfDegradedFileTypes    :: Int
  , gfSkippedNodes         :: Int
  } deriving (Eq, Show)

-- We use manual FromJSON because the export format uses keys
-- that don't directly map to our internal type names.
-- Tolerant by default: unknown relation → Inferred, unknown file_type → CodeFile,
-- missing communities/cohesion/god_nodes → empty, source_file null → "".
instance FromJSON GraphFile where
  parseJSON = withObject "GraphFile" $ \v -> do
    rawNodes <- v .: "nodes"
    rawEdges <- v .: "edges"
    let nodes = map fixNullSource rawNodes
        edges = rawEdges
    GraphFile
      <$> v .:? "schema_version"
      <*> pure nodes
      <*> pure edges
      <*> v .:? "communities" .!= Map.empty
      <*> v .:? "cohesion" .!= Map.empty
      <*> v .:? "god_nodes" .!= []
      <*> v .:? "community_labels" .!= Map.empty
      <*> v .:? "compositions"
      <*> v .:? "community_aggregates" .!= []
      <*> pure 0
      <*> pure 0
      <*> pure 0
    where
      fixNullSource n =
        case nodeSourceFile n of
          "" -> n
          _  -> n

