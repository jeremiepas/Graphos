-- | Load a previously built graph from the JSON export file.
-- This is the shared helper used by CLI query, path, explain, and MCP commands.
module Graphos.UseCase.Load
  ( loadGraphFromFile
  , LoadResult(..)
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), (.!=), eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph)

-- | Result of loading a graph from disk
data LoadResult = LoadResult
  { lrGraph            :: Graph
  , lrCommunities      :: CommunityMap
  , lrCohesion         :: CohesionMap
  , lrGodNodes         :: [GodNode]
  , lrCommunityLabels  :: Map Int Text
  } deriving (Eq, Show)

-- | Load a graph from a JSON file produced by the export pipeline.
-- The JSON format matches Infrastructure.Export.JSON.exportGraph:
--
-- > { "nodes": [...], "edges": [...], "communities": {...}, "cohesion": {...}, "god_nodes": [...] }
--
-- Returns Left with an error message on file-not-found or parse errors.
loadGraphFromFile :: FilePath -> IO (Either Text LoadResult)
loadGraphFromFile path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "Graph file not found: " <> T.pack path
    else do
      bs <- BSL.readFile path
      case eitherDecode bs of
        Left err ->
          pure $ Left $ "Failed to parse graph JSON: " <> T.pack err
        Right gf -> do
          let extraction = Extraction
                { extractionNodes      = gfNodes gf
                , extractionEdges      = gfEdges gf
                , extractionHyperedges = []
                , extractionInputTokens  = 0
                , extractionOutputTokens = 0
                }
              graph = buildGraph False extraction
          pure $ Right LoadResult
            { lrGraph           = graph
            , lrCommunities     = gfCommunities gf
            , lrCohesion        = gfCohesion gf
            , lrGodNodes        = gfGodNodes gf
            , lrCommunityLabels = gfCommunityLabels gf
            }

-- ───────────────────────────────────────────────
-- Internal JSON parsing type
-- ───────────────────────────────────────────────

data GraphFile = GraphFile
  { gfNodes           :: [Node]
  , gfEdges           :: [Edge]
  , gfCommunities     :: CommunityMap
  , gfCohesion        :: CohesionMap
  , gfGodNodes        :: [GodNode]
  , gfCommunityLabels :: Map Int Text
  } deriving (Eq, Show)

-- We use manual FromJSON because the export format uses keys
-- that don't directly map to our internal type names.
instance FromJSON GraphFile where
  parseJSON = withObject "GraphFile" $ \v -> GraphFile
    <$> v .:  "nodes"
    <*> v .:  "edges"
    <*> v .:  "communities"
    <*> v .:  "cohesion"
    <*> v .:  "god_nodes"
    <*> v .:? "community_labels" .!= Map.empty