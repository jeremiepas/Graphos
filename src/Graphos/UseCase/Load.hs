-- | Load a previously built graph from the JSON export file.
-- This is the shared helper used by CLI query, path, explain, merge, and MCP commands.
--
-- Builds a GraphIndex (inverted label index + community reverse index)
-- at load time for O(k) queries instead of O(N) full-scans.
--
-- graph.json is a streaming artifact: a file may be partially written,
-- produced by an older graphos version, or produced by an external tool.
-- The loader is tolerant by default:
--
--   * unknown "relation" values degrade to 'Inferred' (counted)
--   * unknown "file_type" values degrade to 'CodeFile' (counted)
--   * malformed node/edge entries are skipped (counted)
--   * missing optional top-level sections default to empty
--
-- 'loadGraphFromFileStrict' (--strict-graph) restores fail-fast behaviour,
-- naming the offending value and the node or edge id.
{-# LANGUAGE OverloadedStrings #-}

module Graphos.UseCase.Load
  ( loadGraphFromFile
  , loadGraphFromFileStrict
  , LoadResult(..)
  , supportedMajorVersions
  ) where

import Control.Monad (when)
import Data.Aeson (FromJSON, Result(..), Value(..), eitherDecode, fromJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import Data.Map.Strict (Map, empty)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>), isAbsolute)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph, gCompositions, gEmbeddings, gEmbeddingsPath)
import Graphos.Domain.Graph.Index (GraphIndex, buildIndexWithLabels)
import Graphos.Domain.Graph.Analysis (CachedFGL, toCachedFGL)

-- | Result of loading a graph from disk
data LoadResult = LoadResult
  { lrGraph               :: Graph
  , lrIndex               :: GraphIndex
  , lrCachedFGL           :: CachedFGL
  , lrCommunities         :: CommunityMap
  , lrCohesion            :: CohesionMap
  , lrGodNodes            :: [GodNode]
  , lrCommunityLabels     :: Map Int Text
  , lrCompositions        :: Maybe Value
  , lrCommunityAggregates :: [CommunityAggregate]
  , lrDegradedRelations   :: Int
  , lrDegradedFileTypes   :: Int
  , lrSkippedNodes        :: Int
  , lrSkippedEdges        :: Int
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
-- malformed nodes/edges, naming the offending value and id.
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
        Right root -> do
          case parseGraphFile strict path root of
            Left e -> pure $ Left e
            Right lr0 -> do
              lr <- loadEmbeddingsSidecar path lr0
              when (lrDegradedRelations lr > 0 || lrDegradedFileTypes lr > 0
                    || lrSkippedNodes lr > 0 || lrSkippedEdges lr > 0) $
                putStrLn $ "WARNING: " ++ show (lrDegradedRelations lr) ++ " degraded relations, "
                           ++ show (lrDegradedFileTypes lr) ++ " degraded file types, "
                           ++ show (lrSkippedNodes lr) ++ " skipped nodes, "
                           ++ show (lrSkippedEdges lr) ++ " skipped edges"
              pure $ Right lr

-- | If the graph points to an embeddings sidecar, read it and attach the
-- vectors. A missing or unparseable sidecar degrades to Nothing with a
-- warning so the rest of the graph still loads.
loadEmbeddingsSidecar :: FilePath -> LoadResult -> IO LoadResult
loadEmbeddingsSidecar graphPath lr =
  case gEmbeddingsPath (lrGraph lr) of
    Nothing -> pure lr
    Just p -> do
      let sidecar = if isAbsolute p then p else takeDirectory graphPath </> p
      exists <- doesFileExist sidecar
      if not exists
        then do
          putStrLn $ "WARNING: embeddings sidecar not found: " ++ sidecar
          pure lr
        else do
          bs <- BSL.readFile sidecar
          case (eitherDecode bs :: Either String (Map NodeId [Double])) of
            Left err -> do
              putStrLn $ "WARNING: failed to parse embeddings sidecar " ++ sidecar ++ ": " ++ err
              pure lr
            Right embs ->
              pure $ lr { lrGraph = (lrGraph lr) { gEmbeddings = Just embs } }

-- ───────────────────────────────────────────────
-- Top-level parsing
-- ───────────────────────────────────────────────

parseGraphFile :: Bool -> FilePath -> Value -> Either Text LoadResult
parseGraphFile strict path (Object km) = do
  _ <- case KM.lookup (Key.fromText "schema_version") km of
    Nothing         -> pure ()
    Just (String v) -> validateSchemaVersion path (Just v)
    Just _          -> Left $ "graph.json: \"schema_version\" must be a string"
  (nodes, degradedTypes, skippedNodes) <- parseNodes strict km
  (edges, degradedRels, skippedEdges) <- parseEdges strict km
  communities <- parseSection "communities" km (empty :: CommunityMap)
  cohesion    <- parseSection "cohesion" km (empty :: CohesionMap)
  godNodes    <- parseSection "god_nodes" km ([] :: [GodNode])
  labels      <- parseSection "community_labels" km (empty :: Map Int Text)
  compositions <- parseSection "compositions" km (Nothing :: Maybe Value)
  aggregates  <- parseSection "community_aggregates" km ([] :: [CommunityAggregate])
  embeddingsPath <- optionalText "embeddings_path" km
  let extraction = extractionFromLists nodes edges
      graph = buildGraph False extraction
      graphWithComps = graph { gCompositions = compositions
                             , gEmbeddingsPath = fmap T.unpack embeddingsPath }
      idx = buildIndexWithLabels graphWithComps communities labels
      cachedFGL = toCachedFGL graphWithComps
  pure LoadResult
    { lrGraph               = graphWithComps
    , lrIndex               = idx
    , lrCachedFGL           = cachedFGL
    , lrCommunities         = communities
    , lrCohesion            = cohesion
    , lrGodNodes            = godNodes
    , lrCommunityLabels     = labels
    , lrCompositions        = compositions
    , lrCommunityAggregates = aggregates
    , lrDegradedRelations   = degradedRels
    , lrDegradedFileTypes   = degradedTypes
    , lrSkippedNodes        = skippedNodes
    , lrSkippedEdges        = skippedEdges
    }
parseGraphFile _ _ _ =
  Left $ "graph.json: top-level value must be a JSON object"

-- | Parse an optional top-level section; absent keys default to the given value.
parseSection :: FromJSON a => Text -> KM.KeyMap Value -> a -> Either Text a
parseSection name km defaultVal =
  case KM.lookup (Key.fromText name) km of
    Nothing -> Right defaultVal
    Just v  -> case fromJSON v of
      Success a -> Right a
      Error e   -> Left $ "graph.json: failed to parse \"" <> name <> "\": " <> T.pack e

-- | Validate the schema version against supported major versions.
-- Absent version is the pre-versioning baseline and always loads.
validateSchemaVersion :: FilePath -> Maybe Text -> Either Text ()
validateSchemaVersion _ Nothing = Right ()
validateSchemaVersion path (Just v) =
  let major = readMajorVersion v
  in if major `elem` supportedMajorVersions
       then Right ()
       else Left $ "Unsupported schema_version \"" <> v <> "\" in " <> T.pack path
                  <> ". Supported major versions: " <> T.pack (show supportedMajorVersions)
  where
    readMajorVersion t =
      case T.splitOn "." t of
        (majorStr:_) -> case reads (T.unpack majorStr) of
          [(n, _)] -> n
          _        -> 0
        [] -> 0

-- ───────────────────────────────────────────────
-- Per-item node/edge parsing (tolerant)
-- ───────────────────────────────────────────────

parseNodes :: Bool -> KM.KeyMap Value -> Either Text ([Node], Int, Int)
parseNodes strict km =
  case KM.lookup (Key.fromText "nodes") km of
    Nothing -> Left $ "graph.json: missing required key \"nodes\""
    Just (Array items) -> parseItems strict (parseNodeItem strict) (V.toList items)
    Just _ -> Left $ "graph.json: \"nodes\" must be an array"

parseEdges :: Bool -> KM.KeyMap Value -> Either Text ([Edge], Int, Int)
parseEdges strict km =
  case KM.lookup (Key.fromText "edges") km of
    Nothing -> Left $ "graph.json: missing required key \"edges\""
    Just (Array items) -> parseItems strict (parseEdgeItem strict) (V.toList items)
    Just _ -> Left $ "graph.json: \"edges\" must be an array"

-- | Fold per-item parsing: in strict mode the first malformed entry fails the
-- whole load; in tolerant mode it is skipped and counted.
parseItems :: Bool -> (Value -> Either Text (a, Bool)) -> [Value]
           -> Either Text ([a], Int, Int)
parseItems _ _ [] = Right ([], 0, 0)
parseItems strict parse (item:rest) = do
  (parsed, degraded, skipped) <- step
  (xs, degRest, skipRest) <- parseItems strict parse rest
  pure (maybe [] (\x -> [x]) parsed ++ xs, degraded + degRest, skipped + skipRest)
  where
    step = case parse item of
      Left err -> if strict then Left err else pure (Nothing, 0, 1)
      Right (x, d) -> pure (Just x, fromEnum d, 0)

-- | Parse one node entry. Returns the node plus whether its file_type was
-- degraded. Left = malformed entry (skip in tolerant mode, fail in strict).
parseNodeItem :: Bool -> Value -> Either Text (Node, Bool)
parseNodeItem strict v =
  case v of
    Object km -> do
      nid <- fieldText "id" km
      label <- fieldText "label" km
      (ft, degraded) <- parseFileTypeItem strict nid km
      src <- case KM.lookup (Key.fromText "source_file") km of
        Nothing       -> pure ""
        Just Null     -> pure ""
        Just (String s) -> pure s
        Just _        -> Left $ nodeErr nid "\"source_file\" must be a string"
      lineStart <- optionalInt "line_start" km
      lineEnd   <- optionalInt "line_end" km
      signature <- optionalText "signature" km
      communityId <- optionalInt "community_id" km
      kind <- optionalText "kind" km
      degree <- optionalInt "degree" km
      isBridge <- optionalBool "is_bridge" km
      extra <- optionalValue "extra" km
      pure (Node nid label ft src lineStart lineEnd signature communityId kind degree isBridge extra, degraded)
    _ -> Left $ "graph.json: node entry must be an object"

-- | Parse one edge entry. Returns the edge plus whether its relation was
-- degraded. Left = malformed entry (skip in tolerant mode, fail in strict).
parseEdgeItem :: Bool -> Value -> Either Text (Edge, Bool)
parseEdgeItem strict v =
  case v of
    Object km -> do
      eid <- fieldText "id" km
      src <- fieldText "source" km
      tgt <- fieldText "target" km
      (rel, degraded) <- parseRelationItem strict eid km
      weight <- fieldNumber "weight" km
      confidence <- fieldNumber "confidence" km
      extra <- optionalValue "extra" km
      pure (Edge (EdgeId eid) src tgt rel weight (Confidence confidence) extra, degraded)
    _ -> Left $ "graph.json: edge entry must be an object"

parseFileTypeItem :: Bool -> NodeId -> KM.KeyMap Value -> Either Text (FileType, Bool)
parseFileTypeItem strict nid km =
  case KM.lookup (Key.fromText "file_type") km of
    Nothing -> Left $ nodeErr nid "missing required field \"file_type\""
    Just (String t) ->
      case fileTypeFromText t of
        Just ft -> pure (ft, False)
        Nothing
          | strict    -> Left $ nodeErr nid $ "unknown file_type \"" <> t <> "\""
          | otherwise -> pure (CodeFile, True)
    Just _ -> Left $ nodeErr nid "\"file_type\" must be a string"

parseRelationItem :: Bool -> NodeId -> KM.KeyMap Value -> Either Text (Relation, Bool)
parseRelationItem strict eid km =
  case KM.lookup (Key.fromText "relation") km of
    Nothing -> Left $ edgeErr eid "missing required field \"relation\""
    Just (String t) ->
      case textToRelation t of
        Just r -> pure (r, False)
        Nothing
          | strict    -> Left $ edgeErr eid $ "unknown relation \"" <> t <> "\""
          | otherwise -> pure (Inferred, True)
    Just _ -> Left $ edgeErr eid "\"relation\" must be a string"

-- ───────────────────────────────────────────────
-- Field helpers
-- ───────────────────────────────────────────────

fieldText :: Text -> KM.KeyMap Value -> Either Text Text
fieldText name km =
  case KM.lookup (Key.fromText name) km of
    Just (String s) -> Right s
    Just _          -> Left $ "graph.json: \"" <> name <> "\" must be a string"
    Nothing         -> Left $ "graph.json: missing required field \"" <> name <> "\""

fieldNumber :: Text -> KM.KeyMap Value -> Either Text Double
fieldNumber name km =
  case KM.lookup (Key.fromText name) km of
    Just (Number n) -> Right (realToFrac n)
    Just _          -> Left $ "graph.json: \"" <> name <> "\" must be a number"
    Nothing         -> Left $ "graph.json: missing required field \"" <> name <> "\""

optionalText :: Text -> KM.KeyMap Value -> Either Text (Maybe Text)
optionalText name km =
  case KM.lookup (Key.fromText name) km of
    Nothing       -> pure Nothing
    Just Null     -> pure Nothing
    Just (String s) -> pure (Just s)
    Just _        -> Left $ "graph.json: \"" <> name <> "\" must be a string"

optionalInt :: Text -> KM.KeyMap Value -> Either Text (Maybe Int)
optionalInt name km =
  case KM.lookup (Key.fromText name) km of
    Nothing       -> pure Nothing
    Just Null     -> pure Nothing
    Just (Number n) -> pure (Just (floor (realToFrac n :: Double)))
    Just _        -> Left $ "graph.json: \"" <> name <> "\" must be a number"

optionalBool :: Text -> KM.KeyMap Value -> Either Text (Maybe Bool)
optionalBool name km =
  case KM.lookup (Key.fromText name) km of
    Nothing       -> pure Nothing
    Just Null     -> pure Nothing
    Just (Bool b) -> pure (Just b)
    Just _        -> Left $ "graph.json: \"" <> name <> "\" must be a boolean"

optionalValue :: Text -> KM.KeyMap Value -> Either Text (Maybe Value)
optionalValue name km =
  case KM.lookup (Key.fromText name) km of
    Nothing   -> pure Nothing
    Just Null -> pure Nothing
    Just v    -> pure (Just v)

nodeErr :: NodeId -> Text -> Text
nodeErr nid msg = "graph.json: node \"" <> nid <> "\": " <> msg

edgeErr :: NodeId -> Text -> Text
edgeErr eid msg = "graph.json: edge \"" <> eid <> "\": " <> msg

fileTypeFromText :: Text -> Maybe FileType
fileTypeFromText = \case
  "code"   -> Just CodeFile
  "doc"    -> Just DocFile
  "paper"  -> Just PaperFile
  "image"  -> Just ImageFile
  "video"  -> Just VideoFile
  "audio"  -> Just AudioFile
  "office" -> Just OfficeFile
  _        -> Nothing
