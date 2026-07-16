-- | Memgraph Bolt-protocol export + push.
--
-- Memgraph is a Bolt-protocol compatible in-memory graph database.
-- This module generates Memgraph-compatible Cypher and pushes it via
-- mgconsole CLI tool (or exports Cypher files for manual import).
--
-- Key design decisions:
--   - Cypher is written to a temp file and piped to mgconsole via stdin
--     to avoid all shell/Haskell string escaping issues
--   - Uses double-quoted Cypher strings with backslash escaping
--   - All special chars (", ', \, newlines, etc.) are properly escaped
--
-- Three entity types (same as Neo4j):
--   - Node:     code/doc concepts from the graph
--   - Community: detected clusters with label + cohesion
--   - BELONGS_TO: edges linking Node → Community
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Export.Memgraph
  ( exportMemgraphCypher
  , pushToMemgraph
  , pushToMemgraphWithCommunities
  , pushSubgraphToMemgraph
  , pushCommunityGraphToMemgraph
  , pushMemgraphStatements
  , createMemgraphIndexes
  ) where

import Control.Exception (catch, SomeException)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode(..))
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, hFlush, hPutStrLn, IOMode(..), openFile, openTempFile)
import System.Process (readProcessWithExitCode)
import Data.List (sortOn)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges, neighbors)
import Graphos.Domain.Community (selectRepresentatives, filterEdgesByNodeSet)
import Graphos.Domain.Community.Label (suggestCommunityLabels)

-- ───────────────────────────────────────────────
-- Cypher file export
-- ───────────────────────────────────────────────

-- | Generate Memgraph-compatible Cypher and write to file.
-- Includes index creation statements before data.
-- Streams statements to handle to reduce peak memory for large graphs.
exportMemgraphCypher :: Graph -> FilePath -> IO ()
exportMemgraphCypher g path = do
  h <- openFile path WriteMode
  -- Write index creation statements
  mapM_ (hPutStrLn h . T.unpack) (T.lines generateIndexCypher)
  hPutStrLn h ""
  -- Stream node statements one by one
  mapM_ (\n -> hPutStrLn h (T.unpack (generateInlineNodeStatement n))) (Map.elems (gNodes g))
  -- Stream edge statements one by one
  mapM_ (\e -> hPutStrLn h (T.unpack (generateInlineEdgeStatement e))) (Map.elems (gEdges g))
  hFlush h
  hClose h

-- ───────────────────────────────────────────────
-- Index creation
-- ───────────────────────────────────────────────

-- | Create indexes on Memgraph for fast lookups.
createMemgraphIndexes :: Text -> Text -> Text -> IO (Text, Int, Int)
createMemgraphIndexes uri user password = catch (do
  let stmts :: [Text]
      stmts = [ "CREATE INDEX ON :Node(id);"
               , "CREATE INDEX ON :Community(id);"
               , "CREATE INDEX ON :Node(file_type);"
               ]
  results <- mapM (execMemgraphCypher uri user password) stmts
  let failures = [ err | Left err <- results ]
  if null failures
    then pure ("Created Memgraph indexes", length stmts, length stmts)
    else pure (T.pack $ "Index creation had " ++ show (length failures) ++ " error(s): "
                       ++ T.unpack (T.take 200 (T.intercalate "; " failures)), length stmts, length stmts)
  ) $ \(e :: SomeException) ->
    pure (T.pack $ "Memgraph index creation failed: " ++ show e, 0, 0)

-- ───────────────────────────────────────────────
-- Memgraph push (basic — nodes + edges only)
-- ───────────────────────────────────────────────

-- | Push graph to Memgraph via mgconsole.
pushToMemgraph :: Graph -> Text -> Text -> Text -> IO (Text, Int, Int)
pushToMemgraph g uri user password = do
  _ <- createMemgraphIndexes uri user password
  let stmts = generateInlineStatements g
  pushMemgraphStatements uri user password stmts

-- ───────────────────────────────────────────────
-- Memgraph push (with communities)
-- ───────────────────────────────────────────────

-- | Push graph + community structure to Memgraph via mgconsole.
pushToMemgraphWithCommunities :: Graph -> CommunityMap -> CohesionMap -> Text -> Text -> Text -> IO (Text, Int, Int)
pushToMemgraphWithCommunities g commMap cohesionMap uri user password = do
  _ <- createMemgraphIndexes uri user password
  let labels = suggestCommunityLabels g commMap
      stmts = generateInlineStatements g
           ++ generateCommunityInlineStatements g commMap cohesionMap labels
  pushMemgraphStatements uri user password stmts

-- ───────────────────────────────────────────────
-- Community-only push (fastest)
-- ───────────────────────────────────────────────

-- | Push community-level graph to Memgraph.
pushCommunityGraphToMemgraph :: Graph -> CommunityMap -> CohesionMap -> Text -> Text -> Text -> IO (Text, Int, Int)
pushCommunityGraphToMemgraph g commMap cohesionMap uri user password = do
  _ <- createMemgraphIndexes uri user password
  let labels = suggestCommunityLabels g commMap
      stmts = generateCommunityOnlyInlineStatements g commMap cohesionMap labels
  pushMemgraphStatements uri user password stmts

-- ───────────────────────────────────────────────
-- Sub-graph push (communities + representative nodes)
-- ───────────────────────────────────────────────

-- | Push communities + representative sub-graphs to Memgraph.
pushSubgraphToMemgraph :: Graph -> CommunityMap -> CohesionMap -> Int -> [NodeId] -> Text -> Text -> Text -> IO (Text, Int, Int)
pushSubgraphToMemgraph g commMap cohesionMap topN artPoints uri user password = do
  _ <- createMemgraphIndexes uri user password
  let labels = suggestCommunityLabels g commMap
      reps = selectRepresentatives g commMap topN artPoints
      allRepNodeIds = Set.fromList (concat (Map.elems reps))
      stmts = generateInlineStatementsForNodes g allRepNodeIds
           ++ generateCommunityInlineStatements g commMap cohesionMap labels
           ++ generateBelongsToInlineStatements reps
           ++ generateInlineEdgesForNodes g allRepNodeIds
           ++ generateCommunityEdgeInlineStatements g commMap
  pushMemgraphStatements uri user password stmts

-- ───────────────────────────────────────────────
-- Shared push implementation
-- ───────────────────────────────────────────────

-- | Push a list of Cypher statements to Memgraph via mgconsole in batches.
-- Uses a temp file to avoid shell/Haskell string escaping issues.
pushMemgraphStatements :: Text -> Text -> Text -> [Text] -> IO (Text, Int, Int)
pushMemgraphStatements uri user password statements = catch (do
  let batches = chunkList 50 statements
      totalBatches = length batches

  results <- mapM (pushMemgraphBatchViaFile uri user password) (zip [1..] batches)
  let failures = [err | Left err <- results]
      totalStmts = length statements

  if null failures
    then pure (T.pack $ "Pushed " ++ show totalStmts ++ " statements in "
              ++ show totalBatches ++ " batch(es) to Memgraph", totalStmts, totalBatches)
    else pure (T.pack $ "Pushed with " ++ show (length failures) ++ " error(s) in "
              ++ show totalBatches ++ " batch(es): "
              ++ T.unpack (T.take 300 (T.intercalate "; " failures)), totalStmts, totalBatches)
  ) $ \(e :: SomeException) ->
    pure (T.pack $ "Memgraph push failed: " ++ show e, 0, 0)

-- | Parse a bolt://host:port URI into (host, port) for mgconsole.
-- Returns ("127.0.0.1", 7687) as default if parsing fails.
parseBoltUri :: Text -> (String, Int)
parseBoltUri uri =
  let noPrefix = case T.stripPrefix "bolt://" uri of
        Just rest -> rest
        Nothing   -> uri
  in case T.breakOn ":" noPrefix of
       (host, "")    -> (T.unpack host, 7687)
       (host, port)  -> case reads (T.unpack (T.drop 1 port)) of
                          [(p, "")] -> (T.unpack host, p)
                          _         -> (T.unpack host, 7687)

-- | Build mgconsole CLI arguments from a Bolt URI, user, and password.
mgconsoleArgs :: Text -> Text -> Text -> [String]
mgconsoleArgs uri user password =
  let (host, port) = parseBoltUri uri
      userArg = if T.null user then [] else ["-username", T.unpack user]
      passArg = if T.null password then [] else ["-password", T.unpack password]
  in userArg ++ passArg ++ ["-host", host, "-port", show port]

-- | Execute a single Cypher statement via mgconsole.
execMemgraphCypher :: Text -> Text -> Text -> Text -> IO (Either Text ())
execMemgraphCypher uri user password cypher = catch (do
  let args = mgconsoleArgs uri user password

  (exitCode, _stdout, stderr) <- readProcessWithExitCode "mgconsole"
    args
    (T.unpack cypher)

  case exitCode of
    ExitSuccess -> pure $ Right ()
    ExitFailure code -> pure $ Left $ T.pack $ "mgconsole failed (exit " ++ show code ++ "): " ++ take 200 stderr
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "mgconsole exception: " ++ show e

-- | Push a batch of Cypher statements to Memgraph via mgconsole using a temp file.
-- Writes Cypher to a temp file and pipes it to mgconsole to avoid
-- Haskell/shell string escaping issues with special characters in node IDs.
pushMemgraphBatchViaFile :: Text -> Text -> Text -> (Int, [Text]) -> IO (Either Text ())
pushMemgraphBatchViaFile uri user password (batchNum, stmts) = catch (do
  tmpDir <- getTemporaryDirectory
  (tmpPath, tmpHandle) <- openTempFile tmpDir "graphos-memgraph-batch.cypher"
  -- Write all statements to temp file as UTF-8 (no shell escaping needed)
  TIO.hPutStr tmpHandle (T.intercalate "\n" stmts)
  hClose tmpHandle

  -- Read file content and pipe to mgconsole via stdin
  cypherContent <- TIO.readFile tmpPath
  let args = mgconsoleArgs uri user password

  (exitCode, _stdout, stderr) <- readProcessWithExitCode "mgconsole"
    args
    (T.unpack cypherContent)

  -- Clean up temp file
  removeFile tmpPath `catch` \(_ :: SomeException) -> pure ()

  case exitCode of
    ExitSuccess -> pure $ Right ()
    ExitFailure code -> pure $ Left $ T.pack $ "mgconsole batch " ++ show batchNum
      ++ " failed (exit " ++ show code ++ "): " ++ take 300 stderr
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Memgraph batch " ++ show batchNum ++ " exception: " ++ show e

-- ───────────────────────────────────────────────
-- Inline Cypher generation (Memgraph-compatible)
-- ───────────────────────────────────────────────
-- Memgraph doesn't support Neo4j's parameterized transaction API,
-- so we generate inline Cypher with values embedded in strings.
-- Uses double-quoted strings with backslash escaping for robustness.

-- | Generate all inline Cypher statements for nodes + edges.
generateInlineStatements :: Graph -> [Text]
generateInlineStatements g =
  [ generateInlineNodeStatement n | n <- Map.elems (gNodes g) ]
  ++ [ generateInlineEdgeStatement e | e <- Map.elems (gEdges g) ]

-- | Generate inline Cypher statements for a subset of nodes.
generateInlineStatementsForNodes :: Graph -> Set.Set NodeId -> [Text]
generateInlineStatementsForNodes g nodeIds =
  [ generateInlineNodeStatement n
  | nid <- Set.toList nodeIds
  , Just n <- [Map.lookup nid (gNodes g)]
  ]

-- | Generate inline edge statements for edges within a node set.
generateInlineEdgesForNodes :: Graph -> Set.Set NodeId -> [Text]
generateInlineEdgesForNodes g nodeIds =
  [ generateInlineEdgeStatement e
  | (_, e) <- Map.toList (filterEdgesByNodeSet nodeIds (gEdges g))
  ]

-- | Generate an inline MERGE statement for a single node.
generateInlineNodeStatement :: Node -> Text
generateInlineNodeStatement n =
  let baseProps = [ "id: " <> cypherQuote (nodeId n)
                  , "label: " <> cypherQuote (nodeLabel n)
                  , "file_type: " <> cypherQuote (T.pack (show (nodeFileType n)))
                  ]
      locProp = maybe [] (\loc -> ["source_location: " <> cypherQuote loc]) (nodeSourceLocation n)
      urlProp = maybe [] (\url -> ["source_url: " <> cypherQuote url]) (nodeSourceUrl n)
      props = T.intercalate ", " (baseProps ++ locProp ++ urlProp)
  in "MERGE (:Node {" <> props <> "});"

-- | Generate an inline MERGE statement for a single edge.
generateInlineEdgeStatement :: Edge -> Text
generateInlineEdgeStatement e =
  let rel = escapeCypherId (relationToText (edgeRelation e))
  in "MATCH (src:Node {id: " <> cypherQuote (edgeSource e) <> "}) "
  <> "MATCH (tgt:Node {id: " <> cypherQuote (edgeTarget e) <> "}) "
  <> "MERGE (src)-[" <> rel
  <> " {confidence: " <> cypherQuote (T.pack (show (edgeConfidence e)))
  <> ", weight: " <> T.pack (show (edgeWeight e))
  <> "}]->(tgt);"

-- | Generate community inline statements (communities + BELONGS_TO).
generateCommunityInlineStatements :: Graph -> CommunityMap -> CohesionMap -> Map.Map CommunityId Text -> [Text]
generateCommunityInlineStatements _g commMap cohesionMap labels =
  [ T.concat
      [ "MERGE (:Community {id: \"community_", T.pack (show cid), "\", label: "
      , cypherQuote (Map.findWithDefault ("Community " <> T.pack (show cid)) cid labels)
      , ", size: ", T.pack (show (length members))
      , ", cohesion: ", T.pack (show (Map.findWithDefault 0.0 cid cohesionMap))
      , "});"
      ]
  | (cid, members) <- Map.toList commMap
  ]
  ++
  [ T.concat
      [ "MATCH (n:Node {id: ", cypherQuote nid, "}) "
      , "MATCH (c:Community {id: \"community_", T.pack (show cid), "\"}) "
      , "MERGE (n)-[:BELONGS_TO]->(c);"
      ]
  | (cid, members) <- Map.toList commMap
  , nid <- members
  ]

-- | Generate community-only inline statements (no individual nodes).
generateCommunityOnlyInlineStatements :: Graph -> CommunityMap -> CohesionMap -> Map.Map CommunityId Text -> [Text]
generateCommunityOnlyInlineStatements g commMap cohesionMap labels =
  [ T.concat
      [ "MERGE (:Community {id: \"community_", T.pack (show cid), "\", label: "
      , cypherQuote (Map.findWithDefault ("Community " <> T.pack (show cid)) cid labels)
      , ", size: ", T.pack (show (length members))
      , ", cohesion: ", T.pack (show (Map.findWithDefault 0.0 cid cohesionMap))
      , ", top_members: ", cypherQuote (T.intercalate ", " (topMemberLabels g members 5))
      , "});"
      ]
  | (cid, members) <- Map.toList commMap
  ]
  ++ generateCommunityEdgeInlineStatements g commMap

-- | Generate BELONGS_TO inline statements for subgraph push.
generateBelongsToInlineStatements :: Map.Map CommunityId [NodeId] -> [Text]
generateBelongsToInlineStatements reps =
  [ T.concat
      [ "MATCH (n:Node {id: ", cypherQuote nid, "}) "
      , "MATCH (c:Community {id: \"community_", T.pack (show cid), "\"}) "
      , "MERGE (n)-[:BELONGS_TO]->(c);"
      ]
  | (cid, members) <- Map.toList reps
  , nid <- members
  ]

-- | Generate CONNECTED_TO inter-community edges (inline).
generateCommunityEdgeInlineStatements :: Graph -> CommunityMap -> [Text]
generateCommunityEdgeInlineStatements g commMap =
  let reverseIdx = Map.fromList
        [(nid, cid) | (cid, members) <- Map.toList commMap, nid <- members]
      edgeCounts :: Map.Map (CommunityId, CommunityId) (Int, [NodeId])
      edgeCounts = Map.fromListWith (\(c1, b1) (c2, b2) -> (c1 + c2, take 5 (b1 ++ b2)))
        [ let sC = Map.findWithDefault (-1) (edgeSource e) reverseIdx
              tC = Map.findWithDefault (-1) (edgeTarget e) reverseIdx
              (c1, c2) = if sC <= tC then (sC, tC) else (tC, sC)
          in ((c1, c2), (1 :: Int, [edgeSource e]))
        | (_, e) <- Map.toList (gEdges g)
        , let srcComm = Map.findWithDefault (-1) (edgeSource e) reverseIdx
              tgtComm = Map.findWithDefault (-1) (edgeTarget e) reverseIdx
        , srcComm /= tgtComm
        , srcComm >= 0 && tgtComm >= 0
        ]
  in [ T.concat
         [ "MATCH (c1:Community {id: \"community_", T.pack (show c1), "\"}) "
         , "MATCH (c2:Community {id: \"community_", T.pack (show c2), "\"}) "
         , "MERGE (c1)-[:CONNECTED_TO {edge_count: ", T.pack (show count)
         , ", bridge_nodes: ", cypherQuote (T.intercalate "," (map (\nid -> maybe nid nodeLabel (Map.lookup nid (gNodes g))) bridges))
         , "}]->(c2);"
         ]
     | ((c1, c2), (count, bridges)) <- Map.toList edgeCounts
 ]

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Split a list into chunks of given size.
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)

-- | Get the top N member node labels for a community.
topMemberLabels :: Graph -> [NodeId] -> Int -> [Text]
topMemberLabels g members n =
  let sortedByDegree = sortOn (\nid -> negate (fromIntegral (Set.size (neighbors g nid)) :: Double)) members
  in take n [ nodeLabel nd | nid <- sortedByDegree, Just nd <- [Map.lookup nid (gNodes g)] ]

-- | Escape a Cypher identifier by wrapping in backticks.
escapeCypherId :: Text -> Text
escapeCypherId t =
  let escaped = T.replace "`" "``" t
  in "`" <> escaped <> "`"

-- | Escape a value for Cypher double-quoted string literals.
-- Handles all special characters: backslash, double-quote, single-quote,
-- newlines, tabs, carriage returns. Uses backslash escaping only.
escapeCypherString :: Text -> Text
escapeCypherString = T.replace "\\" "\\\\"
                   . T.replace "\"" "\\\""
                   . T.replace "'" "\\'"
                   . T.replace "\n" "\\n"
                   . T.replace "\r" "\\r"
                   . T.replace "\t" "\\t"

-- | Quote a Text value as a Cypher double-quoted string literal.
cypherQuote :: Text -> Text
cypherQuote t = "\"" <> escapeCypherString t <> "\""

-- | Generate CREATE INDEX statements for Memgraph.
generateIndexCypher :: Text
generateIndexCypher = T.unlines
  [ "CREATE INDEX ON :Node(id);"
  , "CREATE INDEX ON :Community(id);"
  , "CREATE INDEX ON :Node(file_type);"
  ]