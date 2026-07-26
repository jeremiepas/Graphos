-- | Neo4j Cypher export + HTTP push via curl.
--
-- Three entity types in Neo4j:
--   - Node:     code/doc concepts from the graph
--   - Community: detected clusters with label + cohesion
--   - BELONGS_TO: edges linking Node → Community
--
-- The push uses Neo4j's parameterized statement API, passing all values
-- as JSON parameters rather than embedding in Cypher strings. This
-- eliminates ALL escaping issues with special characters.
--
-- Each batch contains up to 50 statements to stay within Neo4j's limits.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Export.Neo4j
  ( exportCypher
  , pushToNeo4j
  , pushToNeo4jWithCommunities
  , pushSubgraphToNeo4j
  , pushCommunityGraphToNeo4j
  , pushFileExtraction
  , pushEdgeRepair
  , generateSubgraphStatements
  , generateCommunityOnlyStatements
  , generateCommunityStatements
  , generateFileStatements
  , generateEdgeRepairStatements
  ) where

import Control.Exception (catch, SomeException)
import Data.List (sortOn)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.IO (IOMode(..), hFlush, hClose, openFile, hPutStrLn)
import System.Process (readProcessWithExitCode)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges, neighbors)
import Graphos.Domain.Community.Label (suggestCommunityLabels)
import Graphos.Domain.Community (selectRepresentatives, filterEdgesByNodeSet)


-- ───────────────────────────────────────────────
-- Cypher file export (pure IO)
-- ───────────────────────────────────────────────

-- | Generate Cypher statements and write to file (without communities).
-- Streams statements to handle to reduce peak memory for large graphs.
exportCypher :: Graph -> FilePath -> IO ()
exportCypher g path = do
  h <- openFile path WriteMode
  -- Stream node statements one by one
  mapM_ (\n -> hPutStrLn h (T.unpack (generateCypherNodeStatement n))) (Map.elems (gNodes g))
  -- Stream edge statements one by one
  mapM_ (\e -> hPutStrLn h (T.unpack (generateCypherEdgeStatement e))) (Map.elems (gEdges g))
  hFlush h
  hClose h

-- ───────────────────────────────────────────────
-- Neo4j push (basic — nodes + edges only)
-- ───────────────────────────────────────────────

-- | Push graph to Neo4j via curl + transactional API (no communities).
pushToNeo4j :: Graph -> Text -> Text -> Text -> IO (Text, Int, Int)
pushToNeo4j g uri user password = pushStatements uri user password (generateParameterizedStatements g)

-- ───────────────────────────────────────────────
-- Neo4j push (with communities)
-- ───────────────────────────────────────────────

-- | Push graph + community structure to Neo4j via curl + transactional API.
--
-- Creates three entity types:
--   1. (:Node {id, label, file_type, ...})     — code/doc concepts
--   2. (:Community {id, label, size, cohesion}) — detected clusters
--   3. [:BELONGS_TO]                           — Node → Community membership
--
-- Community labels are generated using TF-IDF scoring on member node labels.
-- Cohesion is computed as internal edge density per community.
pushToNeo4jWithCommunities :: Graph -> CommunityMap -> CohesionMap -> Text -> Text -> Text -> IO (Text, Int, Int)
pushToNeo4jWithCommunities g commMap cohesionMap uri user password =
  let labels = suggestCommunityLabels g commMap
      stmts = generateParameterizedStatements g
           ++ generateCommunityStatements g commMap cohesionMap labels
  in pushStatements uri user password stmts

-- ───────────────────────────────────────────────
-- Shared push implementation
-- ───────────────────────────────────────────────

-- | Push a list of parameterized Cypher statements to Neo4j in batches.
pushStatements :: Text -> Text -> Text -> [Aeson.Value] -> IO (Text, Int, Int)
pushStatements uri user password statements = catch (do
  let batches = chunkList 50 statements
      totalBatches = length batches

  results <- mapM (pushBatch uri user password) (zip [1..] batches)
  let failures = [err | Left err <- results]
      totalStmts = length statements

  if null failures
    then pure (T.pack $ "Pushed " ++ show totalStmts ++ " statements in "
              ++ show totalBatches ++ " batch(es)", totalStmts, totalBatches)
    else pure (T.pack $ "Pushed with " ++ show (length failures) ++ " error(s) in "
              ++ show totalBatches ++ " batch(es): "
              ++ T.unpack (T.take 300 (T.intercalate "; " failures)), totalStmts, totalBatches)
  ) $ \(e :: SomeException) ->
    pure (T.pack $ "Neo4j push failed: " ++ show e, 0, 0)

-- | Push a single batch of parameterized Cypher statements to Neo4j via curl.
pushBatch :: Text -> Text -> Text -> (Int, [Aeson.Value]) -> IO (Either Text ())
pushBatch uri user password (batchNum, stmts) = catch (do
  let payload = Aeson.encode $ Aeson.object
        [ "statements" Aeson..= stmts
        ]
      payloadPath = "/tmp/graphos-neo4j-batch-" ++ show batchNum ++ ".json"

  -- Write payload to temp file
  BSL8.writeFile payloadPath payload

  -- Send via curl
  let uriStr = T.unpack uri
      endpoint = uriStr ++ "/db/neo4j/tx/commit"
      userPass = T.unpack user ++ ":" ++ T.unpack password

  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    [ "-s"                        -- silent (no progress)
    , "--max-time", "300"         -- 5 minute timeout
    , "-X", "POST"
    , "-H", "Content-Type: application/json"
    , "-H", "Accept: application/json"
    , "-u", userPass
    , "--data-binary", "@" ++ payloadPath
    , endpoint
    ]
    ""

  -- Cleanup temp file
  removeFile payloadPath `catch` \(_ :: SomeException) -> pure ()

  -- Check for errors
  let response = T.pack stdout
  case exitCode of
    ExitSuccess
      | "\"errors\":[{\"code\"" `T.isInfixOf` response ->
        pure $ Left $ T.pack $ "Neo4j error in batch " ++ show batchNum ++ ": " ++ take 300 stdout
      | "\"errors\":[]" `T.isInfixOf` response || "\"errors\":[0]" `T.isInfixOf` response ->
        pure $ Right ()
      | otherwise ->
        pure $ Right ()
    ExitFailure code ->
      pure $ Left $ T.pack $ "curl failed (exit " ++ show code ++ "): " ++ take 200 stderr
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "Batch " ++ show batchNum ++ " exception: " ++ show e

-- ───────────────────────────────────────────────
-- Parameterized statement generation
-- ───────────────────────────────────────────────

-- | Generate parameterized Cypher statements for nodes + edges.
generateParameterizedStatements :: Graph -> [Aeson.Value]
generateParameterizedStatements g =
  [ generateParameterizedNodeStatement n | n <- Map.elems (gNodes g) ]
  ++ [ generateParameterizedEdgeStatement e | e <- Map.elems (gEdges g) ]

-- | Generate parameterized Cypher statements for communities + BELONGS_TO edges.
generateCommunityStatements :: Graph -> CommunityMap -> CohesionMap -> Map.Map CommunityId Text -> [Aeson.Value]
generateCommunityStatements _g commMap cohesionMap labels =
  -- Community nodes
  [ Aeson.object
      [ "statement" Aeson..= ("MERGE (c:Community {id: $id}) ON CREATE SET c.label = $label, c.size = $size, c.cohesion = $cohesion" :: Text)
      , "parameters" Aeson..= Aeson.object
          [ "id"       Aeson..= T.pack ("community_" ++ show cid)
          , "label"    Aeson..= Map.findWithDefault ("Community " <> T.pack (show cid)) cid labels
          , "size"     Aeson..= length members
          , "cohesion" Aeson..= Map.findWithDefault 0.0 cid cohesionMap
          ]
      ]
  | (cid, members) <- Map.toList commMap
  ]
  ++
  -- BELONGS_TO edges: each node → its community
  [ Aeson.object
      [ "statement" Aeson..= ("MATCH (n:Node {id: $node_id}) MATCH (c:Community {id: $community_id}) MERGE (n)-[:BELONGS_TO]->(c)" :: Text)
      , "parameters" Aeson..= Aeson.object
          [ "node_id"      Aeson..= nid
          , "community_id" Aeson..= T.pack ("community_" ++ show cid)
          ]
      ]
  | (cid, members) <- Map.toList commMap
  , nid <- members
  ]

-- | Generate a parameterized MERGE statement for a single node.
generateParameterizedNodeStatement :: Node -> Aeson.Value
generateParameterizedNodeStatement n =
  let stmt = T.concat
        [ "MERGE (n:Node {id: $id})"
        , " ON CREATE SET n.label = $label, n.file_type = $file_type"
        , maybe "" (const ", n.line_start = $line_start") (nodeLineStart n)
        , maybe "" (const ", n.line_end = $line_end") (nodeLineEnd n)
        ]
      params = Aeson.object $
        [ "id"              Aeson..= nodeId n
        , "label"           Aeson..= nodeLabel n
        , "file_type"       Aeson..= T.pack (show (nodeFileType n))
        ]
        ++ maybe [] (\start -> ["line_start" Aeson..= start]) (nodeLineStart n)
        ++ maybe [] (\end   -> ["line_end" Aeson..= end])     (nodeLineEnd n)
  in Aeson.object
       [ "statement" Aeson..= stmt
       , "parameters" Aeson..= params
       ]

-- | Generate a parameterized statement for a single edge.
generateParameterizedEdgeStatement :: Edge -> Aeson.Value
generateParameterizedEdgeStatement e =
  let rel = T.replace "`" "``" (relationToText (edgeRelation e))
  in Aeson.object
       [ "statement" Aeson..= T.concat
           [ "MATCH (src:Node {id: $source_id}) "
           , "MATCH (tgt:Node {id: $target_id}) "
           , "MERGE (src)-[:`" <> rel <> "` {confidence: $confidence, weight: $weight}]->(tgt)"
           ]
       , "parameters" Aeson..= Aeson.object
           [ "source_id"  Aeson..= edgeSource e
           , "target_id"  Aeson..= edgeTarget e
           , "confidence" Aeson..= T.pack (show (edgeConfidence e))
           , "weight"     Aeson..= edgeWeight e
           ]
       ]

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Split a list into chunks of given size.
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)

-- ───────────────────────────────────────────────
-- Cypher file generation (for .cypher export)
-- ───────────────────────────────────────────────

-- | Generate a MERGE statement for a single node (for .cypher file).
generateCypherNodeStatement :: Node -> Text
generateCypherNodeStatement n =
  let baseProps :: [Text]
      baseProps =
        [ "id: " <> cypherQuote (nodeId n)
        , "label: " <> cypherQuote (nodeLabel n)
        , "file_type: " <> cypherQuote (T.pack (show (nodeFileType n)))
        ]
      lineStartProp = maybe [] (\start -> ["line_start: " <> T.pack (show start)]) (nodeLineStart n)
      lineEndProp   = maybe [] (\end   -> ["line_end: " <> T.pack (show end)])     (nodeLineEnd n)
      props = T.intercalate ", " (baseProps ++ lineStartProp ++ lineEndProp)
  in "MERGE (:Node {" <> props <> "})"
  where
    cypherQuote :: Text -> Text
    cypherQuote t = "'" <> escapeCypherString t <> "'"

-- | Generate a MERGE statement for a single edge (for .cypher file).
generateCypherEdgeStatement :: Edge -> Text
generateCypherEdgeStatement e =
  let rel = escapeCypherId (relationToText (edgeRelation e))
  in "MATCH (src:Node {id: " <> cypherQuote (edgeSource e) <> "}) "
   <> "MATCH (tgt:Node {id: " <> cypherQuote (edgeTarget e) <> "}) "
   <> "MERGE (src)-[:" <> rel
   <> " {confidence: " <> cypherQuote (T.pack (show (edgeConfidence e)))
   <> ", weight: " <> T.pack (show (edgeWeight e))
   <> "}]->(tgt)"
  where
    cypherQuote :: Text -> Text
    cypherQuote t = "'" <> escapeCypherString t <> "'"

-- ───────────────────────────────────────────────
-- Cypher escaping helpers (for .cypher file only)
-- ───────────────────────────────────────────────

-- | Escape a Cypher identifier by wrapping in backticks.
escapeCypherId :: Text -> Text
escapeCypherId t =
  let escaped = T.replace "`" "``" t
  in "`" <> escaped <> "`"

-- | Escape a value for Cypher string literals (for .cypher file only).
escapeCypherString :: Text -> Text
escapeCypherString = T.replace "\\" "\\\\"
                   . T.replace "'" "''"

-- ───────────────────────────────────────────────
-- Community-only push (fastest — no individual nodes)
-- ───────────────────────────────────────────────

-- | Push community-level graph to Neo4j (no individual nodes or edges).
-- Creates Community nodes and CONNECTED_TO inter-community edges.
-- Fastest mode: ~3k-8k statements for large codebases.
pushCommunityGraphToNeo4j :: Graph -> CommunityMap -> CohesionMap -> Text -> Text -> Text -> IO (Text, Int, Int)
pushCommunityGraphToNeo4j g commMap cohesionMap uri user password =
  let labels = suggestCommunityLabels g commMap
      stmts = generateCommunityOnlyStatements g commMap cohesionMap labels
  in pushStatements uri user password stmts

-- | Generate parameterized Cypher statements for community-only push.
-- Pure function — testable without Neo4j.
generateCommunityOnlyStatements :: Graph -> CommunityMap -> CohesionMap -> Map.Map CommunityId Text -> [Aeson.Value]
generateCommunityOnlyStatements g commMap cohesionMap labels =
  -- Community nodes
  [ Aeson.object
      [ "statement" Aeson..= ("MERGE (c:Community {id: $id}) ON CREATE SET c.label = $label, c.size = $size, c.cohesion = $cohesion, c.top_members = $top_members" :: Text)
      , "parameters" Aeson..= Aeson.object
          [ "id"          Aeson..= T.pack ("community_" ++ show cid)
          , "label"       Aeson..= Map.findWithDefault ("Community " <> T.pack (show cid)) cid labels
          , "size"         Aeson..= length members
          , "cohesion"    Aeson..= Map.findWithDefault 0.0 cid cohesionMap
          , "top_members" Aeson..= topMemberLabels g members 5
          ]
      ]
  | (cid, members) <- Map.toList commMap
  ]
  ++
  -- Inter-community CONNECTED_TO edges
  generateCommunityEdgeStatements g commMap

-- ───────────────────────────────────────────────
-- Sub-graph push (communities + representative nodes)
-- ───────────────────────────────────────────────

-- | Push communities + representative sub-graphs to Neo4j.
-- Creates Community nodes, representative Node nodes, BELONGS_TO edges,
-- intra-community edges between representatives, and CONNECTED_TO inter-community edges.
pushSubgraphToNeo4j :: Graph -> CommunityMap -> CohesionMap -> Int -> [NodeId] -> Text -> Text -> Text -> IO (Text, Int, Int)
pushSubgraphToNeo4j g commMap cohesionMap topN artPoints uri user password =
  let labels = suggestCommunityLabels g commMap
      reps = selectRepresentatives g commMap topN artPoints
      allRepNodeIds = Set.fromList (concat (Map.elems reps))
      stmts = generateSubgraphStatements g commMap cohesionMap labels reps allRepNodeIds
  in pushStatements uri user password stmts

-- | Generate parameterized Cypher statements for sub-graph push.
-- Pure function — testable without Neo4j.
generateSubgraphStatements
  :: Graph
  -> CommunityMap
  -> CohesionMap
  -> Map.Map CommunityId Text
  -> Map.Map CommunityId [NodeId]   -- ^ representatives per community
  -> Set.Set NodeId                 -- ^ all representative/bridge node IDs
  -> [Aeson.Value]
generateSubgraphStatements g commMap cohesionMap labels reps allRepNodeIds =
  -- 1. Community nodes
  [ Aeson.object
      [ "statement" Aeson..= ("MERGE (c:Community {id: $id}) ON CREATE SET c.label = $label, c.size = $size, c.cohesion = $cohesion" :: Text)
      , "parameters" Aeson..= Aeson.object
          [ "id"       Aeson..= T.pack ("community_" ++ show cid)
          , "label"    Aeson..= Map.findWithDefault ("Community " <> T.pack (show cid)) cid labels
          , "size"     Aeson..= length members
          , "cohesion" Aeson..= Map.findWithDefault 0.0 cid cohesionMap
          ]
      ]
  | (cid, members) <- Map.toList commMap
  ]
  ++
  -- 2. Representative Node nodes
  [ generateRepresentativeNodeStatement n
  | nid <- Set.toList allRepNodeIds
  , Just n <- [Map.lookup nid (gNodes g)]
  ]
  ++
  -- 3. BELONGS_TO edges (representative nodes → their community)
  [ Aeson.object
      [ "statement" Aeson..= ("MATCH (n:Node {id: $node_id}) MATCH (c:Community {id: $community_id}) MERGE (n)-[:BELONGS_TO]->(c)" :: Text)
      , "parameters" Aeson..= Aeson.object
          [ "node_id"      Aeson..= nid
          , "community_id" Aeson..= T.pack ("community_" ++ show cid)
          ]
      ]
  | (cid, members) <- Map.toList reps
  , nid <- members
  ]
  ++
  -- 4. Intra-community edges between representative nodes
  [ generateParameterizedEdgeStatement e
  | (_, e) <- Map.toList (filterEdgesByNodeSet allRepNodeIds (gEdges g))
  ]
  ++
  -- 5. Inter-community CONNECTED_TO edges
  generateCommunityEdgeStatements g commMap

-- ───────────────────────────────────────────────
-- Shared helpers for community push modes
-- ───────────────────────────────────────────────

-- | Generate a parameterized MERGE statement for a representative node.
-- Marks the node as representative=true so it can be distinguished from full-push nodes.
generateRepresentativeNodeStatement :: Node -> Aeson.Value
generateRepresentativeNodeStatement n =
  let stmt = T.concat
        [ "MERGE (n:Node {id: $id})"
        , " ON CREATE SET n.label = $label, n.file_type = $file_type, n.representative = true"
        , maybe "" (const ", n.line_start = $line_start") (nodeLineStart n)
        , maybe "" (const ", n.line_end = $line_end") (nodeLineEnd n)
        ]
      params = Aeson.object $
        [ "id"              Aeson..= nodeId n
        , "label"           Aeson..= nodeLabel n
        , "file_type"       Aeson..= T.pack (show (nodeFileType n))
        , "representative"  Aeson..= Aeson.Bool True
        ]
        ++ maybe [] (\start -> ["line_start" Aeson..= start]) (nodeLineStart n)
        ++ maybe [] (\end   -> ["line_end" Aeson..= end])     (nodeLineEnd n)
  in Aeson.object
       [ "statement" Aeson..= stmt
       , "parameters" Aeson..= params
       ]

-- | Generate CONNECTED_TO inter-community edge statements.
-- Shared between community-only and sub-graph push modes.
generateCommunityEdgeStatements :: Graph -> CommunityMap -> [Aeson.Value]
generateCommunityEdgeStatements g commMap =
  let reverseIdx = Map.fromList
        [(nid, cid) | (cid, members) <- Map.toList commMap, nid <- members]
      edgeCounts :: Map.Map (CommunityId, CommunityId) (Int, [NodeId])
      edgeCounts = Map.fromListWith (\(c1, b1) (c2, b2) -> (c1 + c2, take 5 (b1 ++ b2)))
        [ let srcComm = Map.findWithDefault (-1) (edgeSource e) reverseIdx
              tgtComm = Map.findWithDefault (-1) (edgeTarget e) reverseIdx
              (c1, c2) = if srcComm <= tgtComm then (srcComm, tgtComm) else (tgtComm, srcComm)
          in ((c1, c2), (1 :: Int, [edgeSource e]))
        | (_, e) <- Map.toList (gEdges g)
        , let srcC = Map.findWithDefault (-1) (edgeSource e) reverseIdx
              tgtC = Map.findWithDefault (-1) (edgeTarget e) reverseIdx
        , srcC /= tgtC
        , srcC >= 0 && tgtC >= 0
        ]
  in [ Aeson.object
         [ "statement" Aeson..= ("MATCH (c1:Community {id: $source_id}) MATCH (c2:Community {id: $target_id}) MERGE (c1)-[:CONNECTED_TO {edge_count: $edge_count, bridge_nodes: $bridge_nodes}]->(c2)" :: Text)
         , "parameters" Aeson..= Aeson.object
             [ "source_id"    Aeson..= T.pack ("community_" ++ show c1)
             , "target_id"    Aeson..= T.pack ("community_" ++ show c2)
             , "edge_count"   Aeson..= count
             , "bridge_nodes" Aeson..= map (\nid -> maybe nid nodeLabel (Map.lookup nid (gNodes g))) bridges
             ]
         ]
     | ((c1, c2), (count, bridges)) <- Map.toList edgeCounts
     ]

-- | Get the top N member node labels for a community (used in community-only push).
topMemberLabels :: Graph -> [NodeId] -> Int -> [Text]
topMemberLabels g members n =
  let sortedByDegree = sortOn (\nid -> negate (fromIntegral (Set.size (neighbors g nid)) :: Double)) members
  in take n [ nodeLabel nd | nid <- sortedByDegree, Just nd <- [Map.lookup nid (gNodes g)] ]

-- ───────────────────────────────────────────────
-- Streaming node-by-node push (during extraction)
-- ───────────────────────────────────────────────

-- | Push a single file's extraction to Neo4j immediately.
--
-- Each file's nodes and edges are pushed as a small batch using MERGE,
-- making this idempotent and safe for incremental/streaming use.
--
-- Returns: (message, statementCount, batchCount)
pushFileExtraction :: Extraction -> Text -> Text -> Text -> IO (Text, Int, Int)
pushFileExtraction extraction uri user password =
  let stmts = generateFileStatements extraction
  in if null stmts
     then pure ("Skipped empty extraction", 0, 0)
     else pushStatements uri user password stmts

-- | Generate parameterized Cypher statements for a single file's extraction.
--
-- Pure function — testable without Neo4j.
-- Produces MERGE statements for each node and edge in the extraction,
-- so re-pushing is safe (idempotent).
generateFileStatements :: Extraction -> [Aeson.Value]
generateFileStatements extraction =
  [ generateParameterizedNodeStatement n | n <- Map.elems (extNodes extraction) ]
  ++ [ generateParameterizedEdgeStatement e | e <- Map.elems (extEdges extraction) ]

-- | Push edge-repair statements to Neo4j.
--
-- After all extractions are complete, edges may reference nodes from
-- other files. MERGE already handles this (creates nodes on match),
-- but edges with MATCH require the target nodes to exist. This function
-- re-pushes all edges to ensure MATCH clauses resolve correctly.
--
-- Returns: (message, statementCount, batchCount)
pushEdgeRepair :: Graph -> Text -> Text -> Text -> IO (Text, Int, Int)
pushEdgeRepair g uri user password =
  pushStatements uri user password (generateEdgeRepairStatements g)

-- | Generate edge-repair statements: re-push all edges with MATCH.
--
-- This ensures that edges between nodes extracted from different files
-- are properly connected, since nodes may have been pushed before their
-- cross-file neighbors existed in Neo4j.
generateEdgeRepairStatements :: Graph -> [Aeson.Value]
generateEdgeRepairStatements g =
  [ generateParameterizedEdgeStatement e | e <- Map.elems (gEdges g) ]