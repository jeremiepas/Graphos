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
  ) where

import Control.Exception (catch, SomeException)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges)
import Graphos.Domain.Community.Label (suggestCommunityLabels)

-- ───────────────────────────────────────────────
-- Cypher file export (pure IO)
-- ───────────────────────────────────────────────

-- | Generate Cypher statements and write to file (without communities).
exportCypher :: Graph -> FilePath -> IO ()
exportCypher g path = do
  let cypher = generateCypher g
  writeFile path (T.unpack cypher)

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
        , maybe "" (const ", n.source_location = $source_location") (nodeSourceLocation n)
        , maybe "" (const ", n.source_url = $source_url") (nodeSourceUrl n)
        ]
      params = Aeson.object $
        [ "id"              Aeson..= nodeId n
        , "label"           Aeson..= nodeLabel n
        , "file_type"       Aeson..= T.pack (show (nodeFileType n))
        ]
        ++ maybe [] (\loc -> ["source_location" Aeson..= loc]) (nodeSourceLocation n)
        ++ maybe [] (\url -> ["source_url" Aeson..= url]) (nodeSourceUrl n)
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

-- | Generate Cypher MERGE statements for the .cypher file (no communities).
generateCypher :: Graph -> Text
generateCypher g =
  let nodeStatements = [ generateCypherNodeStatement n | n <- Map.elems (gNodes g) ]
      edgeStatements = [ generateCypherEdgeStatement e | e <- Map.elems (gEdges g) ]
  in T.unlines (nodeStatements ++ edgeStatements)

-- | Generate a MERGE statement for a single node (for .cypher file).
generateCypherNodeStatement :: Node -> Text
generateCypherNodeStatement n =
  let baseProps :: [Text]
      baseProps =
        [ "id: " <> cypherQuote (nodeId n)
        , "label: " <> cypherQuote (nodeLabel n)
        , "file_type: " <> cypherQuote (T.pack (show (nodeFileType n)))
        ]
      locProp = maybe [] (\loc -> ["source_location: " <> cypherQuote loc]) (nodeSourceLocation n)
      urlProp = maybe [] (\url -> ["source_url: " <> cypherQuote url]) (nodeSourceUrl n)
      props = T.intercalate ", " (baseProps ++ locProp ++ urlProp)
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