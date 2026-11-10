-- | Neo4j Cypher export + HTTP push via an in-process persistent client.
--
-- Three entity types in Neo4j:
--   - Node:     code/doc concepts from the graph
--   - Community: detected clusters with label + cohesion
--   - BELONGS_TO: edges linking Node → Community
--
-- The push uses Neo4j's parameterized UNWIND row batches: one statement per
-- entity kind per chunk instead of one statement per node\/edge. All values
-- pass as JSON parameters (never interpolated into Cypher strings), so
-- special characters in labels\/ids cannot cause injection.
--
-- Lookup keys: every node row carries @id_hash = sha1(id)@ (lowercase hex
-- over UTF-8 bytes — see "Graphos.Domain.CypherHash"), and all node
-- MERGE\/MATCH lookups key on @id_hash@, never raw @id@. Raw ids can exceed
-- Neo4j's ~8 KB RANGE-index key limit, which leaves an id-constraint FAILED
-- and every lookup unindexed.
--
-- Before the first data statement the push runs a schema setup phase:
-- backfill @id_hash@ on hashless @:Node@ rows (batched LIMIT windows, no
-- APOC dependency), then @CREATE CONSTRAINT ... IF NOT EXISTS@ on
-- @:Node(id_hash)@ and @:Community(id)@ — each schema statement in its own
-- transaction (Neo4j forbids mixing schema and write statements in one
-- transaction). A failed constraint creation (pre-existing duplicate
-- @id_hash@ rows) aborts the push with a named error before any data
-- statement is sent.
--
-- Transport: one shared @http-client@ 'Manager' with keep-alive connection
-- reuse across the whole push; request bodies stay in memory. No external
-- curl process and no temp-file payload.
--
-- Nodes are always fully pushed before the first edge batch, so edge
-- endpoint lookups always resolve.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
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
    , generateParameterizedStatements
   , generateCommunityOnlyStatements
  , generateCommunityStatements
  , generateFileStatements
  , generateEdgeRepairStatements
    -- * UNWIND row batch generation (pure, testable without Neo4j)
  , nodeRow
  , edgeRow
  , communityRow
  , communityOnlyRow
  , representativeRow
  , belongsToRow
  , communityKey
  , fileTypeText
  , nodeBatchStatement
  , communityBatchStatement
  , communityOnlyBatchStatement
  , belongsToBatchStatement
  , edgeBatchStatement
  , planNodeBatches
  , planEdgeBatches
  , chunkBy
  , defaultNodeChunkRows
  , defaultNodeChunkBytes
  , defaultEdgeChunkRows
  , graphNodeRows
  , nodeStatements
  , edgeStatements
    -- * Transport (exposed for integration tests)
  , sendTx
  , firstRowScalar
  ) where

import Control.Exception (catch, SomeException)
import Data.Function ((&))
import Data.List (sortOn)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Short (toText)
import System.IO (hFlush, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)

import Network.HTTP.Client
  ( Manager
  , RequestBody(..)
  , applyBasicAuth
  , defaultManagerSettings
  , httpLbs
  , method
  , newManager
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  )
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (statusCode)

import Graphos.Domain.CypherHash (nodeIdHash)
import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges, neighbors)
import Graphos.Domain.Community.Label (suggestCommunityLabels)
import Graphos.Domain.Community (selectRepresentatives, filterEdgesByNodeSet)
import Graphos.Infrastructure.FileSystem.AtomicWrite (withAtomicHandle)

-- ───────────────────────────────────────────────
-- Transport: in-process persistent HTTP client (D4)
-- ───────────────────────────────────────────────

-- | Shared connection manager: created lazily once per process and reused by
-- every Neo4j request, so connections stay alive across the entire push.
-- Replaces the historical per-batch curl subprocess + \/tmp payload file.
neo4jManager :: Manager
neo4jManager = unsafePerformIO (newManager defaultManagerSettings)
{-# NOINLINE neo4jManager #-}

-- | Per-request timeout: 5 minutes, matching the historical curl --max-time.
neo4jRequestTimeout :: Int
neo4jRequestTimeout = 5 * 60 * 1000 * 1000

-- | Transactional endpoint for a Neo4j HTTP URI. Accepts
-- @http(s):\/\/host:port@; @bolt:\/\/@ URIs are normalized to @http:\/\/@
-- (the push is HTTP; the config historically stored bolt-style hosts).
txEndpoint :: Text -> String
txEndpoint uri = T.unpack (T.dropWhileEnd (== '/') (boltToHttp uri)) ++ "/db/neo4j/tx/commit"
  where
    boltToHttp u = case T.stripPrefix "bolt://" u of
      Just rest -> "http://" <> rest
      Nothing   -> u

-- | Parse a transactional commit response into either an error message or
-- the raw response body (callers like the backfill loop parse counts from
-- it). Non-2xx statuses and responses carrying a nonempty @errors@ array
-- both surface as errors carrying the HTTP status and Neo4j's error JSON —
-- which names the failing constraint\/statement.
interpretTxResponse :: Int -> BSL.ByteString -> Either Text BSL.ByteString
interpretTxResponse st body
  | st < 200 || st >= 300 = Left (responseError st bodyText)
  | hasErrors             = Left (responseError st bodyText)
  | otherwise             = Right body
  where
    bodyText  = T.pack (BSL8.unpack (BSL.take 2000 body))
    hasErrors = "\"errors\":[" `T.isInfixOf` bodyText
            && not ("\"errors\":[]" `T.isInfixOf` bodyText)
    responseError s b = T.pack $
      "Neo4j push failed (HTTP " ++ show s ++ "): " ++ T.unpack b

-- | POST one transactions payload. @Right body@ carries the raw JSON
-- response on success; @Left@ names the HTTP status and Neo4j's error JSON.
sendTx :: Text -> Text -> Text -> [Aeson.Value] -> IO (Either Text BSL.ByteString)
sendTx uri user password stmts = catch go handler
  where
    go = do
      baseReq <- parseRequest (txEndpoint uri)
      let req = baseReq
            { method          = "POST"
            , requestHeaders  = [(hContentType, "application/json")]
            , requestBody     = RequestBodyLBS (Aeson.encode (Aeson.object ["statements" Aeson..= stmts]))
            , responseTimeout = responseTimeoutMicro neo4jRequestTimeout
            }
            & applyBasicAuth (encodeUtf8 user) (encodeUtf8 password)
      res <- httpLbs req neo4jManager
      pure (interpretTxResponse (statusCode (responseStatus res)) (responseBody res))
    handler :: SomeException -> IO (Either Text BSL.ByteString)
    handler = pure . Left . T.pack . ("Neo4j transport error: " ++) . show

-- | POST one statements payload and discard the response body on success.
sendStatements :: Text -> Text -> Text -> [Aeson.Value] -> IO (Either Text ())
sendStatements uri user password stmts = (() <$) <$> sendTx uri user password stmts

-- | Extract the first scalar of the first result row from a transactional
-- commit response (@{"results":[{"data":[{"row":[N]}]}]}@), if present.
firstRowScalar :: BSL.ByteString -> Maybe Int
firstRowScalar body =
  case (Aeson.eitherDecode body :: Either String Aeson.Value) of
    Right v -> do
      results <- firstArrayElement =<< lookupKey "results" v
      datum   <- firstArrayElement =<< lookupKey "data" results
      scalar  <- lookupKey "row" datum >>= firstArrayElement
      case scalar of
        Aeson.Number s -> Just (round s)
        _              -> Nothing
    _ -> Nothing
  where
    lookupKey k (Aeson.Object o) = KeyMap.lookup k o
    lookupKey _ _                = Nothing
    firstArrayElement (Aeson.Array a) =
      if V.null a then Nothing else Just (V.head a)
    firstArrayElement _               = Nothing

-- ───────────────────────────────────────────────
-- Schema setup (D2) — backfill + constraints
-- ───────────────────────────────────────────────

-- | Backfill window size: hashless @:Node@ rows processed per transaction.
schemaBackfillBatchSize :: Int
schemaBackfillBatchSize = 5000

data NodeRow = NodeRow
  { nrId      :: Text
  , nrIdHash  :: Text
  } deriving (Eq, Show)

-- | One backfill window, client-side: read a window of hashless node ids,
-- hash them with the pusher's own pure 'nodeIdHash' (parity with any future
-- push by construction), and write the hashes back via UNWIND. This part
-- returns the raw ids; 'backfillRows' loops it.
--
-- Vanilla Neo4j (community, no APOC) has no @sha1()@ function, so the hash
-- runs in Haskell instead of in Cypher. Batched read + write keeps each
-- transaction bounded; the window's @SKIP@ advances deterministically because
-- unhashed rows are ordered by @elementId@.
--
-- Note: a server-side backfill was originally planned
-- (@SET n.id_hash = sha1(n.id)@); it cannot exist on vanilla Neo4j — there is
-- no sha1 function and an equivalent cannot be expressed sanely in Cypher PL
-- (no bitwise ops). Deviation approved during implementation: hashing moved
-- client-side.
--
-- Returns the number of node ids read in this window (0 ⇒ backfill done).
backfillWindow :: Text -> Text -> Text -> IO (Either Text [Text])
backfillWindow uri user password = do
  let readStmt = Aeson.object
        [ "statement" Aeson..= T.concat
            [ "MATCH (n:Node) WHERE n.id_hash IS NULL "
            , "WITH n ORDER BY elementId(n) LIMIT "
            , T.pack (show schemaBackfillBatchSize), " "
            , "RETURN n.id AS id"
            ]
        , "parameters" Aeson..= Aeson.object []
        ]
  r <- sendTx uri user password [readStmt]
  case r of
    Left err -> pure (Left err)
    Right body -> pure (Right (rowsTextColumn "id" body))

-- | Write hashed rows back (@SET n.id_hash@) in one UNWIND transaction.
backfillWrite :: Text -> Text -> Text -> [(Text, Text)] -> IO (Either Text ())
backfillWrite uri user password hashed =
  sendStatements uri user password
    [ Aeson.object
        [ "statement" Aeson..= T.concat
            [ "UNWIND $rows AS row "
            , "MATCH (n:Node {id: row.id}) "
            , "SET n.id_hash = row.id_hash"
            ]
        , "parameters" Aeson..= Aeson.object
            [ "rows" Aeson..=
                [ Aeson.object [ "id" Aeson..= nid, "id_hash" Aeson..= h ]
                | (nid, h) <- hashed ]
            ]
        ]
    ]

-- | Extract the id column over all result rows of a commit response
-- (@{"results":[{"data":[{"row":["id-as-text"]}]}]}@ for the backfill's
-- single-column RETURN).
rowsTextColumn :: Text -> BSL.ByteString -> [Text]
rowsTextColumn _col body =
  case (Aeson.eitherDecode body :: Either String Aeson.Value) of
    Right v -> concatMap rowCells (resultsData v)
    _       -> []
  where
    resultsData v = case lookupKey "results" v of
      Just (Aeson.Array rs) | not (V.null rs) -> case lookupKey "data" (V.head rs) of
        Just (Aeson.Array ds) -> foldr (:) [] ds
        _                     -> []
      _ -> []
    rowCells datum = case lookupKey "row" datum of
      Just (Aeson.Array a) -> [ t | Aeson.String t <- foldr (:) [] a ]
      _ -> []
    lookupKey k (Aeson.Object o) = KeyMap.lookup k o
    lookupKey _ _                = Nothing

-- | Full client-side backfill: loop windows until one reads fewer rows than
-- the batch size. Returns the total number of nodes backfilled.
backfillRows :: Text -> Text -> Text -> IO (Either Text Int)
backfillRows uri user password = go 0
  where
    go !total = do
      r <- backfillWindow uri user password
      case r of
        Left err -> pure (Left err)
        Right ids
          | null ids  -> pure (Right total)
          | otherwise -> do
              w <- backfillWrite uri user password
                [ (nid, nodeIdHash nid) | nid <- ids ]
              case w of
                Left err -> pure (Left err)
                Right () -> go (total + length ids)

-- | Uniqueness constraints created before data flows. Each is sent in its
-- own transaction; @IF NOT EXISTS@ makes re-pushing a no-op.
constraintStatements :: [Aeson.Value]
constraintStatements =
  [ Aeson.object
      [ "statement" Aeson..=
          ("CREATE CONSTRAINT node_id_hash_unique IF NOT EXISTS FOR (n:Node) REQUIRE n.id_hash IS UNIQUE" :: Text)
      , "parameters" Aeson..= Aeson.object []
      ]
  , Aeson.object
      [ "statement" Aeson..=
          ("CREATE CONSTRAINT community_id_unique IF NOT EXISTS FOR (c:Community) REQUIRE c.id IS UNIQUE" :: Text)
      , "parameters" Aeson..= Aeson.object []
      ]
  ]

-- | Run the schema setup phase: backfill @id_hash@ on hashless @:Node@ rows
-- (client-side hashing in batched read\/write windows — vanilla Neo4j has no
-- sha1 function), then create the uniqueness constraints.
--
-- @Right ()@ means every subsequent lookup is an indexed seek. @Left@
-- carries a named, actionable error — constraint creation fails on
-- pre-existing duplicate @id_hash@ rows, and the message names the violated
-- constraint. No data statement may be sent after a schema failure.
runSchemaSetup :: Text -> Text -> Text -> IO (Either Text ())
runSchemaSetup uri user password = do
  backfill <- backfillRows uri user password
  case backfill of
    Left err -> pure (Left ("schema backfill failed: " <> err))
    Right _  -> constraintsLoop constraintStatements
  where
    constraintsLoop [] = pure (Right ())
    constraintsLoop (c : rest) = do
      r <- sendStatements uri user password [c]
      case r of
        Left err -> pure (Left ("schema setup failed: " <> err))
        Right () -> constraintsLoop rest

-- | Guardrail asserted by tests (task 3.3): no data statement may follow a
-- schema failure. 'pushWithSchema' enforces it structurally — the data list
-- is only consumed when 'runSchemaSetup' returns @Right@.

-- ───────────────────────────────────────────────
-- UNWIND row builders (pure)
-- ───────────────────────────────────────────────

-- | Node row: keyed on @id_hash@; raw @id@ kept as a regular property.
nodeRow :: Node -> Aeson.Value
nodeRow n = Aeson.object $
  [ "id_hash"    Aeson..= nodeIdHash (nodeId n)
  , "id"         Aeson..= nodeId n
  , "label"      Aeson..= toText (nodeLabel n)
  , "file_type"  Aeson..= fileTypeText (nodeFileType n)
  ]
  ++ maybe [] (\s -> ["line_start" Aeson..= s]) (nodeLineStart n)
  ++ maybe [] (\e -> ["line_end"   Aeson..= e]) (nodeLineEnd n)

-- | All node rows for a graph (node-id order — deterministic chunks).
graphNodeRows :: Graph -> [Aeson.Value]
graphNodeRows g = [ nodeRow n | n <- Map.elems (gNodes g) ]

-- | Edge row: endpoints referenced by @id_hash@.
edgeRow :: Edge -> Aeson.Value
edgeRow e = Aeson.object
  [ "source_hash" Aeson..= nodeIdHash (edgeSource e)
  , "target_hash" Aeson..= nodeIdHash (edgeTarget e)
  , "confidence"  Aeson..= edgeConfidence e
  , "weight"      Aeson..= edgeWeight e
  ]

-- | Community node row.
communityRow :: CohesionMap -> Map.Map CommunityId Text -> (CommunityId, [NodeId]) -> Aeson.Value
communityRow cohesionMap labels (cid, members) = Aeson.object
  [ "id"       Aeson..= communityKey cid
  , "label"    Aeson..= Map.findWithDefault ("Community " <> T.pack (show cid)) cid labels
  , "size"     Aeson..= length members
  , "cohesion" Aeson..= Map.findWithDefault 0.0 cid cohesionMap
  ]

-- | Community node row with top members (community-only push).
communityOnlyRow :: Graph -> CohesionMap -> Map.Map CommunityId Text -> (CommunityId, [NodeId]) -> Aeson.Value
communityOnlyRow g cohesionMap labels (cid, members) = Aeson.object
  [ "id"          Aeson..= communityKey cid
  , "label"       Aeson..= Map.findWithDefault ("Community " <> T.pack (show cid)) cid labels
  , "size"        Aeson..= length members
  , "cohesion"    Aeson..= Map.findWithDefault 0.0 cid cohesionMap
  , "top_members" Aeson..= topMemberLabels g members 5
  ]

-- | Representative node row (subgraph push): adds the @representative@ flag.
representativeRow :: Node -> Aeson.Value
representativeRow n = case nodeRow n of
  Aeson.Object o -> Aeson.Object (KeyMap.insert "representative" (Aeson.Bool True) o)
  other          -> other

-- | BELONGS_TO row: node by @id_hash@, community by id.
belongsToRow :: NodeId -> CommunityId -> Aeson.Value
belongsToRow nid cid = Aeson.object
  [ "node_hash"    Aeson..= nodeIdHash nid
  , "community_id" Aeson..= communityKey cid
  ]

-- | The Community id key (kept in its historical @community_N@ form).
communityKey :: CommunityId -> Text
communityKey cid = T.pack ("community_" ++ show cid)

-- | Aeson encoding of a 'FileType' for push payloads (matches the JSON
-- encoder's FileType serialization).
fileTypeText :: FileType -> Text
fileTypeText ft = case ft of
  CodeFile   -> "code"
  DocFile    -> "doc"
  PaperFile  -> "paper"
  ImageFile  -> "image"
  VideoFile  -> "video"
  AudioFile  -> "audio"
  OfficeFile -> "office"

-- ───────────────────────────────────────────────
-- UNWIND statement builders (pure)
-- ───────────────────────────────────────────────

-- | Node UNWIND statement carrying one chunk of rows. MERGE keys on
-- @id_hash@; @ON CREATE SET@ stores the raw @id@.
nodeBatchStatement :: [Aeson.Value] -> Aeson.Value
nodeBatchStatement rows = Aeson.object
  [ "statement" Aeson..= T.concat
      [ "UNWIND $rows AS row "
      , "MERGE (n:Node {id_hash: row.id_hash}) "
      , "ON CREATE SET n.id = row.id, n.label = row.label, n.file_type = row.file_type, "
      , "n.line_start = row.line_start, n.line_end = row.line_end"
      ]
  , "parameters" Aeson..= Aeson.object ["rows" Aeson..= rows]
  ]

-- | Edge UNWIND statement for one relationship type; the type is
-- backtick-escaped (a relationship type cannot be a parameter), endpoints
-- matched by @id_hash@.
edgeBatchStatement :: Text -> [Aeson.Value] -> Aeson.Value
edgeBatchStatement rel rows =
  let backticked = "`" <> T.replace "`" "``" rel <> "`"
  in Aeson.object
       [ "statement" Aeson..= T.concat
           [ "UNWIND $rows AS row "
           , "MATCH (src:Node {id_hash: row.source_hash}) "
           , "MATCH (tgt:Node {id_hash: row.target_hash}) "
           , "MERGE (src)-[r:" <> backticked <> "]->(tgt) "
           , "ON CREATE SET r.weight = row.weight, r.confidence = row.confidence"
           ]
       , "parameters" Aeson..= Aeson.object ["rows" Aeson..= rows]
       ]

-- | Community UNWIND statement carrying one chunk of rows.
communityBatchStatement :: [Aeson.Value] -> Aeson.Value
communityBatchStatement rows = Aeson.object
  [ "statement" Aeson..= T.concat
      [ "UNWIND $rows AS row "
      , "MERGE (c:Community {id: row.id}) "
      , "ON CREATE SET c.label = row.label, c.size = row.size, c.cohesion = row.cohesion"
      ]
  , "parameters" Aeson..= Aeson.object ["rows" Aeson..= rows]
  ]

-- | Community-only UNWIND statement (adds top_members).
communityOnlyBatchStatement :: [Aeson.Value] -> Aeson.Value
communityOnlyBatchStatement rows = Aeson.object
  [ "statement" Aeson..= T.concat
      [ "UNWIND $rows AS row "
      , "MERGE (c:Community {id: row.id}) "
      , "ON CREATE SET c.label = row.label, c.size = row.size, c.cohesion = row.cohesion, "
      , "c.top_members = row.top_members"
      ]
  , "parameters" Aeson..= Aeson.object ["rows" Aeson..= rows]
  ]

-- | BELONGS_TO UNWIND statement carrying one chunk of rows.
belongsToBatchStatement :: [Aeson.Value] -> Aeson.Value
belongsToBatchStatement rows = Aeson.object
  [ "statement" Aeson..= T.concat
      [ "UNWIND $rows AS row "
      , "MATCH (n:Node {id_hash: row.node_hash}) "
      , "MATCH (c:Community {id: row.community_id}) "
      , "MERGE (n)-[:BELONGS_TO]->(c)"
      ]
  , "parameters" Aeson..= Aeson.object ["rows" Aeson..= rows]
  ]

-- ───────────────────────────────────────────────
-- Batch planner (pure) — D3
-- ───────────────────────────────────────────────

-- | Default node chunk bounds: 1,000 rows, or ~4 MB of row payload,
-- whichever comes first (node labels can carry full source text).
defaultNodeChunkRows :: Int
defaultNodeChunkRows = 1000

-- | ~4 MB payload cap for node chunks.
defaultNodeChunkBytes :: Int64
defaultNodeChunkBytes = 4 * 1024 * 1024

-- | Default edge chunk size in rows (edge rows are small and uniform).
defaultEdgeChunkRows :: Int
defaultEdgeChunkRows = 5000

-- | Split a list into chunks of at most @n@ elements.
chunkBy :: Int -> [a] -> [[a]]
chunkBy n xs = case splitAt n xs of
  ([], _)   -> []
  (c, rest) -> c : chunkBy n rest

-- | Row JSON-encoding length (approximates its wire payload size).
rowBytes :: Aeson.Value -> Int64
rowBytes = fromIntegral . BSL.length . Aeson.encode

-- | Split node rows into ordered chunks under row-count and payload-byte
-- caps, whichever comes first. A single row larger than the byte cap ships
-- alone (the transactional API handles multi-MB payloads; dropping the row
-- would silently corrupt the graph).
planNodeBatches :: Int -> Int64 -> [Aeson.Value] -> [[Aeson.Value]]
planNodeBatches maxRows maxBytes rows =
  reverse (map fst (foldl step [] rows))
  where
    -- Accumulator: finished-or-growing chunks (reversed internally), each
    -- paired with its accumulated byte size; newest chunk at the head.
    step :: [([Aeson.Value], Int64)] -> Aeson.Value -> [([Aeson.Value], Int64)]
    step acc row =
      let b = rowBytes row
      in case acc of
        ((chunk, chunkB) : rest)
          | length chunk < maxRows
          , chunkB + b <= maxBytes -> ((row : chunk, chunkB + b) : rest)
        _ -> (([row], b) : acc)

-- | Group edge rows by relationship type and chunk each group under the row
-- cap. Grouping by type is required because a relationship type cannot be a
-- parameter. The returned pairs iterate in ascending type-name order
-- (deterministic pushes).
planEdgeBatches :: Int -> [Edge] -> [(Text, [Aeson.Value])]
planEdgeBatches maxRows edges =
  [ (rel, chunk)
  | (rel, rowsRev) <- Map.toList groupedByType
  , chunk <- chunkBy maxRows (reverse rowsRev)
  ]
  where
    groupedByType :: Map.Map Text [Aeson.Value]
    groupedByType = Map.fromListWith (++)
      [ (relationToText (edgeRelation e), [edgeRow e])
      | e <- edges
      ]

-- ───────────────────────────────────────────────
-- Ordered statement assembly (pure)
-- ───────────────────────────────────────────────

-- | Node UNWIND statements for a graph, chunked under row\/byte caps.
nodeStatements :: Graph -> [Aeson.Value]
nodeStatements g =
  [ nodeBatchStatement chunk
  | chunk <- planNodeBatches defaultNodeChunkRows defaultNodeChunkBytes (graphNodeRows g)
  ]

-- | Edge UNWIND statements grouped by relationship type.
edgeStatements :: [Edge] -> [Aeson.Value]
edgeStatements edges =
  [ edgeBatchStatement rel chunk
  | (rel, chunk) <- planEdgeBatches defaultEdgeChunkRows edges
  ]

-- | Community + BELONGS_TO UNWIND statements (communities first).
communityDataStatements :: CommunityMap -> CohesionMap -> Map.Map CommunityId Text -> [Aeson.Value]
communityDataStatements commMap cohesionMap labels =
  [ communityBatchStatement chunk
  | chunk <- chunkBy defaultEdgeChunkRows
      [ communityRow cohesionMap labels cm | cm <- Map.toList commMap ]
  ]
  ++ [ belongsToBatchStatement chunk
     | chunk <- chunkBy defaultEdgeChunkRows
         [ belongsToRow nid cid | (cid, members) <- Map.toList commMap, nid <- members ]
     ]

-- ───────────────────────────────────────────────
-- Legacy statement generators (compat entry points, one Aeson.Value per
-- UNWIND batch statement)
-- ───────────────────────────────────────────────

-- | Generate parameterized UNWIND statements for nodes + edges.
--
-- Nodes are fully flushed before edges so edge endpoint MATCHes always
-- resolve. Historically one statement per entity; now one per entity kind
-- per chunk.
generateParameterizedStatements :: Graph -> [Aeson.Value]
generateParameterizedStatements g =
  nodeStatements g ++ edgeStatements (Map.elems (gEdges g))

-- | Generate UNWIND statements for communities + BELONGS_TO edges.
generateCommunityStatements :: Graph -> CommunityMap -> CohesionMap -> Map.Map CommunityId Text -> [Aeson.Value]
generateCommunityStatements _g commMap cohesionMap labels =
  communityDataStatements commMap cohesionMap labels

-- | Generate UNWIND statements for community-only push.
generateCommunityOnlyStatements :: Graph -> CommunityMap -> CohesionMap -> Map.Map CommunityId Text -> [Aeson.Value]
generateCommunityOnlyStatements g commMap cohesionMap labels =
  [ communityOnlyBatchStatement chunk
  | chunk <- chunkBy defaultEdgeChunkRows
      [ communityOnlyRow g cohesionMap labels cm | cm <- Map.toList commMap ]
  ]
  ++ generateCommunityEdgeStatements g commMap

-- | Generate UNWIND statements for subgraph push.
generateSubgraphStatements
  :: Graph
  -> CommunityMap
  -> CohesionMap
  -> Map.Map CommunityId Text
  -> Map.Map CommunityId [NodeId]   -- ^ representatives per community
  -> Set.Set NodeId                 -- ^ all representative\/bridge node IDs
  -> [Aeson.Value]
generateSubgraphStatements g commMap cohesionMap labels reps allRepNodeIds =
  [ communityBatchStatement chunk
  | chunk <- chunkBy defaultEdgeChunkRows
      [ communityRow cohesionMap labels cm | cm <- Map.toList commMap ]
  ]
  ++ [ nodeBatchStatement chunk
     | chunk <- planNodeBatches defaultNodeChunkRows defaultNodeChunkBytes
         [ representativeRow n
         | nid <- Set.toList allRepNodeIds
         , Just n <- [Map.lookup nid (gNodes g)]
         ]
     ]
  ++ [ belongsToBatchStatement chunk
     | chunk <- chunkBy defaultEdgeChunkRows
         [ belongsToRow nid cid | (cid, members) <- Map.toList reps, nid <- members ]
     ]
  ++ [ edgeBatchStatement rel chunk
     | (rel, rowsRev) <- Map.toList representativeEdgesGrouped
     , chunk <- chunkBy defaultEdgeChunkRows (reverse rowsRev)
     ]
  ++ generateCommunityEdgeStatements g commMap
  where
    repEdges = Map.elems (filterEdgesByNodeSet allRepNodeIds (gEdges g))
    representativeEdgesGrouped :: Map.Map Text [Aeson.Value]
    representativeEdgesGrouped = Map.fromListWith (++)
      [ (relationToText (edgeRelation e), [edgeRow e]) | e <- repEdges ]

-- | Generate parameterized UNWIND statements for a single file's extraction.
-- Pure — testable without Neo4j.
generateFileStatements :: Extraction -> [Aeson.Value]
generateFileStatements extraction =
  [ nodeBatchStatement chunk
  | chunk <- planNodeBatches defaultNodeChunkRows defaultNodeChunkBytes
      [ nodeRow n | n <- Map.elems (extNodes extraction) ]
  ]
  ++ edgeStatements (Map.elems (extEdges extraction))

-- | Generate edge-repair statements: one UNWIND per relationship type.
generateEdgeRepairStatements :: Graph -> [Aeson.Value]
generateEdgeRepairStatements g = edgeStatements (Map.elems (gEdges g))

-- | Generate CONNECTED_TO inter-community edge statements (kept in its
-- historical per-edge parameterized form — low volume).
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
             [ "source_id"    Aeson..= communityKey c1
             , "target_id"    Aeson..= communityKey c2
             , "edge_count"   Aeson..= count
             , "bridge_nodes" Aeson..= map (\nid -> maybe nid (toText . nodeLabel) (Map.lookup nid (gNodes g))) bridges
             ]
         ]
     | ((c1, c2), (count, bridges)) <- Map.toList edgeCounts
     ]

-- | Get the top N member node labels for a community (community-only push).
topMemberLabels :: Graph -> [NodeId] -> Int -> [Text]
topMemberLabels g members n =
  let sortedByDegree = sortOn (\nid -> negate (fromIntegral (Set.size (neighbors g nid)) :: Double)) members
   in take n [ toText (nodeLabel nd) | nid <- sortedByDegree, Just nd <- [Map.lookup nid (gNodes g)] ]

-- ───────────────────────────────────────────────
-- Cypher file export (pure IO)
-- ───────────────────────────────────────────────

-- | Generate Cypher statements and write to file (without communities).
-- Streams into a temp file which is renamed over the target only on success.
exportCypher :: Graph -> FilePath -> IO ()
exportCypher g path =
  withAtomicHandle path $ \h -> do
    mapM_ (\n -> hPutStrLn h (T.unpack (generateCypherNodeStatement n))) (Map.elems (gNodes g))
    mapM_ (\e -> hPutStrLn h (T.unpack (generateCypherEdgeStatement e))) (Map.elems (gEdges g))
    hFlush h

-- | Generate a MERGE statement for a single node (for .cypher file).
generateCypherNodeStatement :: Node -> Text
generateCypherNodeStatement n =
  let baseProps :: [Text]
      baseProps =
        [ "id: " <> cypherQuote (nodeId n)
         , "label: " <> cypherQuote (toText (nodeLabel n))
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
-- Push entry points
-- ───────────────────────────────────────────────

-- | Push parameterized statements in order, one statement per transaction.
-- Returns (message, statementCount, transactionCount). Short-circuits on the
-- first failed statement.
pushStatements :: Text -> Text -> Text -> [Aeson.Value] -> IO (Text, Int, Int)
pushStatements uri user password stmts = do
  (msg, txs) <- go 0 (0 :: Int)
  pure (msg, length stmts, txs)
  where
    go !sent !txs = case drop sent stmts of
      [] -> pure (T.pack ("Pushed " ++ show sent ++ " statement(s) in " ++ show txs ++ " transaction(s)"), txs)
      (stmt : _) -> do
        r <- sendStatements uri user password [stmt]
        case r of
          Left err -> pure (T.pack ("Push failed: " ++ T.unpack err), txs)
          Right () -> go (sent + 1) (txs + 1)

-- | Push with the schema phase first: backfill + constraints, then data.
-- Structural guarantee (task 3.3): no data statement is sent when the
-- schema phase fails — the data list is only consumed on @Right@.
pushWithSchema :: Text -> Text -> Text -> [Aeson.Value] -> IO (Text, Int, Int)
pushWithSchema uri user password stmts = do
  schema <- runSchemaSetup uri user password
  case schema of
    Left err -> pure (err, 0, 0)
    Right () -> pushStatements uri user password stmts

-- | Push graph to Neo4j (nodes + edges only, no communities).
pushToNeo4j :: Graph -> Text -> Text -> Text -> IO (Text, Int, Int)
pushToNeo4j g uri user password =
  pushWithSchema uri user password (generateParameterizedStatements g)

-- | Push graph + community structure to Neo4j.
--
-- Order: schema setup, community nodes + BELONGS_TO, graph nodes, edges.
-- Community labels are generated using TF-IDF scoring on member node labels.
-- Cohesion is computed as internal edge density per community.
pushToNeo4jWithCommunities :: Graph -> CommunityMap -> CohesionMap -> Text -> Text -> Text -> IO (Text, Int, Int)
pushToNeo4jWithCommunities g commMap cohesionMap uri user password = do
  let labels = suggestCommunityLabels g commMap
      stmts  = generateCommunityStatements g commMap cohesionMap labels
            ++ generateParameterizedStatements g
  pushWithSchema uri user password stmts

-- | Push communities + representative sub-graphs to Neo4j.
pushSubgraphToNeo4j :: Graph -> CommunityMap -> CohesionMap -> Int -> [NodeId] -> Text -> Text -> Text -> IO (Text, Int, Int)
pushSubgraphToNeo4j g commMap cohesionMap topN artPoints uri user password = do
  let labels = suggestCommunityLabels g commMap
      reps = selectRepresentatives g commMap topN artPoints
      allRepNodeIds = Set.fromList (concat (Map.elems reps))
      stmts = generateSubgraphStatements g commMap cohesionMap labels reps allRepNodeIds
  pushWithSchema uri user password stmts

-- | Push community-level graph to Neo4j (no individual nodes or edges).
pushCommunityGraphToNeo4j :: Graph -> CommunityMap -> CohesionMap -> Text -> Text -> Text -> IO (Text, Int, Int)
pushCommunityGraphToNeo4j g commMap cohesionMap uri user password = do
  let labels = suggestCommunityLabels g commMap
      stmts = generateCommunityOnlyStatements g commMap cohesionMap labels
  pushWithSchema uri user password stmts

-- ───────────────────────────────────────────────
-- Streaming node-by-node push (during extraction)
-- ───────────────────────────────────────────────

-- | Push a single file's extraction to Neo4j immediately.
--
-- The file's nodes and edges go out as parameterized UNWIND batches keyed on
-- @id_hash@, making this idempotent and safe for incremental\/streaming use.
--
-- Returns: (message, statementCount, batchCount)
pushFileExtraction :: Extraction -> Text -> Text -> Text -> IO (Text, Int, Int)
pushFileExtraction extraction uri user password =
  let stmts = generateFileStatements extraction
  in if null stmts
     then pure ("Skipped empty extraction", 0, 0)
     else pushWithSchema uri user password stmts

-- | Push edge-repair statements to Neo4j.
--
-- After all extractions complete, edges may reference nodes from other
-- files. This re-pushes all edges — endpoints matched by @id_hash@, MERGE
-- on the (endpoint pair, type) triple — so cross-file edges are connected.
--
-- Returns: (message, statementCount, batchCount)
pushEdgeRepair :: Graph -> Text -> Text -> Text -> IO (Text, Int, Int)
pushEdgeRepair g uri user password =
  pushWithSchema uri user password (generateEdgeRepairStatements g)