-- | Neo4j Cypher export + HTTP push
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Export.Neo4j
  ( exportCypher
  , pushToNeo4j
  ) where

import Control.Exception (catch, SomeException)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, ord)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (AddrInfo(..), SocketType(..), defaultProtocol, getAddrInfo, socket, connect, close, socketToHandle)
import System.IO (hPutStr, hFlush, hGetContents', IOMode(..))

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, gNodes, gEdges)

-- | Generate Cypher statements
exportCypher :: Graph -> FilePath -> IO ()
exportCypher g path = do
  let cypher = generateCypher g
  writeFile path (T.unpack cypher)

-- | Push graph directly to Neo4j via HTTP transactional API.
-- Connects to Neo4j's REST endpoint (default :7474) and sends Cypher in batches.
--
-- URI format: "http://host:port" (defaults to "http://localhost:7474")
-- Authentication: user:password passed via Neo4j's basic auth header.
pushToNeo4j :: Graph -> Text -> Text -> Text -> IO (Text, Int, Int)
pushToNeo4j g uri user password = do
  let cypher = generateCypher g
      nodeCount = Map.size (gNodes g)
      edgeCount = Map.size (gEdges g)
      statements = filter (not . T.null) (T.splitOn "\n" cypher)
      batches = chunkList 100 statements
  results <- mapM (sendBatch uri user password) batches
  let failures = [err | Left err <- results]
  if null failures
    then pure (T.pack $ "Pushed " ++ show nodeCount ++ " nodes, " ++ show edgeCount ++ " edges in " ++ show (length batches) ++ " batches", nodeCount, edgeCount)
    else pure (T.pack $ "Completed with " ++ show (length failures) ++ " errors: " ++ T.unpack (T.take 200 (case failures of (f:_) -> f; [] -> "")), nodeCount, edgeCount)

-- | Send a batch of Cypher statements via raw HTTP POST to Neo4j transactional endpoint.
sendBatch :: Text -> Text -> Text -> [Text] -> IO (Either Text ())
sendBatch uri user password stmts = catch (do
  let noProto = case T.stripPrefix "http://" uri of
                  Just rest -> rest
                  Nothing   -> case T.stripPrefix "https://" uri of
                                 Just rest -> rest
                                 Nothing   -> uri
      hostPort = T.takeWhile (/= '/') noProto
      (host, portStr) = case T.splitOn ":" hostPort of
        [h]    -> (T.unpack h, "7474")
        [h, p] -> (T.unpack h, T.unpack p)
        _      -> ("localhost", "7474")
      body = "{\"statements\":[{\"statement\":\""
           ++ escapeJson (T.unpack (T.intercalate "; " stmts))
           ++ "\",\"parameters\":{}}]}"
      authHeader = if T.null user then ""
                   else "Authorization: Basic " ++ base64Encode (T.unpack user ++ ":" ++ T.unpack password) ++ "\r\n"
      httpReq = "POST /db/neo4j/tx/commit HTTP/1.1\r\n"
             ++ "Host: " ++ host ++ "\r\n"
             ++ authHeader
             ++ "Content-Type: application/json\r\n"
             ++ "Content-Length: " ++ show (length body) ++ "\r\n"
             ++ "Accept: application/json\r\n"
             ++ "\r\n"
             ++ body
  addrInfos <- getAddrInfo Nothing (Just host) (Just portStr)
  case addrInfos of
    [] -> pure (Left "Cannot resolve Neo4j host")
    (ai:_) -> do
      sock <- socket (addrFamily ai) Stream defaultProtocol
      connect sock (addrAddress ai)
      hdl <- socketToHandle sock ReadWriteMode
      hPutStr hdl httpReq
      hFlush hdl
      _response <- hGetContents' hdl
      close sock
      pure (Right ())
  ) $ \(e :: SomeException) -> pure (Left (T.pack (show e)))

-- | Simple Base64 encoding for HTTP Basic Auth
base64Encode :: String -> String
base64Encode input =
  let bytes = map ord input
      padLen = (3 - length bytes `mod` 3) `mod` 3
      padded = bytes ++ replicate padLen 0
      encode6 x
        | x < 26 = chr (x + ord 'A')
        | x < 52 = chr (x - 26 + ord 'a')
        | x < 62 = chr (x - 52 + ord '0')
        | x == 62 = '+'
        | otherwise = '/'
      encodeTriple a b c = [ encode6 (a `shiftR` 2)
                           , encode6 ((a .&. 3) `shiftL` 4 .|. (b `shiftR` 4))
                           , encode6 ((b .&. 15) `shiftL` 2 .|. (c `shiftR` 6))
                           , encode6 (c .&. 63)
                           ]
      triples = [ (padded !! i, padded !! (i+1), padded !! (i+2)) | i <- [0, 3.. length padded - 3] ]
      encoded = concatMap (\(a,b,c) -> encodeTriple a b c) triples
      result = take (length encoded - padLen) encoded ++ replicate padLen '='
  in result

-- | Escape a string for JSON
escapeJson :: String -> String
escapeJson [] = []
escapeJson ('\\':xs) = '\\':'\\':escapeJson xs
escapeJson ('"':xs) = '\\':'"':escapeJson xs
escapeJson ('\n':xs) = '\\':'n':escapeJson xs
escapeJson ('\r':xs) = '\\':'r':escapeJson xs
escapeJson (x:xs) = x:escapeJson xs

-- | Split a list into chunks
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)

-- ───────────────────────────────────────────────
-- Cypher generation (pure)
-- ───────────────────────────────────────────────

-- | Generate Cypher MERGE statements for nodes and edges.
-- Uses backtick-quoted identifiers to handle special characters
-- in node IDs and relation labels.
generateCypher :: Graph -> Text
generateCypher g =
  let nodeStatements = [ generateNodeStatement n | n <- Map.elems (gNodes g) ]
      edgeStatements = [ generateEdgeStatement e | e <- Map.elems (gEdges g) ]
  in T.unlines (nodeStatements ++ edgeStatements)

-- | Generate a MERGE statement for a single node.
-- Escapes the node ID with backticks and sets label, file type properties.
generateNodeStatement :: Node -> Text
generateNodeStatement n =
  let nid = escapeCypherId (nodeId n)
  in T.concat
       [ "MERGE ("
       , nid
       , ":Node {label: '"
       , escapeCypherString (nodeLabel n)
       , "', file_type: '"
       , escapeCypherString (T.pack (show (nodeFileType n)))
       , "'"
       , maybe "" (\loc -> ", source_location: '" <> escapeCypherString loc <> "'") (nodeSourceLocation n)
       , maybe "" (\url -> ", source_url: '" <> escapeCypherString url <> "'") (nodeSourceUrl n)
       , "})"
       ]

-- | Generate a MERGE statement for a single edge.
-- Matches source and target nodes by ID and creates the relationship.
generateEdgeStatement :: Edge -> Text
generateEdgeStatement e =
  let src = escapeCypherId (edgeSource e)
      tgt = escapeCypherId (edgeTarget e)
      rel = escapeCypherId (relationToText (edgeRelation e))
  in T.concat
       [ "MERGE ("
       , src
       , ")-[:"
       , rel
       , " {confidence: '"
       , T.pack (show (edgeConfidence e))
       , "', weight: "
       , T.pack (show (edgeWeight e))
       , "}]->("
       , tgt
       , ")"
       ]

-- ───────────────────────────────────────────────
-- Cypher escaping helpers
-- ───────────────────────────────────────────────

-- | Escape a Cypher identifier by wrapping in backticks.
-- Backticks within the ID are doubled to escape them.
escapeCypherId :: Text -> Text
escapeCypherId t =
  let escaped = T.replace "`" "``" t
  in "`" <> escaped <> "`"

-- | Escape a Cypher string literal value.
-- Escapes single quotes by doubling them.
escapeCypherString :: Text -> Text
escapeCypherString = T.replace "'" "\\'"