{-# LANGUAGE OverloadedStrings #-}
-- | HTTP API for the query family.
--
-- Routes: /api/query, /api/path, /api/explain, /api/symbols, /api/neighbors,
-- /api/cypher/mutate (POST; explicit write surface, opencypher-write-mutations).
-- All responses: Content-Type application/json; charset=utf-8 + CORS header.
-- OPTIONS -> 200; unknown /api/* -> 404; reads are GET, the mutation route is POST.
module Graphos.Infrastructure.Server.QueryAPI
  ( apiApp
  , apiAppRef
  , SharedLoad(..)
  , startQueryServer
  ) where

import Network.Wai
import Network.Wai.Handler.Warp (runSettings, setPort, setHost, defaultSettings)
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types
  ( Status
  , status200
  , status400
  , status404
  , status405
  , hContentType
  , methodGet
  , methodOptions
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Aeson (object, (.=), (.:), (.:?), (.!=), eitherDecode, withObject, FromJSON(..))
import Data.IORef (IORef, readIORef, writeIORef)
import Graphos.Domain.Graph (gNodes)
import Graphos.Domain.Graph.Mutation (MutationSummary(..))
import Graphos.Domain.Query.Cypher.Parser (parseStatement)
import Graphos.Domain.Query.Cypher.AST (CypherStatement(..))
import Graphos.Domain.Query.Cypher.Eval (evaluateStatement, MutationResult(..), CypherResult(..))
import Graphos.Infrastructure.Export.PersistMutation (persistMutatedGraph)
import Graphos.UseCase.Load (LoadResult(..))
import Graphos.UseCase.Query
  ( queryGraphWithIndexScored
  , pathQueryWithIndex
  , explainNodeWithIndex
  , symbolLookup
  , neighborhoodExpansion
  )
import Graphos.UseCase.Query.Refine (defaultRefineConfig, refineResponse)
import Graphos.UseCase.Query.Render
  ( renderQueryResponseJSON
  , renderSymbolResultJSON
  , renderNeighborsResultJSON
  , renderPathResultJSON
  , renderExplainResultJSON
  , encodeText
  )

-- | Shared mutable load state. Pure-read deployments hold a constant;
-- mutations replace the in-memory graph for subsequent reads.
data SharedLoad
  = SharedLoad LoadResult
  | SharedLoadRef (IORef LoadResult) FilePath

readShared :: SharedLoad -> IO LoadResult
readShared (SharedLoad lr)   = pure lr
readShared (SharedLoadRef r _) = readIORef r

sharedGraphPath :: SharedLoad -> Maybe FilePath
sharedGraphPath (SharedLoad _)     = Nothing
sharedGraphPath (SharedLoadRef _ p) = Just p

replaceSharedGraph :: SharedLoad -> LoadResult -> IO ()
replaceSharedGraph (SharedLoadRef r _) lr' = writeIORef r lr'
replaceSharedGraph (SharedLoad _) _        = pure ()

-- | Application serving the query API from a pre-loaded LoadResult.
apiApp :: LoadResult -> Application
apiApp lr = apiAppRef (SharedLoad lr)

-- | Mutable view over the shared load state: mutations via
-- /api/cypher/mutate replace the in-memory graph for subsequent reads.
apiAppRef :: SharedLoad -> Application
apiAppRef shared req respond = do
  let method = requestMethod req
      path   = pathInfo req
  if method == methodOptions
    then respond $ corsResponse status200 ""
    else case path of
      ["api", "cypher", "mutate"] -> handleCypherMutate shared req respond
      _ | method /= methodGet -> respond $ corsResponse status405 "Method not allowed"
        | otherwise -> do
            lr <- readShared shared
            case path of
              ["api", "query"]    -> handleQuery lr req respond
              ["api", "path"]     -> handlePath lr req respond
              ["api", "explain"]  -> handleExplain lr req respond
              ["api", "symbols"]  -> handleSymbols lr req respond
              ["api", "neighbors"] -> handleNeighbors lr req respond
              _ -> respond $ corsResponse status404 "Not found"

-- | GET /api/query?q=<question>&mode=bfs|dfs&budget=<n>
handleQuery :: LoadResult -> Application
handleQuery lr req respond = do
  let params = queryString req
      q = paramValue "q" params
      mode = paramValueWithDefault "mode" "bfs" params
      budget = readIntParam 2000 "budget" params
      g = lrGraph lr
      idx = lrIndex lr
      resp = queryGraphWithIndexScored g idx q mode budget
      refined = refineResponse defaultRefineConfig (gNodes g) resp
      body = renderQueryResponseJSON refined
  respond $ jsonResponse body

-- | GET /api/path?from=<a>&to=<b>
handlePath :: LoadResult -> Application
handlePath lr req respond = do
  let params = queryString req
      from = paramValue "from" params
      to = paramValue "to" params
      g = lrGraph lr
      idx = lrIndex lr
      result = pathQueryWithIndex g idx from to
      body = renderPathResultJSON result
  respond $ jsonResponse body

-- | GET /api/explain?node=<id>
handleExplain :: LoadResult -> Application
handleExplain lr req respond = do
  let params = queryString req
      node = paramValue "node" params
      g = lrGraph lr
      idx = lrIndex lr
      result = explainNodeWithIndex g idx node
      body = renderExplainResultJSON result
  respond $ jsonResponse body

-- | GET /api/symbols?name=<n>
handleSymbols :: LoadResult -> Application
handleSymbols lr req respond = do
  let params = queryString req
      name = paramValue "name" params
      g = lrGraph lr
      idx = lrIndex lr
      result = symbolLookup name g idx
      body = renderSymbolResultJSON result
  respond $ jsonResponse body

-- | GET /api/neighbors?id=<id>&depth=<n>
handleNeighbors :: LoadResult -> Application
handleNeighbors lr req respond = do
  let params = queryString req
      nid = paramValue "id" params
      depth = readIntParam 2 "depth" params
      g = lrGraph lr
      idx = lrIndex lr
      result = neighborhoodExpansion nid depth g idx
      body = renderNeighborsResultJSON result
  respond $ jsonResponse body

-- | POST /api/cypher/mutate — body {"query": ..., "persist": false}.
-- Evaluates an openCypher statement against the shared in-memory graph;
-- mutations replace the graph for subsequent reads. With persist=true,
-- the mutated graph is written back to the loaded graph.json (with a
-- timestamped backup).
-- | Read the full request body (chunked).
fullBody :: Request -> IO BSL.ByteString
fullBody req = go []
  where
    go acc = do
      chunk <- getRequestBodyChunk req
      if BS.null chunk
        then pure (BSL.fromChunks (reverse acc))
        else go (chunk : acc)

handleCypherMutate :: SharedLoad -> Application
handleCypherMutate shared req respond = do
  body <- fullBody req
  case eitherDecode body :: Either String MutateBody of
    Left err -> respond $ jsonResponseWithStatus status400 (encodeText (object ["error" .= ("Invalid JSON body: " ++ err :: String)]))
    Right mb -> case parseStatement (mubQuery mb) of
      Left err -> respond $ jsonResponseWithStatus status400 (encodeText (object ["error" .= ("Cypher parse error: " <> err :: Text)]))
      Right st -> do
        lr <- readShared shared
        let g = lrGraph lr
            idx = lrIndex lr
        case evaluateStatement 2000 st g idx of
          Left err -> respond $ jsonResponseWithStatus status400 (encodeText (object ["error" .= err]))
          Right mr -> do
            let lr' = lr { lrGraph = mrGraph mr }
            replaceSharedGraph shared lr'
            backupMsg <- case (mubPersist mb, mMutationOf st, sharedGraphPath shared) of
              (True, Just _, Just path) -> do
                res <- persistMutatedGraph path (mcpLoadResultFor lr') (mrGraph mr)
                pure (either (Just . T.unpack) Just res)
              _ -> pure Nothing
            respond $ jsonResponse $ encodeText $ object $
              [ "summary"   .= object
                  [ "nodes_created"      .= msNodesCreated (mrSummary mr)
                  , "rels_created"       .= msRelsCreated (mrSummary mr)
                  , "rels_upserted"      .= msRelsUpserted (mrSummary mr)
                  , "properties_set"     .= msPropertiesSet (mrSummary mr)
                  , "properties_removed" .= msPropertiesRemoved (mrSummary mr)
                  , "nodes_deleted"      .= msNodesDeleted (mrSummary mr)
                  , "rels_deleted"       .= msRelsDeleted (mrSummary mr)
                  ]
              , "columns"   .= crColumns (mrResult mr)
              , "rows"      .= crRows (mrResult mr)
              , "truncated" .= crTruncated (mrResult mr)
              ] ++ maybe [] (\b -> ["backup" .= b]) backupMsg

-- | Decoded POST body for the mutation route.
data MutateBody = MutateBody { mubQuery :: Text, mubPersist :: Bool }

instance FromJSON MutateBody where
  parseJSON = withObject "MutateBody" $ \v -> MutateBody
    <$> v .:  "query"
    <*> v .:? "persist" .!= False

-- | Whether the statement mutates (Nothing = read-only).
mMutationOf :: CypherStatement -> Maybe ()
mMutationOf (MutStatement _) = Just ()
mMutationOf (ReadStatement _) = Nothing

-- | LoadResult for persistence carry-over.
mcpLoadResultFor :: LoadResult -> LoadResult
mcpLoadResultFor = id

-- | Start a query API server on the given port.
startQueryServer :: Int -> LoadResult -> IO ()
startQueryServer port lr = do
  let app = apiApp lr
      warpSettings = setPort port
                   $ setHost "0.0.0.0"
                   $ defaultSettings
  runSettings warpSettings app

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Get a required query parameter value (empty if missing).
paramValue :: BS.ByteString -> [(BS.ByteString, Maybe BS.ByteString)] -> Text
paramValue key params =
  case lookup key params of
    Just (Just v) -> T.pack (BSC.unpack v)
    _ -> T.empty

-- | Get a query parameter with a default value.
paramValueWithDefault :: BS.ByteString -> Text -> [(BS.ByteString, Maybe BS.ByteString)] -> Text
paramValueWithDefault key def params =
  case lookup key params of
    Just (Just v) -> T.pack (BSC.unpack v)
    _ -> def

-- | Read an integer query parameter with a default.
readIntParam :: Int -> BS.ByteString -> [(BS.ByteString, Maybe BS.ByteString)] -> Int
readIntParam def key params =
  case lookup key params of
    Just (Just v) -> case reads (BSC.unpack v) of
      [(n, _)] -> n
      _ -> def
    _ -> def

-- | Build a JSON response with CORS headers.
jsonResponse :: Text -> Response
jsonResponse = jsonResponseWithStatus status200

-- | Build a JSON response with an explicit status and CORS headers.
jsonResponseWithStatus :: Status -> Text -> Response
jsonResponseWithStatus status body =
  responseLBS status
    [ (hContentType, "application/json; charset=utf-8")
    , ("Access-Control-Allow-Origin", "*")
    ]
    (BSL.fromStrict (TE.encodeUtf8 body))

-- | Build a CORS response with a status and plain text body.
corsResponse :: Status -> BS.ByteString -> Response
corsResponse status body =
  responseLBS status
    [ (hContentType, "text/plain; charset=utf-8")
    , ("Access-Control-Allow-Origin", "*")
    ]
    (BSL.fromStrict body)