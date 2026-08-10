{-# LANGUAGE OverloadedStrings #-}
-- | HTTP API for the query family.
--
-- Routes: /api/query, /api/path, /api/explain, /api/symbols, /api/neighbors
-- All responses: Content-Type application/json; charset=utf-8 + CORS header.
-- OPTIONS -> 200; non-GET (except OPTIONS) -> 405; unknown /api/* -> 404.
module Graphos.Infrastructure.Server.QueryAPI
  ( apiApp
  , startQueryServer
  ) where

import Network.Wai
import Network.Wai.Handler.Warp (runSettings, setPort, setHost, defaultSettings)
import Network.HTTP.Types
  ( Status
  , status200
  , status404
  , status405
  , hContentType
  , methodGet
  , methodOptions
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Graphos.Domain.Graph (gNodes)
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
  )

-- | Application serving the query API from a pre-loaded LoadResult.
apiApp :: LoadResult -> Application
apiApp lr req respond = do
  let method = requestMethod req
      path   = pathInfo req
  if method == methodOptions
    then respond $ corsResponse status200 ""
    else if method /= methodGet
      then respond $ corsResponse status405 "Method not allowed"
      else case path of
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
      g = lrGraph lr
      idx = lrIndex lr
      resp = queryGraphWithIndexScored g idx q mode 2000
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
jsonResponse body =
  responseLBS status200
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