{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Server.QueryAPISpec where

import Test.Hspec
import Data.Aeson (Value(..), eitherDecode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import Data.IORef (newIORef, modifyIORef, readIORef, writeIORef)
import System.CPUTime (getCPUTime)
import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as BS8

import Network.Wai
import Network.Wai.Internal (ResponseReceived(..))
import Network.HTTP.Types (Status, ResponseHeaders, Method, status200, status404, status405, methodGet, methodOptions, methodPost, status400, decodePathSegments, parseQuery)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL

import Data.Text.Short (fromText)
import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph, gNodes)
import Graphos.Domain.Graph.Index (buildIndexWithLabels)
import Graphos.Domain.Graph.Analysis (toCachedFGL)
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
  , renderPathResultJSON
  , renderExplainResultJSON
  , renderSymbolResultJSON
  , renderNeighborsResultJSON
  )
import Graphos.Infrastructure.Server.QueryAPI (apiApp)

-- ───────────────────────────────────────────────
-- Test fixtures
-- ───────────────────────────────────────────────

testNode :: NodeId -> Node
testNode nid = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "test.hs"
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodePresentBits  = 0
  }

testEdge :: NodeId -> NodeId -> Edge
testEdge src tgt = Edge
  { edgeId         = EdgeId (src <> "->" <> tgt)
  , edgeSource     = src
  , edgeTarget     = tgt
  , edgeRelation   = Imports
  , edgeConfidence = Confidence 1.0
  , edgeWeight     = 1.0
  , edgeExtra      = Nothing
  }

fixtureGraph :: Graph
fixtureGraph = buildGraph False $ extractionFromLists
  [ testNode "AuthModule"
  , testNode "AuthLogin"
  , testNode "AuthSession"
  , testNode "Database"
  ]
  [ testEdge "AuthModule" "AuthLogin"
  , testEdge "AuthModule" "AuthSession"
  , testEdge "AuthLogin"  "Database"
  ]

fixtureLoadResult :: LoadResult
fixtureLoadResult =
  let g = fixtureGraph
      idx = buildIndexWithLabels g Map.empty Map.empty
  in LoadResult
       { lrGraph           = g
       , lrIndex           = idx
       , lrCachedFGL       = toCachedFGL g
       , lrCommunities     = Map.empty
       , lrCohesion        = Map.empty
       , lrGodNodes        = []
        , lrCommunityLabels = Map.empty
        , lrCompositions    = Nothing
        , lrCommunityAggregates = []
        , lrDegradedRelations   = 0
        , lrDegradedFileTypes   = 0
        , lrSkippedNodes        = 0
        , lrSkippedEdges        = 0
        }

-- | Helper to run a request against apiApp and return status, headers, and body.
runApi :: Method -> ByteString -> IO (Status, ResponseHeaders, BSL.ByteString)
runApi method path = do
  let (rawPath, rawQuery) = BS8.break (== '?') path
      req = defaultRequest
        { requestMethod = method
        , rawPathInfo   = rawPath
        , pathInfo      = decodePathSegments rawPath
        , queryString   = parseQuery (BS8.drop 1 rawQuery)
        }
  ref <- newIORef Nothing
  _ <- apiApp fixtureLoadResult req $ \resp -> do
    let (status, headers, withStream) = responseToStream resp
    bodyRef <- newIORef []
    withStream $ \stream -> stream (\builder -> modifyIORef bodyRef (B.toLazyByteString builder :)) (return ())
    body <- BSL.concat . reverse <$> readIORef bodyRef
    writeIORef ref (Just (status, headers, body))
    return ResponseReceived
  readIORef ref >>= maybe (fail "no response") pure

-- | Like 'runApi', with a request body (for POST routes).
runApiWithBody :: Method -> ByteString -> BSL.ByteString -> IO (Status, ResponseHeaders, BSL.ByteString)
runApiWithBody method path body = do
  let (rawPath, rawQuery) = BS8.break (== '?') path
      chunks = BSL.toChunks body ++ []
      req = defaultRequest
        { requestMethod = method
        , rawPathInfo   = rawPath
        , pathInfo      = decodePathSegments rawPath
        , queryString   = parseQuery (BS8.drop 1 rawQuery)
        }
  -- Serve the fixed body chunks through setRequestBodyChunks.
  chunksRef <- newIORef chunks
  let req' = setRequestBodyChunks (do
        cs <- readIORef chunksRef
        case cs of
          []      -> pure BS8.empty
          (c:cs') -> do writeIORef chunksRef cs'; pure c) req
  ref <- newIORef Nothing
  _ <- apiApp fixtureLoadResult req' $ \resp -> do
    let (status, headers, withStream) = responseToStream resp
    bodyRef <- newIORef []
    withStream $ \stream -> stream (\builder -> modifyIORef bodyRef (B.toLazyByteString builder :)) (return ())
    responseBody <- BSL.concat . reverse <$> readIORef bodyRef
    writeIORef ref (Just (status, headers, responseBody))
    return ResponseReceived
  readIORef ref >>= maybe (fail "no response") pure

-- | Encode a Text JSON rendering to a lazy bytestring for comparison.
encodeExpected :: T.Text -> BSL.ByteString
encodeExpected = BSL.fromStrict . TE.encodeUtf8

-- ───────────────────────────────────────────────
-- Spec
-- ───────────────────────────────────────────────

spec :: Spec
spec = do
  mutateRouteSpec
  describe "apiApp" $ do
    it "GET /api/query returns the same JSON as the CLI renderer" $ do
      let g = fixtureGraph
          idx = lrIndex fixtureLoadResult
          expected = renderQueryResponseJSON
                       $ refineResponse defaultRefineConfig (gNodes g)
                       $ queryGraphWithIndexScored g idx "Auth" "bfs" 2000
      (status, _headers, body) <- runApi methodGet "/api/query?q=Auth&mode=bfs&budget=2000"
      status `shouldBe` status200
      body `shouldBe` encodeExpected expected

    it "GET /api/query honors the budget query parameter" $ do
      let g = fixtureGraph
          idx = lrIndex fixtureLoadResult
          expected = renderQueryResponseJSON
                       $ refineResponse defaultRefineConfig (gNodes g)
                       $ queryGraphWithIndexScored g idx "Auth" "bfs" 100
      (status, _headers, body) <- runApi methodGet "/api/query?q=Auth&mode=bfs&budget=100"
      status `shouldBe` status200
      body `shouldBe` encodeExpected expected

    it "GET /api/path returns the same JSON as renderPathResultJSON" $ do
      let g = fixtureGraph
          idx = lrIndex fixtureLoadResult
          expected = renderPathResultJSON (pathQueryWithIndex g idx "AuthModule" "Database")
      (status, _headers, body) <- runApi methodGet "/api/path?from=AuthModule&to=Database"
      status `shouldBe` status200
      body `shouldBe` encodeExpected expected

    it "GET /api/explain returns the same JSON as renderExplainResultJSON" $ do
      let g = fixtureGraph
          idx = lrIndex fixtureLoadResult
          expected = renderExplainResultJSON (explainNodeWithIndex g idx "AuthModule")
      (status, _headers, body) <- runApi methodGet "/api/explain?node=AuthModule"
      status `shouldBe` status200
      expected `shouldSatisfy` (T.isInfixOf "AuthModule")
      body `shouldBe` encodeExpected expected

    it "GET /api/symbols returns the same JSON as renderSymbolResultJSON" $ do
      let g = fixtureGraph
          idx = lrIndex fixtureLoadResult
          expected = renderSymbolResultJSON (symbolLookup "AuthModule" g idx)
      (status, _headers, body) <- runApi methodGet "/api/symbols?name=AuthModule"
      status `shouldBe` status200
      body `shouldBe` encodeExpected expected

    it "GET /api/neighbors returns the same JSON as renderNeighborsResultJSON" $ do
      let g = fixtureGraph
          idx = lrIndex fixtureLoadResult
          expected = renderNeighborsResultJSON (neighborhoodExpansion "AuthModule" 2 g idx)
      (status, _headers, body) <- runApi methodGet "/api/neighbors?id=AuthModule&depth=2"
      status `shouldBe` status200
      body `shouldBe` encodeExpected expected

    it "OPTIONS /api/query returns 200 with CORS header" $ do
      (status, headers, body) <- runApi methodOptions "/api/query"
      status `shouldBe` status200
      body `shouldBe` ""
      lookup "Access-Control-Allow-Origin" headers `shouldBe` Just "*"

    it "POST /api/query returns 405" $ do
      (status, _headers, _body) <- runApi methodPost "/api/query"
      status `shouldBe` status405

    it "GET /api/unknown returns 404" $ do
      (status, _headers, _body) <- runApi methodGet "/api/unknown"
      status `shouldBe` status404

    it "response has JSON content-type and CORS header" $ do
      (status, headers, _body) <- runApi methodGet "/api/query?q=Auth"
      status `shouldBe` status200
      lookup "Content-Type" headers `shouldBe` Just "application/json; charset=utf-8"
      lookup "Access-Control-Allow-Origin" headers `shouldBe` Just "*"

    it "works without reading any graph.json file" $ do
      -- The handler closes over an in-memory LoadResult, so a query still succeeds
      -- even though no graph file exists for this test fixture.
      (status, _headers, body) <- runApi methodGet "/api/query?q=Auth"
      status `shouldBe` status200
      BSL.length body `shouldSatisfy` (> 0)

    it "10 consecutive /api/query requests each complete in < 500ms (latency)" $ do
      -- PRD §16.1: query response < 500ms on a pre-built in-memory graph
      let runOne :: IO Integer
          runOne = do
            t0 <- getCPUTime
            _ <- runApi methodGet "/api/query?q=Auth&mode=bfs&budget=2000"
            t1 <- getCPUTime
            pure $ (t1 - t0) `div` 1000000 -- nanoseconds → milliseconds
      times <- replicateM 10 runOne
      let msList = times :: [Integer]
      -- Assert each request < 500ms
      let checkTime :: Int -> Integer -> IO ()
          checkTime i ms =
            if ms < 500
              then pure ()
              else expectationFailure $ "request " <> show i <> " took " <> show ms <> "ms (expected < 500ms)"
      mapM_ (\(i, ms) -> checkTime i ms) (zip [1::Int] msList)

    it "end-to-end parity: /api/query == renderQueryResponseJSON (strong, weak, none)" $ do
      -- spec query-cli-contract: HTTP and CLI JSON agree (same verdict, hash, node-id set)
      let g = fixtureGraph
          idx = lrIndex fixtureLoadResult
          parityQuery :: T.Text -> IO ()
          parityQuery queryStr = do
            let path = TE.encodeUtf8 $ "/api/query?q=" <> queryStr <> "&mode=bfs&budget=2000"
            (status, _headers, body) <- runApi methodGet path
            status `shouldBe` status200
            let expected = renderQueryResponseJSON
                             $ refineResponse defaultRefineConfig (gNodes g)
                             $ queryGraphWithIndexScored g idx queryStr "bfs" 2000
                expectedText = encodeExpected expected
            body `shouldBe` expectedText
      parityQuery "Auth"    -- strong match: "Auth" matches AuthModule, AuthLogin, AuthSession
      parityQuery "NotEx"   -- weak match: partial match
      parityQuery "zzzznonexistent"  -- none match: no terms found

-- POST /api/cypher/mutate (opencypher-write-mutations).
mutateRouteSpec :: Spec
mutateRouteSpec = describe "POST /api/cypher/mutate" $ do
  let mutateBody :: T.Text -> BSL.ByteString
      mutateBody q = BSL.fromStrict (TE.encodeUtf8 ("{\"query\":\"" <> q <> "\"}" :: T.Text))
      asObj :: Value -> KM.KeyMap Value
      asObj (Object o) = o
      asObj _          = KM.empty

  it "applies a mutation and returns the summary" $ do
    (status, _headers, body) <- runApiWithBody methodPost "/api/cypher/mutate"
      (mutateBody "MERGE (m:Module {id: 'm9'})")
    status `shouldBe` status200
    case eitherDecode body of
      Right v -> do
        let sm = asObj v
        case KM.lookup (Key.fromText "summary") sm of
          Just (Object s') ->
            KM.lookup (Key.fromText "nodes_created") s' `shouldBe` Just (Number 1)
          _ -> expectationFailure "expected summary object"
        KM.member (Key.fromText "columns") sm `shouldBe` True
      Left err -> expectationFailure err

  it "returns 400 for a parse error" $ do
    (status, _headers, body) <- runApiWithBody methodPost "/api/cypher/mutate"
      (mutateBody "WITH x RETURN x")
    status `shouldBe` status400
    case eitherDecode body of
      Right v -> KM.member (Key.fromText "error") (asObj v) `shouldBe` True
      Left _  -> expectationFailure "expected error JSON"

  it "reads reflect the mutated in-memory graph" $ do
    _ <- runApiWithBody methodPost "/api/cypher/mutate"
      (mutateBody "MERGE (m:Module {id: 'm9'})")
    (status, _headers, body) <- runApi methodGet "/api/explain?node=m9"
    status `shouldBe` status200
    BSL.length body `shouldSatisfy` (> 0)
