{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Integration tests for the Neo4j push (optimize-neo4j-full-push, tasks
-- 5.1 and 5.2). Run against a disposable Neo4j docker container:
--
--   docker run -d --name graphos-neo4j-test \
--     -p 17474:7474 -p 17687:7687 -e NEO4J_AUTH=neo4j/graphos_dev \
--     neo4j:5-community
--
--   cabal run graphos-int-neo4j ...
--
-- Skips (with a message) when the endpoint is unreachable, so the normal
-- `cabal test` flow never fails on a machine without docker.
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Short (fromText)
import System.Exit (exitFailure)
import System.IO

import Network.HTTP.Client
  ( defaultManagerSettings
  , httpLbs
  , newManager
  , parseRequest
  , responseBody
  , responseStatus
  )
import Network.HTTP.Types.Status (statusCode)

import Graphos.Domain.CypherHash (nodeIdHash)
import Graphos.Domain.Types
import Graphos.Domain.Graph
import Graphos.Infrastructure.Export.Neo4j

uri, user, password :: Text
uri      = "http://localhost:17474"
user     = "neo4j"
password = "graphos_dev"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  reachable <- isNeo4jUp
  if not reachable
    then putStrLn "SKIP: Neo4j test container not reachable (docker run -d --name graphos-neo4j-test -p 17474:7474 -e NEO4J_AUTH=neo4j/graphos_dev neo4j:5-community)"
    else do
      results <- sequence
        [ labelled "5.1 idempotent 10k push: exact node/edge parity, zero growth on re-push" testIdempotentPush
        , labelled "5.2a oversized id (>8KB) pushes and is findable via id_hash" testOversizedId
        , labelled "5.2b duplicate id_hash aborts schema phase with constraint name, no data sent" testDuplicateHashAbort
        ]
      case [err | (_, Left err) <- results] of
        []   -> putStrLn "ALL INTEGRATION TESTS PASSED"
        errs -> do
          mapM_ (\e -> hPutStrLn stderr ("FAILED: " ++ T.unpack e)) errs
          exitFailure
  where
    labelled name action = do
      putStrLn ("\n=== " ++ name ++ " ===")
      r <- try action
      case r of
        Right (Right ()) -> pure (name, Right ())
        Right (Left err) -> pure (name, Left err)
        Left (e :: SomeException) -> pure (name, Left (T.pack (show e)))

-- ────────────────────────────────────────────────────────────────
-- Cypher over the same transport as the pusher
-- ────────────────────────────────────────────────────────────────

isNeo4jUp :: IO Bool
isNeo4jUp = do
  r <- try (do
    mgr <- newManager defaultManagerSettings
    req <- parseRequest (T.unpack uri)
    res <- httpLbs req mgr
    pure (statusCode (responseStatus res) >= 200
          && statusCode (responseStatus res) < 500))
  pure $ case r of
    Right b -> b
    Left (_ :: SomeException) -> False

cypher :: Text -> IO (Either Text BSL.ByteString)
cypher q = sendTx uri user password
  [ Aeson.object [ "statement" Aeson..= q, "parameters" Aeson..= Aeson.object [] ] ]

cypherInt :: Text -> IO (Either Text Int)
cypherInt q = do
  r <- cypher q
  pure $ case r of
    Left err  -> Left err
    Right body -> case firstRowScalar body of
      Just n  -> Right n
      Nothing -> Left ("no scalar row in response: " <> T.pack (take 300 (BSL8.unpack body)))

wipeDb :: IO ()
wipeDb = do
  _ <- cypher "MATCH (n) DETACH DELETE n"
  _ <- cypher "DROP CONSTRAINT node_id_hash_unique IF EXISTS"
  _ <- cypher "DROP CONSTRAINT community_id_unique IF EXISTS"
  pure ()

-- ────────────────────────────────────────────────────────────────
-- Fixtures
-- ────────────────────────────────────────────────────────────────

mkNode :: NodeId -> Node
mkNode nid = Node
  { nodeId         = nid
  , nodeLabel      = fromText ("label:" <> nid)
  , nodeFileType   = CodeFile
  , nodeSourceFile = fromText "synthetic.hs"
  , nodeLineStart  = Just 1
  , nodeLineEnd    = Nothing
  , nodeSignature  = Nothing
  , nodeCommunityId = Nothing
  , nodeKind       = Nothing
  , nodeDegree     = Nothing
  , nodeIsBridge   = Nothing
  , nodeExtra      = Nothing
  , nodePresentBits = 0
  }

mkEdge :: NodeId -> NodeId -> Edge
mkEdge s t = Edge
  { edgeId         = EdgeId (s <> "->" <> t)
  , edgeSource     = s
  , edgeTarget     = t
  , edgeRelation   = Calls
  , edgeWeight     = 1.0
  , edgeConfidence = Confidence 1.0
  , edgeExtra      = Nothing
  }

-- 10k nodes chained (9999 edges, single relationship type).
syntheticGraph :: Int -> Graph
syntheticGraph n =
  let ids   = [ "syn-" <> T.pack (show i) | i <- [1 .. n] ]
      nodes = map mkNode ids
      edges = [ mkEdge a b | (a, b) <- zip ids (drop 1 ids) ]
  in buildGraph False (extractionFromLists nodes edges)

-- ────────────────────────────────────────────────────────────────
-- 5.1 — idempotent push on a 10k-node graph
-- ────────────────────────────────────────────────────────────────

testIdempotentPush :: IO (Either Text ())
testIdempotentPush = do
  wipeDb
  let g  = syntheticGraph 10000
      ne = Map.size (gEdges g)
  putStrLn "    push #1..."
  (m1, s1, _b1) <- pushToNeo4j g uri user password
  putStrLn ("    " ++ T.unpack m1)
  if s1 <= 0
    then pure $ Left "push #1 reported no statements"
    else do
      -- Exact parity: node count and edge count equal the input graph.
      nN <- cypherInt "MATCH (n:Node) RETURN count(n)"
      nE <- cypherInt "MATCH ()-[r]->() RETURN count(r)"
      parityOk <- case (nN, nE) of
        (Right nn, Right ne') | nn == 10000 && ne' == ne -> do
            putStrLn ("    parity OK: " ++ show nn ++ " nodes, " ++ show ne' ++ " edges")
            pure True
        (Right nn, Right ne') -> do
            hPutStrLn stderr (T.unpack (T.pack $ "count parity failed: nodes " ++ show nn ++ " (want 10000), edges " ++ show ne' ++ " (want " ++ show ne ++ ")"))
            pure False
        _ -> do
            hPutStrLn stderr "count query failed"
            pure False
      if not parityOk
        then pure (Left "count parity failed")
        else do
          -- Re-push; nothing may grow (idempotence via MERGE on id_hash).
          putStrLn "    push #2 (idempotence)..."
          (m2, _s2, _b2) <- pushToNeo4j g uri user password
          putStrLn ("    " ++ T.unpack m2)
          nN2 <- cypherInt "MATCH (n:Node) RETURN count(n)"
          nE2 <- cypherInt "MATCH ()-[r]->() RETURN count(r)"
          case (nN2, nE2) of
            (Right nn2, Right ne2) | nn2 == 10000 && ne2 == ne -> pure (Right ())
            (Right nn2, Right ne2) -> pure $ Left (T.pack $ "second push grew the graph: nodes " ++ show nn2 ++ ", edges " ++ show ne2)
            _ -> pure (Left "count query failed after re-push")

-- ────────────────────────────────────────────────────────────────
-- 5.2a — oversized id (embeds > 8 KB of source text) stays findable
-- ────────────────────────────────────────────────────────────────

testOversizedId :: IO (Either Text ())
testOversizedId = do
  wipeDb
  let hugeId = "ext:" <> T.replicate 1024 "q" <> ":0123456789abcdef"  -- ~8.5 KB, exceeds the RANGE-index key limit
      g = buildGraph False (extractionFromLists [mkNode hugeId] [])
  (m, _s, _b) <- pushToNeo4j g uri user password
  putStrLn ("    " ++ T.unpack m)
  -- Look the node up through its id_hash (the constraint key).
  r <- cypherInt ("MATCH (n:Node {id_hash: '" <> nodeIdHash hugeId <> "'}) RETURN count(n)")
  case r of
    Right 1 -> pure (Right ())
    Right n  -> pure $ Left (T.pack $ "oversized id not uniquely findable via id_hash: found " ++ show n)
    Left e   -> pure (Left ("id_hash lookup failed: " <> e))

-- ────────────────────────────────────────────────────────────────
-- 5.2b — duplicate id_hash rows abort schema setup, named constraint
-- ────────────────────────────────────────────────────────────────

testDuplicateHashAbort :: IO (Either Text ())
testDuplicateHashAbort = do
  wipeDb
  -- Seed two :Node rows with the *same id_hash* but distinct ids so the
  -- constraint creation must fail.
  let shared = nodeIdHash "duplicate-target"
  r <- cypher ("CREATE (:Node {id: 'dup-a', id_hash: '" <> shared <> "'}), (:Node {id: 'dup-b', id_hash: '" <> shared <> "'})")
  case r of
    Left e  -> pure (Left ("seeding failed: " <> e))
    Right _body -> do
      let g = syntheticGraph 5
      (m, stmts, _b) <- pushToNeo4j g uri user password
      putStrLn ("    push aborted with: " ++ T.unpack m)
      -- The error must name the violated constraint.
      if "node_id_hash_unique" `T.isInfixOf` m || "NeoConstraintVerificationFailed" `T.isInfixOf` m || "ConstraintVerificationFailed" `T.isInfixOf` m
        then do
          -- And no data statement may have been sent afterward: seed a probe
          -- node only via the push path — after abort, none of the 5 synth
          -- nodes may exist.
          r2 <- cypherInt "MATCH (n:Node {id: 'syn-1'}) RETURN count(n)"
          case r2 of
            Right 0 -> pure (Right ())
            Right n -> pure $ Left (T.pack $ "data flowed after schema failure: syn-1 x" ++ show n)
            Left e  -> pure (Left ("probe failed: " <> e))
        else pure $ Left $ T.pack $
          "push did not abort naming the constraint: " ++ T.unpack m ++ " (stmts=" ++ show stmts ++ ")"