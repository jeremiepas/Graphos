{-# LANGUAGE OverloadedStrings #-}
-- | Benchmark: FullPush of a synthetic 100k-node graph against localhost
-- Neo4j (optimize-neo4j-full-push, task 5.3).
--
--   docker run -d --name graphos-neo4j-test \
--     -p 17474:7474 -p 17687:7687 -e NEO4J_AUTH=<user>/<password> \
--     neo4j:5-community
--
-- Requires < 5 min (spec), ~1 min expected.
module Main (main) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (lookupEnv)
import Prelude

import Graphos.Domain.Types
import Graphos.Domain.Graph
import Graphos.Infrastructure.Export.Neo4j

uri, user :: Text
uri      = "http://localhost:17474"
user     = "neo4j"

-- Neo4j test password: sourced from the environment so no plaintext credential
-- is committed. Falls back to a non-secret placeholder that authenticates
-- against nothing real. Set GRAPHOS_NEO4J_TEST_PASSWORD to match your container's
-- NEO4J_AUTH (the password portion of username/password).
password :: IO Text
password = do
  fromEnv <- lookupEnv "GRAPHOS_NEO4J_TEST_PASSWORD"
  pure $ case fromEnv of
    Just p | not (null p) -> T.pack p
    _                      -> "graphos_dev-dev"

nodesN :: Int
nodesN = 100000

main :: IO ()
main = do
  pw <- password
  up <- isNeo4jUp pw
  if not up
    then putStrLn "SKIP: Neo4j not reachable"
    else do
      _ <- sendTx uri user pw
        [ Aeson.object [ "statement" Aeson..= ("MATCH (n) DETACH DELETE n" :: Text)
                       , "parameters" Aeson..= Aeson.object [] ] ]
      let ids   = [ "bench-" <> T.pack (show i) | i <- [1 .. nodesN] ]
          nodes = [ benchNode (T.pack (show i)) | i <- [(1 :: Int) .. nodesN] ]
      -- Simple chain + forward-2 chain: 100k nodes, ~110k edges
      let edges' = [ benchEdge a b
                   | (a, b) <- zip ids (drop 1 ids) ]
                ++ [ benchEdge a b
                   | (a, b) <- zip (take (nodesN - 2) ids) (drop 2 ids) ]
          g = buildGraph False (extractionFromLists nodes edges')
      putStrLn ("graph: " ++ show (Map.size (gNodes g)) ++ " nodes, "
                ++ show (Map.size (gEdges g)) ++ " edges")
      t0 <- getCurrentTime
      r <- try (pushToNeo4jWithCommunities g Map.empty Map.empty uri user pw)
      t1 <- getCurrentTime
      case r of
        Right (msg, _s, _b) -> do
          let dt = diffUTCTime t1 t0
              secs = realToFrac dt :: Double
          putStrLn ("push: " ++ T.unpack msg)
          putStrLn ("TIME: " ++ show secs ++ " s")
          putStrLn (if secs < 300 then "BENCHMARK OK (< 5 min)" else "BENCHMARK FAIL (>= 5 min)")
        Left (e :: SomeException) -> putStrLn ("ERROR: " ++ show e)
  where
    benchNode i = Node
      { nodeId         = "bench-" <> i
      , nodeLabel      = fromText ("bench node " <> i)
      , nodeFileType   = CodeFile
      , nodeSource     = Nothing
      , nodeSourceFile = fromText ("src/File" <> T.takeEnd 2 i <> ".hs")
      , nodeLineStart  = Just 1
      , nodeLineEnd    = Nothing
      , nodeSignature  = Nothing
      , nodeCommunityId = Nothing
      , nodeKind       = Nothing
      , nodeDegree     = Nothing
      , nodeIsBridge   = Nothing
      , nodeExtra      = Nothing
      , nodePresentBits = 1
      }
    benchEdge s t = Edge
      { edgeId         = EdgeId (s <> "->" <> t)
      , edgeSource     = s
      , edgeTarget     = t
      , edgeRelation   = Calls
      , edgeWeight     = 1.0
      , edgeConfidence = Confidence 1.0
      , edgeExtra      = Nothing
      }
    isNeo4jUp :: Text -> IO Bool
    isNeo4jUp pw = do
      r <- try (probe pw) :: IO (Either SomeException (Either Text BSL.ByteString))
      pure $ case r of
        Right (Right _) -> True
        _               -> False
    probe :: Text -> IO (Either Text BSL.ByteString)
    probe pw = sendTx uri user pw
      [ Aeson.object [ "statement" Aeson..= ("RETURN 1" :: Text)
                     , "parameters" Aeson..= Aeson.object [] ] ]