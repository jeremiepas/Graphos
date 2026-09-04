{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.StartupValidationSpec (spec) where

import Data.Aeson (Value(..), eitherDecode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.ByteString.Lazy as BL
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import Test.Hspec

import Graphos.UseCase.Load
  (validateGraphFile, validateMinimalShape, corruptGraphMessage)

spec :: Spec
spec = do
  describe "validateGraphFile" $ do
    it "passes when the graph file does not exist" $ do
      res <- validateGraphFile "/nonexistent/graphos-out/graph.json"
      res `shouldBe` Right ()

    it "passes a valid graph" $ do
      withTempGraph validGraph $ \path -> do
        res <- validateGraphFile path
        res `shouldBe` Right ()

    it "reports a non-JSON file as invalid" $ do
      withTempText "this is not json" $ \path -> do
        res <- validateGraphFile path
        case res of
          Left e -> T.isInfixOf "failed to parse" e `shouldBe` True
          Right _ -> fail "expected failure"

    it "reports a JSON array (not an object) as invalid" $ do
      withTempText "[1,2,3]" $ \path -> do
        res <- validateGraphFile path
        case res of
          Left e -> T.isInfixOf "must be a JSON object" e `shouldBe` True
          Right _ -> fail "expected failure"

    it "reports a graph missing the edges array" $ do
      withTempGraph nodesOnlyGraph $ \path -> do
        res <- validateGraphFile path
        case res of
          Left e -> T.isInfixOf "edges" e `shouldBe` True
          Right _ -> fail "expected failure"

    it "reports a graph missing the nodes array" $ do
      withTempGraph edgesOnlyGraph $ \path -> do
        res <- validateGraphFile path
        case res of
          Left e -> T.isInfixOf "nodes" e `shouldBe` True
          Right _ -> fail "expected failure"

  describe "validateMinimalShape" $ do
    it "accepts an object with nodes and edges" $ do
      validateMinimalShape (decodeValue validGraph) `shouldBe` Right ()

    it "rejects a non-object value" $ do
      case validateMinimalShape (String "nope") of
        Left e -> T.isInfixOf "must be a JSON object" e `shouldBe` True
        Right _ -> fail "expected failure"

  describe "corruptGraphMessage" $ do
    it "includes the path and a recovery hint" $ do
      let msg = corruptGraphMessage "/out/graph.json" "boom"
      T.isInfixOf "/out/graph.json" msg `shouldBe` True
      T.isInfixOf "Recovery" msg `shouldBe` True

    it "composes the full startup error from a validation failure" $ do
      withTempText "not json" $ \path -> do
        err <- validateGraphFile path
        case err of
          Left e -> T.isInfixOf "Recovery" (corruptGraphMessage path e) `shouldBe` True
          Right _ -> fail "expected failure"

decodeValue :: Text -> Value
decodeValue t = case eitherDecode (BL.fromStrict (T.encodeUtf8 t)) of
  Left e -> error ("test fixture not valid JSON: " ++ e)
  Right v -> v

withTempGraph :: Text -> (FilePath -> IO a) -> IO a
withTempGraph json action = withSystemTempDirectory "graphos-validation-spec" $ \dir -> do
  let path = dir </> "graph.json"
  writeFile path (T.unpack json)
  action path

withTempText :: String -> (FilePath -> IO a) -> IO a
withTempText content action = withSystemTempDirectory "graphos-validation-spec" $ \dir -> do
  let path = dir </> "graph.json"
  writeFile path content
  action path

validNodes :: Text
validNodes =
  "[{\"id\":\"a\",\"label\":\"A\",\"file_type\":\"code\",\"source_file\":\"a.hs\"}"
  <> ",{\"id\":\"b\",\"label\":\"B\",\"file_type\":\"code\",\"source_file\":\"b.hs\"}]"

validEdges :: Text
validEdges =
  "[{\"id\":\"e1\",\"source\":\"a\",\"target\":\"b\",\"relation\":\"calls\",\"weight\":1.0,\"confidence\":0.9}]"

validGraph :: Text
validGraph = "{\"nodes\":" <> validNodes <> ",\"edges\":" <> validEdges <> "}"

nodesOnlyGraph :: Text
nodesOnlyGraph = "{\"nodes\":" <> validNodes <> "}"

edgesOnlyGraph :: Text
edgesOnlyGraph = "{\"edges\":" <> validEdges <> "}"
