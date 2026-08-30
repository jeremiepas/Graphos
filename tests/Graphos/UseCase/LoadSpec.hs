{-# LANGUAGE OverloadedStrings #-}
module Graphos.UseCase.LoadSpec (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Short (fromText)
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph (gNodes, gEdges, gEmbeddings, gEmbeddingsPath)
import Graphos.UseCase.Load (LoadResult(..), loadGraphFromFile, loadGraphFromFileStrict)
import qualified Graphos.Infrastructure.Export.IncrementalJSON as Inc

spec :: Spec
spec = do
  describe "schema version" $ do
    it "loads a legacy graph without schema_version" $ do
      res <- withTempGraph legacyGraph loadGraphFromFile
      case res of
        Left e -> fail $ "expected success, got: " <> T.unpack e
        Right lr -> do
          nodeCount lr `shouldBe` 2
          edgeCount lr `shouldBe` 1
          lrDegradedRelations lr `shouldBe` 0
          lrDegradedFileTypes lr `shouldBe` 0
          lrSkippedNodes lr `shouldBe` 0
          lrSkippedEdges lr `shouldBe` 0
    it "loads a graph declaring schema_version 1" $ do
      let json = "{\"schema_version\":\"1\",\"nodes\":" <> baseNodes <> ",\"edges\":" <> baseEdges <> "}"
      res <- withTempGraph json loadGraphFromFile
      case res of
        Left e -> fail $ "expected success, got: " <> T.unpack e
        Right lr -> nodeCount lr `shouldBe` 2
    it "refuses an unsupported major version with an actionable error" $ do
      let json = "{\"schema_version\":\"9\",\"nodes\":" <> baseNodes <> ",\"edges\":" <> baseEdges <> "}"
      res <- withTempGraph json loadGraphFromFile
      case res of
        Right _ -> fail "expected failure"
        Left e -> do
          T.isInfixOf "9" e `shouldBe` True
          T.isInfixOf "schema_version" e `shouldBe` True
          T.isInfixOf "Supported major versions" e `shouldBe` True

  describe "tolerant enum degradation" $ do
    it "degrades an unknown relation to inferred and counts it" $ do
      res <- withTempGraph unknownRelGraph loadGraphFromFile
      case res of
        Left e -> fail $ "expected success, got: " <> T.unpack e
        Right lr -> do
          lrDegradedRelations lr `shouldBe` 1
          edgeCount lr `shouldBe` 1
          case Map.elems (gEdges (lrGraph lr)) of
            (e : _) -> edgeRelation e `shouldBe` Inferred
            [] -> fail "expected at least one edge"
    it "degrades an unknown file_type to code and counts it" $ do
      res <- withTempGraph unknownFtGraph loadGraphFromFile
      case res of
        Left e -> fail $ "expected success, got: " <> T.unpack e
        Right lr -> do
          lrDegradedFileTypes lr `shouldBe` 1
          let a = Map.lookup "a" (gNodes (lrGraph lr))
          nodeFileType (fromJust a) `shouldBe` CodeFile

  describe "strict mode" $ do
    it "fails on an unknown relation, naming value and edge id" $ do
      res <- withTempGraph unknownRelGraph loadGraphFromFileStrict
      case res of
        Right _ -> fail "expected failure"
        Left e -> do
          T.isInfixOf "re_exports" e `shouldBe` True
          T.isInfixOf "e1" e `shouldBe` True
    it "fails on an unknown file_type, naming value and node id" $ do
      res <- withTempGraph unknownFtGraph loadGraphFromFileStrict
      case res of
        Right _ -> fail "expected failure"
        Left e -> do
          T.isInfixOf "other" e `shouldBe` True
          T.isInfixOf "\"a\"" e `shouldBe` True

  describe "optional fields and sections" $ do
    it "loads a node whose source_file is null" $ do
      res <- withTempGraph nullSrcGraph loadGraphFromFile
      case res of
        Left e -> fail $ "expected success, got: " <> T.unpack e
        Right lr -> do
          let a = Map.lookup "a" (gNodes (lrGraph lr))
          nodeSourceFile (fromJust a) `shouldBe` ""
    it "loads an un-clustered graph with empty community data" $ do
      res <- withTempGraph legacyGraph loadGraphFromFile
      case res of
        Left e -> fail $ "expected success, got: " <> T.unpack e
        Right lr -> do
          lrCommunities lr `shouldBe` Map.empty
          lrCohesion lr `shouldBe` Map.empty
          lrGodNodes lr `shouldBe` []
    it "skips malformed nodes and counts them" $ do
      let goodNodes = T.intercalate ","
            [ "{\"id\":\"n" <> T.pack (show i) <> "\",\"label\":\"N" <> T.pack (show i)
              <> "\",\"file_type\":\"code\",\"source_file\":\"n" <> T.pack (show i) <> ".hs\"}"
            | i <- ([1..98] :: [Int]) ]
          bad1 = "{\"label\":\"no id\",\"file_type\":\"code\"}"
          bad2 = "{\"id\":\"bad\",\"label\":\"Bad\"}"
          json = "{\"nodes\":[" <> goodNodes <> "," <> bad1 <> "," <> bad2 <> "],\"edges\":[]}"
      res <- withTempGraph json loadGraphFromFile
      case res of
        Left e -> fail $ "expected success, got: " <> T.unpack e
        Right lr -> do
          nodeCount lr `shouldBe` 98
          lrSkippedNodes lr `shouldBe` 2

  describe "writer/loader round-trip" $ do
    it "preserves all top-level sections" roundTripTest

  describe "embeddings sidecar" $ do
    it "loads node embeddings from the sidecar referenced by embeddings_path" $ do
      res <- withTempGraphAndSidecar embeddingsGraph (Just sidecarJSON)
      case res of
        Left e -> fail $ "expected success, got: " <> T.unpack e
        Right lr -> do
          gEmbeddingsPath (lrGraph lr) `shouldBe` Just "embeddings.json"
          gEmbeddings (lrGraph lr) `shouldBe` Just (Map.fromList [("a", [1.0, 2.0]), ("b", [3.0, 4.0])])
    it "degrades to no embeddings when the sidecar file is missing" $ do
      res <- withTempGraphAndSidecar embeddingsGraph Nothing
      case res of
        Left e -> fail $ "expected success, got: " <> T.unpack e
        Right lr -> do
          gEmbeddingsPath (lrGraph lr) `shouldBe` Just "embeddings.json"
          gEmbeddings (lrGraph lr) `shouldBe` Nothing
    it "degrades to no embeddings when the sidecar is unparseable" $ do
      res <- withTempGraphAndSidecar embeddingsGraph (Just "not json")
      case res of
        Left e -> fail $ "expected success, got: " <> T.unpack e
        Right lr -> gEmbeddings (lrGraph lr) `shouldBe` Nothing
    it "leaves embeddings unset for a legacy graph without embeddings_path" $ do
      res <- withTempGraph legacyGraph loadGraphFromFile
      case res of
        Left e -> fail $ "expected success, got: " <> T.unpack e
        Right lr -> do
          gEmbeddingsPath (lrGraph lr) `shouldBe` Nothing
          gEmbeddings (lrGraph lr) `shouldBe` Nothing

roundTripTest :: IO ()
roundTripTest = withSystemTempDirectory "graphos-roundtrip" $ \dir -> do
  let path = dir </> "graph.json"
  iw <- Inc.openWriter path
  let nodes =
        [ Node "a" (fromText "A") CodeFile (fromText "a.hs") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
        , Node "b" (fromText "B") CodeFile (fromText "b.hs") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
        ]
      edges = [ Edge (EdgeId "e1") "a" "b" Calls 1.0 (Confidence 0.9) Nothing ]
      commMap = Map.fromList [(1, ["a", "b"])] :: CommunityMap
      cohMap = Map.fromList [(1, 0.8)] :: CohesionMap
      gods = [ GodNode "a" "A" 5 ]
      labels = Map.fromList [(1, "auth")] :: Map Int Text
      comps = Just (Object (KM.fromList [("1", Object KM.empty)]))
      aggs = [ CommunityAggregate "1" 2 0.8 0 "blue" "auth" ["A", "B"] [(1, 2)] (Just "code") 0.1 3 ]
  Inc.writeNodes iw nodes
  Inc.writeEdges iw edges
  Inc.writeCommunities iw commMap
  Inc.writeCohesion iw cohMap
  Inc.writeGodNodes iw gods
  Inc.writeCommunityAggregates iw aggs
  Inc.writeCompositions iw comps
  Inc.writeAnalysisTail iw (Just labels)
  Inc.closeWriter iw
  res <- loadGraphFromFile path
  case res of
    Left e -> fail $ "expected success, got: " <> T.unpack e
    Right lr -> do
      nodeCount lr `shouldBe` 2
      edgeCount lr `shouldBe` 1
      lrCommunities lr `shouldBe` commMap
      lrCohesion lr `shouldBe` cohMap
      lrGodNodes lr `shouldBe` gods
      lrCommunityLabels lr `shouldBe` labels
      lrCompositions lr `shouldBe` comps
      lrCommunityAggregates lr `shouldBe` aggs
      lrDegradedRelations lr `shouldBe` 0
      lrDegradedFileTypes lr `shouldBe` 0
      lrSkippedNodes lr `shouldBe` 0
      lrSkippedEdges lr `shouldBe` 0

-- ───────────────────────────────────────────────
-- Fixtures
-- ───────────────────────────────────────────────

baseNodes :: Text
baseNodes =
  "[{\"id\":\"a\",\"label\":\"A\",\"file_type\":\"code\",\"source_file\":\"a.hs\"},"
  <> "{\"id\":\"b\",\"label\":\"B\",\"file_type\":\"code\",\"source_file\":\"b.hs\"}]"

baseEdges :: Text
baseEdges =
  "[{\"id\":\"e1\",\"source\":\"a\",\"target\":\"b\",\"relation\":\"calls\",\"weight\":1.0,\"confidence\":0.9}]"

legacyGraph :: Text
legacyGraph = "{\"nodes\":" <> baseNodes <> ",\"edges\":" <> baseEdges <> "}"

unknownRelGraph :: Text
unknownRelGraph =
  "{\"nodes\":" <> baseNodes
  <> ",\"edges\":[{\"id\":\"e1\",\"source\":\"a\",\"target\":\"b\",\"relation\":\"re_exports\",\"weight\":1.0,\"confidence\":0.9}]}"

unknownFtGraph :: Text
unknownFtGraph =
  "{\"nodes\":[{\"id\":\"a\",\"label\":\"A\",\"file_type\":\"other\",\"source_file\":\"a.hs\"},"
  <> "{\"id\":\"b\",\"label\":\"B\",\"file_type\":\"code\",\"source_file\":\"b.hs\"}],\"edges\":[]}"

nullSrcGraph :: Text
nullSrcGraph =
  "{\"nodes\":[{\"id\":\"a\",\"label\":\"A\",\"file_type\":\"code\",\"source_file\":null},"
  <> "{\"id\":\"b\",\"label\":\"B\",\"file_type\":\"code\",\"source_file\":\"b.hs\"}],\"edges\":[]}"

embeddingsGraph :: Text
embeddingsGraph = "{\"nodes\":" <> baseNodes <> ",\"edges\":[],\"embeddings_path\":\"embeddings.json\"}"

sidecarJSON :: Text
sidecarJSON = "{\"a\":[1.0,2.0],\"b\":[3.0,4.0]}"

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

withTempGraph :: Text -> (FilePath -> IO (Either Text LoadResult)) -> IO (Either Text LoadResult)
withTempGraph json load = withSystemTempDirectory "graphos-loadspec" $ \dir -> do
  let path = dir </> "graph.json"
  writeFile path (T.unpack json)
  load path

withTempGraphAndSidecar :: Text -> Maybe Text -> IO (Either Text LoadResult)
withTempGraphAndSidecar json sidecar = withSystemTempDirectory "graphos-loadspec" $ \dir -> do
  let path = dir </> "graph.json"
  writeFile path (T.unpack json)
  forM_ sidecar $ \sc -> writeFile (dir </> "embeddings.json") (T.unpack sc)
  loadGraphFromFile path

nodeCount :: LoadResult -> Int
nodeCount = Map.size . gNodes . lrGraph

edgeCount :: LoadResult -> Int
edgeCount = Map.size . gEdges . lrGraph
