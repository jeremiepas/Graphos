{-# LANGUAGE OverloadedStrings #-}
-- | JGF serialization round-trip and compatibility tests (jgf-serialization).
--
-- Covers the spec scenarios:
--   * write→read round-trip equality on a fixture graph
--   * node metadata carries graphos fields
--   * legacy graph.json still loads
--   * JGF file loads
--   * unknown major schemaVersion rejected
module Graphos.Infrastructure.Export.JGFSpec (spec) where

import Data.Aeson (Value(..), eitherDecode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph (Graph, buildGraph, gNodes, gEdges, gEmbeddingsPath)
import Graphos.UseCase.Load (LoadResult(..), loadGraphFromFile, loadGraphFromFileStrict)
import Graphos.Infrastructure.Export.JSON (exportGraphWithLabels)
import Graphos.Infrastructure.Export.IncrementalJSON
  ( openWriter, closeWriter, writeNodes, writeEdges, writeCommunities, writeCohesion
  , writeGodNodes, writeAnalysisTail, writeCommunityAggregates, writeCompositions
  , writeEmbeddingsPath
  )

fixtureNodes :: [Node]
fixtureNodes =
  [ Node "a" (fromText "A") CodeFile (fromText "src/a.hs") Nothing
          (Just 1) (Just 10) (Just (fromText "a :: Int")) (Just 1) (Just (fromText "function"))
          (Just 5) (Just True) Nothing 0
  , Node "b" (fromText "B") DocFile (fromText "docs/b.md") Nothing
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
  ]

fixtureEdges :: [Edge]
fixtureEdges =
  [ Edge (EdgeId "e1") "a" "b" References 2.0 (Confidence 0.75)
         (Just (object ["why" .= String "doc link"]))
  ]

fixtureCommunities :: CommunityMap
fixtureCommunities = Map.fromList [(1, ["a", "b"]), (2, ["c"])]

fixtureCohesion :: CohesionMap
fixtureCohesion = Map.fromList [(1, 0.8), (2, 0.5)]

fixtureGodNodes :: [GodNode]
fixtureGodNodes = [GodNode "a" "A" 5]

fixtureLabels :: Map Int Text
fixtureLabels = Map.fromList [(1, "auth"), (2, "docs")]

spec :: Spec
spec = do
  describe "JGF write→read round-trip" $
    it "preserves all node and edge fields, communities, cohesion, god nodes, and labels" $
      withSystemTempDirectory "graphos-jgf-roundtrip" $ \dir -> do
        let path = dir </> "graph.json"
        exportGraphWithLabels g analysis (Just fixtureLabels) path
        res <- loadGraphFromFile path
        case res of
          Left e -> fail $ "expected success, got: " <> T.unpack e
          Right lr -> do
            Map.toList (gNodes (lrGraph lr)) `shouldBe` Map.toList (gNodes fixtureGraph)
            Map.toList (gEdges (lrGraph lr)) `shouldBe` Map.toList (gEdges fixtureGraph)
            lrCommunities lr `shouldBe` fixtureCommunities
            lrCohesion lr `shouldBe` fixtureCohesion
            lrGodNodes lr `shouldBe` fixtureGodNodes
            lrCommunityLabels lr `shouldBe` fixtureLabels
            lrDegradedRelations lr `shouldBe` 0
            lrDegradedFileTypes lr `shouldBe` 0
            lrSkippedNodes lr `shouldBe` 0
            lrSkippedEdges lr `shouldBe` 0

  describe "emitted file is a JGF document" $ do
    it "has a top-level graph object with nodes, edges, metadata, directed, and type" $
      withSystemTempDirectory "graphos-jgf-shape" $ \dir -> do
        let path = dir </> "graph.json"
        exportGraphWithLabels g analysis (Just fixtureLabels) path
        bs <- BSL.readFile path
        case eitherDecode bs of
          Left e -> fail $ "not valid JSON: " ++ e
          Right (Object km) -> do
            Object gkm <- case lookupKV "graph" km of
              Just gv -> pure gv
              Nothing -> fail "missing top-level \"graph\" object"
            isJust (lookupKV "nodes" gkm) `shouldBe` True
            isJust (lookupKV "edges" gkm) `shouldBe` True
            isJust (lookupKV "metadata" gkm) `shouldBe` True
            lookupKV "directed" gkm `shouldBe` Just (Bool True)
            lookupKV "type" gkm `shouldBe` Just (String "graphos.code-knowledge-graph")
          Right _ -> fail "top-level value must be an object"

    it "carries graphos sections under graph.metadata.graphos" $
      withSystemTempDirectory "graphos-jgf-meta" $ \dir -> do
        let path = dir </> "graph.json"
        exportGraphWithLabels g analysis (Just fixtureLabels) path
        bs <- BSL.readFile path
        case eitherDecode bs of
          Right doc -> do
            let graphos = graphosOf doc
            isJust (lookupKV "communities" graphos) `shouldBe` True
            isJust (lookupKV "cohesion" graphos) `shouldBe` True
            isJust (lookupKV "god_nodes" graphos) `shouldBe` True
            isJust (lookupKV "community_labels" graphos) `shouldBe` True
            isJust (lookupKV "schemaVersion" graphos) `shouldBe` True
          _ -> fail "not a JGF document"

    it "stores node graphos fields under node metadata" $
      withSystemTempDirectory "graphos-jgf-nodemeta" $ \dir -> do
        let path = dir </> "graph.json"
        exportGraphWithLabels g analysis (Just fixtureLabels) path
        bs <- BSL.readFile path
        case eitherDecode bs of
          Right doc -> do
            let nodes = nodesOf doc
            Object aMeta <- case lookupKV "a" nodes >>= metaOf of
              Just mv -> pure mv
              Nothing -> fail "node \"a\" missing"
            lookupKV "source_file" aMeta `shouldBe` Just (String "src/a.hs")
          _ -> fail "not a JGF document"

  describe "legacy graph.json still loads" $
    it "parses into the same in-memory graph" $
      withSystemTempDirectory "graphos-legacy-load" $ \dir -> do
        let path = dir </> "graph.json"
        writeFile path (T.unpack legacyGraphText)
        res <- loadGraphFromFile path
        case res of
          Left e -> fail $ "expected success, got: " <> T.unpack e
          Right lr -> do
            nodeCount lr `shouldBe` 2
            edgeCount lr `shouldBe` 1
            lrCommunities lr `shouldBe` fixtureCommunities

  describe "JGF file loads" $
    it "parses a hand-written JGF document into the in-memory graph" $
      withSystemTempDirectory "graphos-jgf-load" $ \dir -> do
        let path = dir </> "graph.json"
        writeFile path (T.unpack jgfGraphText)
        res <- loadGraphFromFileStrict path
        case res of
          Left e -> fail $ "expected success, got: " <> T.unpack e
          Right lr -> do
            nodeCount lr `shouldBe` 2
            edgeCount lr `shouldBe` 1
            lrCommunities lr `shouldBe` Map.fromList [(1, ["a", "b"])]
            lrCommunityLabels lr `shouldBe` Map.fromList [(1, "auth")]

  describe "unknown major schemaVersion rejected" $
    it "fails with an error identifying the unsupported version" $
      withSystemTempDirectory "graphos-jgf-badversion" $ \dir -> do
        let path = dir </> "graph.json"
        writeFile path (T.unpack badVersionJGFText)
        res <- loadGraphFromFile path
        case res of
          Right _ -> fail "expected failure"
          Left e -> do
            T.isInfixOf "9" e `shouldBe` True
            T.isInfixOf "schema_version" e `shouldBe` True

  describe "incremental writer round-trip" $
    it "streams a JGF document that loads back losslessly" $
      withSystemTempDirectory "graphos-jgf-incremental" $ \dir -> do
        let path = dir </> "graph.json"
        iw <- openWriter path
        writeNodes iw fixtureNodes
        writeEdges iw fixtureEdges
        writeCommunities iw fixtureCommunities
        writeCohesion iw fixtureCohesion
        writeGodNodes iw fixtureGodNodes
        writeCommunityAggregates iw [aggFixture]
        writeCompositions iw (Just (Object KM.empty))
        writeEmbeddingsPath iw (Just "embeddings.json")
        writeAnalysisTail iw (Just fixtureLabels)
        closeWriter iw
        res <- loadGraphFromFile path
        case res of
          Left e -> fail $ "expected success, got: " <> T.unpack e
          Right lr -> do
            nodeCount lr `shouldBe` 2
            edgeCount lr `shouldBe` 1
            lrCommunities lr `shouldBe` fixtureCommunities
            lrCohesion lr `shouldBe` fixtureCohesion
            lrGodNodes lr `shouldBe` fixtureGodNodes
            lrCommunityLabels lr `shouldBe` fixtureLabels
            gEmbeddingsPath (lrGraph lr) `shouldBe` Just "embeddings.json"

-- ───────────────────────────────────────────────
-- Fixtures
-- ───────────────────────────────────────────────

fixtureGraph :: Graph
fixtureGraph = buildGraph False (extractionFromLists fixtureNodes fixtureEdges)

g :: Graph
g = fixtureGraph

analysis :: Analysis
analysis = Analysis
  { analysisCommunities = fixtureCommunities
  , analysisNullModel   = DefaultNullModel
  , analysisCohesion    = fixtureCohesion
  , analysisGodNodes    = fixtureGodNodes
  , analysisSurprises   = []
  , analysisQuestions   = []
  , analysisArticulation = []
  , analysisBccCount    = 0
  }

aggFixture :: CommunityAggregate
aggFixture = CommunityAggregate "1" 2 0.8 0 "blue" "auth" ["A", "B"] [(1, 2)] (Just "code") 0.1 3

legacyGraphText :: Text
legacyGraphText = T.pack $
  "{\"communities\":{\"1\":[\"a\",\"b\"],\"2\":[\"c\"]},\"cohesion\":{\"1\":0.8,\"2\":0.5}"
  <> ",\"god_nodes\":[{\"id\":\"a\",\"label\":\"A\",\"edges\":5}]"
  <> ",\"community_labels\":{\"1\":\"auth\",\"2\":\"docs\"}"
  <> ",\"nodes\":[{\"id\":\"a\",\"label\":\"A\",\"file_type\":\"code\",\"source_file\":\"src/a.hs\""
  <> ",\"line_start\":1,\"line_end\":10,\"signature\":\"a :: Int\",\"community_id\":1"
  <> ",\"kind\":\"function\",\"degree\":5,\"is_bridge\":true}"
  <> ",{\"id\":\"b\",\"label\":\"B\",\"file_type\":\"doc\",\"source_file\":\"docs/b.md\"}]"
  <> ",\"edges\":[{\"id\":\"e1\",\"source\":\"a\",\"target\":\"b\",\"relation\":\"references\""
  <> ",\"weight\":2.0,\"confidence\":0.9}]}"

jgfGraphText :: Text
jgfGraphText = T.unlines
  [ "{\"graph\": {"
  , "  \"directed\": true,"
  , "  \"type\": \"graphos.code-knowledge-graph\","
  , "  \"nodes\": {"
  , "    \"a\": {\"id\": \"a\", \"label\": \"A\", \"metadata\": {\"file_type\": \"code\", \"source_file\": \"src/a.hs\", \"community_id\": 1, \"kind\": \"function\"}},"
  , "    \"b\": {\"id\": \"b\", \"label\": \"B\", \"metadata\": {\"file_type\": \"doc\", \"source_file\": \"docs/b.md\"}}"
  , "  },"
  , "  \"edges\": ["
  , "    {\"source\": \"a\", \"relation\": \"calls\", \"target\": \"b\", \"directed\": true, \"metadata\": {\"id\": \"e1\", \"weight\": 1.0, \"confidence\": 0.9}}"
  , "  ],"
  , "  \"metadata\": {"
  , "    \"graphos\": {"
  , "      \"schemaVersion\": \"1.0\","
  , "      \"communities\": {\"1\": [\"a\", \"b\"]},"
  , "      \"community_labels\": {\"1\": \"auth\"}"
  , "    }"
  , "  }"
  , "}}"
  ]

badVersionJGFText :: Text
badVersionJGFText = T.pack
  "{\"graph\": {\"directed\": true, \"nodes\": {\"a\": {\"id\": \"a\", \"label\": \"A\", \"metadata\": {\"file_type\": \"code\"}}}, \"edges\": [], \"metadata\": {\"graphos\": {\"schemaVersion\": \"9.0\"}}}}"

-- ───────────────────────────────────────────────
-- JSON helpers
-- ───────────────────────────────────────────────

lookupKV :: Text -> KM.KeyMap Value -> Maybe Value
lookupKV k = KM.lookup (Key.fromText k)

metaOf :: Value -> Maybe Value
metaOf (Object km) = lookupKV "metadata" km
metaOf _ = Nothing

graphosOf :: Value -> KM.KeyMap Value
graphosOf doc =
  case doc of
    Object km -> case lookupKV "graph" km of
      Just (Object gkm) -> case lookupKV "metadata" gkm of
        Just (Object mm) -> case lookupKV "graphos" mm of
          Just (Object gm) -> gm
          _ -> KM.empty
        _ -> KM.empty
      _ -> KM.empty
    _ -> KM.empty

nodesOf :: Value -> KM.KeyMap Value
nodesOf doc =
  case doc of
    Object km -> case lookupKV "graph" km of
      Just (Object gkm) -> case lookupKV "nodes" gkm of
        Just (Object nm) -> nm
        _ -> KM.empty
      _ -> KM.empty
    _ -> KM.empty

nodeCount :: LoadResult -> Int
nodeCount = Map.size . gNodes . lrGraph

edgeCount :: LoadResult -> Int
edgeCount = Map.size . gEdges . lrGraph