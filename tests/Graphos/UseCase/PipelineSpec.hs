module Graphos.UseCase.PipelineSpec where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph.Core (Graph(..))
import Graphos.UseCase.Pipeline.Core (generateGraphEmbeddings, writeEmbeddingsSidecar)
import Graphos.UseCase.Port.LLMPort (LLMPort(..))

spec :: Spec
spec = do
  describe "PipelineConfig" $ do
    it "has sensible defaults" $ do
      let cfg = defaultConfig
      cfgInputPath cfg `shouldBe` "."
      cfgOutputDir cfg `shouldBe` "graphos-out"
      cfgDirected cfg `shouldBe` False
      cfgNoViz cfg `shouldBe` False

  describe "generateGraphEmbeddings" $ do
    it "collects a vector for every node the LLM succeeds on" $ do
      let llm = stubLLM (const (const (pure (Right [1.0, 2.0] :: Either Text [Double]))))
          graph = testGraph [testNode "a" "A" "a.hs", testNode "b" "B" "b.hs"]
      embs <- generateGraphEmbeddings llm defaultEmbeddingConfig graph
      embs `shouldBe` Map.fromList [("a", [1.0, 2.0]), ("b", [1.0, 2.0])]
    it "omits nodes whose embedding call fails" $ do
      let llm = stubLLM $ \_cfg input ->
                if input == "A a.hs"
                  then pure (Right [1.0, 2.0])
                  else pure (Left "boom")
          graph = testGraph [testNode "a" "A" "a.hs", testNode "b" "B" "b.hs"]
      embs <- generateGraphEmbeddings llm defaultEmbeddingConfig graph
      embs `shouldBe` Map.fromList [("a", [1.0, 2.0])]
    it "returns an empty map for a graph with no nodes" $ do
      let llm = stubLLM (const (const (pure (Right [1.0] :: Either Text [Double]))))
      embs <- generateGraphEmbeddings llm defaultEmbeddingConfig (testGraph [])
      embs `shouldBe` Map.empty

  describe "writeEmbeddingsSidecar" $ do
    it "writes a JSON object that decodes back to the same map" $ do
      withSystemTempDirectory "graphos-pipelinespec" $ \dir -> do
        let path = dir </> "embeddings.json"
            embs = Map.fromList [("a", [1.0, 2.0]), ("b", [3.0, 4.0])] :: Map Text [Double]
        writeEmbeddingsSidecar path embs
        bs <- BSL.readFile path
        eitherDecode bs `shouldBe` Right embs

-- ───────────────────────────────────────────────
-- Fixtures
-- ───────────────────────────────────────────────

testNode :: NodeId -> Text -> Text -> Node
testNode nid label src = Node nid (fromText label) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

testGraph :: [Node] -> Graph
testGraph ns = Graph
  { gNodes = Map.fromList [(nodeId n, n) | n <- ns]
  , gEdges = Map.empty
  , gAdjFwd = Map.empty
  , gAdjBack = Map.empty
  , gDirected = False
  , gCompositions = Nothing
  , gHash = ""
  , gEmbeddings = Nothing
  , gEmbeddingsPath = Nothing
  }

stubLLM :: (EmbeddingConfig -> Text -> IO (Either Text [Double])) -> LLMPort
stubLLM gen = LLMPort
  { lpCallLLM = error "not used"
  , lpParseLabelsFromResponse = const Map.empty
  , lpGenerateEmbedding = gen
  , lpAnalyzeImage = error "not used"
  , lpValidateUrl = pure
  }
