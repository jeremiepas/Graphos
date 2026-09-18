module Graphos.UseCase.PipelineSpec where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Short (fromText, toText)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph.Core (Graph(..))
import Graphos.UseCase.Pipeline.Core (generateGraphEmbeddings, writeEmbeddingsSidecar)
import Graphos.UseCase.Port.LLMPort (LLMPort(..))
import Graphos.Infrastructure.Export.JSON (saveCheckpoint, loadCheckpointInputSource)

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
      embs <- generateGraphEmbeddings llm defaultEmbeddingConfig graph "/cache"
      embs `shouldBe` Map.fromList [("a", [1.0, 2.0]), ("b", [1.0, 2.0])]
    it "omits nodes whose embedding call fails" $ do
      -- Per-text failure isolation: the failing text sits in its own batch
      -- (batchSize 1), mirroring the sequential one-call-per-node loop.
      let llm = stubLLM $ \_cfg input ->
                if input == "A a.hs"
                  then pure (Right [1.0, 2.0])
                  else pure (Left "boom")
          graph = testGraph [testNode "a" "A" "a.hs", testNode "b" "B" "b.hs"]
          oneByOne = defaultEmbeddingConfig { embBatchSize = 1 }
      embs <- generateGraphEmbeddings llm oneByOne graph "/cache"
      embs `shouldBe` Map.fromList [("a", [1.0, 2.0])]
    it "returns an empty map for a graph with no nodes" $ do
      let llm = stubLLM (const (const (pure (Right [1.0] :: Either Text [Double]))))
      embs <- generateGraphEmbeddings
                  llm defaultEmbeddingConfig (testGraph []) "/cache"
      embs `shouldBe` Map.empty

    it "submits only unique texts and redistributes their vectors (dedup)" $ do
      callsRef <- newIORef ([] :: [[Text]])
      let llm = countingLLM callsRef (pure . Right . (: []) . fromIntegral . T.length)
          -- 5000 nodes sharing one label + 100 nodes with distinct labels.
          dupes  = [testNode ("dup-" <> T.pack (show i)) "Dup" "d.hs" | i <- [1 :: Int .. 5000]]
          others = [testNode ("u-" <> T.pack (show i)) ("U" <> T.pack (show i)) ("u" <> T.pack (show i) <> ".hs") | i <- [1 :: Int .. 100]]
          graph  = testGraph (dupes ++ others)
      embs <- generateGraphEmbeddings llm defaultEmbeddingConfig graph "/nonexistent-cache"
      submitted <- readIORef callsRef
      -- ≤ 101 distinct texts submitted across all calls.
      length (concat submitted) `shouldSatisfy` (<= 101)
      Map.size embs `shouldBe` 5100
      -- Every duplicate-label node receives the same vector.
      let vecs = [Map.findWithDefault [] ("dup-" <> T.pack (show i)) embs | i <- [1 :: Int .. 5000]]
      case vecs of
        (v0:rest) -> rest `shouldSatisfy` all (== v0)
        []        -> expectationFailure "expected 5000 duplicate vectors"

    it "matches the sequential one-text-per-call assignment on a small fixture" $ do
      -- Fake port that maps each text to a deterministic pseudo-vector, as a
      -- server would. The failing text lands in its own batch (batchSize 1),
      -- mirroring per-text failure isolation of the sequential loop.
      let deterministic :: Text -> IO (Either Text [Double])
          deterministic t = pure $
            if "fail" `T.isInfixOf` t then Left "boom" else Right [fromIntegral (T.length t)]
          llm = stubLLM (\_ input -> deterministic input)
          mkNode :: Text -> Text -> Text -> Node
          mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
          ns = [ mkNode "n1" "getUser" "a.hs"
               , mkNode "n2" "Promise" "b.hs"
               , mkNode "n3" "Promise" "c.hs"
               , mkNode "n4" "failing" "fail.hs"
               , mkNode "n5" "getUser" "d.hs" ]
          g = testGraph ns
          oneByOne = defaultEmbeddingConfig { embBatchSize = 1 }
      -- Sequential baseline: one call per node, omitting failed calls.
      baseline <- mapM (\n -> do
                          r <- deterministic (toText (nodeLabel n) <> " " <> toText (nodeSourceFile n))
                          pure (nodeId n, either (const Nothing) Just r)) ns
      -- Optimized pipeline under test (batchSize 1 = one text per batch).
      embs <- generateGraphEmbeddings llm oneByOne g "/cache"
      let expected = Map.fromList [ (nid, v) | (nid, Just v) <- baseline ]
      embs `shouldBe` expected

    it "withholds only the texts of a failed batch when a whole batch fails" $ do
      -- A batch-level failure (server error) withholds exactly its own texts;
      -- other batches are unaffected, mirroring sequential per-node failure.
      let deterministic :: Text -> IO (Either Text [Double])
          deterministic t = pure $
            if "fail" `T.isInfixOf` t then Left "boom" else Right [fromIntegral (T.length t)]
          llm = stubLLM (\_ input -> deterministic input)
          mkNode :: Text -> Text -> Text -> Node
          mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
          ns = [ mkNode "n1" "getUser" "a.hs"
               , mkNode "n4" "failing" "fail.hs" ]
          g = testGraph ns
          oneByOne = defaultEmbeddingConfig { embBatchSize = 1 }
      embs <- generateGraphEmbeddings llm oneByOne g "/cache"
      embs `shouldBe` Map.fromList [("n1", [fromIntegral (T.length ("getUser a.hs" :: Text))])]

    it "serves a repeated text from the cache without an API call (AC: second run hits cache)" $ do
      withSystemTempDirectory "graphos-embcache-pipeline" $ \dir -> do
        callsRef <- newIORef ([] :: [[Text]])
        let perText :: Text -> IO (Either Text [Double])
            perText t = pure (Right [fromIntegral (T.length t)])
            llm = countingLLM callsRef perText
            mkNode :: Text -> Text -> Text -> Node
            mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
            g = testGraph [mkNode "n1" "getUser" "a.hs"]
            cfg = defaultEmbeddingConfig { embBatchSize = 64 }
        embs1 <- generateGraphEmbeddings llm cfg g dir
        embs2 <- generateGraphEmbeddings llm cfg g dir
        submitted <- readIORef callsRef
        -- First run: exactly one text submitted; second run: zero calls.
        length (concat submitted) `shouldBe` 1
        embs1 `shouldBe` Map.fromList [("n1", [fromIntegral (T.length ("getUser a.hs" :: Text))])]
        embs2 `shouldBe` embs1

    it "produces the same assignment under concurrency 4 as under 1" $ do
      callsRef <- newIORef ([] :: [[Text]])
      let perText :: Text -> IO (Either Text [Double])
          perText t = pure (Right [fromIntegral (T.length t)])
          llm = countingLLM callsRef perText
          mkNode :: Text -> Text -> Text -> Node
          mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
          -- 3 chunks' worth of unique texts; the stub's latency differs per
          -- batch so completions may invert, but the assignment is stable.
          ns = [ mkNode ("n" <> T.pack (show i)) ("L" <> T.pack (show i)) ("s" <> T.pack (show i) <> ".hs")
               | i <- [1 :: Int .. 200] ]
          g = testGraph ns
          concurrent4 = defaultEmbeddingConfig { embBatchSize = 10, embConcurrency = 4 }
          sequential1 = defaultEmbeddingConfig { embBatchSize = 10, embConcurrency = 1 }
      embsC4 <- generateGraphEmbeddings llm concurrent4 g "/nonexistent-cache"
      embsS1 <- generateGraphEmbeddings llm sequential1 g "/nonexistent-cache"
      embsC4 `shouldBe` embsS1

  describe "writeEmbeddingsSidecar" $ do
    it "writes a JSON object that decodes back to the same map" $ do
      withSystemTempDirectory "graphos-pipelinespec" $ \dir -> do
        let path = dir </> "embeddings.json"
            embs = Map.fromList [("a", [1.0, 2.0]), ("b", [3.0, 4.0])] :: Map Text [Double]
        writeEmbeddingsSidecar path embs
        bs <- BSL.readFile path
        eitherDecode bs `shouldBe` Right embs

  describe "checkpoint provenance (AC#3)" $ do
    it "records input_source via saveCheckpoint and restores it via loadCheckpointInputSource" $ do
      withSystemTempDirectory "graphos-checkpoint-roundtrip" $ \dir -> do
        let path = dir </> "graph.checkpoint.json"
            g = testGraph [testNode "a" "A" "a.hs"]
        saveCheckpoint g path (T.pack "./src")
        mSrc <- loadCheckpointInputSource path
        mSrc `shouldBe` Just (T.pack "./src")

    it "returns Nothing when the checkpoint file is absent" $ do
      withSystemTempDirectory "graphos-checkpoint-absent" $ \dir -> do
        mSrc <- loadCheckpointInputSource (dir </> "missing.checkpoint.json")
        mSrc `shouldBe` Nothing

    it "returns Nothing for a checkpoint written without input_source (old schema)" $ do
      withSystemTempDirectory "graphos-checkpoint-old" $ \dir -> do
        let path = dir </> "old.checkpoint.json"
        -- A checkpoint from before input_source provenance was recorded.
        BSL.writeFile path ("{\"nodes\":[],\"edges\":[],\"checkpoint\":true,\"schema_version\":\"1\"}")
        mSrc <- loadCheckpointInputSource path
        mSrc `shouldBe` Nothing

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
  , lpGenerateEmbeddings = \cfg ts -> do
      rs <- mapM (gen cfg) ts
      pure $ mapM id rs
  , lpAnalyzeImage = error "not used"
  , lpValidateUrl = pure
  }

-- | A stub that records every batch of submitted texts into an IORef and
-- answers each text with @gen@ applied to it.
countingLLM :: IORef [[Text]]
            -> (Text -> IO (Either Text [Double]))
            -> LLMPort
countingLLM callsRef gen = (stubLLM (\_ input -> gen input))
  { lpGenerateEmbeddings = \_ ts -> do
      modifyIORef' callsRef (ts :)
      rs <- mapM gen ts
      pure $ mapM id rs
  }
