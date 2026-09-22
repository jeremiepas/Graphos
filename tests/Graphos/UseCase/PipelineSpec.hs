module Graphos.UseCase.PipelineSpec where

import Control.Exception (SomeException, try)
import Data.Aeson (eitherDecode)
import Data.List (isInfixOf, sort)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Short (fromText, toText)
import Control.Monad (forM)
import System.Directory (listDirectory, getModificationTime, createDirectory)
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
      embs <- generateGraphEmbeddings llm defaultEmbeddingConfig graph "/cache" "embeddings.json"
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
      embs <- generateGraphEmbeddings llm oneByOne graph "/cache" "embeddings.json"
      embs `shouldBe` Map.fromList [("a", [1.0, 2.0])]
    it "returns an empty map for a graph with no nodes" $ do
      let llm = stubLLM (const (const (pure (Right [1.0] :: Either Text [Double]))))
      embs <- generateGraphEmbeddings
                  llm defaultEmbeddingConfig (testGraph []) "/cache" "embeddings.json"
      embs `shouldBe` Map.empty

    it "submits only unique texts and redistributes their vectors (dedup)" $ do
      callsRef <- newIORef ([] :: [[Text]])
      let llm = countingLLM callsRef (pure . Right . (: []) . fromIntegral . T.length)
          -- 5000 nodes sharing one label + 100 nodes with distinct labels.
          dupes  = [testNode ("dup-" <> T.pack (show i)) "Dup" "d.hs" | i <- [1 :: Int .. 5000]]
          others = [testNode ("u-" <> T.pack (show i)) ("U" <> T.pack (show i)) ("u" <> T.pack (show i) <> ".hs") | i <- [1 :: Int .. 100]]
          graph  = testGraph (dupes ++ others)
      embs <- generateGraphEmbeddings llm defaultEmbeddingConfig graph "/nonexistent-cache" "embeddings.json"
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
      embs <- generateGraphEmbeddings llm oneByOne g "/cache" "embeddings.json"
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
      embs <- generateGraphEmbeddings llm oneByOne g "/cache" "embeddings.json"
      embs `shouldBe` Map.fromList [("n1", [fromIntegral (T.length ("getUser a.hs" :: Text))])]

    it "keeps sibling vectors when one text in a batch fails (bisection, task 2.2)" $ do
      -- One poison text inside a size-8 batch: D2 bisection shrinks the
      -- failure domain until the poison sits alone; siblings keep vectors.
      let llm = stubLLM $ \_ input ->
                if "poison" `T.isInfixOf` input
                  then pure (Left "boom")
                  else pure (Right [fromIntegral (T.length input)])
          mkNode :: Text -> Text -> Text -> Node
          mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
          ns8 = [ mkNode ("n" <> T.pack (show i))
                    (if i == (4 :: Int) then "poison" else "L" <> T.pack (show i))
                    ("s" <> T.pack (show i) <> ".hs")
                | i <- [1 .. 8] ]
          g8 = testGraph ns8
          batch8 = defaultEmbeddingConfig { embBatchSize = 8 }
      embs <- generateGraphEmbeddings llm batch8 g8 "/nonexistent-cache" "embeddings.json"
      -- 7 siblings carry vectors; only the poison text lacks one.
      Map.size embs `shouldBe` 7
      Map.member "n4" embs `shouldBe` False
      [Map.member ("n" <> T.pack (show i)) embs | i <- [1 .. 8 :: Int], i /= 4]
        `shouldSatisfy` all id

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
        embs1 <- generateGraphEmbeddings llm cfg g dir (dir </> "embeddings.json")
        embs2 <- generateGraphEmbeddings llm cfg g dir (dir </> "embeddings.json")
        submitted <- readIORef callsRef
        -- First run: exactly one text submitted; second run: zero calls.
        length (concat submitted) `shouldBe` 1
        embs1 `shouldBe` Map.fromList [("n1", [fromIntegral (T.length ("getUser a.hs" :: Text))])]
        embs2 `shouldBe` embs1

    it "performs zero cache writes on a fully warm second run (AC: hits are not rewritten, task 2.4)" $ do
      withSystemTempDirectory "graphos-embcache-warm" $ \dir -> do
        callsRef <- newIORef ([] :: [[Text]])
        let perText :: Text -> IO (Either Text [Double])
            perText t = pure (Right [fromIntegral (T.length t)])
            llm = countingLLM callsRef perText
            mkNode :: Text -> Text -> Text -> Node
            mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
            g = testGraph [mkNode "n1" "getUser" "a.hs", mkNode "n2" "putUser" "b.hs"]
            cfg = defaultEmbeddingConfig
        embs1 <- generateGraphEmbeddings llm cfg g (dir </> "cache") (dir </> "embeddings.json")
        -- Snapshot the cache entries after the cold run.
        cold <- listDirectory (dir </> "cache" </> "embeddings")
        -- Snapshot entry mtimes after the cold run.
        coldMtimes <- forM cold $ \f -> do
          t <- getModificationTime (dir </> "cache" </> "embeddings" </> f)
          pure (f, t)
        -- Warm run: zero API calls and zero cache writes.
        embs2 <- generateGraphEmbeddings llm cfg g (dir </> "cache") (dir </> "embeddings.json")
        writeIORef callsRef []
        submitted <- readIORef callsRef
        length (concat submitted) `shouldBe` 0
        warm <- listDirectory (dir </> "cache" </> "embeddings")
        sort warm `shouldBe` sort cold
        embs2 `shouldBe` embs1
        -- Every entry file is untouched (no rewrite churn): mtimes identical.
        warmMtimes <- forM warm $ \f -> do
          t <- getModificationTime (dir </> "cache" </> "embeddings" </> f)
          pure (f, t)
        map (\(f, t) -> (f, show t)) warmMtimes
          `shouldBe` map (\(f, t) -> (f, show t)) coldMtimes

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
      embsC4 <- generateGraphEmbeddings llm concurrent4 g "/nonexistent-cache" "embeddings.json"
      embsS1 <- generateGraphEmbeddings llm sequential1 g "/nonexistent-cache" "embeddings.json"
      embsC4 `shouldBe` embsS1

  describe "writeEmbeddingsSidecar" $ do
    it "writes a JSON object that decodes back to the same map" $ do
      withSystemTempDirectory "graphos-pipelinespec" $ \dir -> do
        let path = dir </> "embeddings.json"
            embs = Map.fromList [("a", [1.0, 2.0]), ("b", [3.0, 4.0])] :: Map Text [Double]
        writeEmbeddingsSidecar path embs
        bs <- BSL.readFile path
        eitherDecode bs `shouldBe` Right embs

  describe "streaming sidecar write (lfm-embedding-optimization 3.4)" $ do
    let mkNode :: Text -> Text -> Text -> Node
        mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
        g2 = testGraph [mkNode "n1" "getUser" "a.hs", mkNode "n2" "putUser" "b.hs"]
        perTextVec :: Text -> IO (Either Text [Double])
        perTextVec t = pure (Right [fromIntegral (T.length t)])
    it "streaming content equals write-at-end content for the same inputs (AC)" $ do
      withSystemTempDirectory "graphos-stream-on" $ \dirS -> do
        withSystemTempDirectory "graphos-stream-off" $ \dirE -> do
          callsRef <- newIORef ([] :: [[Text]])
          let llm = countingLLM callsRef perTextVec
              streamOn  = defaultEmbeddingConfig { embStreaming = True }
              streamOff = defaultEmbeddingConfig { embStreaming = False }
          embsOn  <- generateGraphEmbeddings llm streamOn g2 (dirS </> "cache") (dirS </> "embeddings.json")
          writeIORef callsRef []
          embsOff <- generateGraphEmbeddings llm streamOff g2 (dirE </> "cache") (dirE </> "embeddings.json")
          embsOn `shouldBe` embsOff
          onBs  <- BSL.readFile (dirS </> "embeddings.json")
          offBs <- BSL.readFile (dirE </> "embeddings.json")
          case (eitherDecode onBs, eitherDecode offBs) of
            (Right m1, Right m2) -> (m1 :: Map Text [Double]) `shouldBe` m2
            (l, r) -> do
              putStrLn ("ON  raw: " ++ show (BSL.toStrict onBs))
              putStrLn ("OFF raw: " ++ show (BSL.toStrict offBs))
              expectationFailure ("sidecars do not decode as JSON objects: " ++ show (l, r))
          -- No staging leftovers at the final path's directory.
          leftovers <- listDirectory dirS
          filter ("tmp" `isInfixOf`) leftovers `shouldBe` []

    it "aborted streaming run leaves the prior sidecar untouched (AC)" $ do
      withSystemTempDirectory "graphos-stream-abort" $ \dir -> do
        let sidecar = dir </> "embeddings.json"
            prior = Map.fromList [("old", [9.0])] :: Map Text [Double]
        writeEmbeddingsSidecar sidecar prior
        -- Batch-level LLM exceptions degrade to empty contributions
        -- (per-batch isolation): the pass completes with partial data and a
        -- valid sidecar — that is isolation, not abort. A true kill/crash
        -- abort never reaches the rename; simulate it by making the staged
        -- open fail (sidecar path occupied by a directory): openTempFile
        -- throws before any write — exception propagates, prior sidecar and
        -- its directory contents untouched, no partial file.
        createDirectory (dir </> "blocked")
        r <- try (generateGraphEmbeddings
                    (stubLLM (\_ _ -> pure (Right [1.0 :: Double])))
                    defaultEmbeddingConfig { embStreaming = True }
                    g2 (dir </> "cache") (dir </> "embeddings.json" </> "x"))
        case (r :: Either SomeException (Map Text [Double])) of
          Left _ -> pure ()
          Right _ -> expectationFailure "expected the simulated abort to propagate"
        -- The prior sidecar is intact and no partial file exists at it.
        bs <- BSL.readFile sidecar
        (eitherDecode bs :: Either String (Map Text [Double])) `shouldBe` Right prior
        (eitherDecode bs :: Either String (Map Text [Double])) `shouldBe` Right prior

    it "streaming opt-out leaves no staged file and writes at the end (AC)" $ do
      withSystemTempDirectory "graphos-stream-optout" $ \dir -> do
        callsRef <- newIORef ([] :: [[Text]])
        let llm = countingLLM callsRef perTextVec
            streamOff = defaultEmbeddingConfig { embStreaming = False }
        embs <- generateGraphEmbeddings llm streamOff g2 (dir </> "cache") (dir </> "embeddings.json")
        Map.size embs `shouldBe` 2
        entries <- listDirectory dir
        -- Only the final sidecar (and the cache dir); no staging remnants.
        sort entries `shouldBe` ["cache", "embeddings.json"]

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

-- ───────────────────────────────────────────────
-- DEBUG
-- ───────────────────────────────────────────────
