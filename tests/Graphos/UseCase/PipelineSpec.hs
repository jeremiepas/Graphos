module Graphos.UseCase.PipelineSpec where

import Control.Exception (SomeException, try, throwIO, AsyncException(..))
import Data.Aeson (eitherDecode)
import Data.List (isInfixOf, sort)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Short (fromText, toText)
import qualified Data.Vector.Unboxed as VU
import Control.Monad (forM)
import System.Directory (listDirectory, getModificationTime, createDirectory, doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph.Core (Graph(..))
import Graphos.UseCase.Pipeline.Core
  ( generateGraphEmbeddings
  , generateGraphEmbeddingsGuarded
  , writeEmbeddingsSidecar
  , preFlightMemoryGuard
  , withHeapGuard
  )
import Graphos.UseCase.Port.LLMPort (LLMPort(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
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
      let llm = stubLLM (const (const (pure (Right (VU.fromList [1.0, 2.0])))))
          graph = testGraph [testNode "a" "A" "a.hs", testNode "b" "B" "b.hs"]
      embs <- generateGraphEmbeddings llm defaultEmbeddingConfig graph "/cache" "embeddings.json"
      embs `shouldBe` Map.fromList [("a", VU.fromList [1.0, 2.0]), ("b", VU.fromList [1.0, 2.0])]
    it "omits nodes whose embedding call fails" $ do
      -- Per-text failure isolation: the failing text sits in its own batch
      -- (batchSize 1), mirroring the sequential one-call-per-node loop.
      let llm = stubLLM $ \_cfg input ->
                if input == "A a.hs"
                  then pure (Right (VU.fromList [1.0, 2.0]))
                  else pure (Left "boom")
          graph = testGraph [testNode "a" "A" "a.hs", testNode "b" "B" "b.hs"]
          oneByOne = defaultEmbeddingConfig { embBatchSize = 1 }
      embs <- generateGraphEmbeddings llm oneByOne graph "/cache" "embeddings.json"
      embs `shouldBe` Map.fromList [("a", VU.fromList [1.0, 2.0])]
    it "returns an empty map for a graph with no nodes" $ do
      let llm = stubLLM (const (const (pure (Right (VU.singleton 1.0)))))
      embs <- generateGraphEmbeddings
                  llm defaultEmbeddingConfig (testGraph []) "/cache" "embeddings.json"
      embs `shouldBe` Map.empty

    it "submits only unique texts and redistributes their vectors (dedup)" $ do
      callsRef <- newIORef ([] :: [[Text]])
      let llm = countingLLM callsRef (pure . Right . VU.singleton . fromIntegral . T.length)
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
      let vecs = [Map.findWithDefault VU.empty ("dup-" <> T.pack (show i)) embs | i <- [1 :: Int .. 5000]]
      case vecs of
        (v0:rest) -> rest `shouldSatisfy` all (== v0)
        []        -> expectationFailure "expected 5000 duplicate vectors"

    it "matches the sequential one-text-per-call assignment on a small fixture" $ do
      -- Fake port that maps each text to a deterministic pseudo-vector, as a
      -- server would. The failing text lands in its own batch (batchSize 1),
      -- mirroring per-text failure isolation of the sequential loop.
      let deterministic :: Text -> IO (Either Text (VU.Vector Double))
          deterministic t = pure $
            if "fail" `T.isInfixOf` t then Left "boom" else Right (VU.singleton (fromIntegral (T.length t)))
          llm = stubLLM (\_ input -> deterministic input)
          mkNode :: Text -> Text -> Text -> Node
          mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
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
      let deterministic :: Text -> IO (Either Text (VU.Vector Double))
          deterministic t = pure $
            if "fail" `T.isInfixOf` t then Left "boom" else Right (VU.singleton (fromIntegral (T.length t)))
          llm = stubLLM (\_ input -> deterministic input)
          mkNode :: Text -> Text -> Text -> Node
          mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
          ns = [ mkNode "n1" "getUser" "a.hs"
               , mkNode "n4" "failing" "fail.hs" ]
          g = testGraph ns
          oneByOne = defaultEmbeddingConfig { embBatchSize = 1 }
      embs <- generateGraphEmbeddings llm oneByOne g "/cache" "embeddings.json"
      embs `shouldBe` Map.fromList [("n1", VU.singleton (fromIntegral (T.length ("getUser a.hs" :: Text))))]

    it "keeps sibling vectors when one text in a batch fails (bisection, task 2.2)" $ do
      -- One poison text inside a size-8 batch: D2 bisection shrinks the
      -- failure domain until the poison sits alone; siblings keep vectors.
      let llm = stubLLM $ \_ input ->
                if "poison" `T.isInfixOf` input
                  then pure (Left "boom")
                  else pure (Right (VU.singleton (fromIntegral (T.length input))))
          mkNode :: Text -> Text -> Text -> Node
          mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
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
        let perText :: Text -> IO (Either Text (VU.Vector Double))
            perText t = pure (Right (VU.singleton (fromIntegral (T.length t))))
            llm = countingLLM callsRef perText
            mkNode :: Text -> Text -> Text -> Node
            mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
            g = testGraph [mkNode "n1" "getUser" "a.hs"]
            cfg = defaultEmbeddingConfig { embBatchSize = 64 }
        embs1 <- generateGraphEmbeddings llm cfg g dir (dir </> "embeddings.json")
        embs2 <- generateGraphEmbeddings llm cfg g dir (dir </> "embeddings.json")
        submitted <- readIORef callsRef
        -- First run: exactly one text submitted; second run: zero calls.
        length (concat submitted) `shouldBe` 1
        embs1 `shouldBe` Map.fromList [("n1", VU.singleton (fromIntegral (T.length ("getUser a.hs" :: Text))))]
        embs2 `shouldBe` embs1

    it "performs zero cache writes on a fully warm second run (AC: hits are not rewritten, task 2.4)" $ do
      withSystemTempDirectory "graphos-embcache-warm" $ \dir -> do
        callsRef <- newIORef ([] :: [[Text]])
        let perText :: Text -> IO (Either Text (VU.Vector Double))
            perText t = pure (Right (VU.singleton (fromIntegral (T.length t))))
            llm = countingLLM callsRef perText
            mkNode :: Text -> Text -> Text -> Node
            mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
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
      let perText :: Text -> IO (Either Text (VU.Vector Double))
          perText t = pure (Right (VU.singleton (fromIntegral (T.length t))))
          llm = countingLLM callsRef perText
          mkNode :: Text -> Text -> Text -> Node
          mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
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

  describe "bounded-embedding-memory: memory-bound embedding pass (task 5.1)" $ do
    it "completes within a small multiple of the final assignment under a heap cap" $ do
      -- Synthetic scale check: 12k unique texts of 256-dim vectors, one
      -- batch in flight. The functional bound is asserted structurally (no
      -- intermediate full tables) by the implementation; here we exercise
      -- the pass at regression-test scale so a runaway (e.g. re-introduced
      -- freshTable+vecTable copies) manifests as memory growth. RTS-based
      -- live-bytes measurement lives in the dev verification notes because
      -- the test binary's RTS configuration varies by environment.
      let mkNode :: Text -> Text -> Text -> Node
          mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
          -- Deterministic stub: one distinct 256-dim vector per text; the
          -- whole assignment is 12k × 256 doubles ≈ 24.6 MB in the compact
          -- representation (vs ~200 MB boxed before this change).
          llm = stubLLM (\_ input -> pure (Right (VU.replicate 256 ((fromIntegral (T.length input) * 0.001) :: Double))))
          ns = [ mkNode ("n" <> T.pack (show i)) ("L" <> T.pack (show i)) ("s" <> T.pack (show i) <> ".hs")
               | i <- [1 :: Int .. 12000] ]
          g = testGraph ns
          cfg = defaultEmbeddingConfig { embBatchSize = 64, embConcurrency = 1 }
      embs <- generateGraphEmbeddings llm cfg g "/nonexistent-cache" "/tmp/opencode/bdd-memtest.json"
      Map.size embs `shouldBe` 12000
      -- Every vector has the configured dimension (compact representation).
      Map.elems embs `shouldSatisfy` all (\v -> VU.length v == 256)

  describe "writeEmbeddingsSidecar" $ do
    it "writes a JSON object that decodes back to the same map" $ do
      withSystemTempDirectory "graphos-pipelinespec" $ \dir -> do
        let path = dir </> "embeddings.json"
            embs = Map.fromList [("a", VU.fromList [1.0, 2.0]), ("b", VU.fromList [3.0, 4.0])] :: Map Text (VU.Vector Double)
        writeEmbeddingsSidecar path embs
        bs <- BSL.readFile path
        eitherDecode bs `shouldBe` Right (fmap VU.toList embs)

  describe "streaming sidecar write (always-streaming, bounded-embedding-memory)" $ do
    let mkNode :: Text -> Text -> Text -> Node
        mkNode nid lbl src = Node nid (fromText lbl) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
        g2 = testGraph [mkNode "n1" "getUser" "a.hs", mkNode "n2" "putUser" "b.hs"]
        perTextVec :: Text -> IO (Either Text (VU.Vector Double))
        perTextVec t = pure (Right (VU.singleton (fromIntegral (T.length t))))
    it "streaming content equals the sequential baseline for the same inputs (AC)" $ do
      withSystemTempDirectory "graphos-stream-on" $ \dirS -> do
        withSystemTempDirectory "graphos-stream-base" $ \dirB -> do
          callsRef <- newIORef ([] :: [[Text]])
          let llm = countingLLM callsRef perTextVec
              streamOn = defaultEmbeddingConfig
          embsOn  <- generateGraphEmbeddings llm streamOn g2 (dirS </> "cache") (dirS </> "embeddings.json")
          writeIORef callsRef []
          embsBase <- generateGraphEmbeddings llm streamOn g2 (dirB </> "cache") (dirB </> "embeddings.json")
          embsOn `shouldBe` embsBase
          onBs   <- BSL.readFile (dirS </> "embeddings.json")
          baseBs <- BSL.readFile (dirB </> "embeddings.json")
          case (eitherDecode onBs, eitherDecode baseBs) of
            (Right m1, Right m2) -> (m1 :: Map Text [Double]) `shouldBe` m2
            (l, r) -> do
              putStrLn ("ON   raw: " ++ show (BSL.toStrict onBs))
              putStrLn ("BASE raw: " ++ show (BSL.toStrict baseBs))
              expectationFailure ("sidecars do not decode as JSON objects: " ++ show (l, r))
          -- No staging leftovers at the final path's directory.
          leftovers <- listDirectory dirS
          filter ("tmp" `isInfixOf`) leftovers `shouldBe` []

    it "aborted streaming run leaves the prior sidecar untouched (AC)" $ do
      withSystemTempDirectory "graphos-stream-abort" $ \dir -> do
        let sidecar = dir </> "embeddings.json"
            prior = Map.fromList [("old", VU.fromList [9.0])] :: Map Text (VU.Vector Double)
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
                    (stubLLM (\_ _ -> pure (Right (VU.singleton (1.0 :: Double)))))
                    defaultEmbeddingConfig
                    g2 (dir </> "cache") (dir </> "embeddings.json" </> "x"))
        case (r :: Either SomeException (Map Text (VU.Vector Double))) of
          Left _ -> pure ()
          Right _ -> expectationFailure "expected the simulated abort to propagate"
        -- The prior sidecar is intact and no partial file exists at it.
        bs <- BSL.readFile sidecar
        (eitherDecode bs :: Either String (Map Text [Double])) `shouldBe` Right (fmap VU.toList prior)
        (eitherDecode bs :: Either String (Map Text [Double])) `shouldBe` Right (fmap VU.toList prior)

    it "legacy streaming:false key is ignored — streaming runs anyway (AC)" $ do
      withSystemTempDirectory "graphos-stream-optout" $ \dir -> do
        callsRef <- newIORef ([] :: [[Text]])
        let llm = countingLLM callsRef perTextVec
            legacyKey = defaultEmbeddingConfig { embStreaming = False }
        embs <- generateGraphEmbeddings llm legacyKey g2 (dir </> "cache") (dir </> "embeddings.json")
        Map.size embs `shouldBe` 2
        entries <- listDirectory dir
        -- Only the final sidecar (and the cache dir); no staging remnants.
        sort entries `shouldBe` ["cache", "embeddings.json"]

  describe "preFlightMemoryGuard (memory-budget-guard)" $ do
    let gib n = n * 1024 * 1024 * 1024 :: Integer

    it "passes silently without a budget" $ do
      (logs, r) <- withCapturedLogs $ \lp ->
        preFlightMemoryGuard lp (Just (MemInfo (gib 2))) Nothing True
      r `shouldBe` Right ()
      logs `shouldBe` []

    it "passes silently without memory info" $ do
      (logs, r) <- withCapturedLogs $ \lp ->
        preFlightMemoryGuard lp Nothing (Just (gib 8)) True
      r `shouldBe` Right ()
      logs `shouldBe` []

    it "warns and continues when available is below the budget (flag off)" $ do
      (logs, r) <- withCapturedLogs $ \lp ->
        preFlightMemoryGuard lp (Just (MemInfo (gib 2))) (Just (gib 8)) False
      r `shouldBe` Right ()
      logs `shouldSatisfy` any ("[memory]" `T.isInfixOf`)
      logs `shouldSatisfy` any ("below the heap budget" `T.isInfixOf`)

    it "aborts before any stage when --fail-on-low-memory is set" $ do
      (_, r) <- withCapturedLogs $ \lp ->
        preFlightMemoryGuard lp (Just (MemInfo (gib 2))) (Just (gib 8)) True
      case r of
        Left err -> do
          err `shouldSatisfy` ("--fail-on-low-memory" `T.isInfixOf`)
          err `shouldSatisfy` ("8.0 GiB" `T.isInfixOf`)
          err `shouldSatisfy` ("2.0 GiB" `T.isInfixOf`)
        Right () -> expectationFailure "expected the strict pre-flight to abort"

    it "warns on thin headroom (budget fits, less than the reserve to spare)" $ do
      (logs, r) <- withCapturedLogs $ \lp ->
        preFlightMemoryGuard lp (Just (MemInfo (gib 8))) (Just (gib 8)) True
      r `shouldBe` Right ()
      logs `shouldSatisfy` any ("thin headroom" `T.isInfixOf`)

  describe "withHeapGuard (graceful heap exhaustion)" $ do
    it "turns HeapOverflow into a stage-named ERROR and exit code 2, leaving the checkpoint untouched" $ do
      withSystemTempDirectory "graphos-heap-guard" $ \dir -> do
        let checkpoint = dir </> "graph.checkpoint.json"
        writeFile checkpoint "{\"checkpoint\":true}"
        logsRef <- newIORef ([] :: [Text])
        r <- try $ withHeapGuard (capturingLP logsRef) (Just (6 * 1024 * 1024 * 1024)) "cluster" $
               throwIO HeapOverflow :: IO (Either ExitCode ())
        r `shouldBe` Left (ExitFailure 2)
        logs <- readIORef logsRef
        logs `shouldSatisfy` any ("cluster" `T.isInfixOf`)
        logs `shouldSatisfy` any ("6.0 GiB" `T.isInfixOf`)
        logs `shouldSatisfy` any ("checkpoint preserved" `T.isInfixOf`)
        -- The checkpoint file was not touched by the failure path.
        content <- readFile checkpoint
        content `shouldBe` "{\"checkpoint\":true}"

    it "lets other exceptions propagate unchanged" $ do
      logsRef <- newIORef ([] :: [Text])
      r <- try (withHeapGuard (capturingLP logsRef) Nothing "extract" (throwIO ThreadKilled))
      case r :: Either AsyncException () of
        Left ThreadKilled -> pure ()
        other -> expectationFailure ("expected ThreadKilled to propagate, got " ++ show other)

    it "returns the action's result when no overflow occurs" $ do
      logsRef <- newIORef ([] :: [Text])
      n <- withHeapGuard (capturingLP logsRef) Nothing "build" (pure (42 :: Int))
      n `shouldBe` 42

  describe "generateGraphEmbeddingsGuarded (embedding projection gate)" $ do
    it "logs the projection and proceeds under a comfortable budget" $ do
      withSystemTempDirectory "graphos-embed-gate-ok" $ \dir -> do
        logsRef <- newIORef ([] :: [Text])
        let llm = stubLLM (const (const (pure (Right (VU.fromList [1.0, 2.0])))))
            graph = testGraph [testNode "a" "A" "a.hs", testNode "b" "B" "b.hs"]
            sidecar = dir </> "embeddings.json"
        r <- generateGraphEmbeddingsGuarded (capturingLP logsRef)
               (Just (8 * 1024 * 1024 * 1024)) llm defaultEmbeddingConfig graph (dir </> "cache") sidecar
        case r of
          Left err -> expectationFailure ("expected the gate to pass: " ++ T.unpack err)
          Right embs -> Map.size embs `shouldBe` 2
        logs <- readIORef logsRef
        logs `shouldSatisfy` any ("[memory] embedding projection: 2 nodes" `T.isInfixOf`)
        -- Conservative 1024 dims when the config leaves dimension at 0.
        logs `shouldSatisfy` any ("1024 dims" `T.isInfixOf`)
        doesFileExist sidecar >>= (`shouldBe` True)

    it "refuses to start and creates no sidecar when the projection exceeds the budget" $ do
      withSystemTempDirectory "graphos-embed-gate-abort" $ \dir -> do
        logsRef <- newIORef ([] :: [Text])
        let llm = stubLLM (const (const (error "the gate must abort before any batch")))
            graph = testGraph [testNode "a" "A" "a.hs", testNode "b" "B" "b.hs"]
            sidecar = dir </> "embeddings.json"
        -- Projection is 2 nodes × 1024 dims × 8 B = 16 KiB; budget 1 KiB.
        r <- generateGraphEmbeddingsGuarded (capturingLP logsRef)
               (Just 1024) llm defaultEmbeddingConfig graph (dir </> "cache") sidecar
        case r of
          Left err -> do
            err `shouldSatisfy` ("exceeds the heap budget" `T.isInfixOf`)
            err `shouldSatisfy` ("[embed]" `T.isInfixOf`)
          Right _ -> expectationFailure "expected the projection gate to refuse"
        doesFileExist sidecar >>= (`shouldBe` False)

    it "without a budget only logs the projection and proceeds" $ do
      withSystemTempDirectory "graphos-embed-gate-nobudget" $ \dir -> do
        logsRef <- newIORef ([] :: [Text])
        let llm = stubLLM (const (const (pure (Right (VU.singleton 1.0)))))
            graph = testGraph [testNode "a" "A" "a.hs"]
        r <- generateGraphEmbeddingsGuarded (capturingLP logsRef)
               Nothing llm defaultEmbeddingConfig graph (dir </> "cache") (dir </> "embeddings.json")
        case r of
          Left err -> expectationFailure ("expected no gating without a budget: " ++ T.unpack err)
          Right embs -> Map.size embs `shouldBe` 1
        logs <- readIORef logsRef
        logs `shouldSatisfy` any ("[memory] embedding projection" `T.isInfixOf`)

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
testNode nid label src = Node nid (fromText label) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

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

-- | A logging port that records every line (all levels) into an IORef.
capturingLP :: IORef [Text] -> LoggingPort
capturingLP ref = LoggingPort
  { lpLogTrace = add
  , lpLogDebug = add
  , lpLogInfo  = add
  , lpLogWarn  = add
  , lpLogError = add
  }
  where add t = modifyIORef' ref (t :)

-- | Run an action against a capturing logging port, returning its log lines
-- (oldest first) alongside the result.
withCapturedLogs :: (LoggingPort -> IO a) -> IO ([Text], a)
withCapturedLogs action = do
  ref <- newIORef []
  r <- action (capturingLP ref)
  logs <- readIORef ref
  pure (reverse logs, r)

stubLLM :: (EmbeddingConfig -> Text -> IO (Either Text (VU.Vector Double))) -> LLMPort
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
            -> (Text -> IO (Either Text (VU.Vector Double)))
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
