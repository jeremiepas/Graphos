{-# LANGUAGE OverloadedStrings #-}
-- | Specs for the batched node-embedding path in UseCase.Ingest
-- (streaming-embeddings task 3.4): content-derived source hashes and
-- per-batch failure isolation.
module Graphos.UseCase.IngestSpec where

import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)

import Test.Hspec

import Graphos.Domain.Types (Node(..), FileType(..), IngestEmbedding(..))
import Graphos.Domain.Config (defaultEmbeddingConfig, EmbeddingConfig(..))
import Graphos.UseCase.AppEnv (AppEnv(..))
import Graphos.UseCase.Port.LLMPort (LLMPort(..))
import Graphos.UseCase.Port.LoggingPort (LoggingPort(..))
import Graphos.UseCase.Ingest (generateEmbeddingsForNodes)

testNode :: Text -> Text -> Text -> Node
testNode nid label src = Node nid (fromText label) CodeFile (fromText src) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0

-- | AppEnv whose only meaningful part is an LLMPort; every other port is
-- unused by 'generateEmbeddingsForNodes'.
stubEnv :: LLMPort -> AppEnv
stubEnv llm = AppEnv
  { extractionPort = error "not used"
  , exportPort = error "not used"
  , fileSystemPort = error "not used"
  , loggingPort = LoggingPort
      { lpLogTrace = const (pure ())
      , lpLogDebug = const (pure ())
      , lpLogInfo  = const (pure ())
      , lpLogWarn  = const (pure ())
      , lpLogError = const (pure ())
      }
  , observabilityPort = error "not used"
  , llmPort = llm
  }

-- | A port answering each text with @perText@, recording submitted batches.
fakeLLM :: IORef [[Text]] -> (Text -> IO (Either Text [Double])) -> LLMPort
fakeLLM callsRef perText = LLMPort
  { lpCallLLM = error "not used"
  , lpParseLabelsFromResponse = const Map.empty
  , lpGenerateEmbedding = \_ i -> perText i
  , lpGenerateEmbeddings = \_ ts -> do
      modifyIORef' callsRef (ts :)
      mapM perText ts >>= pure . mapM id
  , lpAnalyzeImage = error "not used"
  , lpValidateUrl = pure
  }

spec :: Spec
spec = do
  describe "generateEmbeddingsForNodes" $ do
    it "gives equal ieSourceHash to same-text nodes, distinct from any path" $ do
      callsRef <- newIORef []
      let llm = fakeLLM callsRef (pure . Right . (: []) . fromIntegral . T.length)
          env = stubEnv llm
          -- Two nodes sharing the same embedded text (label + source file
          -- are content-identical even if they live in different files);
          -- plus a node with a different text.
          ns = [ testNode "n1" "getUser" "a.hs"
               , testNode "n2" "getUser" "a.hs"
               , testNode "n3" "setUser" "c.hs" ]
          cfg = defaultEmbeddingConfig { embBatchSize = 1 }
      embs <- generateEmbeddingsForNodes env cfg ns
      case map ieSourceHash embs of
        [h1, h2, h3] -> do
          h1 `shouldBe` h2                   -- same text + model ⇒ equal hash
          h1 `shouldNotBe` h3                -- different text ⇒ different hash
          -- The hash is a SHA-256 hex digest, never a source file path.
          T.length h1 `shouldBe` 64
          h1 `shouldNotBe` "a.hs"
          h2 `shouldNotBe` "b.hs"
        _ -> expectationFailure "expected 3 embeddings"

    it "yields empty vectors for exactly the nodes of a failed batch" $ do
      callsRef <- newIORef []
      let perText :: Text -> IO (Either Text [Double])
          perText t = pure $ if "fail" `T.isInfixOf` t
                        then Left "boom"
                        else Right [fromIntegral (T.length t)]
          llm = fakeLLM callsRef perText
          env = stubEnv llm
          ns = [ testNode "n1" "getUser" "a.hs"
               , testNode "n4" "failing" "fail.hs" ]
          cfg = defaultEmbeddingConfig { embBatchSize = 1 }
      embs <- generateEmbeddingsForNodes env cfg ns
      case embs of
        [e1, e4] -> do
          ieNodeId e1 `shouldBe` "n1"
          ieVector e1 `shouldBe` [fromIntegral (T.length ("getUser a.hs" :: Text))]
          ieNodeId e4 `shouldBe` "n4"
          ieVector e4 `shouldBe` []          -- failed batch ⇒ metadata-only
          -- Even the failed entry carries the content hash, not the path.
          T.length (ieSourceHash e4) `shouldBe` 64
          ieSourceHash e4 `shouldNotBe` "fail.hs"
        _ -> expectationFailure "expected 2 embeddings"

    it "submits only unique texts and redistributes their vectors" $ do
      callsRef <- newIORef []
      let llm = fakeLLM callsRef (pure . Right . (: []) . fromIntegral . T.length)
          env = stubEnv llm
          dup1 = testNode "d1" "Dup" "x.hs"
          dup2 = testNode "d2" "Dup" "x.hs"
          other = testNode "o1" "Other" "z.hs"
          cfg = defaultEmbeddingConfig { embBatchSize = 64 }
      embs <- generateEmbeddingsForNodes env cfg [dup1, dup2, other]
      submitted <- readIORef callsRef
      length (concat submitted) `shouldBe` 2
      case map ieVector embs of
        [vd1, vd2, vo] -> do
          vd1 `shouldBe` vd2
          vd1 `shouldNotBe` vo
        _ -> expectationFailure "expected 3 vectors"