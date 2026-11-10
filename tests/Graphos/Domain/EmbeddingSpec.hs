{-# LANGUAGE OverloadedStrings #-}
-- | Pure specs for Domain embedding preparation (change
-- lfm-embedding-optimization task 1.3).
module Graphos.Domain.EmbeddingSpec where

import qualified Data.Text as T

import Test.Hspec

import Graphos.Domain.Config (EmbeddingConfig(..), defaultEmbeddingConfig)
import Graphos.Domain.Embedding (effectiveTokenLimit, estimateTokens, prepare)

spec :: Spec
spec = do
  describe "effectiveTokenLimit" $ do
    it "returns the model-table limit for a known model when maxTokens is 0" $
      effectiveTokenLimit (mkCfg "LFM2.5-Embedding-350M-GGUF:Q4_K_M" 0) `shouldBe` 512

    it "returns the nomic table limit" $
      effectiveTokenLimit (mkCfg "nomic-embed-text" 0) `shouldBe` 8192

    it "returns the all-minilm table limit" $
      effectiveTokenLimit (mkCfg "all-minilm-l6-v2" 0) `shouldBe` 256

    it "maxTokens override wins over the model-table limit" $
      effectiveTokenLimit (mkCfg "nomic-embed-text" 256) `shouldBe` 256

    it "returns 0 (no truncation) for an unknown model without maxTokens" $
      effectiveTokenLimit (mkCfg "some-unknown-model" 0) `shouldBe` 0

  describe "prepare" $ do
    it "passes a short text through byte-identical (no prefix, under limit)" $
      prepare (mkCfg "nomic-embed-text" 0) "getUser a.hs"
        `shouldBe` "getUser a.hs"

    it "prepends docPrefix when configured" $
      prepare ((mkCfg "nomic-embed-text" 0) { embDocPrefix = "document: " })
              "getUser a.hs"
        `shouldBe` "document: getUser a.hs"

    it "truncates an over-limit text to <= effectiveTokenLimit tokens" $ do
      let cfg = mkCfg "LFM2.5-Embedding-350M" 0
          t = T.replicate 7000 "word "
          prepared = prepare cfg t
      estimateTokens prepared `shouldSatisfy` (<= effectiveTokenLimit cfg)

    it "maxTokens override wins over the model-table limit when preparing" $ do
      let cfg = (mkCfg "nomic-embed-text" 0) { embMaxTokens = 256 }
          t = T.replicate 10000 "word "
      estimateTokens (prepare cfg t) `shouldSatisfy` (<= 256)

    it "returns text unchanged for an unknown model without maxTokens" $ do
      let t = T.replicate 10000 "word "
      prepare (mkCfg "some-unknown-model" 0) t `shouldBe` t

    it "keeps the prefix even when the text itself is truncated" $ do
      let cfg = (mkCfg "LFM2.5-Embedding-350M" 0) { embDocPrefix = "document: " }
          t = T.replicate 7000 "word "
          prepared = prepare cfg t
      "document: " `T.isPrefixOf` prepared `shouldBe` True
      estimateTokens prepared `shouldSatisfy` (<= effectiveTokenLimit cfg)

-- ───────────────────────────────────────────────
-- Fixtures
-- ───────────────────────────────────────────────

mkCfg :: String -> Int -> EmbeddingConfig
mkCfg model maxTokens = defaultEmbeddingConfig
  { embModel = model
  , embMaxTokens = maxTokens
  }