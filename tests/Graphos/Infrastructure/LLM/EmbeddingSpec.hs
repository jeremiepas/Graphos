{-# LANGUAGE OverloadedStrings #-}
-- | Pure response-parser specs for the batched embedding client
-- (streaming-embeddings task 2.2): order permutation, missing 'data',
-- wrong arity, non-permutation indices.
module Graphos.Infrastructure.LLM.EmbeddingSpec where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), object)
import Data.Either (isLeft)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T

import Test.Hspec

import Graphos.Domain.Config (EmbeddingConfig(..), defaultEmbeddingConfig)
import Graphos.Infrastructure.LLM.Embedding (parseEmbeddingsResponse, truncateForEmbedding, prepareEmbed)

-- | Build an OpenAI-compatible batch response body.
respBody :: [Int] -> [[Double]] -> Aeson.Value
respBody indices vecs = object
  [ "object" .= ("list" :: Text)
  , "data"   .= Aeson.Array (V.fromList
        [ object
            [ "object"    .= ("embedding" :: Text)
            , "index"     .= i
            , "embedding" .= v
            ]
        | (i, v) <- zip indices vecs
        ])
  ]

spec :: Spec
spec = do
  describe "parseEmbeddingsResponse" $ do
    let n = 3

    it "returns vectors in input order for an in-order response" $ do
      let r = respBody [0, 1, 2] [[1], [2], [3]] :: Aeson.Value
          expected = Right [[1], [2], [3]] :: Either Text [[Double]]
      parseEmbeddingsResponse n r `shouldBe` expected

    it "sorts by 'index' when data arrives out of order (permutation)" $ do
      let r = respBody [2, 0, 1] [[3], [1], [2]] :: Aeson.Value
          expected = Right [[1], [2], [3]] :: Either Text [[Double]]
      parseEmbeddingsResponse n r `shouldBe` expected

    it "rejects a response whose indices are not a permutation of [0..n-1]" $ do
      let r = respBody [0, 1, 1] [[1], [2], [3]] :: Aeson.Value
      parseEmbeddingsResponse n r `shouldSatisfy` isLeft

    it "rejects a response with a wrong-arity data array" $ do
      let r = respBody [0, 1] [[1], [2]] :: Aeson.Value
      parseEmbeddingsResponse n r `shouldSatisfy` isLeft

    it "rejects a response without a 'data' key" $ do
      parseEmbeddingsResponse n (object ["object" .= ("list" :: Text)])
        `shouldSatisfy` isLeft

    it "rejects an item missing the 'embedding' array" $ do
      let r = object
            [ "data" .= Aeson.Array (V.fromList
                [ object ["object" .= ("embedding" :: Text), "index" .= (0 :: Int)]
                ])
            ]
      parseEmbeddingsResponse 1 r `shouldSatisfy` isLeft

    it "rejects an item missing the 'index' field" $ do
      let r = object
            [ "data" .= Aeson.Array (V.fromList
                [ object ["object" .= ("embedding" :: Text), "embedding" .= [1.0 :: Double]]
                ])
            ]
      parseEmbeddingsResponse 1 r `shouldSatisfy` isLeft

    it "accepts a singleton batch (n = 1) and projects to one vector" $ do
      let r = respBody [0] [[7.5]] :: Aeson.Value
          expected = Right [[7.5]] :: Either Text [[Double]]
      parseEmbeddingsResponse 1 r `shouldBe` expected

    it "returns an empty result for an empty input batch" $ do
      let r = object ["data" .= Aeson.Array V.empty]
          expected = Right [] :: Either Text [[Double]]
      parseEmbeddingsResponse 0 r `shouldBe` expected

    it "rejects non-numeric entries inside an embedding array" $ do
      let r = object
            [ "data" .= Aeson.Array (V.fromList
                [ object
                    [ "object"    .= ("embedding" :: Text)
                    , "index"     .= (0 :: Int)
                    , "embedding" .= [Aeson.Number 1.0, Aeson.String "x"]
                    ]
                ])
            ]
      parseEmbeddingsResponse 1 r `shouldSatisfy` isLeft

  describe "truncateForEmbedding" $ do
    -- Budget at 512 tokens is floor(512 / 1.33) = 384 words.
    let budget512 = 384 :: Int

    it "leaves a short text unchanged" $
      truncateForEmbedding 512 "hello world" `shouldBe` "hello world"

    it "leaves a text at exactly the word budget unchanged" $ do
      let t = T.unwords (replicate budget512 "w")
      truncateForEmbedding 512 t `shouldBe` t

    it "truncates an over-budget text to the word budget" $ do
      let t = T.unwords (replicate 6628 "w")
      length (T.words (truncateForEmbedding 512 t)) `shouldBe` budget512

    it "keeps the leading words (prefix), not a suffix" $ do
      let t = T.unwords (map (T.pack . show) [1 .. 1000 :: Int])
      T.words (truncateForEmbedding 512 t) `shouldBe`
        map (T.pack . show) [1 .. budget512]

    it "disables truncation when maxTokens is 0" $ do
      let t = T.unwords (replicate 10000 "w")
      truncateForEmbedding 0 t `shouldBe` t

    it "disables truncation for a negative budget" $ do
      let t = T.unwords (replicate 10000 "w")
      truncateForEmbedding (-1) t `shouldBe` t

    it "keeps at least one word for a tiny positive budget" $
      truncateForEmbedding 1 "alpha beta gamma" `shouldBe` "alpha"

  describe "prepareEmbed (task 2.1: prepared texts are what the API receives)" $ do
    let cfg = defaultEmbeddingConfig { embModel = "LFM2.5-Embedding-350M"
                                     , embMaxTokens = 0 }
    it "passes a normal text through unchanged (what dedup/caller sees)" $
      prepareEmbed cfg "getUser a.hs" `shouldBe` "getUser a.hs"

    it "prepends docPrefix exactly as the payload would carry it" $
      prepareEmbed cfg { embDocPrefix = "document: " } "getUser a.hs"
        `shouldBe` "document: getUser a.hs"

    it "truncates an over-limit text so the API payload never exceeds the limit" $ do
      let t = T.replicate 7000 "word "
          prepared = prepareEmbed cfg t
      T.length prepared `shouldSatisfy` (<= 4 * 512)