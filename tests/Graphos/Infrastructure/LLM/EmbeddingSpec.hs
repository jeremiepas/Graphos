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

import Test.Hspec

import Graphos.Infrastructure.LLM.Embedding (parseEmbeddingsResponse)

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