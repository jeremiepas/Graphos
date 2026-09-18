{-# LANGUAGE OverloadedStrings #-}
-- | Specs for the content-addressed embedding cache (streaming-embeddings
-- task 4.1): save/load round-trip, corrupt-file miss, model-change miss.
module Graphos.Infrastructure.LLM.EmbeddingCacheSpec where

import qualified Data.ByteString.Lazy as BSL
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import Graphos.Infrastructure.LLM.EmbeddingCache
  ( cacheKey, loadVector, saveVector )

spec :: Spec
spec = do
  describe "EmbeddingCache" $ do
    it "round-trips a saved vector (save then load)" $ do
      withSystemTempDirectory "graphos-embcache" $ \dir -> do
        saveVector dir "nomic-embed-text" "hello world" [1.5, -2.0, 0.0]
        v <- loadVector dir "nomic-embed-text" "hello world"
        v `shouldBe` Just [1.5, -2.0, 0.0 :: Double]

    it "treats an undecodable (corrupt) file as a miss" $ do
      withSystemTempDirectory "graphos-embcache-corrupt" $ \dir -> do
        saveVector dir "m" "t" [1.0]
        -- Overwrite the entry with garbage (simulates a truncated write).
        BSL.writeFile (dir ++ "/embeddings/" ++ cacheKey "m" "t" ++ ".json") "{corrupt"
        v <- loadVector dir "m" "t"
        v `shouldBe` Nothing

    it "yields a different key (miss) for a different model" $ do
      withSystemTempDirectory "graphos-embcache-model" $ \dir -> do
        saveVector dir "model-a" "same text" [1.0]
        vOther <- loadVector dir "model-b" "same text"
        vOther `shouldBe` Nothing
        vSame <- loadVector dir "model-a" "same text"
        vSame `shouldNotBe` Nothing

    it "returns Nothing for an absent key (pure miss)" $ do
      withSystemTempDirectory "graphos-embcache-absent" $ \dir -> do
        v <- loadVector dir "no-model" "no text" :: IO (Maybe [Double])
        v `shouldBe` Nothing

    it "derives distinct keys from distinct texts and from paths" $ do
      cacheKey "m" "t1" `shouldNotBe` cacheKey "m" "t2"
      cacheKey "m1" "t" `shouldNotBe` cacheKey "m2" "t"
      cacheKey "m" "t" `shouldNotBe` "t"
      cacheKey "m" "t" `shouldNotBe` "m"