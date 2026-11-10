{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the content-cache eviction policy config (wire-incremental-update 2.1).
module Graphos.Domain.Config.CacheSpec where

import Data.Aeson (decode, encode)

import Test.Hspec

import Graphos.Domain.Config

spec :: Spec
spec = describe "Graphos.Domain.Config.Cache" $ do

  it "defaults to the 512 MB cap" $
    ccMaxBytes defaultCacheConfig `shouldBe` 512 * 1024 * 1024

  it "round-trips the default" $
    (decode (encode defaultCacheConfig) :: Maybe CacheConfig)
      `shouldBe` Just defaultCacheConfig

  it "parses explicit max_mb" $
    (decode "{\"max_mb\": 100}" :: Maybe CacheConfig)
      `shouldBe` Just (CacheConfig (100 * 1024 * 1024))

  it "parses 0 as unlimited (zero cap disables eviction)" $
    (decode "{\"max_mb\": 0}" :: Maybe CacheConfig)
      `shouldBe` Just (CacheConfig 0)

  it "serializes MB-aligned values back to the same cap" $
    (decode (encode (CacheConfig (100 * 1024 * 1024))) :: Maybe CacheConfig)
      `shouldBe` Just (CacheConfig (100 * 1024 * 1024))

  it "merges project over global when non-default" $
    gcCache (mergeGraphosConfig
               defaultGraphosConfig { gcCache = CacheConfig 0 }
               defaultGraphosConfig)
      `shouldBe` CacheConfig 0