{-# LANGUAGE OverloadedStrings #-}
-- | Tests for multi-source configuration types (SourceConfig).
module Graphos.Domain.Config.SourceSpec where

import Data.Aeson (eitherDecode, encode)
import Data.Text (Text)
import qualified Data.Text as T

import Test.Hspec
import Test.QuickCheck

import Graphos.Domain.Config.Source

spec :: Spec
spec = describe "Graphos.Domain.Config.Source" $ do

  describe "validSources" $ do

    it "accepts a well-formed list" $
      validSources [ SourceConfig "repoA" "/tmp/repoA" []
                   , SourceConfig "repoB" "/tmp/repoB" ["vendor"]
                   ] `shouldBe` Right [ SourceConfig "repoA" "/tmp/repoA" []
                                      , SourceConfig "repoB" "/tmp/repoB" ["vendor"]
                                      ]

    it "accepts an empty list" $
      validSources [] `shouldBe` Right []

    it "rejects duplicate names naming the offender" $ do
      let r = validSources [ SourceConfig "repoA" "/tmp/a" []
                           , SourceConfig "repoB" "/tmp/b" []
                           , SourceConfig "repoA" "/tmp/c" []
                           ]
      case r of
        Left err -> err `shouldContain` "repoA"
        Right _  -> expectationFailure "expected Left"

    it "rejects empty names naming the path" $ do
      let r = validSources [SourceConfig "" "/tmp/x" []]
      case r of
        Left err -> err `shouldContain` "/tmp/x"
        Right _  -> expectationFailure "expected Left"

    it "round-trips distinct names" $
      property $ \names ->
        let unique :: [Text]
            unique = filter (not . T.null) (map T.pack (nub' names))
            srcs = [ SourceConfig n (T.unpack n) [] | n <- unique ]
        in validSources srcs == Right srcs

  describe "mkSourceConfig" $ do

    it "rejects an empty name" $
      mkSourceConfig "" "/tmp/x" [] `shouldBe` Left "source name must not be empty"

    it "constructs with a valid name" $
      mkSourceConfig "wt" "~/wt" ["gen"] `shouldBe` Right (SourceConfig "wt" "~/wt" ["gen"])

  describe "FromJSON SourceConfig" $ do

    it "parses the mapping form" $
      (eitherDecode "{ \"name\": \"repoA\", \"path\": \"/tmp/repoA\", \"ignore\": [\"vendor\"] }"
        :: Either String SourceConfig)
        `shouldBe` Right (SourceConfig "repoA" "/tmp/repoA" ["vendor"])

    it "parses the shorthand string form deriving the base name" $
      (eitherDecode "\"~/code/myrepo/\"" :: Either String SourceConfig)
        `shouldBe` Right (SourceConfig "myrepo" "~/code/myrepo/" [])

    it "parseShorthand' matches the shorthand FromJSON branch" $
      parseShorthand' "~/code/myrepo/"
        `shouldBe` SourceConfig "myrepo" "~/code/myrepo/" []

    it "parseShorthand' tolerates a trailing slash" $
      parseShorthand' "/tmp/repoA/"
        `shouldBe` SourceConfig "repoA" "/tmp/repoA/" []

    it "defaults ignore to empty when absent" $
      (eitherDecode "{ \"name\": \"x\", \"path\": \"/tmp/x\" }"
        :: Either String SourceConfig)
        `shouldBe` Right (SourceConfig "x" "/tmp/x" [])

    it "round-trips through toJSON" $
      (eitherDecode (encode $ SourceConfig "repoB" "/tmp/repoB" ["gen"])
        :: Either String SourceConfig)
        `shouldBe` Right (SourceConfig "repoB" "/tmp/repoB" ["gen"])

-- | Keep only the first occurrence of each element.
nub' :: [String] -> [String]
nub' = foldr (\x acc -> x : filter (/= x) acc) []