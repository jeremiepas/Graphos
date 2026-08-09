{-# LANGUAGE OverloadedStrings #-}
module Graphos.Domain.Types.IngestSpec where

import Test.Hspec
import Data.Aeson (decode, encode)
import qualified Data.Map.Strict as Map

import Graphos.Domain.Config.Ingest (FileEntry(..))
import Graphos.Domain.Types.Ingest

spec :: Spec
spec = do
  describe "IngestIndex v2 round-trip" $ do
    it "round-trips empty index with version 2" $ do
      decode (encode emptyIngestIndex) `shouldBe` Just emptyIngestIndex

    it "round-trips index with nodes and files" $ do
      let idx = emptyIngestIndex
            { iiFiles = Map.fromList
                [ ("src/Foo.hs", FileEntry "abc123" "2026-01-01T00:00:00Z")
                ]
            , iiNodes = Map.fromList
                [ ("node-1", [1.0, 2.0, 3.0])
                ]
            }
      decode (encode idx) `shouldBe` Just idx

  describe "IngestIndex v1 backward compatibility" $ do
    it "loads v1 format with version 1 and empty files" $ do
      let v1Json = "{\"nodes\": {\"node-1\": [1.0, 2.0]}}"
      case decode v1Json of
        Nothing -> expectationFailure "failed to decode v1 index"
        Just idx -> do
          iiVersion idx `shouldBe` 1
          iiFiles idx `shouldBe` Map.empty
          iiNodes idx `shouldBe` Map.fromList [("node-1", [1.0, 2.0])]

  describe "IngestIndex helpers" $ do
    it "lookupEmbedding returns stored vector" $ do
      let idx = emptyIngestIndex { iiNodes = Map.fromList [("node-1", [1.0, 2.0])] }
      lookupEmbedding "node-1" idx `shouldBe` Just [1.0, 2.0]
      lookupEmbedding "missing" idx `shouldBe` Nothing

    it "mergeIndex is right-biased for nodes and files" $ do
      let left = emptyIngestIndex
            { iiFiles = Map.fromList [("a.hs", FileEntry "old" "t1")]
            , iiNodes = Map.fromList [("n1", [1.0])]
            }
          right = emptyIngestIndex
            { iiFiles = Map.fromList [("a.hs", FileEntry "new" "t2")]
            , iiNodes = Map.fromList [("n1", [2.0])]
            }
          merged = mergeIndex left right
      iiFiles merged `shouldBe` Map.fromList [("a.hs", FileEntry "new" "t2")]
      iiNodes merged `shouldBe` Map.fromList [("n1", [2.0])]
