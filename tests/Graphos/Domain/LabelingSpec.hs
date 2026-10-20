{-# LANGUAGE OverloadedStrings #-}

module Graphos.Domain.LabelingSpec where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Aeson (toJSON)
import Data.Text.Short (fromText)
import qualified Data.Set as Set

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph, gCompositions)
import Graphos.Domain.Labeling
  ( labelPrompt
  , batchCommunities
  , LabelCacheEntry(..)
  , MatchKind(..)
  , fingerprintOf
  , matchCommunity
  )
import Graphos.Domain.Community (CommunityComposition(..))

testNode :: Text -> Node
testNode nid = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "test.hs"
,   nodeSource = Nothing
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodePresentBits  = 0
  }

testDocNode :: Text -> Node
testDocNode nid = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = DocFile
  , nodeSourceFile   = fromText "test.md"
,   nodeSource = Nothing
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodePresentBits  = 0
  }

spec :: Spec
spec = do
  describe "labelPrompt" $ do
    it "includes 'concept' and 'unifies' in preamble when compositions available" $ do
      let ext = extractionFromLists [testNode "auth"] []
          g = buildGraph False ext
          commMap = Map.fromList [(0, ["auth"])]
          cohesion = Map.empty
          cids = [0 :: Int]
      let prompt = labelPrompt g commMap cohesion cids
      prompt `shouldSatisfy` (T.isInfixOf "CONCEPT")
      prompt `shouldSatisfy` (T.isInfixOf "unifies")

    it "splits top nodes into code and doc categories for mixed community" $ do
      let ext = extractionFromLists
            [ testNode "verifyToken"
            , testNode "AuthMiddleware"
            , testDocNode "JWT validation"
            , testDocNode "Auth flow"
            ]
            []
          g = buildGraph False ext
          compMap :: Map.Map Int CommunityComposition
          compMap = Map.fromList
            [ (0, CommunityComposition 2 2 0 Nothing 0.5 0)
            ]
          gWithComps = g { gCompositions = Just (toJSON compMap) }
          commMap = Map.fromList [(0, ["verifyToken", "AuthMiddleware", "JWT validation", "Auth flow"])]
          cohesion = Map.empty
          cids = [0 :: Int]
      let prompt = labelPrompt gWithComps commMap cohesion cids
      prompt `shouldSatisfy` (T.isInfixOf "Top code nodes:")
      prompt `shouldSatisfy` (T.isInfixOf "Top doc nodes:")

    it "shows only 'Top code nodes:' for pure-code community" $ do
      let ext = extractionFromLists
            [ testNode "verifyToken"
            , testNode "AuthMiddleware"
            ]
            []
          g = buildGraph False ext
          compMap :: Map.Map Int CommunityComposition
          compMap = Map.fromList
            [ (0, CommunityComposition 2 0 0 Nothing 0.0 0)
            ]
          gWithComps = g { gCompositions = Just (toJSON compMap) }
          commMap = Map.fromList [(0, ["verifyToken", "AuthMiddleware"])]
          cohesion = Map.empty
          cids = [0 :: Int]
      let prompt = labelPrompt gWithComps commMap cohesion cids
      prompt `shouldSatisfy` (T.isInfixOf "Top code nodes:")
      prompt `shouldSatisfy` (not . T.isInfixOf "Top doc nodes:")

    it "shows composition line with code/doc counts and edges" $ do
      let ext = extractionFromLists
            [ testNode "verifyToken"
            , testNode "AuthMiddleware"
            , testDocNode "JWT validation"
            , testDocNode "Auth flow"
            ]
            []
          g = buildGraph False ext
          compMap :: Map.Map Int CommunityComposition
          compMap = Map.fromList
            [ (0, CommunityComposition 2 2 0 Nothing 0.5 3)
            ]
          gWithComps = g { gCompositions = Just (toJSON compMap) }
          commMap = Map.fromList [(0, ["verifyToken", "AuthMiddleware", "JWT validation", "Auth flow"])]
          cohesion = Map.empty
          cids = [0 :: Int]
      let prompt = labelPrompt gWithComps commMap cohesion cids
      prompt `shouldSatisfy` (T.isInfixOf "composition:")
      prompt `shouldSatisfy` (T.isInfixOf "2 code")
      prompt `shouldSatisfy` (T.isInfixOf "2 docs")
      prompt `shouldSatisfy` (T.isInfixOf "3 code")

    it "falls back to flat format when compositions absent" $ do
      let ext = extractionFromLists
            [ testNode "verifyToken"
            , testNode "AuthMiddleware"
            , testDocNode "JWT validation"
            ]
            []
          g = buildGraph False ext
          gNoComps = g { gCompositions = Nothing }
          commMap = Map.fromList [(0, ["verifyToken", "AuthMiddleware", "JWT validation"])]
          cohesion = Map.empty
          cids = [0 :: Int]
      let prompt = labelPrompt gNoComps commMap cohesion cids
      prompt `shouldSatisfy` (T.isInfixOf "Top nodes:")
      prompt `shouldSatisfy` (not . T.isInfixOf "Top code nodes:")
      prompt `shouldSatisfy` (not . T.isInfixOf "Top doc nodes:")
      prompt `shouldSatisfy` (not . T.isInfixOf "composition:")

  describe "batchCommunities" $ do
    it "splits communities into batches of given size" $ do
      let cids = [1..7 :: Int]
      batchCommunities cids 3 `shouldBe` [[1,2,3],[4,5,6],[7]]

    it "returns empty list for empty input" $ do
      batchCommunities ([] :: [Int]) 5 `shouldBe` []

    it "returns empty list for size 0" $ do
      batchCommunities [1,2,3] 0 `shouldBe` []

  describe "fingerprintOf" $ do
    it "is stable across insertion order" $ do
      let a = Set.fromList ["a", "b", "c"]
          b = Set.fromList ["c", "a", "b"]
      fingerprintOf a `shouldBe` fingerprintOf b
  
    it "differs when members differ" $ do
      fingerprintOf (Set.fromList ["a", "b"])
        `shouldNotBe` fingerprintOf (Set.fromList ["a", "b", "c"])
  
  describe "matchCommunity" $ do
    let entry _cid model members label =
          LabelCacheEntry (fingerprintOf members) (Set.toList members) label model "2026-10-06T00:00:00Z"
  
    it "exact hit reuses the cached label with no LLM call" $ do
      let cache = [entry (0 :: Int) "gpt-4o" (Set.fromList ["a", "b"]) "Auth"]
          newC  = Set.fromList ["a", "b"]
      matchCommunity cache "gpt-4o" newC `shouldBe` Just (cache !! 0, ExactMatch)
  
    it "drift hit inherits label within bidirectional containment (0.9)" $ do
      let old = Set.fromList ["m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9"]
          new = Set.difference old (Set.fromList ["m0"]) <> Set.fromList ["n0"]
          cache = [entry (0 :: Int) "gpt-4o" old "Auth"]
      matchCommunity cache "gpt-4o" new `shouldBe` Just (cache !! 0, FuzzyMatch)
  
    it "split miss re-labels when containment from old fails against each half" $ do
      let old  = Set.fromList ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9"]
          half = Set.fromList ["x0", "x1", "x2", "x3", "x4"]
          cache = [entry (0 :: Int) "gpt-4o" old "Auth"]
      matchCommunity cache "gpt-4o" half `shouldBe` Nothing
  
    it "merge miss re-labels when new-side containment of either old fails" $ do
      let x  = Set.fromList ["x0", "x1", "x2", "x3", "x4"]
          y  = Set.fromList ["y0", "y1", "y2", "y3", "y4"]
          m  = x <> y
          cache = [entry (0 :: Int) "gpt-4o" x "AuthX", entry (1 :: Int) "gpt-4o" y "AuthY"]
      matchCommunity cache "gpt-4o" m `shouldBe` Nothing
  
    it "merge does not shadow exact-match lookups" $ do
      let x  = Set.fromList ["x0", "x1", "x2", "x3", "x4"]
          big = x <> Set.fromList ["y0", "y1", "y2", "y3", "y4"]  -- contains all of x
          cache = [entry (0 :: Int) "gpt-4o" x "AuthX"]
      -- the containing community has new-side ratio 0.5 -> no match
      matchCommunity cache "gpt-4o" big `shouldBe` Nothing
  
    it "tie-break by largest intersection then lowest community id" $ do
      let new = Set.fromList ["n0", "n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9"]
          a   = Set.difference new (Set.fromList ["n0"]) <> Set.fromList ["a0"]
          b   = Set.difference new (Set.fromList ["n1"]) <> Set.fromList ["b0"]
          -- both intersect new equally (9); entry 0 has the lower community id
          cache = [entry (0 :: Int) "gpt-4o" a "AuthA", entry (1 :: Int) "gpt-4o" b "AuthB"]
      matchCommunity cache "gpt-4o" new `shouldBe` Just (cache !! 0, FuzzyMatch)
  
    it "ignores entries recorded under a different model" $ do
      let cache = [entry (0 :: Int) "gpt-3.5" (Set.fromList ["a", "b"]) "Auth"]
          newC  = Set.fromList ["a", "b"]
      matchCommunity cache "gpt-4o" newC `shouldBe` Nothing
  
    it "exact match wins over any fuzzy match on the same entry" $ do
      let cache = [entry (0 :: Int) "gpt-4o" (Set.fromList ["a", "b"]) "Auth"]
          newC  = Set.fromList ["a", "b"]
      matchCommunity cache "gpt-4o" newC `shouldBe` Just (cache !! 0, ExactMatch)
  
    it "returns Nothing when the cache is empty" $ do
      matchCommunity ([] :: [LabelCacheEntry]) "gpt-4o" (Set.fromList ["a"]) `shouldBe` Nothing
  
    it "re-labels when containment ratio drops below the 0.8 boundary" $ do
      let old = Set.fromList ["x0", "x1", "x2", "x3", "x4"]
          new = Set.fromList ["x0", "x1", "x2"]  -- 3 of 5 = 0.6 < 0.8
          cache = [entry (0 :: Int) "gpt-4o" old "Auth"]
      matchCommunity cache "gpt-4o" new `shouldBe` Nothing

    it "inherits label when containment ratio sits exactly at the 0.8 boundary" $ do
      let old = Set.fromList ["x0", "x1", "x2", "x3", "x4"]
          new = Set.fromList ["x0", "x1", "x2", "x3"]  -- 4 of 5 = 0.8 >= 0.8
          cache = [entry (0 :: Int) "gpt-4o" old "Auth"]
      matchCommunity cache "gpt-4o" new `shouldBe` Just (cache !! 0, FuzzyMatch)
