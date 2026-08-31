module Graphos.Domain.LabelingSpec where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Aeson (toJSON)
import Data.Text.Short (fromText)

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph, gCompositions)
import Graphos.Domain.Labeling (labelPrompt, batchCommunities)
import Graphos.Domain.Community (CommunityComposition(..))

testNode :: Text -> Node
testNode nid = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "test.hs"
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
