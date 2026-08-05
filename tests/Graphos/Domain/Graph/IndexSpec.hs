module Graphos.Domain.Graph.IndexSpec where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph, gNodes)
import Graphos.Domain.Graph.Index

pathNode :: Text -> Text -> Node
pathNode nid src = Node
  { nodeId           = nid
  , nodeLabel        = nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = src
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineStart    = Just 1
  , nodeLineEnd      = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  }

spec :: Spec
spec = do
  describe "buildPathIndex" $ do
    it "indexes source file path segments" $ do
      let ext = extractionFromLists
            [ pathNode "n1" "src/cli/commands.hs"
            , pathNode "n2" "src/server/main.hs"
            ]
            []
          g = buildGraph False ext
          idx = buildIndex g Map.empty
      lookupPath "cli" idx `shouldSatisfy` (not . null)
      lookupPath "commands.hs" idx `shouldSatisfy` (not . null)

    it "indexes full lowercased paths" $ do
      let ext = extractionFromLists
            [ pathNode "n1" "src/cli/commands.hs"
            ]
            []
          g = buildGraph False ext
          idx = buildIndex g Map.empty
      lookupPath "src/cli/commands.hs" idx `shouldSatisfy` (not . null)

    it "returns empty for non-existent segments" $ do
      let ext = extractionFromLists
            [ pathNode "n1" "src/cli/commands.hs"
            ]
            []
          g = buildGraph False ext
          idx = buildIndex g Map.empty
      lookupPath "nonexistent" idx `shouldBe` []

  describe "pathGlobFilter" $ do
    it "filters nodes by glob with ** matching any depth" $ do
      let n1 = pathNode "n1" "src/cli/commands.hs"
          n2 = pathNode "n2" "src/server/main.hs"
          n3 = pathNode "n3" "lib/core.hs"
          ext = extractionFromLists [n1, n2, n3] []
          g = buildGraph False ext
          nodeMap = gNodes g
          candidates = Set.fromList ["n1", "n2", "n3"]
          result = pathGlobFilter nodeMap "src/**" candidates
      result `shouldBe` Set.fromList ["n1", "n2"]

    it "filters nodes by glob with * matching single segment" $ do
      let n1 = pathNode "n1" "src/main.hs"
          n2 = pathNode "n2" "lib/core.hs"
          ext = extractionFromLists [n1, n2] []
          g = buildGraph False ext
          nodeMap = gNodes g
          candidates = Set.fromList ["n1", "n2"]
          result = pathGlobFilter nodeMap "src/*.hs" candidates
      result `shouldBe` Set.fromList ["n1"]

    it "returns empty when no nodes match glob" $ do
      let n1 = pathNode "n1" "lib/core.hs"
          ext = extractionFromLists [n1] []
          g = buildGraph False ext
          nodeMap = gNodes g
          candidates = Set.fromList ["n1"]
          result = pathGlobFilter nodeMap "src/**" candidates
      result `shouldBe` Set.empty

  describe "matchGlob" $ do
    it "matches exact paths" $ do
      matchGlob "src/main.hs" "src/main.hs" `shouldBe` True

    it "matches ** against any depth" $ do
      matchGlob "src/**" "src/cli/commands.hs" `shouldBe` True
      matchGlob "**/commands.hs" "src/cli/commands.hs" `shouldBe` True
      matchGlob "src/**/main.hs" "src/cli/main.hs" `shouldBe` True

    it "matches * against single segment" $ do
      matchGlob "src/*.hs" "src/main.hs" `shouldBe` True
      matchGlob "src/*" "src/main.hs" `shouldBe` True

    it "rejects non-matching paths" $ do
      matchGlob "src/**" "lib/core.hs" `shouldBe` False
      matchGlob "src/*.hs" "src/cli/main.hs" `shouldBe` False

    it "matches ** between segments" $ do
      matchGlob "src/**/main.hs" "src/a/b/c/main.hs" `shouldBe` True