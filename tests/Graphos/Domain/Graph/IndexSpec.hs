{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Graphos.Domain.Graph.IndexSpec where

import Test.Hspec
import Test.QuickCheck hiding (Confidence)
import Data.Text (Text)
import Data.Text.Short (toText)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text.Short (fromText)

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph, gNodes)
import Graphos.Domain.Graph.Index

pathNode :: Text -> Text -> Node
pathNode nid src = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText src
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

  describe "buildLabelIndex / buildPathIndex order-insensitive equivalence" $ do
    it "buildLabelIndex is order-insensitively equal to reference" $ property $
      \(nm :: NodeMap) -> Map.map Set.fromList (buildLabelIndex (unNodeMap nm))
             == Map.map Set.fromList (buildLabelIndexRef (unNodeMap nm))
    it "buildPathIndex is order-insensitively equal to reference" $ property $
      \(nm :: NodeMap) -> Map.map Set.fromList (buildPathIndex (unNodeMap nm))
             == Map.map Set.fromList (buildPathIndexRef (unNodeMap nm))
    it "findMatchingNodes identical for both index implementations" $ property $
      \(nm :: NodeMap) ->
        let m      = unNodeMap nm
            g      = buildGraph False (extractionFromLists (Map.elems m) [])
            newIdx = buildIndex g Map.empty
            refIdx = newIdx { giLabelIndex = buildLabelIndexRef (gNodes g) }
            terms  = ["auth", "module", "server", "graph", "user", "data"]
        in findMatchingNodes terms newIdx == findMatchingNodes terms refIdx

-- ───────────────────────────────────────────────
-- Reference implementations using (++) accumulation, kept as
-- regression guards for order-insensitive equivalence.
-- ───────────────────────────────────────────────

buildLabelIndexRef :: Map NodeId Node -> Map Text [NodeId]
buildLabelIndexRef nodeMap =
  let splitTokens = Map.map reverse (Map.fromListWith (++)
        [ (word, [nid])
        | (nid, n) <- Map.toList nodeMap
        , word <- tokenizeLabel (toText (nodeLabel n))
        ])
      fullLabels = Map.map reverse (Map.fromListWith (++)
        [ (T.toLower (toText (nodeLabel n)), [nid])
        | (nid, n) <- Map.toList nodeMap
        , T.length (T.toLower (toText (nodeLabel n))) > 2
        ])
  in Map.unionWith (++) splitTokens fullLabels

buildPathIndexRef :: Map NodeId Node -> Map Text [NodeId]
buildPathIndexRef nodeMap =
  let segments = Map.fromListWith (++)
        [ (seg, [nid])
        | (nid, n) <- Map.toList nodeMap
        , let src = toText (nodeSourceFile n)
        , not (T.null src)
        , seg <- T.splitOn "/" (T.toLower src)
        , not (T.null seg)
        ]
      fullPaths = Map.fromListWith (++)
        [ (T.toLower (toText (nodeSourceFile n)), [nid])
        | (nid, n) <- Map.toList nodeMap
        , not (T.null (toText (nodeSourceFile n)))
        ]
  in Map.unionWith (++) segments fullPaths

-- ───────────────────────────────────────────────
-- Generators
-- ───────────────────────────────────────────────

mkNode :: NodeId -> Text -> Text -> Node
mkNode nid lbl src = Node
  { nodeId           = nid
  , nodeLabel        = fromText lbl
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText src
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

labelWords :: [Text]
labelWords =
  [ "AuthModule", "auth_handler", "HTTPServer", "graph.html"
  , "UserService", "data", "the", "module", "foo", "bar"
  ]

pathSegs :: [Text]
pathSegs = ["src", "cli", "server", "core", "main.hs", "commands.hs", "lib"]

genNodeMap :: Gen (Map NodeId Node)
genNodeMap = do
  n <- choose (0, 30 :: Int)
  pairs <- mapM genPair [0, 5 .. 5 * (n - 1)]
  pure (Map.fromList pairs)
  where
    genPair i = do
      lbl   <- elements labelWords
      len   <- choose (1, 4)
      segs  <- vectorOf len (elements pathSegs)
      let src = T.intercalate "/" segs
          nid = T.pack (show i)
      pure (nid, mkNode nid lbl src)

newtype NodeMap = NodeMap { unNodeMap :: Map NodeId Node }
  deriving (Eq, Show)

instance Arbitrary NodeMap where
  arbitrary = NodeMap <$> genNodeMap
  {-# INLINE arbitrary #-}