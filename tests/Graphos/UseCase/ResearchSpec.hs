module Graphos.UseCase.ResearchSpec where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Text.Short (fromText, toText)

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Graph.Index (buildIndexWithLabels)
import Graphos.Domain.Community (CommunityComposition(..))
import Graphos.UseCase.Query.Research (buildResearchView, expandWithSeeds)
import Graphos.UseCase.Query.Refine (EdgeMode (Semantic))
import Graphos.Domain.Query.Research
  (ResearchView (rvNodes, rvEdges, rvTerms, rvMetadata)
  , ResearchNode (rnDiscoveredBy, rnScores, rnBestScore, rnNode)
  , ResearchMetadata (rmNodeCount))

testNode :: T.Text -> Node
testNode nid = Node
  { nodeId = nid
  , nodeLabel = fromText nid
  , nodeFileType = CodeFile
  , nodeSourceFile = fromText "test.hs"
  , nodeLineStart = Nothing
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = Nothing
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodePresentBits = 0
  }

testEdge :: T.Text -> T.Text -> Edge
testEdge src tgt = Edge
  { edgeId = EdgeId (src <> "->" <> tgt)
  , edgeSource = src
  , edgeTarget = tgt
  , edgeRelation = Imports
  , edgeConfidence = Confidence 1.0
  , edgeWeight = 1.0
  , edgeExtra = Nothing
  }

withFirst :: [a] -> (a -> Expectation) -> Expectation
withFirst (x : _) f = f x
withFirst [] _ = expectationFailure "expected a non-empty list"

spec :: Spec
spec = do
  describe "buildResearchView" $ do
    it "attributes a single matched term to a node" $ do
      let ext = extractionFromLists [testNode "AuthModule"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          rv = buildResearchView g idx (Map.empty :: CommunityMap)
            (Map.empty :: Map CommunityId CommunityComposition)
            [T.pack "Auth"] [] (Just Semantic)
      withFirst (rvNodes rv) (\node -> do
        rnDiscoveredBy node `shouldContain` ["Auth"]
        rnBestScore node `shouldSatisfy` (> 0))

    it "unions nodes matched by multiple terms" $ do
      let ext = extractionFromLists [testNode "AuthModule", testNode "Database"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          rv = buildResearchView g idx (Map.empty :: CommunityMap)
            (Map.empty :: Map CommunityId CommunityComposition)
            [T.pack "Auth", T.pack "Database"] [] (Just Semantic)
          labels = Set.fromList [toText (nodeLabel (rnNode n)) | n <- rvNodes rv]
      Set.member (T.pack "AuthModule") labels `shouldBe` True
      Set.member (T.pack "Database") labels `shouldBe` True

    it "attributes a node matched by two terms to both" $ do
      let ext = extractionFromLists [testNode "AuthService"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          rv = buildResearchView g idx (Map.empty :: CommunityMap)
            (Map.empty :: Map CommunityId CommunityComposition)
            [T.pack "Auth", T.pack "Service"] [] (Just Semantic)
      withFirst (rvNodes rv) (\node -> do
        Set.fromList (rnDiscoveredBy node) `shouldSatisfy`
          (\s -> Set.member (T.pack "Auth") s && Set.member (T.pack "Service") s))

    it "fills scores with all terms, zero for non-matchers" $ do
      let ext = extractionFromLists [testNode "AuthOnly"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          rv = buildResearchView g idx (Map.empty :: CommunityMap)
            (Map.empty :: Map CommunityId CommunityComposition)
            [T.pack "Auth", T.pack "Other"] [] (Just Semantic)
      withFirst (rvNodes rv) (\node -> do
        let scoreMap = Map.fromList (rnScores node)
        length (rnScores node) `shouldBe` 2
        Map.lookup (T.pack "Auth") scoreMap `shouldSatisfy`
          (\m -> case m of Just s -> s > 0; Nothing -> False)
        Map.lookup (T.pack "Other") scoreMap `shouldBe` Just 0)

    it "produces empty nodes for a non-matching term" $ do
      let ext = extractionFromLists [testNode "Alpha"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          rv = buildResearchView g idx (Map.empty :: CommunityMap)
            (Map.empty :: Map CommunityId CommunityComposition)
            [T.pack "zzzznotfound"] [] (Just Semantic)
      length (rvNodes rv) `shouldBe` 0
      rmNodeCount (rvMetadata rv) `shouldBe` 0
      rvTerms rv `shouldBe` [T.pack "zzzznotfound"]

    it "induces edges between matched nodes" $ do
      let ext = extractionFromLists [testNode "AuthA", testNode "AuthB"] [testEdge "AuthA" "AuthB"]
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          rv = buildResearchView g idx (Map.empty :: CommunityMap)
            (Map.empty :: Map CommunityId CommunityComposition)
            [T.pack "Auth"] [] (Just Semantic)
          edgeIds = Set.fromList [(edgeSource e, edgeTarget e) | e <- rvEdges rv]
      length (rvEdges rv) `shouldSatisfy` (> 0)
      Set.member (("AuthA", "AuthB") :: (NodeId, NodeId)) edgeIds `shouldBe` True

  describe "expandWithSeeds" $ do
    it "is additive and includes seed-matched neighbours" $ do
      let ext = extractionFromLists [testNode "AuthA", testNode "HelperB"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          union = Set.fromList ["AuthA"]
          expanded = expandWithSeeds g idx union [T.pack "Helper"]
      Set.isSubsetOf union expanded `shouldBe` True
      Set.member "HelperB" expanded `shouldBe` True
      Set.size expanded `shouldSatisfy` (>= 2)
