module Graphos.Infrastructure.Export.ResearchHTMLSpec where

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text.Short (fromText)

import Graphos.Domain.Types
import Graphos.Domain.Graph (buildGraph)
import Graphos.Domain.Graph.Index (buildIndexWithLabels)
import Graphos.Domain.Community (CommunityComposition(..))
import Graphos.UseCase.Query.Research (buildResearchView)
import Graphos.UseCase.Query.Refine (EdgeMode (Semantic))
import Graphos.Infrastructure.Export.HTML (renderResearchHtml)

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

spec :: Spec
spec = do
  describe "renderResearchHtml" $ do
    it "embeds the research view as a JSON data blob" $ do
      let ext = extractionFromLists [testNode "AuthModule"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          rv = buildResearchView g idx (Map.empty :: CommunityMap)
            (Map.empty :: Map CommunityId CommunityComposition)
            [T.pack "Auth"] [] (Just Semantic)
          html = renderResearchHtml rv
      T.isInfixOf "<script type='application/json' id='research-data'>" html `shouldBe` True

    it "contains the legend and detail panels" $ do
      let ext = extractionFromLists [testNode "AuthModule"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          rv = buildResearchView g idx (Map.empty :: CommunityMap)
            (Map.empty :: Map CommunityId CommunityComposition)
            [T.pack "Auth"] [] (Just Semantic)
          html = renderResearchHtml rv
      T.isInfixOf "id='legend'" html `shouldBe` True
      T.isInfixOf "id='research-detail'" html `shouldBe` True

    it "embeds the node label inside the payload" $ do
      let ext = extractionFromLists [testNode "AuthModule"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          rv = buildResearchView g idx (Map.empty :: CommunityMap)
            (Map.empty :: Map CommunityId CommunityComposition)
            [T.pack "Auth"] [] (Just Semantic)
          html = renderResearchHtml rv
      T.isInfixOf "AuthModule" html `shouldBe` True

    it "loads vis-network from the CDN" $ do
      let ext = extractionFromLists [testNode "AuthModule"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          rv = buildResearchView g idx (Map.empty :: CommunityMap)
            (Map.empty :: Map CommunityId CommunityComposition)
            [T.pack "Auth"] [] (Just Semantic)
          html = renderResearchHtml rv
      T.isInfixOf "unpkg.com/vis-network@10.1.1" html `shouldBe` True

    it "titles the document with the query terms" $ do
      let ext = extractionFromLists [testNode "AuthModule"] []
          g = buildGraph False ext
          idx = buildIndexWithLabels g (Map.empty :: CommunityMap) Map.empty
          rv = buildResearchView g idx (Map.empty :: CommunityMap)
            (Map.empty :: Map CommunityId CommunityComposition)
            [T.pack "Auth", T.pack "Database"] [] (Just Semantic)
          html = renderResearchHtml rv
      T.isInfixOf "Auth" html `shouldBe` True
      T.isInfixOf "Database" html `shouldBe` True
