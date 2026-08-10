module Graphos.UseCase.SelectContextNoiseSpec where

import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Graphos.Domain.Types (Node(..), FileType(..), Edge(..), EdgeId(..)
                            , Relation(..), Confidence(..), Analysis(..)
                            , GodNode(..), emptyExtraction, extractionFromLists)
import Graphos.Domain.Graph (Graph, buildGraph)
import Graphos.Domain.Context (QueryComplexity(..), ContextBudget(..)
                              , budgetForComplexity, SelectedContext(..), scNodes
                              , scGodNodes)
import Graphos.UseCase.SelectContext (selectCommunityAware, selectRelevanceWeighted
                                     , selectPathBased, selectArchitectural)

mkNode :: Int -> T.Text -> Node
mkNode i label = Node
  { nodeId           = T.pack ("n" ++ show i)
  , nodeLabel        = label
  , nodeFileType     = CodeFile
  , nodeSourceFile   = "src/Test.hs"
  , nodeLineStart    = Just i
  , nodeLineEnd      = Nothing
  , nodeCommunityId  = Just (if i == 1 then 1 else 2)
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  }

mkEdge :: Int -> T.Text -> T.Text -> Relation -> Edge
mkEdge i src tgt rel = Edge
  { edgeId        = EdgeId (T.pack ("e" ++ show i))
  , edgeSource    = src
  , edgeTarget    = tgt
  , edgeRelation  = rel
  , edgeWeight    = 1.0
  , edgeConfidence = Confidence 0.9
  }

parserGraph :: Graph
parserGraph =
  let nodes = [ mkNode 1 "parseExpression"
              , mkNode 2 "lexerToken"
              , mkNode 3 "Main"]
      edges = [ mkEdge 1 "n1" "n2" Calls
              ]
      ext = extractionFromLists nodes edges
  in buildGraph False ext

parserCommunities :: Map.Map Int [T.Text]
parserCommunities = Map.fromList [(1, ["n1", "n2"]), (2, ["n3"])]

parserAnalysis :: Analysis
parserAnalysis = Analysis
  { analysisCommunities = parserCommunities
  , analysisCohesion    = Map.empty
  , analysisGodNodes    = [GodNode "n3" "Main" 246]
  , analysisSurprises   = []
  , analysisQuestions   = []
  }

budget :: ContextBudget
budget = budgetForComplexity Focused 3000

spec :: Spec
spec = describe "SelectContext god-node noise removal" $ do
  describe "selectCommunityAware" $ do
    it "does not force-include Main when query is about Parser" $ do
      let ctx = selectCommunityAware parserGraph parserCommunities parserAnalysis "parseExpression" budget
      map (nodeLabel . snd) (scNodes ctx) `shouldNotContain` ["Main"]
      scGodNodes ctx `shouldBe` []

  describe "selectRelevanceWeighted" $ do
    it "keeps a god node only when it is query-relevant" $ do
      let ctx = selectRelevanceWeighted parserGraph parserCommunities parserAnalysis "Main" budget
      map (nodeLabel . snd) (scNodes ctx) `shouldContain` ["Main"]
      map fst (scGodNodes ctx) `shouldContain` ["n3"]

    it "excludes unrelated god node from exploratory query" $ do
      let ctx = selectRelevanceWeighted parserGraph parserCommunities parserAnalysis "parseExpression" budget
      map fst (scGodNodes ctx) `shouldNotContain` ["n3"]

  describe "selectPathBased" $ do
    it "derives god nodes from the selected set only" $ do
      let ctx = selectPathBased parserGraph parserCommunities parserAnalysis "parseExpression lexerToken" budget
      map fst (scGodNodes ctx) `shouldNotContain` ["n3"]

  describe "selectArchitectural" $ do
    it "still includes god nodes by design" $ do
      let ctx = selectArchitectural parserGraph parserCommunities parserAnalysis budget
      map fst (scGodNodes ctx) `shouldContain` ["n3"]
