module Graphos.UseCase.FormatContextHintsSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Data.Text.Short (fromText)
import Graphos.Domain.Types (Node(..), FileType(..), Edge(..), EdgeId(..)
                            , Relation(..), Confidence(..))
import Graphos.Domain.Context (SelectedContext(..), SelectionStrategy(..)
                              , ContextBudget(..), QueryComplexity(..)
                              , chatCommunityId)
import Graphos.UseCase.FormatContext

mkNode :: Int -> T.Text -> Node
mkNode i label = Node
  { nodeId           = T.pack ("n" ++ show i)
  , nodeLabel        = fromText label
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "src/Test.hs"
  , nodeLineStart    = Just i
  , nodeLineEnd      = Nothing
  , nodeCommunityId  = Just i
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
  , nodePresentBits  = 0
  }

mkEdge :: Int -> T.Text -> T.Text -> Relation -> Double -> Edge
mkEdge i src tgt rel conf = Edge
  { edgeId        = EdgeId (T.pack ("e" ++ show i))
  , edgeSource    = src
  , edgeTarget    = tgt
  , edgeRelation  = rel
  , edgeWeight    = 1.0
  , edgeConfidence = Confidence conf
  , edgeExtra       = Nothing
  }

-- | A context with ambiguous edges, a trivia edge, and several normal edges.
edgesFixture :: SelectedContext
edgesFixture =
  let nodes = [ mkNode 1 "Parser"
              , mkNode 2 "Lexer"
              , mkNode 3 "undefined"
              , mkNode 4 "Result"
              ]
      edges = [ mkEdge 1 "Parser" "Lexer" Calls 0.95
              , mkEdge 2 "Parser" "undefined" Contains 0.95
              , mkEdge 3 "Parser" "Result" Calls 0.95
              , mkEdge 4 "Lexer" "Parser" Calls 0.6
              ]
  in SelectedContext
       { scNodes           = zip [nodeId n | n <- nodes] nodes
       , scEdges           = edges
       , scCommunities     = Map.fromList [(1, ["n1"]), (2, ["n2"])]
       , scCommunityLabels = Map.fromList [(1, "Parser"), (2, "Lexer")]
       , scBridgeNodes     = []
       , scGodNodes        = []
       , scStrategy        = CommunityAware
       , scBudget          = ContextBudget 3000 0.2 0.7 ModuleLevel 30 60
       , scMatchScore      = 0.0
       }

-- | A context with a mega-community and the chat community.
hintsFixture :: SelectedContext
hintsFixture =
  let nodes = [ mkNode 1 "Alpha"
              , mkNode 2 "Beta"
              ]
  in SelectedContext
       { scNodes           = zip [nodeId n | n <- nodes] nodes
       , scEdges           = []
       , scCommunities     = Map.fromList
                             [ (chatCommunityId, ["chat1"])
                             , (1, replicate 2563 "member")
                             , (2, ["n1"])
                             , (3, ["n2"])
                             ]
       , scCommunityLabels = Map.fromList
                             [ (chatCommunityId, "Chat History")
                             , (1, "Mega")
                             , (2, "First")
                             , (3, "Second")
                             ]
       , scBridgeNodes     = []
       , scGodNodes        = []
       , scStrategy        = CommunityAware
       , scBudget          = ContextBudget 3000 0.2 0.7 ModuleLevel 30 60
       , scMatchScore      = 0.0
       }

spec :: Spec
spec = describe "FormatContext edges and hints" $ do
  describe "filterAndRankEdges" $ do
    it "drops AMBIGUOUS edges in semantic mode" $ do
      let ranked = filterAndRankEdges Semantic (scEdges edgesFixture)
      map edgeSource ranked `shouldNotContain` ["Lexer"]

    it "preserves all edges in all mode" $ do
      let ranked = filterAndRankEdges All (scEdges edgesFixture)
      length ranked `shouldBe` 4

    it "orders edges by endpoint relevance" $ do
      let ranked = filterAndRankEdges Semantic (scEdges edgesFixture)
      case ranked of
        (x:_) -> edgeSource x `shouldBe` "Parser"
        []    -> expectationFailure "expected non-empty ranked edges"

  describe "formatKeyEdgesFiltered" $ do
    it "drops trivia-targeting contains edges by default" $ do
      let txt = formatKeyEdgesFiltered Semantic edgesFixture
      T.isInfixOf "undefined" txt `shouldBe` False

  describe "formatExpansionHintsBudgeted" $ do
    it "hides mega-communities above maxHintCommunitySize" $ do
      let txt = formatExpansionHintsBudgeted 8 50 hintsFixture
      T.isInfixOf "2563" txt `shouldBe` False

    it "omits the chat community" $ do
      let txt = formatExpansionHintsBudgeted 8 50 hintsFixture
      T.isInfixOf "Chat History" txt `shouldBe` False

    it "omits the section entirely when no hints survive" $ do
      let filteredOnlyChat = hintsFixture { scCommunityLabels = Map.fromList [(chatCommunityId, "Chat History")] }
          txt = formatExpansionHintsBudgeted 8 50 filteredOnlyChat
      T.null txt `shouldBe` True
