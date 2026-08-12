module Graphos.UseCase.FormatContextBudgetSpec where

import Test.Hspec
import qualified Data.Text as T

import Graphos.Domain.Types (Node(..), FileType(..), Edge(..), EdgeId(..)
                            , Relation(..), Confidence(..))
import Graphos.Domain.Context (SelectedContext(..), SelectionStrategy(..)
                              , ContextBudget(..), QueryComplexity(..))
import Graphos.UseCase.FormatContext

mkNode :: Int -> T.Text -> Node
mkNode i label = Node
  { nodeId           = T.pack ("n" ++ show i)
  , nodeLabel        = label
  , nodeFileType     = CodeFile
  , nodeSourceFile   = "src/Test.hs"
  , nodeLineStart    = Just i
  , nodeLineEnd      = Nothing
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeKind         = Nothing
  , nodeSignature    = Nothing
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

-- | A top-ranked node should survive truncation and produce a footer.
bigContextFixture :: SelectedContext
bigContextFixture =
  let labels = [ "Alpha Beta Gamma Delta Epsilon"
               , "Alpha Beta Gamma"
               , "Alpha Beta"
               , "Alpha"
               , "Main"
               ] ++ [ T.pack ("Node " ++ show i) | i <- [1 .. 250 :: Int] ]
      nodes = zipWith mkNode [0..] labels
      edges = [ mkEdge i (T.pack ("n" ++ show i)) (T.pack ("n" ++ show (i + 1))) Calls 0.9
              | i <- [0 .. 199 :: Int]
              ]
  in SelectedContext
       { scNodes           = zip [nodeId n | n <- nodes] nodes
       , scEdges           = edges
       , scCommunities     = mempty
       , scCommunityLabels = mempty
       , scBridgeNodes     = []
       , scGodNodes        = []
       , scStrategy        = CommunityAware
       , scBudget          = ContextBudget 3000 0.2 0.7 ModuleLevel 30 60
       , scMatchScore      = 0.0
       }

spec :: Spec
spec = describe "FormatContext budget truncation" $ do
  describe "formatContextForLLMBudgeted" $ do
    it "truncates a 9000+ token context to within the 3000-token budget" $ do
      let (formatted, toks, _omitted, _) = formatContextForLLMBudgeted 3000 bigContextFixture
      toks `shouldSatisfy` (<= 3000)
      T.length formatted `shouldSatisfy` (> 0)

    it "preserves the highest-ranked node after truncation" $ do
      let (formatted, _, _, _) = formatContextForLLMBudgeted 3000 bigContextFixture
      T.isInfixOf "Alpha Beta Gamma Delta Epsilon" formatted `shouldBe` True

    it "emits an omitted footer when truncation happens" $ do
      let (formatted, _, _, _) = formatContextForLLMBudgeted 3000 bigContextFixture
      T.isInfixOf "_omitted:" formatted `shouldBe` True
      T.isInfixOf "nodes," formatted `shouldBe` True
      T.isInfixOf "edges_" formatted `shouldBe` True

  describe "countContextTokens" $ do
    it "uses the word-count heuristic, not raw character length" $ do
      let short = "a b c"
          long  = T.replicate 100 "word "
      countContextTokens short `shouldBe` ceiling (3 * 1.33 :: Double)
      countContextTokens long `shouldBe` ceiling (100 * 1.33 :: Double)
