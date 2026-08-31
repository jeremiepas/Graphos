module Graphos.UseCase.ExtractSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Data.Text.Short (fromText)
import Graphos.Domain.Types
import Graphos.UseCase.Extract (resolveGranularity, isStubExtraction)

pdfStubNode :: T.Text -> Node
pdfStubNode path = Node
  { nodeId = path
  , nodeLabel = fromText path
  , nodeFileType = PaperFile
  , nodeSourceFile = fromText path
  , nodeLineStart = Nothing
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = Just (fromText "File")
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodePresentBits = 0
  }

makeStubNode :: T.Text -> Node
makeStubNode path = Node
  { nodeId = path
  , nodeLabel = fromText path
  , nodeFileType = CodeFile
  , nodeSourceFile = fromText path
  , nodeLineStart = Nothing
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = Just (fromText "File")
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodePresentBits = 0
  }

spec :: Spec
spec = do
  describe "Extraction" $ do
    it "emptyExtraction has zero nodes and edges" $ do
      Map.size (extractionNodes emptyExtraction) `shouldBe` 0
      Map.size (extractionEdges emptyExtraction) `shouldBe` 0

  describe "resolveGranularity (resolution order)" $ do
    let gcfgWithJson = defaultGraphosConfig  -- .json has a file-level override by default
        gcfgGlobalFile = defaultGraphosConfig { gcGranularity = GranularityFile }

    it "CLI flag wins over per-extension and global config" $ do
      resolveGranularity (Just GranularityFine) gcfgWithJson ".json" `shouldBe` GranularityFine
      resolveGranularity (Just GranularityFine) gcfgGlobalFile ".ts" `shouldBe` GranularityFine

    it "per-extension override wins over global" $ do
      resolveGranularity Nothing gcfgGlobalFile ".json" `shouldBe` GranularityFile
      resolveGranularity Nothing defaultGraphosConfig ".json" `shouldBe` GranularityFile

    it "global config applies when no CLI or per-extension override" $ do
      resolveGranularity Nothing gcfgGlobalFile ".ts" `shouldBe` GranularityFile

    it "built-in default (function) applies when nothing is set" $ do
      resolveGranularity Nothing defaultGraphosConfig ".ts" `shouldBe` GranularityFunction
      resolveGranularity Nothing defaultGraphosConfig ".unknown" `shouldBe` GranularityFunction

  describe "isStubExtraction" $ do
    it "returns True for a stub extraction (1 File node, 0 edges)" $ do
      let stub = extractionFromLists [pdfStubNode "test.pdf"] []
          stub' = extractionFromLists [makeStubNode "test.hs"] []
      isStubExtraction stub `shouldBe` True
      isStubExtraction stub' `shouldBe` True

    it "returns False for an empty extraction" $ do
      isStubExtraction emptyExtraction `shouldBe` False

    it "returns False for a multi-node extraction with no edges" $ do
      let multi = extractionFromLists [makeStubNode "a.hs", makeStubNode "b.hs"] []
      isStubExtraction multi `shouldBe` False

    it "returns False for a single node with edges" $ do
      let single = extractionFromLists [makeStubNode "test.hs"]
              [Edge (EdgeId "e1") "n1" "n2" Contains 1.0 (Graphos.Domain.Types.Confidence 1.0) Nothing]
      isStubExtraction single `shouldBe` False

    it "returns False for a non-File single node with no edges" $ do
      let nonFileNode = makeStubNode "test.hs"
          nonFile = extractionFromLists [nonFileNode { nodeKind = Just (fromText "Function") }] []
      isStubExtraction nonFile `shouldBe` False

    it "single File node with 0 edges is stub" $ do
      let node = makeStubNode "test"
          n = node { nodeKind = Just (fromText "File") }
          ext = extractionFromLists [n] []
      isStubExtraction ext `shouldBe` True

    it "single non-File node with 0 edges is not stub" $ do
      let node = makeStubNode "test"
          n = node { nodeKind = Just (fromText "Function") }
          ext = extractionFromLists [n] []
      isStubExtraction ext `shouldBe` False

    it "empty extraction is not stub" $
      isStubExtraction emptyExtraction `shouldBe` False
