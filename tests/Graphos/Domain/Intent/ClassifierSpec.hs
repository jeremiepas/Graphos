{-# LANGUAGE OverloadedStrings #-}
-- | Hspec fixture tree covering every intent kind (intent-graph-verification 3.1).
--
-- Asserts the classifier is pure, config-driven, deterministic across runs, and
-- falls back to 'Other' when no rule matches.
module Graphos.Domain.Intent.ClassifierSpec where

import Data.Text (Text)
import Data.Text.Short (fromText)
import Test.Hspec

import Graphos.Domain.Intent.Classifier
import Graphos.Domain.Types.Node (Node(..), FileType(..))

-- | Build a node exposing only the fields the classifier reads.
mkNode :: Text -> Text -> FileType -> Text -> Maybe Text -> Node
mkNode nid lbl ft src kind = Node
  { nodeId = nid
  , nodeLabel = fromText lbl
  , nodeFileType = ft
  , nodeSourceFile = fromText src
  , nodeSource = Nothing
  , nodeLineStart = Nothing
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = fmap fromText kind
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  , nodePresentBits = 0
  }

-- | The fixture tree: one representative node per intent kind plus the 'Other'
-- fallback. Each entry exercises a distinct rule (or the no-match fallback).
fixture :: [(Node, IntentKind)]
fixture =
  [ ( mkNode "m1" "Mod" CodeFile "src/Mod.hs" (Just "Module"), Module )
  , ( mkNode "h1" "OrderHandler" CodeFile "src/order.hs" (Just "Method"), Handler )
  , ( mkNode "v1" "ValidateEmail" CodeFile "src/validate.hs" (Just "Function"), Validator )
  , ( mkNode "t1" "RunPipelineTests" CodeFile "src/run.hs" (Just "Function"), Test )
  , ( mkNode "s1" "userId" CodeFile "app/schema/UserSchema.hs" (Just "Field"), SchemaField )
  , ( mkNode "c1" "contractId" CodeFile "app/contract/UserContract.hs" (Just "Field"), ContractField )
  , ( mkNode "d1" "Guide" DocFile "docs/guide.md" (Just "Variable"), Doc )
  , ( mkNode "o1" "helper" CodeFile "src/misc/util.hs" (Just "Variable"), Other )
  ]

spec :: Spec
spec = do
  describe "3.1 deterministic kind classifier" $ do
    describe "classifies every intent kind on the fixture tree (covers all kinds)" coversEveryKind

    it "falls back to 'other' when no rule matches" $
      let orphan = mkNode "op1" "plus" CodeFile "src/ops.hs" (Just "Operator")
      in classifyNode defaultClassifierConfig orphan `shouldBe` Other

    it "is pure and stable across runs (same input yields the same kind)" $ do
      let firstRun  = [ classifyNode defaultClassifierConfig n | (n, _) <- fixture ]
          secondRun = [ classifyNode defaultClassifierConfig n | (n, _) <- fixture ]
      firstRun `shouldBe` secondRun

    it "is config-driven: a custom rule reclassifies the same node" $ do
      let forced = ClassifierConfig
            [ KindRule "force-other" Nothing Nothing Nothing (Just ["code"]) Other ]
      case fixture of
        (n, _) : _ -> classifyNode forced n `shouldBe` Other
        []         -> expectationFailure "fixture is empty"

-- | One assertion per fixture entry, so every intent kind is covered explicitly.
coversEveryKind :: Spec
coversEveryKind = describe "fixture coverage" $ mapM_ row fixture
  where
    row (n, got) =
      it ("classifies the " ++ show got ++ " node") $
        classifyNode defaultClassifierConfig n `shouldBe` got