{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for 'Graphos.OpenSpecView.SpecDiff'.
module Graphos.OpenSpecView.SpecDiffSpec where

import Test.Hspec
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Semigroup ((<>))
import Prelude
  (FilePath, IO, Bool(..), Maybe(..), String, fst, elem, map, filter, (++), (==), ($), (.), readFile, error, show)

import Graphos.OpenSpecView.SpecDiff

mainReq :: String -> SpecRequirement
mainReq n = SpecRequirement (T.pack n) "main-body"

deltaReq :: String -> SpecRequirement
deltaReq n = SpecRequirement (T.pack n) "delta-body"

spec :: Spec
spec = do
  describe "parseRequirements" $ do
    it "extracts name and captures body up to the next requirement heading" $
      parseRequirements "### Requirement: Foo\nbar\n#### Scenario: x\n- WHEN a\n### Requirement: Bar\nbaz"
        `shouldBe` [SpecRequirement "Foo" "bar\n#### Scenario: x\n- WHEN a\n", SpecRequirement "Bar" "baz\n"]

    it "returns an empty list when no heading is present" $
      parseRequirements "just prose" `shouldBe` []

  describe "diffSpec" $ do
    it "categorises a changed requirement as modified" $
      diffSpec (Just [mainReq "A"]) [deltaReq "A"]
        `shouldBe` [SpecChange (deltaReq "A") Modified]

    it "categorises a brand new requirement as added" $
      diffSpec Nothing [deltaReq "B"]
        `shouldBe` [SpecChange (deltaReq "B") Added]

    it "categorises a requirement absent from the delta as removed" $
      diffSpec (Just [mainReq "A"]) []
        `shouldBe` [SpecChange (mainReq "A") Removed]

    it "treats identical content as unchanged (no change emitted)" $
      diffSpec (Just [SpecRequirement (T.pack "A") "same-body"])
        [SpecRequirement (T.pack "A") "same-body"]
        `shouldBe` []

  describe "diffSpecSummary" $ do
    it "groups entries under their category labels" $
      let summary = diffSpecSummary (Just [mainReq "OnlyMain"]) [deltaReq "NewDelta"]
          in ("Added:" `T.isPrefixOf` summary) `shouldBe` True

  describe "validateFidelity" $ do
    it "flags a redundant no-op modification" $
      let issues = validateFidelity (Just [SpecRequirement (T.pack "Same") "same-body"])
                [SpecRequirement (T.pack "Same") "same-body"]
      in ("redundant" `elem` (map fst issues)) `shouldBe` True

    it "reports when the main spec is absent" $
      let issues = validateFidelity Nothing [deltaReq "X"]
      in map fst issues `shouldBe` ["missing-main"]

  describe "diffSpec against real node-schema change" $ do
    it "marks Canonical Node field set as modified" $
      assertCategorisation "openspec/specs/node-schema/spec.md"
        "openspec/changes/multi-source-graphs/specs/node-schema/spec.md"
        "Canonical Node field set" Modified

    it "marks Cache tolerance for legacy keys as modified" $
      assertCategorisation "openspec/specs/node-schema/spec.md"
        "openspec/changes/multi-source-graphs/specs/node-schema/spec.md"
        "Cache tolerance for legacy keys" Modified

    it "marks Conversation timestamps migrate to nodeExtra as removed" $
      assertCategorisation "openspec/specs/node-schema/spec.md"
        "openspec/changes/multi-source-graphs/specs/node-schema/spec.md"
        "Conversation timestamps migrate to nodeExtra" Removed

  describe "validateFidelity against real node-schema change" $ do
    it "finds no redundant entries" $ do
      main <- readFile "openspec/specs/node-schema/spec.md"
      delta <- readFile "openspec/changes/multi-source-graphs/specs/node-schema/spec.md"
      let issues = validateFidelity (Just (parseRequirements main)) (parseRequirements delta)
      map fst issues `shouldBe` []

assertCategorisation :: FilePath -> FilePath -> Text -> SpecChangeType -> IO ()
assertCategorisation mainPath deltaPath name expected = do
  main <- readFile mainPath
  delta <- readFile deltaPath
  let changes = diffSpec (Just (parseRequirements main)) (parseRequirements delta)
      matches = filter ((== name) . reqName . changeReq) changes
  case matches of
    (c : _) -> (changeType c) `shouldBe` expected
    [] -> error ("requirement not found in diff: " ++ show name)

main :: IO ()
main = hspec spec
