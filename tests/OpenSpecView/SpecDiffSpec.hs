module OpenSpecView.SpecDiffSpec (spec) where

import Data.Char (toLower)
import Test.Hspec

import OpenSpecView.SpecDiff

normBody :: String -> String
normBody = unwords . map (map toLower) . words

deltaSpec :: String
deltaSpec = unlines
  [ "# Spec: example"
  , ""
  , "## ADDED Requirements"
  , ""
  , "### Requirement: System SHALL do A"
  , "The system SHALL do A differently than before."
  , ""
  , "#### Scenario: a works"
  , "WHEN triggered THEN A happens"
  , ""
  , "### Requirement: System SHALL do B"
  , "The system SHALL do B."
  , ""
  , "### Requirement: System SHALL add D"
  , "The system SHALL introduce a new capability D."
  ]

mainSpec :: String
mainSpec = unlines
  [ "# 01-example Specification"
  , ""
  , "## Requirements"
  , ""
  , "### Requirement: System SHALL do A"
  , "The system SHALL do A."
  , ""
  , "#### Scenario: a works"
  , "WHEN triggered THEN A happens"
  , ""
  , "### Requirement: System SHALL do B"
  , "The system SHALL do B."
  , ""
  , "### Requirement: System SHALL retire C"
  , "The system SHALL remove the legacy capability C."
  ]

spec :: Spec
spec = do
  describe "OpenSpecView.SpecDiff.parseSpecDoc" $ do
    it "extracts requirement titles and bodies, ignoring scenario blocks" $ do
      let doc = parseSpecDoc deltaSpec
          reqs = docRequirements doc
      (map reqTitle reqs) `shouldBe`
        [ "System SHALL do A", "System SHALL do B", "System SHALL add D"]
      (bodyOf "System SHALL do A" reqs) `shouldBe` "The system SHALL do A differently than before."
      (nameOf doc) `shouldBe` "example"

    it "assigns sequential requirement ids" $ do
      let reqs = docRequirements (parseSpecDoc deltaSpec)
          ids = map reqId reqs
      ids `shouldBe` ["R1", "R2", "R3"]

  describe "OpenSpecView.SpecDiff.diffDocs" $ do
    it "categorizes added, modified, and removed requirements" $ do
      let result = diffDocs (parseSpecDoc deltaSpec) (parseSpecDoc mainSpec)
          types = map specChangeType result
      types `shouldBe` ["modified", "added", "removed"]

    it "omits unchanged requirements and reports the right requirement" $ do
      let result = diffDocs (parseSpecDoc deltaSpec) (parseSpecDoc mainSpec)
          addedTitles = map (\c -> reqTitle (specChangeReq c))
                        (filter ((== "added") . specChangeType) result)
          modifiedTitles = map (\c -> reqTitle (specChangeReq c))
                        (filter ((== "modified") . specChangeType) result)
          removedTitles = map (\c -> reqTitle (specChangeReq c))
                        (filter ((== "removed") . specChangeType) result)
          unchangedReported = any isUnchanged result
      addedTitles `shouldBe` ["System SHALL add D"]
      modifiedTitles `shouldBe` ["System SHALL do A"]
      removedTitles `shouldBe` ["System SHALL retire C"]
      unchangedReported `shouldBe` False

    it "treats a spec with no counterpart as fully added" $ do
      let loneDelta = parseSpecDoc "# Spec: brandnew\n\n### Requirement: New thing\nDo new thing."
          result = diffDocs loneDelta (parseSpecDoc "")
      map specChangeType result `shouldBe` ["added"]

  describe "OpenSpecView.SpecDiff.diffPaths" $ do
    it "diffs real delta and main spec files from disk" $ do
      result <- diffPaths
        "openspec/changes/openspec-view/specs/openspec-spec-diff/spec.md"
        "openspec/changes/openspec-view/specs/openspec-spec-diff/spec.md"
      case result of
        Left err -> error ("diffPaths failed: " ++ err)
        Right changes -> do
          map specChangeType changes `shouldBe` []

    it "reports a missing file as a Left" $ do
      result <- diffPaths "does-not-exist-a.md" "does-not-exist-b.md"
      case result of
        Left _ -> pure ()
        Right _ -> error "expected Left for missing file"

bodyOf :: String -> [SpecRequirement] -> String
bodyOf title reqs =
  case filter ((== title) . reqTitle) reqs of
    (r:_) -> reqBody r
    [] -> error ("requirement not found: " ++ title)

nameOf :: SpecDoc -> String
nameOf = docName

isUnchanged :: SpecChange -> Bool
isUnchanged c = case c of
  Modified {beforeReq = b, afterReq = a} -> normBody (reqBody b) == normBody (reqBody a)
  _ -> False
