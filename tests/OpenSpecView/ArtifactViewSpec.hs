{-# LANGUAGE OverloadedStrings #-}
module OpenSpecView.ArtifactViewSpec (spec) where

import Data.Maybe (isJust)
import System.Directory
import Test.Hspec

import OpenSpecView.ArtifactView

combine' :: FilePath -> FilePath -> FilePath
combine' a b
  | not (null a) && last a == '/' = a ++ b
  | null a = b
  | otherwise = a ++ "/" ++ b

takeParent :: FilePath -> FilePath
takeParent p =
  let stripped = drop 1 (dropWhile (/= '/') (reverse p))
      parent = reverse stripped
  in if null parent then "." else parent

findChangesRoot :: IO FilePath
findChangesRoot = go =<< getCurrentDirectory
  where
    go d = do
      let cand = combine' d "openspec/changes"
      exists <- doesDirectoryExist cand
      if exists
        then pure cand
        else
          let parent = takeParent d
          in if parent == "." || parent == d
               then error ("Could not locate openspec/changes under " ++ d)
               else go parent

sampleChangeArtifacts :: ChangeArtifacts
sampleChangeArtifacts =
  ChangeArtifacts
    { artifactsName = "demo"
    , changeProposal = Just (ChangeArtifact "proposal.md" "proposal.md" 0 "# Demo\nProposal body." False)
    , changeDesign = Just (ChangeArtifact "design.md" "design.md" 0 "# Design\nDesign body." False)
    , changeTasks = Just (ChangeArtifact "tasks.md" "tasks.md" 0 "- [ ] t1\n- [x] t2\n" False)
    , changeSpecs
      = [ ChangeArtifact "cypher-query" "specs/cypher-query/spec.md" 1 "# cypher\nSpec body." False
        , ChangeArtifact "canvas-export" "specs/canvas-export/spec.md" 1 "# canvas\nSpec body." False
        ]
    }

spec :: Spec
spec = do
  describe "OpenSpecView.ArtifactView.renderChangeArtifacts" $ do
    it "renders every artifact kind with its content" $ do
      let out = renderChangeArtifacts sampleChangeArtifacts
      out `shouldContain` "Change: demo"
      out `shouldContain` "== proposal.md =="
      out `shouldContain` "# Demo"
      out `shouldContain` "== design.md =="
      out `shouldContain` "# Design"
      out `shouldContain` "== tasks.md =="
      out `shouldContain` "- [x] t2"

    it "lists each spec with its kind and content" $ do
      let out = renderChangeArtifacts sampleChangeArtifacts
      out `shouldContain` "== Specs (2) =="
      out `shouldContain` "== cypher-query =="
      out `shouldContain` "== canvas-export =="
      out `shouldContain` "# cypher"
      out `shouldContain` "# canvas"

    it "marks absent artifacts as missing" $ do
      let bare = sampleChangeArtifacts { changeProposal = Nothing, changeDesign = Nothing }
          out = renderChangeArtifacts bare
      out `shouldContain` "== proposal.md ==\n(missing)"
      out `shouldContain` "== design.md ==\n(missing)"

  describe "OpenSpecView.ArtifactView readers" $ do
    it "returns Nothing for an absent artifact file" $ do
      mArt <- readArtifact "openspec/changes/does-not-exist/proposal.md"
      mArt `shouldBe` Nothing

    it "lists spec directories for a real change" $ do
      root <- findChangesRoot
      specs <- listSpecs (combine' root "consumable-graph-outputs/specs")
      specs `shouldSatisfy` (all (not . null))
      length specs `shouldBe` 5

    it "displays all artifacts for a real change" $ do
      root <- findChangesRoot
      ca <- readChangeArtifacts (combine' root "consumable-graph-outputs")
      artifactsName ca `shouldBe` "consumable-graph-outputs"
      changeProposal ca `shouldSatisfy` isJust
      changeDesign ca `shouldSatisfy` isJust
      changeTasks ca `shouldSatisfy` isJust
      map artifactKind (changeSpecs ca) `shouldSatisfy` (all (not . null))

    it "renders the sample change with a header and no missing markers" $ do
      let out = renderChangeArtifacts sampleChangeArtifacts
      out `shouldContain` "OpenSpec Artifact Viewer"
      out `shouldContain` "Change: demo"
      out `shouldNotContain` "(missing)"
