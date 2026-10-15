{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for 'Graphos.OpenSpecView.ArtifactView'.
module Graphos.OpenSpecView.ArtifactViewSpec where

import Test.Hspec
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.List (isPrefixOf, sortOn)
import Data.Semigroup ((<>))
import Prelude (FilePath, IO, Bool(..), Maybe(..), String, id, fst, elem, map, filter, head, drop, (++), (==), (||), (.), ($))

import Graphos.OpenSpecView.ArtifactView

realChange :: FilePath
realChange = "openspec/changes/multi-source-graphs"

spec :: Spec
spec = do
  describe "splitSections" $ do
    it "partitions markdown into non-overlapping ## sections" $
      splitSections "## H1\nabc\n## H2\nxyz"
        `shouldBe` [("## H1", "abc\n"), ("## H2", "xyz\n")]

    it "discards leading text before the first heading" $
      map fst (splitSections "intro\nbody") `shouldBe` []

  describe "renderSectioned" $ do
    it "re-groups raw content by headings without overlap" $
      renderSectioned "## Section A\ncontent a\n## Section B\ncontent b"
        `shouldBe` ("## Section A\ncontent a\n" <> "## Section B\ncontent b")

  describe "parseArtifacts (real multi-source-graphs)" $ do
    it "lists proposal, design, tasks and capability specs" $ do
      arts <- parseArtifacts realChange
      let names = map artName arts
      ("proposal" :: Text) `elem` names `shouldBe` True
      ("node-schema" :: Text) `elem` names `shouldBe` True
      ("tasks" :: Text) `elem` names `shouldBe` True

    it "orders capability specs alphabetically" $ do
      arts <- parseArtifacts realChange
      let names = drop 3 (map artName arts)
          sorted = sortOn id names
      names `shouldBe` sorted

    it "marks the proposal artifact done when present" $ do
      arts <- parseArtifacts realChange
      let proposal = head (filter ((== "proposal") . artName) arts)
      (artStatus proposal) `shouldBe` ArtDone
      isJust (artContent proposal) `shouldBe` True

  describe "renderArtifact" $ do
    it "renders summary as name + status only" $
      renderArtifact DepthSummary (Artifact "proposal" "p" ArtDone Nothing)
        `shouldBe` "proposal done"

    it "renders full content under full depth" $
      renderArtifact DepthFull (Artifact "proposal" "p" ArtDone (Just "# Hi"))
        `shouldBe` "proposal done\n\n# Hi"

main :: IO ()
main = hspec spec
