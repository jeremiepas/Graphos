module Main (main) where

import qualified Test.Hspec as H

import qualified OpenSpecView.ArtifactViewSpec
import qualified OpenSpecView.ChangeListSpec
import qualified OpenSpecView.SpecDiffSpec
import qualified OpenSpecView.StateDashboardSpec

main :: IO ()
main = H.hspec $ do
  OpenSpecView.ChangeListSpec.spec
  OpenSpecView.ArtifactViewSpec.spec
  OpenSpecView.SpecDiffSpec.spec
  OpenSpecView.StateDashboardSpec.spec
