{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for 'Graphos.OpenSpecView.List' and 'Graphos.OpenSpecView.Types'.
module Graphos.OpenSpecView.ListSpec where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import Data.List (zipWith)
import Prelude (IO, String, all, any, error, id, map, show, drop, Bool(True), ($), (<$>), (==), (<=), (>=), (++))

import Graphos.OpenSpecView.Types (ChangeInfo (..), ChangeStatus (..), parseStatus)
import Graphos.OpenSpecView.List

epoch :: UTCTime
epoch = UTCTime (fromGregorian 0 0 0) 0

sampleTasks :: String
sampleTasks =
  "- [x] 1.1 first\n" ++
  "- [ ] 1.2 second\n" ++
  "## Heading with a fake - [x] inside prose\n" ++
  "-[X] 1.3 third"

spec :: Spec
spec = do
  describe "parseTaskCounts" $ do
    it "counts checked and total boxes, ignoring prose" $
      parseTaskCounts sampleTasks `shouldBe` (2, 3)
    it "returns zeros for empty input" $
      parseTaskCounts "" `shouldBe` (0, 0)

  describe "parseStatus" $ do
    it "maps empty totals to NoTasks" $
      parseStatus 0 0 `shouldBe` NoTasks
    it "maps equal nonzero totals to Complete" $
      parseStatus 5 5 `shouldBe` Complete
    it "maps partial progress to InProgress" $
      parseStatus 2 5 `shouldBe` InProgress

  describe "parseChangeInfo" $ do
    it "returns Nothing for a non-directory path" $ do
      result <- parseChangeInfo "/nonexistent/path/does-not-exist"
      result `shouldBe` Nothing

  describe "parseChanges (real openspec/changes)" $ do
    it "parses known changes with stable counts" $ do
      rows <- parseChanges "openspec/changes"
      let byName = Map.fromList (map (\c -> (changeName c, c)) rows)
          get n = fromMaybe (error ("missing change: " ++ show n)) (Map.lookup n byName)
      (completedTasks (get "openspec-view"), totalTasks (get "openspec-view")) `shouldBe` (0, 13)
      (completedTasks (get "viewer-navigation"), totalTasks (get "viewer-navigation")) `shouldBe` (0, 34)
      (completedTasks (get "multi-source-graphs"), totalTasks (get "multi-source-graphs")) `shouldBe` (2, 19)
      (completedTasks (get "usable-graph-for-code-review"), totalTasks (get "usable-graph-for-code-review")) `shouldBe` (4, 25)

    it "never reports more completed than total" $ do
      rows <- parseChanges "openspec/changes"
      all (\c -> completedTasks c <= totalTasks c) rows `shouldBe` True

    it "includes known changes by name" $ do
      rows <- parseChanges "openspec/changes"
      let names = map changeName rows
      any (== ("openspec-view" :: Text)) names `shouldBe` True

    it "sorts newest-first by lastModified" $ do
      rows <- parseChanges "openspec/changes"
      let ages = lastModified <$> rows
      all id (zipWith (>=) ages (drop 1 ages)) `shouldBe` True

  describe "renderChangeList" $ do
    it "formats rows as name, counts and status label" $
      renderChangeList [ChangeInfo "demo" InProgress 0 3 epoch] `shouldBe`
        "demo 0/3 tasks just now"

main :: IO ()
main = hspec spec
