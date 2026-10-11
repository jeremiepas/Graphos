{-# LANGUAGE OverloadedStrings #-}
module OpenSpecView.ChangeListSpec (spec) where

import Control.Monad (filterM)
import qualified Data.Map.Strict as Map
import System.Directory
import Test.Hspec

import OpenSpecView.ChangeList

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

expected :: ChangeInfo -> Bool
expected info =
  let c = changeCompleted info
      t = changeTotal info
      expectedStatus
        | c == 0    = "not_started"
        | c == t    = "complete"
        | otherwise = "in_progress"
  in c >= 0 && t >= 0 && c <= t
     && changeStatus info == expectedStatus

spec :: Spec
spec = do
  describe "OpenSpecView.ChangeList.parseTasksMd" $ do
    it "counts zero tasks as not_started" $ do
      let info = parseTasksMd "empty" ""
      (changeCompleted info) `shouldBe` 0
      (changeTotal info) `shouldBe` 0
      (changeStatus info) `shouldBe` "not_started"

    it "counts only checkboxes, ignoring headings and check notes" $ do
      let content = "# Tasks: demo\n## Phase 1\n- [ ] T1 do the thing\n  - Check: run cabal test\n- [x] T2 finish the thing\n- [ ] T3 leftover\n"
      let info = parseTasksMd "demo" content
      (changeTotal info) `shouldBe` 3
      (changeCompleted info) `shouldBe` 1
      (changeStatus info) `shouldBe` "in_progress"

    it "marks a fully checked list as complete" $ do
      let content = "- [x] a\n- [X] b\n"
      let info = parseTasksMd "done" content
      (changeTotal info) `shouldBe` 2
      (changeCompleted info) `shouldBe` 2
      (changeStatus info) `shouldBe` "complete"

  it "parses every existing change under openspec/changes correctly" $ do
    root <- findChangesRoot
    infos <- readAllChangeInfos root
    realDirs <- filterM (\e -> doesFileExist (combine' root (combine' e "tasks.md")))
      =<< listDirectory root
    length infos `shouldBe` length realDirs
    infos `shouldSatisfy` (all expected)

  it "parses known changes with the expected task counts" $ do
    root <- findChangesRoot
    infos <- readAllChangeInfos root
    let byName = Map.fromList [(changeName c, c) | c <- infos]
        lookup' k = case Map.lookup k byName of
          Nothing -> error ("missing change in parsed list: " ++ k)
          Just c  -> c
        view = lookup' "openspec-view"
        hg = lookup' "honor-graphosignore"
        ci = lookup' "fix-ci-missing-hackage-package-list"
        ex = lookup' "extract-haskell-libs"
    (changeTotal view) `shouldBe` 13
    (changeCompleted view) `shouldBe` 0
    (changeStatus view) `shouldBe` "not_started"
    (changeTotal hg) `shouldBe` 13
    (changeCompleted hg) `shouldBe` 13
    (changeStatus hg) `shouldBe` "complete"
    (changeTotal ci) `shouldBe` 12
    (changeCompleted ci) `shouldBe` 12
    (changeStatus ci) `shouldBe` "complete"
    (changeTotal ex) `shouldBe` 60
    (changeCompleted ex) `shouldBe` 2
    (changeStatus ex) `shouldBe` "in_progress"
