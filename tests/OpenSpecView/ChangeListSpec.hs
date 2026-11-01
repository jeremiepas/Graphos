{-# LANGUAGE OverloadedStrings #-}
module OpenSpecView.ChangeListSpec (spec) where

import Control.Monad (filterM)
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
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

valid :: ChangeInfo -> Bool
valid info =
  let c = changeCompleted info
      t = changeTotal info
      expectedStatus
        | t == 0    = "no-tasks"
        | c == t    = "complete"
        | otherwise = "in-progress"
  in c >= 0 && t >= 0 && c <= t
     && changeStatus info == expectedStatus

-- `openspec list --json` orders changes by lastModified descending, breaking
-- ties by name ascending.
expectedOrder :: [ChangeInfo] -> [ChangeInfo]
expectedOrder = sortBy (comparing (fmap Down changeLastModified) <> comparing changeName)

-- Write a single change dir under `root` whose tasks.md contains `total`
-- checkboxes of which `done` are marked complete. Used to build a hermetic
-- fixture so `readAllChangeInfos` counts do not depend on the live
-- openspec/changes tree (whose completion counts drift as work progresses).
writeFixture :: FilePath -> ChangeName -> Int -> Int -> IO ()
writeFixture root name total done = do
  let dir = combine' root name
  createDirectoryIfMissing True dir
  let checkboxes =
        [ "- [x] " ++ name ++ " task " ++ show i | i <- [1 .. done] ]
        ++ [ "- [ ] " ++ name ++ " task " ++ show i | i <- [done + 1 .. total] ]
  writeFile
    (combine' dir "tasks.md")
    ("# Tasks: " ++ name ++ "\n## Phase 1\n" ++ unlines checkboxes)

spec :: Spec
spec = do
  describe "OpenSpecView.ChangeList.parseTasksMd" $ do
    it "counts zero tasks as no-tasks" $ do
      let info = parseTasksMd "empty" ""
      (changeCompleted info) `shouldBe` 0
      (changeTotal info) `shouldBe` 0
      (changeStatus info) `shouldBe` "no-tasks"

    it "counts only checkboxes, ignoring headings and check notes" $ do
      let content = "# Tasks: demo\n## Phase 1\n- [ ] T1 do the thing\n  - Check: run cabal test\n- [x] T2 finish the thing\n- [ ] T3 leftover\n"
      let info = parseTasksMd "demo" content
      (changeTotal info) `shouldBe` 3
      (changeCompleted info) `shouldBe` 1
      (changeStatus info) `shouldBe` "in-progress"

    it "marks a fully checked list as complete" $ do
      let content = "- [x] a\n- [X] b\n"
      let info = parseTasksMd "done" content
      (changeTotal info) `shouldBe` 2
      (changeCompleted info) `shouldBe` 2
      (changeStatus info) `shouldBe` "complete"

  it "parses every existing change under openspec/changes correctly" $ do
    root <- findChangesRoot
    infos <- readAllChangeInfos root
    realDirs <- filterM (\e -> do
                          isDir <- doesDirectoryExist (combine' root e)
                          pure (isDir && e /= "archive"))
      =<< listDirectory root
    length infos `shouldBe` length realDirs
    infos `shouldSatisfy` (all valid)

  it "orders changes by lastModified descending, ties by name ascending" $ do
    root <- findChangesRoot
    infos <- readAllChangeInfos root
    infos `shouldBe` expectedOrder infos

  it "includes changes without tasks.md as no-tasks" $ do
    root <- findChangesRoot
    infos <- readAllChangeInfos root
    let byName = Map.fromList [(changeName c, c) | c <- infos]
    case Map.lookup "streaming-embeddings" byName of
      Just c -> do
        (changeTotal c) `shouldBe` 0
        (changeStatus c) `shouldBe` "no-tasks"
      Nothing -> error "streaming-embeddings change missing from parsed list"

  it "parses known changes with the expected task counts" $ do
    -- Hermetic: build a fixture under the current working directory with known
    -- change dirs and tasks.md files instead of reading the live
    -- openspec/changes tree, whose completion counts drift as progress is made
    -- (which broke this assertion). Kept out of openspec/changes so the other
    -- disk-scanning specs are unaffected, and removed on completion.
    cwd <- getCurrentDirectory
    let tmp = combine' cwd ".openspec-change-list-fixture"
    createDirectoryIfMissing True tmp
    writeFixture tmp "openspec-view"          13 0
    writeFixture tmp "extract-haskell-libs"   60 2
    writeFixture tmp "multi-source-graphs"    19 5
    writeFixture tmp "usable-graph-for-code-review" 25 5
    infos <- readAllChangeInfos tmp
    let byName = Map.fromList [(changeName c, c) | c <- infos]
        lookup' k = case Map.lookup k byName of
          Nothing -> error ("missing change in parsed fixture: " ++ k)
          Just c  -> c
        view = lookup' "openspec-view"
        ex = lookup' "extract-haskell-libs"
        mg = lookup' "multi-source-graphs"
        ug = lookup' "usable-graph-for-code-review"
    (changeTotal view) `shouldBe` 13
    (changeCompleted view) `shouldBe` 0
    (changeStatus view) `shouldBe` "in-progress"
    (changeTotal ex) `shouldBe` 60
    (changeCompleted ex) `shouldBe` 2
    (changeStatus ex) `shouldBe` "in-progress"
    (changeTotal mg) `shouldBe` 19
    (changeCompleted mg) `shouldBe` 5
    (changeStatus mg) `shouldBe` "in-progress"
    (changeTotal ug) `shouldBe` 25
    (changeCompleted ug) `shouldBe` 5
    (changeStatus ug) `shouldBe` "in-progress"
    -- Remove the fixture so it is not left behind in the working directory.
    -- `removePathRecursive` is unavailable on this base version, so delete the
    -- task files, then each change dir, then the parent dir.
    let dirs = ["openspec-view", "extract-haskell-libs",
                "multi-source-graphs", "usable-graph-for-code-review"]
    mapM_ (\c -> removeFile (combine' (combine' tmp c) "tasks.md")) dirs
    mapM_ (\c -> removeDirectory (combine' tmp c)) dirs
    removeDirectory tmp >> pure ()
