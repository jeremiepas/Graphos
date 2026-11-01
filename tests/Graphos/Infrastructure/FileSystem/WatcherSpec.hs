{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the multi-source file watcher (AVI-975 groups 5.1/5.2).
--
-- These exercise the pure attribution/coalescing logic that backs
-- `watchSources`, which is what can be tested deterministically without a live
-- fsnotify loop:
--   * longest-prefix source attribution (nested roots belong to the inner source)
--   * debounce-set insertion and coalescing (dedupe identical events)
module Graphos.Infrastructure.FileSystem.WatcherSpec where

import Test.Hspec
import Control.Concurrent.MVar (newMVar, takeMVar)
import Control.Monad (void)
import Graphos.Infrastructure.FileSystem.Watcher
  ( attributeEvent
  , startsWithBase
  , startsWith
  , recordChanged
  , recordChangedForSource
  , resolveWatchPaths
  , SourcePath
  )
import Graphos.Domain.Config.Source (SourceConfig(..))
import Data.Set (Set)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Directory (canonicalizePath, getTemporaryDirectory)
import System.IO.Temp (withTempDirectory)

spec :: Spec
spec = do
  describe "startsWithBase" $ do
    it "is True when path equals base" $
      startsWithBase "/project/src1" "/project/src1" `shouldBe` True
    it "is True when path is under base" $
      startsWithBase "/project/src1/sub/f.hs" "/project/src1" `shouldBe` True
    it "is False when path is unrelated" $
      startsWithBase "/other/f.hs" "/project/src1" `shouldBe` False
    it "is False for a sibling directory (not a loose prefix)" $
      startsWithBase "/project/src2/f.hs" "/project/src1" `shouldBe` False

  describe "startsWith" $ do
    it "reports a common leading prefix" $
      "nested/file.hs" `startsWith` "nested" `shouldBe` True
    it "is case sensitive and order sensitive" $
      "abc" `startsWith` "abd" `shouldBe` False

  describe "attributeEvent (longest-prefix)" $ do
    let src1 = SourceConfig (T.pack "src1") "/project/src1" []
        src2 = SourceConfig (T.pack "src2") "/project/src2" []
        configs = [src1, src2]
    it "attributes a path under src1 to src1" $
      fst (attributeEvent configs "/project/src1/sub/f.hs") `shouldBe` Just "src1"
    it "attributes a path under src2 to src2" $
      fst (attributeEvent configs "/project/src2/x") `shouldBe` Just "src2"
    it "returns Nothing name for a path under neither source" $
      fst (attributeEvent configs "/other/f.hs") `shouldBe` Nothing
    it "prefers the longer prefix when two sources match" $ do
      let cfg = [ SourceConfig (T.pack "short") "/project/s" []
                , SourceConfig (T.pack "long") "/project/short" [] ]
      fst (attributeEvent cfg "/project/short/deep/f.hs") `shouldBe` Just "long"
    it "relativizes the path against the owning source" $
      snd (attributeEvent [SourceConfig (T.pack "inner") "/worktrees/inner" []]
                           "/worktrees/inner/nested/f.hs") `shouldBe` "nested/f.hs"
    it "leaves the untouched path when nothing owns it" $
      snd (attributeEvent configs "/other/f.hs") `shouldBe` "/other/f.hs"

  describe "recordChanged (debounce-set insertion)" $ do
    it "inserts a source path into the debounce set" $ do
      mv <- newMVar (Set.empty :: Set SourcePath)
      recordChanged mv (Just "src1", "/project/src1/f.hs")
      contents <- takeMVar mv
      Set.member (Just "src1", "/project/src1/f.hs") contents `shouldBe` True

  describe "recordChangedForSource attributes a single change" $ do
    it "stores the (sourceName, path) pair" $ do
      mv <- newMVar (Set.empty :: Set SourcePath)
      recordChangedForSource mv (Just "inner") "/worktrees/inner/f.hs"
      contents <- takeMVar mv
      Set.member (Just "inner", "/worktrees/inner/f.hs") contents `shouldBe` True

  describe "coalescing through the debounce set" $ do
    it "dedupes identical events" $ do
      mv <- newMVar (Set.empty :: Set SourcePath)
      recordChangedForSource mv (Just "inner") "/worktrees/inner/a.hs"
      recordChangedForSource mv (Just "inner") "/worktrees/inner/a.hs"
      contents <- takeMVar mv
      Set.size contents `shouldBe` 1
    it "keeps 10 distinct events across 2 sources as 10 pairs" $ do
      mv <- newMVar (Set.empty :: Set SourcePath)
      let pairs = [ (Just "inner", "/worktrees/inner/f" ++ show i ++ ".hs") | i <- [0 .. 4 :: Int] ]
                  ++ [ (Just "outer", "/worktrees/outer/g" ++ show i ++ ".hs") | i <- [0 .. 4 :: Int] ]
      mapM_ (\p -> recordChangedForSource mv (fst p) (snd p)) pairs
      contents <- takeMVar mv
      Set.size contents `shouldBe` 10

  describe "resolveWatchPaths (openable absolute paths)" $ do
    let src1 = SourceConfig (T.pack "src1") "/project/src1" []
        src2 = SourceConfig (T.pack "src2") "/project/src2" []
        cfgs = [src1, src2]

    it "joins a relative path onto its owning source's configured root" $
      resolveWatchPaths cfgs [(Just "src1", "sub/f.hs")] `shouldBe` ["/project/src1/sub/f.hs"]

    it "equals root </> rel for the owning source" $
      resolveWatchPaths cfgs [(Just "src2", "g.hs")] `shouldBe` ["/project/src2/g.hs"]

    it "preserves nested relative components" $
      resolveWatchPaths cfgs [(Just "src1", "deep/nested/f.hs")] `shouldBe` ["/project/src1/deep/nested/f.hs"]

    it "returns an untouched path unchanged when no source owns it" $
      resolveWatchPaths cfgs [(Nothing, "/elsewhere/f.hs")] `shouldBe` ["/elsewhere/f.hs"]

    it "delivers an openable, canonicalizable path for a real change" $
      do
        tmpRoot <- getTemporaryDirectory
        withTempDirectory tmpRoot "graphos-watch-resolve" $ \dir -> do
          let src    = SourceConfig (T.pack "src1") dir []
              rel    = "changed.hs"
              marker = "module WatchResolve where"
              expect = dir ++ "/" ++ rel
          -- resolveWatchPaths yields exactly [root </> rel]
          resolveWatchPaths [src] [(Just "src1", rel)] `shouldBe` (expect : [])
          -- the assembled path is openable and canonicalizes on disk
          BS8.writeFile expect marker
          content <- BS8.readFile expect
          content `shouldBe` marker
          void (canonicalizePath expect)
