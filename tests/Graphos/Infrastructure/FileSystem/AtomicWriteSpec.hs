{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.FileSystem.AtomicWriteSpec (spec) where

import Control.Concurrent (killThread, forkIO, threadDelay)
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLA
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

import Graphos.Infrastructure.FileSystem.AtomicWrite

spec :: Spec
spec = do
  describe "writeFileAtomic" $ do
    it "writes the full content to the target path" $ do
      withSystemTempDirectory "graphos-atomic" $ \dir -> do
        let path = dir </> "out.json"
        writeFileAtomic path "hello world"
        BSL.readFile path `shouldReturn` "hello world"

    it "replaces prior content on successful write" $ do
      withSystemTempDirectory "graphos-atomic" $ \dir -> do
        let path = dir </> "out.json"
        writeFileAtomic path "old content"
        writeFileAtomic path "new content"
        BSL.readFile path `shouldReturn` "new content"

    it "creates the parent directory if missing" $ do
      withSystemTempDirectory "graphos-atomic" $ \dir -> do
        let path = dir </> "sub" </> "dir" </> "out.json"
        writeFileAtomic path "nested"
        BSL.readFile path `shouldReturn` "nested"

    it "leaves the prior file intact when the write is interrupted" $ do
      -- Simulate an interruption: a concurrent writer starts a large write and
      -- is killed mid-flight. The target is either the untouched prior content
      -- or the complete new payload (if the commit finished before the kill) —
      -- never a truncated mixture.
      withSystemTempDirectory "graphos-atomic" $ \dir -> do
        let path = dir </> "graph.json"
            bigPayload = BSL.fromStrict (TE.encodeUtf8 (T.replicate 100000 "x"))
        writeFileAtomic path "previous-good-content"
        tid <- forkIO (writeFileAtomic path bigPayload)
        threadDelay 500
        killThread tid
        contents <- BSL.readFile path
        contents `shouldSatisfy` \c ->
          c == "previous-good-content" || c == bigPayload

  describe "temp file hygiene" $ do
    it "leaves no temp files behind after success" $ do
      withSystemTempDirectory "graphos-atomic" $ \dir -> do
        let path = dir </> "out.json"
        writeFileAtomic path "content"
        entries <- listDirectory dir
        entries `shouldBe` ["out.json"]

    it "keeps prior file and removes temp when the action throws" $ do
      withSystemTempDirectory "graphos-atomic" $ \dir -> do
        let path = dir </> "out.json"
        writeFileAtomic path "prior"
        result <- try $ withAtomicHandle path (\_ -> ioError (userError "boom"))
        case result of
          Left (_ :: SomeException) -> pure ()
          Right () -> fail "expected exception"
        stillThere <- doesFileExist path
        stillThere `shouldBe` True
        entries <- listDirectory dir
        entries `shouldBe` ["out.json"]
        BSL.readFile path `shouldReturn` "prior"

  describe "withAtomicHandle" $ do
    it "commits streamed content on success" $ do
      withSystemTempDirectory "graphos-atomic" $ \dir -> do
        let path = dir </> "stream.json"
        withAtomicHandle path $ \h -> do
          mapM_ (BSL.hPut h) ["chunk1,", "chunk2,", "chunk3"]
        BSL.readFile path `shouldReturn` "chunk1,chunk2,chunk3"

  describe "writeTextFileAtomic / writeStringFileAtomic" $ do
    it "writes UTF-8 text" $ do
      withSystemTempDirectory "graphos-atomic" $ \dir -> do
        let path = dir </> "text.md"
        writeTextFileAtomic path "héllo wörld"
        BSL.readFile path `shouldReturn` TLA.encodeUtf8 (TL.pack "héllo wörld")
        writeStringFileAtomic (dir </> "text2.md") "plain string"
        BSL.readFile (dir </> "text2.md") `shouldReturn` TLA.encodeUtf8 (TL.pack "plain string")
