{-# LANGUAGE OverloadedStrings #-}
-- | Watch-mode wiring tests (wire-incremental-update 5.1).
--
-- The end-to-end update behavior is exercised by
-- 'Graphos.UseCase.UpdateConfluenceSpec' (CLI confluence: --fresh vs warm
-- run). Here we pin the wiring invariant that makes 5.1 true:
-- the production FileSystemPort persists an extraction through the
-- fingerprinted extraction cache using the output-dir convention.
module Graphos.UseCase.WatchCacheSpec (spec) where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (listDirectory)

import Test.Hspec

import Graphos.Domain.Types
import Graphos.Domain.Graph (makeStubNode)
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort(..))
import Graphos.Infrastructure.Wiring (productionFileSystemPort)

spec :: Spec
spec = describe "watch-mode cache wiring (wire-incremental-update 5.1)" $ do

  it "production FileSystemPort persists an extraction through the fingerprinted key" $ do
    withSystemTempDirectory "graphos-watch-cache" $ \outdir -> do
      let srcFile = outdir <> "/fixture.hs"
      writeFile srcFile "module F where\nf :: Int\nf = 1\n"
      let ext = extractionFromLists [makeStubNode srcFile] []
          fingerprint = T.pack "granularity=GranularityFunction;cliGranularity=none"
      fspSaveCachedExtraction productionFileSystemPort fingerprint srcFile ext outdir
      hit <- fspLoadCachedExtraction productionFileSystemPort fingerprint srcFile outdir
      hit `shouldSatisfy` isJust
      entries <- listDirectory (outdir <> "/cache")
      entries `shouldSatisfy` (not . null)