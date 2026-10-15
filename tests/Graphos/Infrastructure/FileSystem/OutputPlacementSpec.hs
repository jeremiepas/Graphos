-- | Effective-output placement tests (multi-source-graphs 2.2): with a
-- configured @output:@ and no CLI override, the cache, manifest, and MCP
-- conversation directories relocate under the configured directory.
module Graphos.Infrastructure.FileSystem.OutputPlacementSpec where

import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Graphos.Infrastructure.FileSystem.Cache (cacheDirIn, embedCacheDirIn)
import Graphos.Infrastructure.FileSystem.Manifest
    ( ManifestEntry(..)
    , loadManifestIn
    , manifestPathIn
    , saveManifestIn
    )

spec :: Spec
spec = do
  describe "cacheDirIn / embedCacheDirIn place caches under the effective output" $
    it "resolves <outDir>/cache and <outDir>/cache/embeddings" $ do
      cacheDirIn "my-graph-out" `shouldBe` ("my-graph-out" </> "cache")
      embedCacheDirIn "my-graph-out"
        `shouldBe` ("my-graph-out" </> "cache" </> "embeddings")

  describe "manifest placement (saveManifestIn / loadManifestIn)" $ do
    it "round-trips through <outDir>/manifest.json" $
      withSystemTempDirectory "graphos-manifest-out" $ \tmp -> do
        let outDir = tmp </> "my-graph-out"
            entries = [ManifestEntry "a.hs" "2026-01-01T00:00:00Z" "deadbeef"]
        saveManifestIn entries outDir
        loaded <- loadManifestIn outDir
        loaded `shouldBe` Right entries

    it "manifestPathIn honors the configured output directory" $
      manifestPathIn "my-graph-out" `shouldBe` ("my-graph-out" </> "manifest.json")

    it "a missing manifest file loads as empty" $
      withSystemTempDirectory "graphos-manifest-out2" $ \tmp -> do
        loaded <- loadManifestIn (tmp </> "never-out")
        loaded `shouldBe` Right []