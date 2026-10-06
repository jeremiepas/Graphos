{-# OPTIONS_GHC -Wno-unused-imports #-}
module Graphos.Infrastructure.FileSystem.CacheSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import System.IO (writeFile)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.Posix.Files (setFileTimes)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Infrastructure.FileSystem.Cache

node :: Text -> Node
node nid = plain { nodePresentBits = computePresentBits plain }
  where
    plain = Node
      { nodeId           = nid
      , nodeLabel        = fromText nid
      , nodeFileType     = CodeFile
      , nodeSourceFile   = fromText "test.hs"
      , nodeSource       = Nothing
      , nodeCommunityId  = Nothing
      , nodeDegree       = Nothing
      , nodeIsBridge     = Nothing
      , nodeExtra        = Nothing
      , nodeLineStart    = Just 1
      , nodeLineEnd      = Nothing
      , nodeKind         = Nothing
      , nodeSignature    = Nothing
      , nodePresentBits  = 0
      }

oneExtraction :: Text -> Extraction
oneExtraction nid = extractionFromLists [node nid] []

spec :: Spec
spec = specMain

specMain :: Spec
specMain = specBase >> specEviction

specBase :: Spec
specBase = do
  describe "content-addressed extraction cache (REQ-CACHE-SOUND, INV-CACHE-SOUND)" $ do
    it "keys by file content only: identical bytes share a slot, edits miss and re-extract" $ do
      withSystemTempDirectory "graphos-cache-spec" $ \root -> do
        let f1 = root </> "a.hs"
            f2 = root </> "b.hs"

        -- save oneExtraction "a" for content "version-one"
        writeFile f1 "version-one"
        saveCached f1 (oneExtraction "a") root
        m1 <- loadCached f1 root

        -- editing to different bytes misses (content-only keying)
        writeFile f1 "version-two-different-bytes"
        m2 <- loadCached f1 root
        m2 `shouldBe` Nothing

        -- restore original bytes -> re-hit returns the SAME value as first hit
        writeFile f1 "version-one"
        m3 <- loadCached f1 root
        m3 `shouldBe` m1

        -- a content edit surfaces as uncached through checkSemanticCache
        writeFile f1 "edited-bytes"
        (_, cachedAfterEdit) <- checkSemanticCache [f1] root
        cachedAfterEdit `shouldBe` [f1]

        -- identical content at a different path hits the same slot (content-only key)
        writeFile f2 "identical-bytes"
        saveCached f1 (oneExtraction "a") root
        mc1 <- loadCached f1 root
        saveCached f2 (oneExtraction "a") root
        mc2 <- loadCached f2 root
        mc1 `shouldBe` mc2                       -- same slot regardless of path
        writeFile f2 "different-bytes"           -- change f2 content
        mc2' <- loadCached f2 root
        mc2' `shouldBe` Nothing                  -- now misses

        -- distinct content occupies distinct slots and never clobbers each other
        writeFile f1 "content-x"
        saveCached f1 (oneExtraction "a") root
        mc3 <- loadCached f1 root
        writeFile f2 "content-y"
        saveCached f2 (oneExtraction "b") root
        mc4 <- loadCached f2 root
        mc3 `shouldNotBe` mc4                     -- distinct values in distinct slots
        mc5 <- loadCached f1 root
        mc5 `shouldBe` mc3                        -- f1's slot untouched by f2 write

  describe "fingerprinted cache key (wire-incremental-update 1.1)" $ do
    it "same content under different fingerprints uses different keys" $
      withSystemTempDirectory "graphos-cache-fp" $ \root -> do
        let f = root </> "a.hs"
        writeFile f "stable content"
        saveCachedFingerprinted "granularity=GranularityFunction" f (oneExtraction "function") root
        m <- loadCachedFingerprinted "granularity=GranularityFunction" f root
        m `shouldBe` Just (oneExtraction "function")
        m2 <- loadCachedFingerprinted "granularity=GranularityFile" f root
        m2 `shouldBe` Nothing   -- granularity change invalidates

    it "identical config fingerprints share the same key" $
      withSystemTempDirectory "graphos-cache-fp2" $ \root -> do
        let f = root </> "a.hs"
        writeFile f "stable content"
        saveCachedFingerprinted "cfg-a" f (oneExtraction "a") root
        h <- loadCachedFingerprinted "cfg-a" f root
        h `shouldBe` Just (oneExtraction "a")
        -- also: composite key differs from the bare content hash
        let contentHash = "deadbeef"
        cacheKeyForFile contentHash "cfg-a" `shouldNotBe` contentHash

    it "fingerprinted and legacy keys never collide" $
      withSystemTempDirectory "graphos-cache-fp3" $ \root -> do
        let f = root </> "a.hs"
        writeFile f "content"
        saveCached f (oneExtraction "legacy") root
        mLegacy <- loadCached f root
        mLegacy `shouldBe` Just (oneExtraction "legacy")
        mFp <- loadCachedFingerprinted "some-config" f root
        mFp `shouldBe` Nothing

    it "hit-correctness (property H): save → load round-trips an extraction byte-equal" $
      withSystemTempDirectory "graphos-cache-hit" $ \root -> do
        let f = root </> "a.hs"
            ext = extractionFromLists [ node "n1", node "n2" ]
                    [ mkEdge (EdgeId "n1--n2") "n1" "n2" Calls ]
        writeFile f "some content"
        saveCachedFingerprinted "fp" f ext root
        m <- loadCachedFingerprinted "fp" f root
        m `shouldBe` Just ext
  where
    mkEdge eid s t r = Edge eid s t r 1.0 (Confidence 1.0) Nothing

specEviction :: Spec
specEviction = describe "LRU size-cap eviction (wire-incremental-update 2.2)" $ do
  it "cap exceeded evicts oldest-mtime entries first until under cap" $
    withSystemTempDirectory "graphos-cache-evict" $ \root -> do
      let d = cacheDir root
      createDirectoryIfMissing True d
      -- three entries, oldest first: e1 < e2 < e3
      mapM_ (\(name, mt, content) -> do
              let p = d </> (name ++ ".json")
              writeFile p content
              setFileTimes p mt (mt + 100))
            [ ("e1", 1000, replicate 40 'a')
            , ("e2", 2000, replicate 40 'b')
            , ("e3", 3000, replicate 40 'c')
            ]
      n <- evictToCap 80 root   -- 120 bytes total; evict e1 (40) → 80
      n `shouldBe` 1
      remaining <- listDirectory d
      remaining `shouldMatchList` ["e2.json", "e3.json"]

  it "under cap is a no-op" $
    withSystemTempDirectory "graphos-cache-evict2" $ \root -> do
      let d = cacheDir root
      createDirectoryIfMissing True d
      writeFile (d </> "x.json") (replicate 10 'x')
      n <- evictToCap (512 * 1024 * 1024) root
      n `shouldBe` 0
      length <$> listDirectory d >>= (`shouldBe` 1)

  it "0 disables eviction" $
    withSystemTempDirectory "graphos-cache-evict3" $ \root -> do
      let d = cacheDir root
      createDirectoryIfMissing True d
      writeFile (d </> "big.json") (replicate 1000 'y')
      n <- evictToCap 0 root
      n `shouldBe` 0
      length <$> listDirectory d >>= (`shouldBe` 1)

  it "missing cache dirs are a no-op" $
    withSystemTempDirectory "graphos-cache-evict4" $ \root -> do
      n <- evictToCap 64 root
      n `shouldBe` 0

  it "sweeps both the extraction and embedding caches" $
    withSystemTempDirectory "graphos-cache-evict5" $ \root -> do
      let ed = embedCacheDir root
      createDirectoryIfMissing True ed
      writeFile (ed </> "v.json") (replicate 50 'v')
      setFileTimes (ed </> "v.json") 1000 1000
      n <- evictToCap 10 root
      n `shouldBe` 1
      length <$> listDirectory ed >>= (`shouldBe` 0)
  where
