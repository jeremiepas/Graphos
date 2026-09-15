{-# OPTIONS_GHC -Wno-unused-imports #-}
module Graphos.Infrastructure.FileSystem.CacheSpec where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import System.IO (writeFile)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Infrastructure.FileSystem.Cache

node :: Text -> Node
node nid = Node
  { nodeId           = nid
  , nodeLabel        = fromText nid
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText "test.hs"
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
spec = do
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
