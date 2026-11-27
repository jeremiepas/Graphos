{-# LANGUAGE OverloadedStrings #-}
-- | The file reader behind @graphos source@ (function-source-access 3.2),
-- exercised through the production port on a temporary directory.
module Graphos.Infrastructure.FileSystem.SourceReaderSpec (spec) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Data.Text.Short (fromText)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Graphos.Domain.Types
import Graphos.Infrastructure.FileSystem.SourceReader (readSourceLines)
import Graphos.Infrastructure.Wiring (productionSourceReaderPort)
import Graphos.UseCase.Source

nodeAt :: FilePath -> Int -> Int -> Node
nodeAt file s e = Node
  { nodeId          = "n"
  , nodeLabel       = fromText "n"
  , nodeFileType    = CodeFile
  , nodeSourceFile  = fromText (T.pack file)
  , nodeSource      = Nothing
  , nodeLineStart   = Just s
  , nodeLineEnd     = Just e
  , nodeCommunityId = Nothing
  , nodeDegree      = Nothing
  , nodeIsBridge    = Nothing
  , nodeExtra       = Nothing
  , nodeKind        = Nothing
  , nodeSignature   = Nothing
  , nodePresentBits = bitNodeLineStart
  }

spec :: Spec
spec = describe "source reader (function-source-access)" $ do
  it "reads an existing file as lines, dropping carriage returns" $
    withSystemTempDirectory "graphos-source" $ \dir -> do
      BC.writeFile (dir </> "a.hs") "one\r\ntwo\nthree"
      readSourceLines (dir </> "a.hs") `shouldReturn` Just ["one", "two", "three"]

  it "returns Nothing for a missing file" $
    withSystemTempDirectory "graphos-source" $ \dir ->
      readSourceLines (dir </> "absent.hs") `shouldReturn` Nothing

  it "prints a node's lines resolved against the root" $
    withSystemTempDirectory "graphos-source" $ \dir -> do
      BC.writeFile (dir </> "a.hs") "l1\nl2\nl3\nl4\n"
      r <- readNodeSource productionSourceReaderPort dir 0 (nodeAt "./a.hs" 2 3)
      case r of
        SourceShown v -> svText v `shouldBe` "l2\nl3"
        other         -> expectationFailure (show other)

  it "reports a missing file under the root" $
    withSystemTempDirectory "graphos-source" $ \dir -> do
      r <- readNodeSource productionSourceReaderPort dir 0 (nodeAt "src/a.hs" 1 1)
      r `shouldBe` SourceFailed (nodeAt "src/a.hs" 1 1) (MissingFile (dir </> "src/a.hs"))

  it "reports a file shorter than the node" $
    withSystemTempDirectory "graphos-source" $ \dir -> do
      BC.writeFile (dir </> "a.hs") "l1\nl2\n"
      r <- readNodeSource productionSourceReaderPort dir 0 (nodeAt "a.hs" 1 5)
      r `shouldBe` SourceFailed (nodeAt "a.hs" 1 5) (ShortFile (dir </> "a.hs") 2 5)
