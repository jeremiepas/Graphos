{-# LANGUAGE OverloadedStrings #-}
-- | @graphos source@ range selection, outcomes and rendering
-- (function-source-access 1.4, 3.1, 3.4). Pure: the file arrives as lines.
module Graphos.UseCase.SourceSpec (spec) where

import Data.Aeson (Value(..), decodeStrict, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Bits ((.|.))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Short (fromText)
import Test.Hspec

import Graphos.Domain.Types
import Graphos.UseCase.Source

-- | A node of @src/a.ts@ with the given range.
mkNode :: Maybe Int -> Maybe Int -> Node
mkNode s e = Node
  { nodeId          = "a_verdict"
  , nodeLabel       = fromText "verdict"
  , nodeFileType    = CodeFile
  , nodeSourceFile  = fromText "src/a.ts"
  , nodeSource      = Nothing
  , nodeLineStart   = s
  , nodeLineEnd     = e
  , nodeCommunityId = Nothing
  , nodeDegree      = Nothing
  , nodeIsBridge    = Nothing
  , nodeExtra       = Nothing
  , nodeKind        = Just (fromText "Function")
  , nodeSignature   = Nothing
  , nodePresentBits = bitNodeKind
                      .|. maybe 0 (const bitNodeLineStart) s
                      .|. maybe 0 (const bitNodeLineEnd) e
  }

withBodyHash :: T.Text -> Node -> Node
withBodyHash h n = n { nodeExtra = Just (object ["body_hash" .= h]) }

fileOf :: Int -> [T.Text]
fileOf k = [ "line " <> T.pack (show i) | i <- [1 .. k] ]

shownNumbers :: SourceResult -> [Int]
shownNumbers (SourceShown v) = map fst (svShown v)
shownNumbers _               = []

jsonField :: T.Text -> SourceResult -> Maybe Value
jsonField k r = case decodeStrict (encodeUtf8 (renderSourceJSON r)) of
  Just (Object o) -> KM.lookup (Key.fromText k) o
  _               -> Nothing

spec :: Spec
spec = describe "graphos source (function-source-access)" $ do

  describe "range selection" $ do
    it "prints the node's lines under a header with id and file:start-end" $ do
      let r = selectSource 0 (mkNode (Just 1) (Just 3)) "/work/p/src/a.ts" (Just (fileOf 10))
      shownNumbers r `shouldBe` [1, 2, 3]
      let out = T.lines (renderSourceText r)
      take 1 out `shouldBe` ["a_verdict  src/a.ts:1-3"]
      drop 1 out `shouldBe` ["1 | line 1", "2 | line 2", "3 | line 3"]

    it "adds context lines before and after" $ do
      let r = selectSource 2 (mkNode (Just 10) (Just 12)) "f" (Just (fileOf 40))
      shownNumbers r `shouldBe` [8 .. 14]

    it "clips context to the file" $ do
      let r = selectSource 5 (mkNode (Just 2) (Just 3)) "f" (Just (fileOf 5))
      shownNumbers r `shouldBe` [1 .. 5]
      sourceSucceeded r `shouldBe` True

    it "prints the start line alone when the end is absent" $ do
      let r = selectSource 0 (mkNode (Just 12) Nothing) "f" (Just (fileOf 20))
      shownNumbers r `shouldBe` [12]

    it "the text is the node's own lines, without context" $ do
      case selectSource 3 (mkNode (Just 2) (Just 3)) "f" (Just (fileOf 9)) of
        SourceShown v -> svText v `shouldBe` "line 2\nline 3"
        other         -> expectationFailure (show other)

  describe "outcomes" $ do
    it "a node without a line range is reported with its source file" $ do
      let r = selectSource 0 (mkNode Nothing Nothing) "f" (Just (fileOf 3))
      r `shouldBe` SourceFailed (mkNode Nothing Nothing) NoLineRange
      sourceSucceeded r `shouldBe` False
      renderSourceText r `shouldSatisfy` T.isInfixOf "no line range"
      renderSourceText r `shouldSatisfy` T.isInfixOf "src/a.ts"

    it "a missing file names the resolved path and --root" $ do
      let path = resolveSourcePath "." "./src/a.ts"
          r = selectSource 0 (mkNode (Just 1) (Just 2)) path Nothing
      r `shouldBe` SourceFailed (mkNode (Just 1) (Just 2)) (MissingFile "src/a.ts")
      renderSourceText r `shouldSatisfy` T.isInfixOf "src/a.ts"
      renderSourceText r `shouldSatisfy` T.isInfixOf "--root"

    it "a file shorter than the node's end is reported" $ do
      let r = selectSource 0 (mkNode (Just 4) (Just 9)) "f" (Just (fileOf 5))
      r `shouldBe` SourceFailed (mkNode (Just 4) (Just 9)) (ShortFile "f" 5 9)

    it "a body hash that differs marks the lines stale and still prints them" $ do
      let n = withBodyHash "0000" (mkNode (Just 1) (Just 2))
          r = selectSource 0 n "f" (Just (fileOf 3))
      sourceSucceeded r `shouldBe` True
      shownNumbers r `shouldBe` [1, 2]
      sourceWarnings r `shouldSatisfy` any (T.isInfixOf "changed since the graph was built")
      jsonField "stale" r `shouldBe` Just (Bool True)

    it "a matching body hash is not stale" $ do
      let n = withBodyHash (spanHash "line 1\nline 2") (mkNode (Just 1) (Just 2))
          r = selectSource 0 n "f" (Just (fileOf 3))
      sourceWarnings r `shouldBe` []
      jsonField "stale" r `shouldBe` Just (Bool False)

    it "a node without a body hash prints no warning" $ do
      sourceWarnings (selectSource 0 (mkNode (Just 1) (Just 2)) "f" (Just (fileOf 3))) `shouldBe` []

  describe "paths" $ do
    it "resolves a relative source file against the root" $ do
      resolveSourcePath "/work/p" "./src/a.ts" `shouldBe` "/work/p/src/a.ts"
    it "keeps an absolute source file" $ do
      resolveSourcePath "/work/p" "/abs/b.hs" `shouldBe` "/abs/b.hs"

  describe "JSON" $ do
    it "one object with id, label, kind, source_file, line_start, line_end and text" $ do
      let r = selectSource 0 (mkNode (Just 1) (Just 2)) "f" (Just (fileOf 3))
      jsonField "id" r `shouldBe` Just (String "a_verdict")
      jsonField "label" r `shouldBe` Just (String "verdict")
      jsonField "kind" r `shouldBe` Just (String "Function")
      jsonField "source_file" r `shouldBe` Just (String "src/a.ts")
      jsonField "line_start" r `shouldBe` Just (Number 1)
      jsonField "line_end" r `shouldBe` Just (Number 2)
      jsonField "text" r `shouldBe` Just (String "line 1\nline 2")

    it "a failure carries an error code" $ do
      jsonField "error" (selectSource 0 (mkNode Nothing Nothing) "f" Nothing)
        `shouldBe` Just (String "no_line_range")
      jsonField "error" (selectSource 0 (mkNode (Just 1) Nothing) "f" Nothing)
        `shouldBe` Just (String "missing_file")
