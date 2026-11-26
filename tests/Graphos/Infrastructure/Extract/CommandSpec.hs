-- | External command extractor infrastructure (custom-extractors 3.1–3.5).
--
-- Covers the pure contract (placeholder expansion, tolerant stdout decoding,
-- id bucketing, gating) and the runner itself, spawned against small shell
-- scripts so the @no shell@ / @timeout@ / @error isolation@ guarantees are
-- exercised rather than assumed.
module Graphos.Infrastructure.Extract.CommandSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Short (fromText, toText)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getPermissions
  , setPermissions
  , executable
  )
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec
import Test.QuickCheck hiding (Confidence)

import Graphos.Domain.Types
  ( Confidence(..)
  , Edge(..)
  , EdgeId(..)
  , ExtractionError(..)
  , FileType(..)
  , Node(..)
  , Relation(..)
  , computePresentBits
  , extEdges
  , extNodes
  )
import Graphos.Domain.Graph (mergeExtractions)
import Graphos.Domain.Extraction (CommandOutcome(..))
import Graphos.Infrastructure.Extract.Command
import Graphos.Infrastructure.FileSystem.Ignore
  ( AnnotatedPattern(..)
  , IgnorePattern(..)
  )

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | A node as an external extractor emits it: id + label only, no
-- source_file (the runner fills it in).
rawNode :: Text -> Node
rawNode nid =
  let n = Node
        { nodeId           = nid
        , nodeLabel        = fromText nid
        , nodeFileType     = CodeFile
        , nodeSourceFile   = ""
        , nodeSource       = Nothing
        , nodeLineStart    = Nothing
        , nodeLineEnd      = Nothing
        , nodeSignature    = Nothing
        , nodeCommunityId  = Nothing
        , nodeKind         = Just "Function"
        , nodeDegree       = Nothing
        , nodeIsBridge     = Nothing
        , nodeExtra        = Nothing
        , nodePresentBits  = 0
        }
  in n { nodePresentBits = computePresentBits n }

-- | Output carrying one node (and no edge).
oneNodeOutput :: Text -> CommandOutput
oneNodeOutput nid = CommandOutput [rawNode nid] [] 0 0

json :: String -> BS.ByteString
json = TE.encodeUtf8 . T.pack

-- | Write an executable shell script; Graphos runs it as argv[0], never
-- through a shell of its own.
writeScript :: FilePath -> String -> IO FilePath
writeScript root body = do
  let path = root </> "extractor.sh"
  writeFile path ("#!/bin/sh\n" ++ body ++ "\n")
  perms <- getPermissions path
  setPermissions path perms { executable = True }
  pure path

specWith :: [String] -> CommandSpec
specWith argv = CommandSpec
  { csArgv           = argv
  , csBatch          = False
  , csTimeoutSeconds = 10
  , csMaxOutputBytes = 1024 * 1024
  }

mkRawEdge :: Text -> Text -> Edge
mkRawEdge source target = Edge
  { edgeId         = EdgeId ""
  , edgeSource     = source
  , edgeTarget     = target
  , edgeRelation   = Calls
  , edgeWeight     = 1
  , edgeConfidence = Confidence 1
  , edgeExtra      = Nothing
  }

sourceFileText :: Node -> Text
sourceFileText = toText . nodeSourceFile

spec :: Spec
spec = describe "custom extractors command runner (avi-1766 3.1-3.5)" $ do
  describe "substituteCommand (3.2)" $ do
    it "replaces {file} for a single-file invocation" $
      substituteCommand ["ex", "{file}"] ["a.pl"] `shouldBe` ["ex", "a.pl"]

    it "splices {files} into one argv element per file" $
      substituteCommand ["ex", "{files}"] ["a.pl", "b.pl"]
        `shouldBe` ["ex", "a.pl", "b.pl"]

    it "expands an embedded {file} once per file" $
      substituteCommand ["ex", "--in={file}"] ["a.pl", "b.pl"]
        `shouldBe` ["ex", "--in=a.pl", "--in=b.pl"]

    it "appends the paths when no placeholder is declared" $
      substituteCommand ["my-extractor", "--json"] ["a.pl"]
        `shouldBe` ["my-extractor", "--json", "a.pl"]

    it "leaves argv untouched when there is nothing to substitute" $
      substituteCommand ["ex", "{file}"] [] `shouldBe` ["ex", "{file}"]

  describe "decodeCommandOutput (3.3)" $ do
    it "decodes graph.json-shaped arrays, defaulting file_type/source_file" $ do
      let out = decodeCommandOutput $ json
            "{\"nodes\":[{\"id\":\"fn:main\",\"label\":\"main\"}],\"edges\":[]}"
      case out of
        Left err -> expectationFailure (T.unpack err)
        Right o  -> do
          map nodeId (coNodes o) `shouldBe` ["fn:main"]
          map nodeFileType (coNodes o) `shouldBe` [CodeFile]
          map sourceFileText (coNodes o) `shouldBe` [""]

    it "accepts nodes keyed by id" $ do
      let out = decodeCommandOutput $ json
            "{\"nodes\":{\"fn:main\":{\"id\":\"fn:main\",\"label\":\"main\"}},\"edges\":[]}"
      fmap (map nodeId . coNodes) out `shouldBe` Right ["fn:main"]

    it "accepts the JGF graph envelope" $ do
      let out = decodeCommandOutput $ json
            "{\"graph\":{\"nodes\":{\"x\":{\"id\":\"x\",\"label\":\"X\"}},\"edges\":[]}}"
      fmap (map nodeId . coNodes) out `shouldBe` Right ["x"]

    it "drops invalid records instead of failing" $ do
      let out = decodeCommandOutput $ json
            "{\"nodes\":[{\"id\":\"fn:main\",\"label\":\"main\"},{\"label\":\"no id\"}],\"edges\":[]}"
      case out of
        Left err -> expectationFailure (T.unpack err)
        Right o  -> do
          length (coNodes o) `shouldBe` 1
          coDroppedNodes o `shouldBe` 1

    it "defaults edge weight/confidence and parses the relation" $ do
      let out = decodeCommandOutput $ json $ concat
            [ "{\"nodes\":[{\"id\":\"a\",\"label\":\"a\"},{\"id\":\"b\",\"label\":\"b\"}],"
            , "\"edges\":[{\"id\":\"e\",\"source\":\"a\",\"target\":\"b\",\"relation\":\"calls\"}]}"
            ]
      case out of
        Left err -> expectationFailure (T.unpack err)
        Right o  -> do
          map edgeWeight (coEdges o) `shouldBe` [1.0]
          map edgeRelation (coEdges o) `shouldBe` [Calls]

    it "drops an edge whose relation is unknown" $ do
      let out = decodeCommandOutput $ json $ concat
            [ "{\"nodes\":[{\"id\":\"a\",\"label\":\"a\"}],"
            , "\"edges\":[{\"source\":\"a\",\"target\":\"a\",\"relation\":\"frobnicates\"}]}"
            ]
      fmap coDroppedEdges out `shouldBe` Right 1

    it "reports malformed JSON as an error" $
      case decodeCommandOutput "not json" of
        Left err -> T.isInfixOf "invalid JSON" err `shouldBe` True
        Right _  -> expectationFailure "expected malformed JSON to be rejected"

    it "reports a document without nodes as an error" $
      case decodeCommandOutput $ json "{\"edges\":[]}" of
        Left err -> T.isInfixOf "missing" err `shouldBe` True
        Right _  -> expectationFailure "expected the missing section to be rejected"

  describe "id bucketing (3.3)" $ do
    it "prefixes a missing id and is idempotent" $ do
      let bucket = commandFileBucket "src/x.pl"
      commandIdPrefix bucket "fn:main" `shouldBe` bucket <> "fn:main"
      commandIdPrefix bucket (bucket <> "fn:main") `shouldBe` bucket <> "fn:main"

    it "fills a missing source_file and remaps edge endpoints" $ do
      let bucket = commandFileBucket "src/x.pl"
          out = CommandOutput
            [ rawNode "fn:main", rawNode "fn:helper" ]
            [ mkRawEdge "fn:main" "fn:helper" ]
            0 0
          ex = outputToExtraction "src/x.pl" out
      sort (Map.keys (extNodes ex))
        `shouldBe` [bucket <> "fn:helper", bucket <> "fn:main"]
      map sourceFileText (Map.elems (extNodes ex))
        `shouldBe` ["src/x.pl", "src/x.pl"]
      case Map.elems (extEdges ex) of
        [e] -> do
          edgeSource e `shouldBe` bucket <> "fn:main"
          edgeTarget e `shouldBe` bucket <> "fn:helper"
          edgeId e `shouldBe` EdgeId (bucket <> "fn:main->" <> bucket <> "fn:helper:calls")
        _ -> expectationFailure "expected exactly one remapped edge"

    it "keeps two files that emit the same raw id distinct" $ do
      let a = outputToExtraction "a/x.pl" (oneNodeOutput "fn:main")
          b = outputToExtraction "b/x.pl" (oneNodeOutput "fn:main")
      Map.size (extNodes (mergeExtractions a b)) `shouldBe` 2

    it "keeps same-stem files of different extensions distinct" $ do
      let a = outputToExtraction "src/x.pl" (oneNodeOutput "fn:main")
          b = outputToExtraction "src/x.pm" (oneNodeOutput "fn:main")
      Map.size (extNodes (mergeExtractions a b)) `shouldBe` 2

    it "keeps distinct files whose names sanitise alike apart" $ do
      let a = outputToExtraction "src/x y.pl" (oneNodeOutput "fn:main")
          b = outputToExtraction "src/xy.pl" (oneNodeOutput "fn:main")
      Map.size (extNodes (mergeExtractions a b)) `shouldBe` 2

    it "satisfies id uniqueness for any two distinct source files" $
      property prop_distinctFilesStayDistinct

    it "satisfies id qualification idempotence" $
      property prop_qualificationIdempotent

  describe "gating (3.4)" $ do
    let root = "/repo"
        plain = root </> "src" </> "x.pl"

    it "reports a disabled gate before anything runs" $
      gateCommandFile root False [] plain
        `shouldSatisfy` maybe False (T.isInfixOf "allow_command_extractors")

    it "reports a sensitive file" $
      gateCommandFile root True [] (root </> ".ssh" </> "config")
        `shouldSatisfy` maybe False (T.isInfixOf "sensitive")

    it "reports an ignored file" $ do
      let ignorePattern = AnnotatedPattern
            { apPattern = ExactPattern "hidden.pl"
            , apNegate = False
            , apPriority = 3
            }
      gateCommandFile root True [ignorePattern] (root </> "hidden.pl")
        `shouldSatisfy` maybe False (T.isInfixOf "ignored")

    it "lets a plain source file through" $
      gateCommandFile root True [] plain `shouldBe` Nothing

  describe "runner (3.1)" $ do
    it "runs the argv for one file and decodes its nodes" $
      withSystemTempDirectory "graphos-command" $ \root -> do
        script <- writeScript root
          "printf '%s' '{\"nodes\":[{\"id\":\"fn:main\",\"label\":\"main\"}],\"edges\":[]}'"
        outcome <- runCommandFile (specWith [script, "{file}"]) root True [] (root </> "x.pl")
        case outcome of
          CommandProduced ex -> Map.size (extNodes ex) `shouldBe` 1
          other -> expectationFailure ("expected CommandProduced, got " ++ show other)

    it "turns a non-zero exit into a recorded error, not an exception" $
      withSystemTempDirectory "graphos-command" $ \root -> do
        script <- writeScript root "exit 3"
        outcome <- runCommandFile (specWith [script, "{file}"]) root True [] (root </> "x.pl")
        case outcome of
          CommandFailed err -> do
            eeFile err `shouldBe` root </> "x.pl"
            eeExtractor err `shouldBe` "command"
            T.isInfixOf "exited with code 3" (eeMessage err) `shouldBe` True
          other -> expectationFailure ("expected CommandFailed, got " ++ show other)

    it "times out a sleeping extractor instead of hanging" $
      withSystemTempDirectory "graphos-command" $ \root -> do
        script <- writeScript root "sleep 5"
        outcome <- runCommandFile
          ((specWith [script, "{file}"]) { csTimeoutSeconds = 1 })
          root True [] (root </> "x.pl")
        case outcome of
          CommandFailed err -> T.isInfixOf "timed out" (eeMessage err) `shouldBe` True
          other -> expectationFailure ("expected CommandFailed, got " ++ show other)

    it "does not spawn anything for a gated file" $
      withSystemTempDirectory "graphos-command" $ \root -> do
        let marker = root </> "spawned"
        script <- writeScript root $
          "touch '" ++ marker ++ "'\n"
            ++ "printf '%s' '{\"nodes\":[{\"id\":\"fn:main\",\"label\":\"main\"}],\"edges\":[]}'"
        outcome <- runCommandFile (specWith [script, "{file}"]) root True []
          (root </> ".ssh" </> "config")
        outcome `shouldBe` CommandSkipped "sensitive file; no external command is spawned"
        (doesFileExist marker >>= (`shouldBe` False))

    it "does not spawn anything when command extractors are disabled" $
      withSystemTempDirectory "graphos-command" $ \root -> do
        let marker = root </> "spawned"
        script <- writeScript root ("touch '" ++ marker ++ "'")
        outcome <- runCommandFile (specWith [script, "{file}"]) root False [] (root </> "x.pl")
        outcome `shouldBe` CommandSkipped
          "command extractors are disabled (allow_command_extractors)"
        (doesFileExist marker >>= (`shouldBe` False))

    it "runs one batch invocation and routes nodes by source_file" $
      withSystemTempDirectory "graphos-command" $ \root -> do
        script <- writeScript root $ "printf '%s' '" ++ concat
          [ "{\"nodes\":["
          , "{\"id\":\"fn:a\",\"label\":\"a\",\"source_file\":\"a.pl\"},"
          , "{\"id\":\"fn:b\",\"label\":\"b\",\"source_file\":\"b.pl\"}"
          , "],\"edges\":[]}"
          ] ++ "'"
        let files = [root </> "a.pl", root </> "b.pl"]
        outcomes <- runCommandExtraction
          ((specWith [script, "{files}"]) { csBatch = True }) root True [] files
        Map.keys outcomes `shouldBe` files
        forM_ files $ \fp -> case Map.lookup fp outcomes of
          Just (CommandProduced ex) -> Map.size (extNodes ex) `shouldBe` 1
          other -> expectationFailure
            ("expected CommandProduced for " ++ fp ++ ", got " ++ show other)

    it "fails every file of a batch when the process fails" $
      withSystemTempDirectory "graphos-command" $ \root -> do
        script <- writeScript root "exit 2"
        let files = [root </> "a.pl", root </> "b.pl"]
        outcomes <- runCommandExtraction
          ((specWith [script, "{files}"]) { csBatch = True }) root True [] files
        forM_ files $ \fp -> case Map.lookup fp outcomes of
          Just (CommandFailed err) ->
            T.isInfixOf "exited with code 2" (eeMessage err) `shouldBe` True
          other -> expectationFailure
            ("expected CommandFailed for " ++ fp ++ ", got " ++ show other)

    it "records an error when a batch basename is ambiguous" $
      -- Two inputs named x.pl share a basename: a record that names only the
      -- basename cannot be routed, so it must surface as a per-file failure
      -- rather than silently disappearing.
      withSystemTempDirectory "graphos-command" $ \root -> do
        createDirectoryIfMissing True (root </> "a")
        createDirectoryIfMissing True (root </> "b")
        script <- writeScript root $ "printf '%s' '" ++
          "{\"nodes\":[{\"id\":\"fn:x\",\"label\":\"x\",\"source_file\":\"x.pl\"}],\"edges\":[]}'"
        let files = [root </> "a" </> "x.pl", root </> "b" </> "x.pl"]
        outcomes <- runCommandExtraction
          ((specWith [script, "{files}"]) { csBatch = True }) root True [] files
        forM_ files $ \fp -> case Map.lookup fp outcomes of
          Just (CommandFailed err) ->
            T.isInfixOf "no valid node" (eeMessage err) `shouldBe` True
          other -> expectationFailure
            ("expected CommandFailed for " ++ fp ++ ", got " ++ show other)

-- ───────────────────────────────────────────────
-- Properties
-- ───────────────────────────────────────────────

genPath :: Gen FilePath
genPath = do
  dir  <- elements ["src", "lib", "app", "a", "b", "src/api", "lib/util"]
  -- "x y" and "xy" sanitise to the same stem on purpose: the id bucket must
  -- still keep those two files apart.
  name <- elements ["x", "y", "main", "util", "x y", "xy"]
  ext  <- elements [".pl", ".pm", ".php"]
  pure (dir </> name ++ ext)

-- | Two distinct files that emit the same raw id must merge into two nodes.
-- The guard is only on the inputs being distinct: a bucket collision must make
-- this /fail/, never be discarded as an assumption.
prop_distinctFilesStayDistinct :: Property
prop_distinctFilesStayDistinct =
  withMaxSuccess 200 $
    forAll ((,) <$> genPath <*> genPath) $ \(fa, fb) ->
      fa /= fb ==>
        classify (takeDirectory fa == takeDirectory fb) "same directory" $
          Map.size (extNodes (mergeExtractions
            (outputToExtraction fa (oneNodeOutput "fn:main"))
            (outputToExtraction fb (oneNodeOutput "fn:main")))) === 2

-- | Re-qualifying an already qualified extraction changes nothing.
prop_qualificationIdempotent :: Property
prop_qualificationIdempotent =
  forAll genPath $ \fp ->
    forAll (elements ["fn:main", "x", "Module:a", ""]) $ \rawId ->
      let firstPass  = outputToExtraction fp (oneNodeOutput rawId)
          secondPass = outputToExtraction fp
                    (CommandOutput (Map.elems (extNodes firstPass)) [] 0 0)
      in extNodes secondPass === extNodes firstPass
