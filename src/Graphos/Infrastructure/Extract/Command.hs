-- | External command extractor runner (custom-extractors 3.1–3.4).
--
-- An extension configured with @mode: command@ declares an argv list (never a
-- shell string) that prints graph.json-shaped @nodes@ / @edges@ on stdout.
-- This module runs that argv for a single file or for a batch of files,
-- decodes the output tolerantly, prefixes node ids with a per-file directory
-- bucket so two files emitting the same id stay distinct, and turns every
-- failure (non-zero exit, timeout, oversized or malformed output) into a
-- recorded 'ExtractionError' — never into an exception that could abort the
-- build.
--
-- Contract (design D2–D4, D8 and spec @external-command-extractor@):
--
--   * 'System.Process.proc' is used deliberately: no @\/bin\/sh@, no string
--     interpolation of user input into a shell command line;
--   * every invocation has a wall-clock timeout ('defaultCommandTimeoutSeconds'
--     when the config omits one) and a bounded stdout read;
--   * the gate ('gateCommandFile') is evaluated /before/ spawning anything, so
--     sensitive and ignored paths never reach an external process.
module Graphos.Infrastructure.Extract.Command
  ( -- * Declared command
    CommandSpec(..)
  , commandSpecFromConfig
  , defaultMaxCommandOutputBytes

    -- * Placeholder expansion
  , substituteCommand

    -- * Output decoding
  , CommandOutput(..)
  , decodeCommandOutput
  , outputToExtraction
  , commandFileBucket
  , commandIdPrefix

    -- * Gating
  , gateCommandFile

    -- * Running
  , runCommandExtraction
  , runCommandFile
  , runCommandBatch
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, fromException, onException, tryJust)
import Control.Monad (void)
import Data.Aeson (Result(..), Value(..), eitherDecodeStrict', fromJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isPrint)
import qualified Data.List as List (foldl', isInfixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Short (fromText, toText)
import qualified Data.Vector as V
import System.Exit (ExitCode(..))
import System.FilePath (normalise, takeBaseName, takeExtension, takeFileName)
import System.IO (Handle)
import System.Process
  ( CreateProcess(..)
  , StdStream(..)
  , proc
  , terminateProcess
  , waitForProcess
  , withCreateProcess
  )
import System.Timeout (timeout)

import Graphos.Domain.Config.Extraction
  ( ExtractorConfig(..)
  , defaultCommandTimeoutSeconds
  )
import Graphos.Domain.Extraction (CommandOutcome(..))
import Graphos.Domain.Types
  ( Edge(..)
  , EdgeId(..)
  , Extraction
  , ExtractionError(..)
  , Node(..)
  , extractionFromLists
  , relationToText
  )
import Graphos.Infrastructure.Extract.TreeSitter.Core (mkSourceFile, sourceDirHash)
import Graphos.Infrastructure.FileSystem.Ignore (AnnotatedPattern, ignoreMatches)
import Graphos.Infrastructure.FileSystem.Sensitive (isSensitive)

-- ───────────────────────────────────────────────
-- Declared command
-- ───────────────────────────────────────────────

-- | One resolved command-extractor invocation plan.
data CommandSpec = CommandSpec
  { csArgv           :: ![String]  -- ^ declared argv; @{file}@ / @{files}@ are expanded at run time
  , csBatch          :: !Bool      -- ^ one invocation for every file instead of one per file
  , csTimeoutSeconds :: !Int       -- ^ wall-clock cap for one invocation
  , csMaxOutputBytes :: !Int       -- ^ stdout cap for one invocation
  } deriving (Eq, Show)

-- | Hard ceiling on captured stdout before an invocation is declared failed.
-- Bounds the memory a misbehaving extractor can force Graphos to hold; the
-- timeout bounds its wall-clock time (design: "Huge stdout -> size cap + timeout").
defaultMaxCommandOutputBytes :: Int
defaultMaxCommandOutputBytes = 64 * 1024 * 1024

-- | Build a 'CommandSpec' from a @mode: command@ entry, applying the
-- documented defaults for every omitted field.
commandSpecFromConfig :: ExtractorConfig -> CommandSpec
commandSpecFromConfig ec = CommandSpec
  { csArgv           = fromMaybe [] (ecCommand ec)
  , csBatch          = fromMaybe False (ecBatch ec)
  , csTimeoutSeconds = max 1 (fromMaybe defaultCommandTimeoutSeconds (ecTimeoutSeconds ec))
  , csMaxOutputBytes = defaultMaxCommandOutputBytes
  }

-- ───────────────────────────────────────────────
-- Placeholder expansion (task 3.2)
-- ───────────────────────────────────────────────

-- | Expand @{file}@ / @{files}@ in a declared argv.
--
--   * an argv element that is exactly @{file}@ or @{files}@ expands to one
--     element per supplied path (in batch mode every path becomes its own
--     argv element — no space-joined string);
--   * inside a larger element each placeholder is replaced once per supplied
--     path, emitting one element per path (@--input={file}@ becomes
--     @--input=a.pl@, @--input=b.pl@);
--   * when the argv contains no placeholder at all, the supplied paths are
--     appended after the declared argv, so @command: \[\"my-extractor\"\]@ is a
--     valid single-file and batch declaration.
substituteCommand :: [String] -> [FilePath] -> [String]
substituteCommand argv files
  | null files = argv
  | not usesPlaceholder = argv ++ files
  | otherwise = concatMap expand argv
  where
    usesPlaceholder = any hasPlaceholder argv

    hasPlaceholder arg = "{file}" `List.isInfixOf` arg || "{files}" `List.isInfixOf` arg

    expand arg
      | arg == "{file}" || arg == "{files}" = files
      | hasPlaceholder arg = map (substituteOne arg) files
      | otherwise = [arg]

    substituteOne arg file =
      T.unpack $ T.replace "{files}" (T.pack file) $ T.replace "{file}" (T.pack file) $ T.pack arg

-- ───────────────────────────────────────────────
-- Decoded output
-- ───────────────────────────────────────────────

-- | A decoded command stdout document, before per-file id qualification.
data CommandOutput = CommandOutput
  { coNodes        :: ![Node]
  , coEdges        :: ![Edge]
  , coDroppedNodes :: !Int  -- ^ node records that failed schema validation
  , coDroppedEdges :: !Int  -- ^ edge records that failed schema validation
  } deriving (Eq, Show)

-- | Decode graph.json-shaped stdout: @{\"nodes\": …, \"edges\": …}@ (also
-- accepted under a JGF @\"graph\"@ envelope). @nodes@ may be an array or an
-- object keyed by id; @edges@ is optional and may use the same two shapes
-- graph.json itself writes. Malformed individual records are dropped and
-- counted (spec: "drop invalid records (not fatal)"); a structurally broken
-- document returns 'Left'.
decodeCommandOutput :: BS.ByteString -> Either Text CommandOutput
decodeCommandOutput raw = do
  value <- case eitherDecodeStrict' raw of
    Left err -> Left $ "invalid JSON: " <> firstLine (T.pack err)
    Right v  -> Right v
  section <- case value of
    Object root
      | Just (Object graph) <- KM.lookup (Key.fromText "graph") root
      , KM.member (Key.fromText "nodes") graph -> Right graph
      | KM.member (Key.fromText "nodes") root -> Right root
      | otherwise -> Left "missing \"nodes\" (expected {\"nodes\": …, \"edges\": …})"
    _ -> Left "stdout must be a JSON object"
  nodeVals <- case KM.lookup (Key.fromText "nodes") section of
    Nothing -> Left "missing \"nodes\" (expected {\"nodes\": …, \"edges\": …})"
    Just v  -> itemsOf "nodes" v
  edgeVals <- case KM.lookup (Key.fromText "edges") section of
    Nothing -> Right []
    Just v  -> itemsOf "edges" v
  let (nodes, badNodes) = parseEach parseNodeValue nodeVals
      (edges, badEdges) = parseEach parseEdgeValue edgeVals
  pure CommandOutput
    { coNodes        = nodes
    , coEdges        = edges
    , coDroppedNodes = badNodes
    , coDroppedEdges = badEdges
    }

itemsOf :: Text -> Value -> Either Text [Value]
itemsOf _    (Array items) = Right (V.toList items)
itemsOf _    (Object obj)  = Right [v | (_, v) <- KM.toList obj]
itemsOf name _             = Left $ "\"" <> name <> "\" must be an array or an object keyed by id"

parseEach :: (Value -> Either Text a) -> [Value] -> ([a], Int)
parseEach parse = foldr step ([], 0)
  where
    step v (acc, bad) = case parse v of
      Right x -> (x : acc, bad)
      Left _  -> (acc, bad + 1)

-- | Parse one node record, defaulting the fields a command extractor is not
-- expected to emit (@file_type@, @source_file@) and accepting the JGF
-- @metadata@ layout by hoisting its fields.
parseNodeValue :: Value -> Either Text Node
parseNodeValue (Object km0) =
  let km = withDefault "file_type" (String "code")
         $ withDefault "source_file" (String "")
         $ hoistMetadata nodeAliases km0
  in case fromJSON (Object km) of
       Error err -> Left (T.pack err)
       Success n
         | T.null (nodeId n)             -> Left "node id is empty"
         | T.null (toText (nodeLabel n)) -> Left "node label is empty"
         | otherwise                     -> Right n
parseNodeValue _ = Left "node record must be an object"

nodeAliases :: [(Text, Text)]
nodeAliases =
  [ ("id", "id"), ("label", "label"), ("kind", "kind")
  , ("file_type", "file_type"), ("source_file", "source_file")
  , ("signature", "signature"), ("line_start", "line_start")
  , ("line_end", "line_end"), ("extra", "extra")
  ]

-- | Parse one edge record. @id@ is recomputed during qualification anyway, so
-- it only needs to be present; @weight@ / @confidence@ default to 1.
parseEdgeValue :: Value -> Either Text Edge
parseEdgeValue (Object km0) =
  let km = withDefault "id" (String "")
         $ withDefault "weight" (Number 1)
         $ withDefault "confidence" (Number 1)
         $ hoistMetadata edgeAliases km0
  in case fromJSON (Object km) of
       Error err -> Left (T.pack err)
       Success e
         | T.null (edgeSource e) -> Left "edge source is empty"
         | T.null (edgeTarget e) -> Left "edge target is empty"
         | otherwise             -> Right e
parseEdgeValue _ = Left "edge record must be an object"

edgeAliases :: [(Text, Text)]
edgeAliases =
  [ ("id", "id"), ("relation", "relation"), ("weight", "weight")
  , ("confidence", "confidence"), ("extra", "extra")
  ]

-- | Copy @metadata.<name>@ up to the top level when the top level lacks it.
hoistMetadata :: [(Text, Text)] -> KM.KeyMap Value -> KM.KeyMap Value
hoistMetadata aliases km =
  case KM.lookup (Key.fromText "metadata") km of
    Just (Object meta) -> List.foldl' addAlias km aliases
      where
        addAlias acc (inner, flat) =
          case KM.lookup (Key.fromText inner) meta of
            Just v | not (KM.member (Key.fromText flat) acc) ->
              KM.insert (Key.fromText flat) v acc
            _ -> acc
    _ -> km

withDefault :: Text -> Value -> KM.KeyMap Value -> KM.KeyMap Value
withDefault name value km
  | KM.member (Key.fromText name) km = km
  | otherwise = KM.insert (Key.fromText name) value km

-- ───────────────────────────────────────────────
-- Id qualification (task 3.3)
-- ───────────────────────────────────────────────

-- | Directory bucket for node ids emitted for @fp@:
-- @\<directory hash\>_\<stem\>_\<extension\>_@ — the tree-sitter id layout's
-- directory hash plus the file stem and extension, so two files that emit the
-- same raw id (@fn:main@) still merge into two distinct nodes.
commandFileBucket :: FilePath -> Text
commandFileBucket fp =
  sourceDirHash (mkSourceFile fp "") <> "_" <> stem <> "_" <> extTag <> lossTag <> "_"
  where
    rawStem = T.pack (takeBaseName fp)
    rawExt  = T.pack (drop 1 (takeExtension fp))
    stem    = sanitize rawStem
    extTag  = case sanitize rawExt of
      "" -> "noext"
      e  -> e
    -- Sanitising /drops/ characters, so two distinct files in one directory
    -- would otherwise collapse to the same bucket ("x y.pl" vs "xy.pl", both
    -- "xy"). When anything was dropped, append a hash of the path to restore
    -- uniqueness; ordinary ids keep their short readable form.
    lossTag
      | sanitize rawStem == rawStem && sanitize rawExt == rawExt = ""
      | otherwise = "_" <> shortHash (T.pack fp)
    sanitize = T.filter (\c -> isAlphaNum c || c `elem` ("-." :: String))
    shortHash = T.pack . show . (`mod` (65536 :: Int))
      . T.foldl' (\acc c -> acc * 31 + fromEnum c) 0

-- | Prefix an emitted id with @bucket@ unless it already carries it, so
-- re-extraction of already-qualified output is idempotent.
commandIdPrefix :: Text -> Text -> Text
commandIdPrefix bucket nid
  | bucket `T.isPrefixOf` nid = nid
  | otherwise                 = bucket <> nid

-- | Turn one file's decoded output into an 'Extraction': node ids gain the
-- bucket when missing, missing @source_file@ is filled in, edge endpoints
-- follow the nodes they name, and edge ids are recomputed from the final
-- endpoints so two files cannot collide on an emitted edge id.
outputToExtraction :: FilePath -> CommandOutput -> Extraction
outputToExtraction fp out =
  let bucket = commandFileBucket fp
      nodes  = map (qualifyNode bucket) (coNodes out)
      remap  = Map.fromList (zip (map nodeId (coNodes out)) (map nodeId nodes))
      edges  = map (qualifyEdge remap) (coEdges out)
  in extractionFromLists nodes edges
  where
    qualifyNode bucket n =
      let sourceFile
            | T.null (toText (nodeSourceFile n)) = fromText (T.pack fp)
            | otherwise                          = nodeSourceFile n
      in n { nodeId = commandIdPrefix bucket (nodeId n)
           , nodeSourceFile = sourceFile
           }

    qualifyEdge remap e =
      let source = remapEndpoint remap (edgeSource e)
          target = remapEndpoint remap (edgeTarget e)
      in e { edgeSource = source
           , edgeTarget = target
           , edgeId = EdgeId (source <> "->" <> target <> ":" <> relationToText (edgeRelation e))
           }

    remapEndpoint remap nid = Map.findWithDefault nid nid remap

-- ───────────────────────────────────────────────
-- Gating (task 3.4)
-- ───────────────────────────────────────────────

-- | Decide whether @fp@ may be handed to an external command. Evaluated
-- before any process is spawned: a disabled gate, a sensitive path or an
-- ignored path returns the reason, and the runner then reports the file as
-- skipped without ever spawning anything.
gateCommandFile :: FilePath -> Bool -> [AnnotatedPattern] -> FilePath -> Maybe Text
gateCommandFile root allow patterns fp
  | not allow = Just "command extractors are disabled (allow_command_extractors)"
  | isSensitive fp = Just "sensitive file; no external command is spawned"
  | ignoreMatches root patterns fp = Just "ignored by an ignore pattern; no external command is spawned"
  | otherwise = Nothing

-- ───────────────────────────────────────────────
-- Running (task 3.1)
-- ───────────────────────────────────────────────

-- | Run the declared command for @files@, honouring @batch@: one invocation
-- for all files when set, one per file otherwise. Every file gets an outcome;
-- gated files are reported as 'CommandSkipped'.
runCommandExtraction
  :: CommandSpec
  -> FilePath          -- ^ scan root the ignore patterns are relative to
  -> Bool              -- ^ allow_command_extractors
  -> [AnnotatedPattern]
  -> [FilePath]
  -> IO (Map FilePath CommandOutcome)
runCommandExtraction spec root allow patterns files
  | csBatch spec = runCommandBatch spec root allow patterns files
  | otherwise = do
      outcomes <- mapM (\fp -> do
                          outcome <- runCommandFile spec root allow patterns fp
                          pure (fp, outcome)) files
      pure (Map.fromList outcomes)

-- | Run the command for exactly one file. Never throws.
runCommandFile
  :: CommandSpec
  -> FilePath
  -> Bool
  -> [AnnotatedPattern]
  -> FilePath
  -> IO CommandOutcome
runCommandFile spec root allow patterns fp =
  case gateCommandFile root allow patterns fp of
    Just reason -> pure (CommandSkipped reason)
    Nothing -> do
      result <- invoke spec [fp]
      pure $ case result of
        Left err -> CommandFailed (ExtractionError fp "command" err)
        Right out
          | null (coNodes out) && coDroppedNodes out + coDroppedEdges out > 0 ->
              CommandFailed (ExtractionError fp "command" "all emitted node records were invalid")
          | otherwise -> CommandProduced (outputToExtraction fp out)

-- | Run one batch invocation and route the output back per source file: nodes
-- are grouped by their declared @source_file@ and each edge follows the file
-- that owns its source endpoint. Records that name no input file are dropped;
-- if nothing routes at all the outcome is a per-file failure.
runCommandBatch
  :: CommandSpec
  -> FilePath
  -> Bool
  -> [AnnotatedPattern]
  -> [FilePath]
  -> IO (Map FilePath CommandOutcome)
runCommandBatch _ _ _ _ [] = pure Map.empty
runCommandBatch spec root allow patterns files = do
  let decisions = [(fp, gateCommandFile root allow patterns fp) | fp <- files]
      allowed   = [fp | (fp, Nothing) <- decisions]
      skipped   = Map.fromList [(fp, CommandSkipped reason) | (fp, Just reason) <- decisions]
  if null allowed
    then pure skipped
    else do
      result <- invoke spec allowed
      case result of
        Left err -> pure $ skipped `Map.union` Map.fromList
          [(fp, CommandFailed (ExtractionError fp "command" err)) | fp <- allowed]
        Right out ->
          let (routed, unroutable) = routeByFile allowed out
              dropped = coDroppedNodes out + coDroppedEdges out + unroutable
              perFile fp = case Map.lookup fp routed of
                Just fo | not (null (coNodes fo)) ->
                  CommandProduced (outputToExtraction fp fo)
                _
                  | dropped > 0 ->
                      CommandFailed (ExtractionError fp "command"
                        "batch output contained no valid node for this file")
                  | otherwise ->
                      CommandSkipped "batch output contained no node for this file"
          in pure $ skipped `Map.union` Map.fromList [(fp, perFile fp) | fp <- allowed]

-- | Route a batch output to its input files, plus the number of records that
-- named no input file (and were therefore dropped).
--
-- A record is routed by its @source_file@: the exact input path when it
-- matches, otherwise the input file whose basename it names (tolerating
-- extractors that echo the paths they were handed in a shortened form). A
-- basename shared by two inputs is ambiguous and routes nothing.
routeByFile :: [FilePath] -> CommandOutput -> (Map FilePath CommandOutput, Int)
routeByFile files out =
  let allowed = Set.fromList (map normalise files)
      byBasename = Map.fromListWith ambiguous
        [(takeFileName p, p) | p <- Set.toList allowed]
      keyOf n =
        let p = normalise (T.unpack (toText (nodeSourceFile n)))
        in if Set.member p allowed
             then Just p
             -- An ambiguous basename is mapped to "" by 'ambiguous'; it must
             -- not count as routed, or the record would vanish without a
             -- recorded per-file error.
             else case Map.lookup (takeFileName p) byBasename of
                    Just q | not (null q) -> Just q
                    _                     -> Nothing
      keyedNodes = [(k, n) | n <- coNodes out, Just k <- [keyOf n]]
      owner = Map.fromList [(nodeId n, k) | (k, n) <- keyedNodes]
      keyedEdges = [(k, e) | e <- coEdges out, Just k <- [Map.lookup (edgeSource e) owner]]
      fromNodes = Map.fromListWith appendOutput
        [(k, CommandOutput [n] [] 0 0) | (k, n) <- keyedNodes]
      fromEdges = Map.fromListWith appendOutput
        [(k, CommandOutput [] [e] 0 0) | (k, e) <- keyedEdges]
      unroutable = length (coNodes out) - length keyedNodes
                 + length (coEdges out) - length keyedEdges
  in (Map.unionWith appendOutput fromNodes fromEdges, unroutable)
  where
    -- Mark a basename as ambiguous so it can never route a record.
    ambiguous _ _ = ""

appendOutput :: CommandOutput -> CommandOutput -> CommandOutput
appendOutput a b = CommandOutput
  { coNodes        = coNodes a ++ coNodes b
  , coEdges        = coEdges a ++ coEdges b
  , coDroppedNodes = coDroppedNodes a + coDroppedNodes b
  , coDroppedEdges = coDroppedEdges a + coDroppedEdges b
  }

-- | Run one invocation (argv already expanded) and decode its stdout.
invoke :: CommandSpec -> [FilePath] -> IO (Either Text CommandOutput)
invoke spec files
  | null (csArgv spec) = pure (Left "no command argv configured")
  | otherwise = case substituteCommand (csArgv spec) files of
      [] -> pure (Left "no command argv configured")
      (program : args) -> do
        let seconds = max 1 (csTimeoutSeconds spec)
        result <- timeout (seconds * 1000000)
                    (runBounded program args (csMaxOutputBytes spec))
        pure $ case result of
          Nothing -> Left $ "timed out after " <> T.pack (show seconds) <> "s"
          Just (Left err) -> Left err
          Just (Right (ExitSuccess, out, _)) -> decodeCommandOutput out
          Just (Right (ExitFailure code, _, errOut)) ->
            Left $ "exited with code " <> T.pack (show code) <> stderrSuffix errOut
  where
    stderrSuffix raw
      | BS.null raw = ""
      | otherwise = " (stderr: " <> firstLine (decodeLenient raw) <> ")"

-- | Spawn @program args@ without a shell, capture bounded stdout/stderr and
-- wait. The child is terminated on timeout (handled by 'timeout' around this
-- function), on truncation, and on any exception, so no process is leaked.
runBounded :: String -> [String] -> Int -> IO (Either Text (ExitCode, BS.ByteString, BS.ByteString))
runBounded program args maxBytes = do
  -- Only /synchronous/ spawn failures (an 'IOException' from
  -- 'createProcess') become an error value: the async exception 'timeout'
  -- raises must reach it so the caller can report a timeout instead.
  outcome <- tryJust synchronousException spawn
    :: IO (Either IOException (Either Text (ExitCode, BS.ByteString, BS.ByteString)))
  pure $ case outcome of
    Left err -> Left $ "could not run " <> T.pack program <> ": " <> firstLine (T.pack (show err))
    Right r  -> r
  where
    synchronousException e = case fromException e :: Maybe IOException of
      Just io -> Just io
      Nothing -> Nothing
    spawn =
      withCreateProcess cp $ \_ mOut mErr ph ->
        case (mOut, mErr) of
          (Just hOut, Just hErr) -> do
            outVar <- newEmptyMVar
            errVar <- newEmptyMVar
            void $ forkIO (readBounded hOut maxBytes >>= putMVar outVar)
            void $ forkIO (readBounded hErr stderrCap >>= putMVar errVar)
            (do
                (out, truncated) <- takeMVar outVar
                if truncated then terminateProcess ph else pure ()
                code <- waitForProcess ph
                (errOut, _) <- takeMVar errVar
                pure $ if truncated
                  then Left $ "stdout exceeded " <> T.pack (show maxBytes) <> " bytes"
                  else Right (code, out, errOut))
              `onException` terminateProcess ph
          _ -> pure (Left "failed to create process pipes")

    cp = (proc program args)
      { std_in  = NoStream
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
    stderrCap = 64 * 1024

-- | Read at most @cap@ bytes; 'True' when the stream held more.
readBounded :: Handle -> Int -> IO (BS.ByteString, Bool)
readBounded handle cap = go [] 0
  where
    chunkSize = 64 * 1024
    go acc n
      | n >= cap = pure (BS.concat (reverse acc), True)
      | otherwise = do
          chunk <- BS.hGetSome handle (min chunkSize (cap - n))
          if BS.null chunk
            then pure (BS.concat (reverse acc), False)
            else go (chunk : acc) (n + BS.length chunk)

decodeLenient :: BS.ByteString -> Text
decodeLenient = TE.decodeUtf8With lenientDecode

-- | First line of a message, trimmed and bounded so one noisy extractor
-- cannot flood the recorded error.
firstLine :: Text -> Text
firstLine = T.take 400 . T.filter isPrint . T.strip . T.takeWhile (/= '\n')
