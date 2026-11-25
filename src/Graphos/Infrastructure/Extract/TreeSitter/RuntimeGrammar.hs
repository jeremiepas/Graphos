-- | Runtime tree-sitter grammar loading (custom-extractors 4.1–4.4).
--
-- A user-supplied grammar is a shared library exporting
-- @const TSLanguage *tree_sitter_<lang>(void)@. This module opens it with
-- @dlopen@, resolves that entry symbol, checks the reported language ABI
-- against the bundled runtime (9–11), and returns the 'Ptr Language' the rest
-- of the tree-sitter pipeline already knows how to parse with.
--
-- The optional @definitions.scm@ / @imports.scm@ / @calls.scm@ queries are
-- compiled against the loaded language (or a built-in one) and can be executed
-- on file content through 'runRuntimeQuery'. A missing or invalid query
-- disables only that kind and yields a warning instead of failing the build.
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Extract.TreeSitter.RuntimeGrammar
  ( -- * ABI guard
    minSupportedAbi
  , maxSupportedAbi
  , abiSupported
  , checkGrammarAbi
    -- * Entry symbol
  , entrySymbolForPath
    -- * Grammar loading
  , RuntimeGrammar(..)
  , RuntimeGrammarError(..)
  , loadRuntimeGrammar
  , unloadRuntimeGrammar
    -- * Runtime queries
  , QueryKind(..)
  , queryKindFile
  , RuntimeQuerySource(..)
  , discoverRuntimeQueries
  , RuntimeQuery(..)
  , RuntimeQueries(..)
  , runtimeQueryList
  , loadRuntimeQueries
  , deleteRuntimeQueries
  , QueryCapture(..)
  , runRuntimeQuery
    -- * Paths
  , resolveRuntimePath
  ) where

import Control.Exception (SomeException, bracket, catch, try)
import Control.Monad (forM, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import Foreign.C.String (CString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable (Storable(..), peek, poke)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath
  ( dropExtensions, takeFileName, (</>) )
import System.IO (hPutStrLn, stderr)
import System.Posix.DynamicLinker
  ( DL, RTLDFlags(..), dlclose, dlerror, dlopen, dlsym )
import System.Timeout (timeout)

import TreeSitter.Language (Language)
import TreeSitter.Node (Node(..), TSNode)
import TreeSitter.Parser (Parser, withParser, ts_parser_set_timeout_micros)
import TreeSitter.Tree (Tree, ts_tree_delete, withRootNode)

-- ───────────────────────────────────────────────
-- ABI guard
-- ───────────────────────────────────────────────

-- | Lowest language ABI the bundled @tree-sitter@ runtime accepts.
minSupportedAbi :: Word32
minSupportedAbi = 9

-- | Highest language ABI the bundled @tree-sitter@ runtime accepts.
maxSupportedAbi :: Word32
maxSupportedAbi = 11

-- | Pure ABI guard. Kept separate from 'loadRuntimeGrammar' so the boundary
-- can be exercised without constructing a shared library.
abiSupported :: Word32 -> Bool
abiSupported abi = abi >= minSupportedAbi && abi <= maxSupportedAbi

-- | Reject an out-of-range ABI, naming both the path and the ABI. The loader
-- turns this into a warning and the caller falls back to stub extraction.
checkGrammarAbi :: FilePath -> Word32 -> Either Text ()
checkGrammarAbi path abi
  | abiSupported abi = Right ()
  | otherwise = Left $
      T.pack path <> ": grammar ABI " <> T.pack (show abi)
        <> " is outside the supported range "
        <> T.pack (show minSupportedAbi) <> "-" <> T.pack (show maxSupportedAbi)
        <> "; refusing to load it"

-- ───────────────────────────────────────────────
-- Entry symbol
-- ───────────────────────────────────────────────

-- | Derive the exported entry symbol from a grammar library filename.
--
-- @libtree-sitter-php.so@ → @tree_sitter_php@;
-- @tree-sitter-c-sharp.so@ → @tree_sitter_c_sharp@;
-- a bare @php.so@ → @tree_sitter_php@.
entrySymbolForPath :: FilePath -> String
entrySymbolForPath path =
  "tree_sitter_" ++ map normalize base
  where
    base0 = dropExtensions (takeFileName path)
    base1 = maybe base0 id (stripPrefix "lib" base0)
    base2 = maybe base1 id (stripPrefix "tree-sitter-" base1)
    base = maybe base2 id (stripPrefix "tree_sitter_" base2)
    normalize c
      | c == '-' || c == '.' = '_'
      | otherwise = c

stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix _ [] = Nothing
stripPrefix (x : xs) (y : ys)
  | x == y = stripPrefix xs ys
  | otherwise = Nothing

-- ───────────────────────────────────────────────
-- Grammar loading
-- ───────────────────────────────────────────────

-- | A successfully loaded runtime grammar. Keep it alive until parsing (and
-- any query execution) is finished, then 'unloadRuntimeGrammar' it.
data RuntimeGrammar = RuntimeGrammar
  { rgPath         :: FilePath
  , rgEntrySymbol  :: String
  , rgAbi          :: Word32
  , rgLanguage     :: Ptr Language
  , rgDlHandle     :: DL
  } deriving (Show)

-- | Failure to load a runtime grammar. @rgePath@ is carried separately so the
-- caller can print the exact library path alongside the message.
data RuntimeGrammarError = RuntimeGrammarError
  { rgePath    :: FilePath
  , rgeMessage :: Text
  } deriving (Eq, Show)

foreign import ccall unsafe "ts_language_version"
  ts_language_version :: Ptr Language -> IO Word32

foreign import ccall "dynamic"
  callLanguageGetter :: FunPtr (IO (Ptr Language)) -> IO (Ptr Language)

-- | Open a shared library, resolve its @tree_sitter_<lang>@ entry point, and
-- return the language pointer after checking its ABI.
loadRuntimeGrammar :: FilePath -> IO (Either RuntimeGrammarError RuntimeGrammar)
loadRuntimeGrammar rawPath = do
  path <- resolveRuntimePath rawPath
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ RuntimeGrammarError path "grammar library not found or unreadable"
    else do
      opened <- try (dlopen path [RTLD_NOW, RTLD_LOCAL])
      case opened of
        Left (e :: SomeException) ->
          pure $ Left $ RuntimeGrammarError path $
            "could not open grammar library: " <> T.pack (show e)
        Right dl -> do
          loaded <- try (loadFromHandle path dl)
          case loaded of
            Left (e :: SomeException) ->
              dlclose dl >> pure (Left (RuntimeGrammarError path
                ("could not resolve grammar entry point: " <> T.pack (show e))))
            Right (Left err) -> dlclose dl >> pure (Left err)
            Right (Right rg) -> pure (Right rg)
  where
    loadFromHandle path dl = do
      _ <- dlerror                      -- clear any stale loader error
      let symbol = entrySymbolForPath path
      rawPtr <- dlsym dl symbol
      err <- dlerror
      if rawPtr == nullFunPtr || not (null err)
        then pure $ Left $ RuntimeGrammarError path $
          "entry symbol " <> T.pack symbol <> " not found"
            <> (if null err then "" else T.pack (": " ++ err))
        else do
          lang <- callLanguageGetter (castFunPtr rawPtr)
          if lang == nullPtr
            then pure $ Left $ RuntimeGrammarError path $
              "entry symbol " <> T.pack symbol <> " returned a null language"
            else do
              abi <- ts_language_version lang
              case checkGrammarAbi path abi of
                Left msg -> pure $ Left (RuntimeGrammarError path msg)
                Right () -> pure $ Right RuntimeGrammar
                  { rgPath = path
                  , rgEntrySymbol = symbol
                  , rgAbi = abi
                  , rgLanguage = lang
                  , rgDlHandle = dl
                  }

-- | Release the shared library. The language pointer is invalid afterwards.
unloadRuntimeGrammar :: RuntimeGrammar -> IO ()
unloadRuntimeGrammar = dlclose . rgDlHandle

-- | Expand a leading @~@ and resolve a relative path against the current
-- directory. Config files use @~@ for the common out-of-tree grammar case.
resolveRuntimePath :: FilePath -> IO FilePath
resolveRuntimePath raw = do
  expanded <- case raw of
    "~" -> getHomeDirectory
    '~' : '/' : rest -> do
      home <- getHomeDirectory
      pure (home ++ "/" ++ rest)
    _ -> pure raw
  pure expanded

-- ───────────────────────────────────────────────
-- Runtime queries
-- ───────────────────────────────────────────────

-- | The three query kinds the runtime grammar contract recognises.
data QueryKind
  = QueryDefinitions
  | QueryImports
  | QueryCalls
  deriving (Eq, Show)

-- | Filename for a query kind inside the configured @queries@ directory.
queryKindFile :: QueryKind -> FilePath
queryKindFile QueryDefinitions = "definitions.scm"
queryKindFile QueryImports     = "imports.scm"
queryKindFile QueryCalls       = "calls.scm"

-- | A query file that exists on disk and could be read.
data RuntimeQuerySource = RuntimeQuerySource
  { rqsKind   :: QueryKind
  , rqsPath   :: FilePath
  , rqsSource :: Text
  } deriving (Eq, Show)

-- | Read the query files that exist in @dir@. Missing files are reported in
-- the warning list and simply omitted, so one absent kind (e.g. @calls.scm@)
-- does not disable the other two.
discoverRuntimeQueries :: FilePath -> IO ([RuntimeQuerySource], [Text])
discoverRuntimeQueries dir = do
  results <- forM [QueryDefinitions, QueryImports, QueryCalls] $ \kind -> do
    let path = dir </> queryKindFile kind
    exists <- doesFileExist path
    if not exists
      then pure (Left $ T.pack path <> " is missing: "
                     <> kindName kind <> " extraction disabled")
      else do
        source <- try (BS.readFile path) :: IO (Either SomeException ByteString)
        pure $ case source of
          Left e -> Left $ T.pack path <> ": could not read query: " <> T.pack (show e)
          Right bytes -> Right RuntimeQuerySource
            { rqsKind = kind
            , rqsPath = path
            , rqsSource = TE.decodeUtf8With (\_ _ -> Just '\xfffd') bytes
            }
  let sources = [ s | Right s <- results ]
      warnings = [ w | Left w <- results ]
  pure (sources, warnings)

kindName :: QueryKind -> Text
kindName QueryDefinitions = "definitions"
kindName QueryImports     = "imports"
kindName QueryCalls       = "calls"

-- | A compiled query. The pointer is deleted by 'deleteRuntimeQueries'.
data RuntimeQuery = RuntimeQuery
  { rqKind   :: QueryKind
  , rqSource :: Text
  , rqQuery  :: Ptr ()
  } deriving (Show)

-- | Compiled queries for the kinds that were present and valid.
data RuntimeQueries = RuntimeQueries
  { rqDefinitions :: Maybe RuntimeQuery
  , rqImports     :: Maybe RuntimeQuery
  , rqCalls       :: Maybe RuntimeQuery
  } deriving (Show)

runtimeQueryList :: RuntimeQueries -> [RuntimeQuery]
runtimeQueryList qs =
  [ q | Just q <- [rqDefinitions qs, rqImports qs, rqCalls qs] ]

foreign import ccall unsafe "graphos_ts_query_new"
  c_ts_query_new :: Ptr Language -> CString -> Word32 -> Ptr Word32
                 -> Ptr Word32 -> IO (Ptr ())

foreign import ccall unsafe "graphos_ts_query_delete"
  c_ts_query_delete :: Ptr () -> IO ()

foreign import ccall unsafe "graphos_ts_query_captures"
  c_ts_query_captures :: Ptr TSNode -> Ptr () -> Ptr Word32
                      -> IO (Ptr QueryCapture)

foreign import ccall unsafe "graphos_ts_query_captures_free"
  c_ts_query_captures_free :: Ptr QueryCapture -> IO ()

-- | Compile the readable queries against @language@. An invalid query disables
-- only its own kind and is reported as a warning.
loadRuntimeQueries :: Ptr Language -> FilePath -> IO (RuntimeQueries, [Text])
loadRuntimeQueries language dir = do
  (sources, warnings) <- discoverRuntimeQueries dir
  compiled <- mapM (compileOne language) sources
  let queries = [ q | Right q <- compiled ]
      errors  = [ w | Left w <- compiled ]
      find k = case [ q | q <- queries, rqKind q == k ] of
        (q : _) -> Just q
        []      -> Nothing
  pure ( RuntimeQueries
           { rqDefinitions = find QueryDefinitions
           , rqImports     = find QueryImports
           , rqCalls       = find QueryCalls
           }
       , warnings ++ errors )

compileOne :: Ptr Language -> RuntimeQuerySource
           -> IO (Either Text RuntimeQuery)
compileOne language source = do
  let queryBytes = TE.encodeUtf8 (rqsSource source)
  BSU.unsafeUseAsCStringLen queryBytes $ \(cstr, len) ->
    alloca $ \errOffsetPtr -> alloca $ \errTypePtr -> do
      poke errOffsetPtr (0 :: Word32)
      poke errTypePtr (0 :: Word32)
      q <- c_ts_query_new language cstr (fromIntegral len) errOffsetPtr errTypePtr
      if q == nullPtr
        then do
          offset <- peek errOffsetPtr
          errType <- peek errTypePtr
          pure $ Left $ T.pack (rqsPath source) <> ": invalid "
            <> kindName (rqsKind source) <> " query at byte "
            <> T.pack (show offset) <> " (" <> queryErrorName errType <> ")"
        else pure $ Right RuntimeQuery
          { rqKind = rqsKind source
          , rqSource = rqsSource source
          , rqQuery = q
          }

queryErrorName :: Word32 -> Text
queryErrorName 1 = "syntax error"
queryErrorName 2 = "unknown node type"
queryErrorName 3 = "unknown field"
queryErrorName 4 = "unknown capture"
queryErrorName _ = "query error"

-- | Delete every compiled query. Safe on a 'RuntimeQueries' with no queries.
deleteRuntimeQueries :: RuntimeQueries -> IO ()
deleteRuntimeQueries qs = mapM_ (c_ts_query_delete . rqQuery) (runtimeQueryList qs)

-- ───────────────────────────────────────────────
-- Query execution
-- ───────────────────────────────────────────────

-- | One capture returned by a query, with its byte and point range.
data QueryCapture = QueryCapture
  { qcCaptureIndex :: Word32
  , qcStartByte    :: Word32
  , qcEndByte      :: Word32
  , qcStartRow     :: Word32
  , qcStartColumn  :: Word32
  , qcEndRow       :: Word32
  , qcEndColumn    :: Word32
  } deriving (Eq, Show)

instance Storable QueryCapture where
  sizeOf _ = 7 * 4
  alignment _ = 4
  peek p = QueryCapture
    <$> peekByteOff p 0
    <*> peekByteOff p 4
    <*> peekByteOff p 8
    <*> peekByteOff p 12
    <*> peekByteOff p 16
    <*> peekByteOff p 20
    <*> peekByteOff p 24
  poke p q = do
    pokeByteOff p 0 (qcCaptureIndex q)
    pokeByteOff p 4 (qcStartByte q)
    pokeByteOff p 8 (qcEndByte q)
    pokeByteOff p 12 (qcStartRow q)
    pokeByteOff p 16 (qcStartColumn q)
    pokeByteOff p 20 (qcEndRow q)
    pokeByteOff p 24 (qcEndColumn q)

foreign import ccall interruptible "ts_parser_parse_string"
  ts_parser_parse_string :: Ptr Parser -> Ptr Tree -> CString -> Int
                         -> IO (Ptr Tree)

-- | Parse @content@ and execute one compiled query on the resulting tree.
-- Returns all captures in match order. Parse/query failures yield no captures
-- and a warning on stderr rather than aborting the build.
runRuntimeQuery :: Ptr Language -> ByteString -> RuntimeQuery -> IO [QueryCapture]
runRuntimeQuery language content query =
  catch go $ \(e :: SomeException) -> do
    hPutStrLn stderr $ "[tree-sitter] runtime query "
      ++ T.unpack (kindName (rqKind query)) ++ " failed: " ++ show e
    pure []
  where
    go = withParser language $ \parser -> do
      ts_parser_set_timeout_micros parser (5000000 :: Word64)
      BSU.unsafeUseAsCStringLen content $ \(source, len) -> do
        parsed <- timeout 6000000 (ts_parser_parse_string parser nullPtr source len)
        case parsed of
          Nothing -> do
            hPutStrLn stderr "[tree-sitter] runtime query parse timed out"
            pure []
          Just tree
            | tree == nullPtr -> pure []
            | otherwise -> bracket (pure tree)
                (\t -> when (t /= nullPtr) (void (ts_tree_delete t)))
                (\t -> withRootNode t $ \rootPtr -> do
                    root <- peek rootPtr
                    alloca $ \nodePtr -> alloca $ \countPtr -> do
                      poke nodePtr (nodeTSNode root)
                      poke countPtr (0 :: Word32)
                      capsPtr <- c_ts_query_captures nodePtr (rqQuery query) countPtr
                      n <- peek countPtr
                      if capsPtr == nullPtr || n == 0
                        then do
                          when (capsPtr /= nullPtr) (c_ts_query_captures_free capsPtr)
                          pure []
                        else do
                          caps <- peekArray (fromIntegral n) capsPtr
                          c_ts_query_captures_free capsPtr
                          pure caps
                )
