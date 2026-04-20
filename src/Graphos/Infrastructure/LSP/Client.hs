-- | LSP Client - connects to language servers and extracts symbol trees.
-- Uses proper JSON-RPC over stdio protocol with Content-Length framing.
-- Shares one LSP connection per language server for efficiency.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.LSP.Client
  ( -- * LSP Client
    LSPClient(..)
  , LSPClientConfig(..)
  , defaultLSPConfig

    -- * Lifecycle
  , connectToLSP
  , disconnectLSP

    -- * Extraction via LSP
  , extractViaLSP
  , extractDocumentSymbols
  , extractCallHierarchy
  , extractWorkspaceSymbols
  , parseServerCapabilities
  , workspaceSymbolsToDocumentSymbols
  , symbolToNodes
  , symbolTreeToEdges

    -- * Language server registry
  , findLSPServer
  , languageServerCommands
  , languageIdFromExt
  ) where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent (threadDelay)
import Control.Exception (catch, try, SomeException(..))
import Data.Aeson (ToJSON, encode, eitherDecode, Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AK (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as B8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Process (ProcessHandle, createProcess, proc, std_in, std_out, std_err, StdStream(CreatePipe), terminateProcess)
import System.IO (Handle, hFlush)
import System.Timeout (timeout)

import Graphos.Domain.Types
import Graphos.Infrastructure.LSP.Protocol

-- ───────────────────────────────────────────────
-- LSP Client types
-- ───────────────────────────────────────────────

data LSPClientConfig = LSPClientConfig
  { lspCommand    :: FilePath
  , lspArgs       :: [String]
  , lspRootUri    :: FilePath
  , lspTimeout    :: Int  -- seconds
  } deriving (Eq, Show)

defaultLSPConfig :: FilePath -> LSPClientConfig
defaultLSPConfig root = LSPClientConfig
  { lspCommand = ""
  , lspArgs     = []
  , lspRootUri  = root
  , lspTimeout  = 60
  }

data LSPClient = LSPClient
  { lspHandle     :: ProcessHandle
  , lspStdin      :: Handle
  , lspStdout     :: Handle
  , lspConfig     :: LSPClientConfig
  , lspMessageId  :: MVar Int
  , lspServerCaps :: ServerCapabilities
  }

-- ───────────────────────────────────────────────
-- Language server registry (default / hardcoded)
-- Prefer using findLSPServerFromConfig with a GraphosConfig
-- for user-configurable LSP servers. This fallback remains
-- for cases where no config is available (e.g. tests).
-- ───────────────────────────────────────────────

-- | Default hardcoded LSP server commands.
-- Kept for backward compatibility; prefer 'defaultLSPServers' from Domain.Config.
languageServerCommands :: Map String (String, [String])
languageServerCommands = Map.fromList
  [ (".ts",  ("typescript-language-server", ["--stdio"]))
  , (".tsx", ("typescript-language-server", ["--stdio"]))
  , (".js",  ("typescript-language-server", ["--stdio"]))
  , (".jsx", ("typescript-language-server", ["--stdio"]))
  , (".py",  ("pyright-langserver", ["--stdio"]))
  , (".go",  ("gopls", []))
  , (".rs",  ("rust-analyzer", []))
  , (".c",   ("clangd", []))
  , (".cpp", ("clangd", []))
  , (".h",   ("clangd", []))
  , (".hpp", ("clangd", []))
  , (".java", ("jdtls", []))
  , (".cs",  ("omnisharp", []))
  , (".rb",  ("solargraph", ["--stdio"]))
  , (".hs",  ("haskell-language-server", ["--lsp"]))
  , (".lhs", ("haskell-language-server", ["--lsp"]))
  , (".php", ("phpactor", []))
  , (".swift", ("sourcekit-lsp", []))
  , (".kt",  ("kotlin-language-server", []))
  , (".kts", ("kotlin-language-server", []))
  , (".scala", ("metals", []))
  , (".lua", ("lua-language-server", []))
  , (".zig", ("zls", []))
  , (".ex",  ("elixir-ls", []))
  , (".exs", ("elixir-ls", []))
  , (".dart", ("dart", ["analyze", "--stdio"]))
  , (".vue", ("vue-language-server", []))
  , (".svelte", ("svelte-language-server", []))
  , (".nix", ("nixd", []))
  , (".json", ("vscode-json-language-server", ["--stdio"]))
  ]

-- | Find an LSP server for a file extension using the default hardcoded registry.
-- Prefer 'findLSPServerFromConfig' from Infrastructure.Config for user-configurable lookups.
findLSPServer :: String -> IO (Maybe (String, [String]))
findLSPServer ext = do
  case Map.lookup ext languageServerCommands of
    Nothing    -> pure Nothing
    Just (cmd, args) -> do
      found <- findExecutable cmd
      case found of
        Just path -> pure $ Just (path, args)
        Nothing   -> pure Nothing

-- ───────────────────────────────────────────────
-- LSP Protocol: sending and receiving messages
-- ───────────────────────────────────────────────

-- | Send a JSON-RPC message with proper Content-Length framing
sendLSPMessage :: ToJSON a => Handle -> a -> IO ()
sendLSPMessage h msg = do
  let content = BSL.toStrict (encode msg)
      contentLen = BS.length content
      header = B8.pack $ "Content-Length: " ++ show contentLen ++ "\r\n\r\n"
  BS.hPut h (header `BS.append` content)
  hFlush h

-- | Read a byte until newline, stripping \r
readLineLF :: Handle -> IO String
readLineLF h = do
  let loop acc = do
        c <- BS.hGet h 1
        if BS.null c
          then pure (reverse acc)
          else case BS.head c of
            10 -> pure (reverse acc)   -- \n = end of line
            13 -> loop acc             -- \r = skip
            b  -> loop (toEnum (fromEnum b) : acc)
  loop []

-- | Read a single LSP message (Content-Length header + JSON body)
-- LSP protocol format: Content-Length: N\r\n\r\n{...N bytes of JSON...}
readLSPMessage :: Handle -> IO (Either String Value)
readLSPMessage outh = catch (do
  -- Read header lines until we find Content-Length
  let findHeader = do
        line <- readLineLF outh  -- reads until \n, strips \r
        if null line
          then findHeader  -- skip blank lines
          else do
            let contentLen = parseContentLength line
            if contentLen < 0
              then findHeader  -- not a Content-Length line, keep reading
              else do
                -- After Content-Length line, there may be more headers or an empty line (\r\n)
                -- Read until we get an empty line (end of headers)
                skipHeaders
                -- Now read JSON body
                result <- timeout 10000000 (BSL.hGet outh contentLen)
                case result of
                  Nothing -> pure $ Left "Timeout reading body"
                  Just bodyBytes -> case eitherDecode bodyBytes of
                    Right val -> pure $ Right val
                    Left err  -> pure $ Left $ "JSON parse error (" ++ show contentLen ++ " bytes): " ++ err
  mResult <- timeout 10000000 findHeader  -- 10s overall timeout
  case mResult of
    Nothing -> pure $ Left "Timeout waiting for LSP response"
    Just result -> pure result
  ) $ \(e :: SomeException) -> pure $ Left $ "Read error: " ++ show e
  where
    -- Skip remaining headers until empty line (separator between headers and body)
    skipHeaders = do
      line <- readLineLF outh
      if null line then pure ()  -- empty line = end of headers
      else skipHeaders  -- skip this header line

-- | Parse Content-Length from header line like "Content-Length: 1234"
parseContentLength :: String -> Int
parseContentLength header =
  let prefix :: String = "Content-Length:"
      trimmed = dropWhile (== ' ') $ drop (length prefix) (takeWhile (/= '\r') header)
  in case reads trimmed of
       [(n, "")] -> n
       _ -> -1

-- | Read LSP messages until we get one with a matching "id" field
readLSPResponseForId :: Handle -> Int -> IO (Either String Value)
readLSPResponseForId outh targetId = loop
  where
    loop = do
      result <- readLSPMessage outh
      case result of
        Left err -> pure $ Left err
        Right val@(Object o) ->
          case KM.lookup "id" o of
            Just (Aeson.Number n) | round n == targetId -> pure $ Right val
            Just (Aeson.Number _) -> loop  -- different id, keep reading
            _ -> loop  -- notification (no id), keep reading
        Right _ -> loop

-- | Drain all pending notifications from LSP server
drainNotifications :: Handle -> Int -> IO ()
drainNotifications outh micros = do
  mMsg <- timeout micros (readLSPMessage outh)
  case mMsg of
    Just (Right _) -> drainNotifications outh micros
    _ -> pure ()

-- ───────────────────────────────────────────────
-- LSP Client lifecycle
-- ───────────────────────────────────────────────

connectToLSP :: LSPClientConfig -> IO (Either Text LSPClient)
connectToLSP config = catch (do
  putStrLn $ "[lsp] Starting: " ++ lspCommand config ++ " " ++ unwords (lspArgs config)
  let processSpec = proc (lspCommand config) (lspArgs config)
  (minH, moutH, _, ph) <- createProcess processSpec
    { std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  case (minH, moutH) of
    (Just inh, Just outh) -> do
      idVar <- newMVar 2  -- start at 2 since 1 is used for initialize

      -- 1. Send initialize request
      let initMsg = lspInitialize (lspRootUri config)
      sendLSPMessage inh initMsg

      -- 2. Read responses until we get the initialize response (id=1)
      --     Use extended timeout for initialization (LSP servers like HLS can be slow)
      let initTimeoutMicros = lspTimeout config * 1000000
      initResp <- timeout initTimeoutMicros (readLSPResponseForId outh 1)
      case initResp of
        Nothing -> do
          putStrLn "[lsp] Initialize failed: Timeout waiting for LSP response"
          terminateProcess ph
          pure $ Left $ T.pack "LSP initialize failed: Timeout waiting for LSP response"
        Just (Left err) -> do
          putStrLn $ "[lsp] Initialize failed: " ++ err
          terminateProcess ph
          pure $ Left $ T.pack $ "LSP initialize failed: " ++ err
        Just (Right respVal) -> do
          putStrLn "[lsp] Initialize successful"

          -- 3. Send initialized notification
          sendLSPMessage inh lspInitialized

          -- 4. Drain post-init notifications (slow servers like HLS need time to index)
          let drainMicros = case lspCommand config of
                cmd | "haskell-language-server" `isInfixOf` cmd -> 15000000  -- 15s for HLS
                    | otherwise -> 3000000  -- 3s for others
          drainNotifications outh drainMicros

          let caps = parseServerCapabilities respVal
          putStrLn $ "[lsp] Server capabilities: documentSymbol=" ++ show (scpDocumentSymbolProvider caps)
                   ++ " workspaceSymbol=" ++ show (scpWorkspaceSymbolProvider caps)

          putStrLn $ "[lsp] Connected to " ++ lspCommand config
          pure $ Right LSPClient
            { lspHandle     = ph
            , lspStdin      = inh
            , lspStdout     = outh
            , lspConfig     = config
            , lspMessageId  = idVar
            , lspServerCaps = caps
            }
    _ -> pure $ Left $ T.pack "Failed to create LSP process handles"
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "LSP connection error: " ++ show e

disconnectLSP :: LSPClient -> IO ()
disconnectLSP client = do
  result <- try $ do
    sendLSPMessage (lspStdin client) lspShutdown
    _ <- timeout 3000000 (readLSPResponseForId (lspStdout client) 999)
    pure ()
  case result of
    Left (_ :: SomeException) -> pure ()
    Right _ -> pure ()
  -- Send exit notification after shutdown, then allow brief cleanup time
  catch (sendLSPMessage (lspStdin client) lspExit) $ \(_ :: SomeException) -> pure ()
  threadDelay 100000  -- 100ms for server cleanup before SIGTERM
  terminateProcess (lspHandle client)
  putStrLn $ "[lsp] Disconnected from " ++ lspCommand (lspConfig client)

-- ───────────────────────────────────────────────
-- Extraction via LSP
-- ───────────────────────────────────────────────

-- | Extract entities and relationships from a file using LSP.
-- Opens the file, gets document symbols, then closes it.
-- Returns empty extraction on any error (never throws).
extractViaLSP :: LSPClient -> FilePath -> IO Extraction
extractViaLSP client filePath =
  catch (do
    putStrLn $ "[lsp] Extracting: " ++ filePath
    let ext = takeExtension filePath
        langId = languageIdFromExt ext

    -- Read file content for didOpen
    fileContent <- catch (T.pack <$> readFile filePath) $ \(_ :: SomeException) -> pure ""

    -- Open the document
    let openMsg = lspDidOpen filePath langId fileContent
    catch (sendLSPMessage (lspStdin client) openMsg) $ \(_ :: SomeException) -> pure ()
    catch (drainNotifications (lspStdout client) 500000) $ \(_ :: SomeException) -> pure ()

    -- Request document symbols
    symbols <- extractDocumentSymbols client filePath
    putStrLn $ "[lsp] Got " ++ show (length symbols) ++ " symbols from " ++ filePath

    -- Close the document
    let closeMsg = lspDidClose filePath
    catch (sendLSPMessage (lspStdin client) closeMsg) $ \(_ :: SomeException) -> pure ()

    let nodes = symbolToNodes filePath symbols
        edges = symbolTreeToEdges filePath symbols
    pure emptyExtraction
      { extractionNodes = nodes
      , extractionEdges = edges
      }
  ) $ \(e :: SomeException) -> do
    -- LSP communication failed for this file — return stub instead of crashing
    putStrLn $ "[lsp] Warning: extraction failed for " ++ filePath ++ ": " ++ show e
    pure emptyExtraction
      { extractionNodes = [makeStubNode filePath]
      , extractionEdges = []
      }

-- | Extract document symbols from a file
extractDocumentSymbols :: LSPClient -> FilePath -> IO [DocumentSymbolResult]
extractDocumentSymbols client filePath = do
  nextId <- takeMVar (lspMessageId client)
  putMVar (lspMessageId client) (nextId + 1)
  let req = lspDocumentSymbolWithId filePath nextId
  sendLSPMessage (lspStdin client) req

  resp <- readLSPResponseForId (lspStdout client) nextId
  case resp of
    Left err -> do
      putStrLn $ "[lsp] Failed to get symbols: " ++ err
      pure []
    Right val -> pure $ parseSymbolsFromResponse val

-- | Parse symbol tree from JSON-RPC response.
-- LSP returns a tree: each symbol may have "children" forming parent→child edges.
parseSymbolsFromResponse :: Value -> [DocumentSymbolResult]
parseSymbolsFromResponse (Object o) =
  case KM.lookup "result" o of
    Just (Array arr) -> concatMap (flattenSymbols []) (V.toList arr)
    Just (Object obj) ->
      case KM.lookup "children" obj of
        Just (Array arr) -> concatMap (flattenSymbols []) (V.toList arr)
        _ -> [parseSingleSymbol obj]
    _ -> []
  where
    -- Flatten the tree into a flat list, keeping parent info
    flattenSymbols :: [Text] -> Value -> [DocumentSymbolResult]
    flattenSymbols parents (Object s) =
      let name = case KM.lookup "name" s of
            Just (Aeson.String t) -> t
            _ -> ""
          kind = case KM.lookup "kind" s of
            Just (Aeson.Number n) -> round n
            _ -> 0
          range = case KM.lookup "range" s of
            Just (Object r) -> parseRange r
            _ -> dummyRange
          childrenVals = case KM.lookup "children" s of
            Just (Array arr) -> V.toList arr
            _ -> []
      in if T.null name
         then concatMap (flattenSymbols parents) childrenVals
         else DocumentSymbolResult
              { dsrName = name
              , dsrKind = kind
              , dsrRange = range
              , dsrChildren = []  -- flat list, we track parent→child in edges
              }
               : concatMap (flattenSymbols (name : parents)) childrenVals
    flattenSymbols _ _ = []  -- ignore non-object values

    parseSingleSymbol s =
      let name = case KM.lookup "name" s of
            Just (Aeson.String t) -> t
            _ -> ""
          kind = case KM.lookup "kind" s of
            Just (Aeson.Number n) -> round n
            _ -> 0
          range = case KM.lookup "range" s of
            Just (Object r) -> parseRange r
            _ -> dummyRange
      in DocumentSymbolResult { dsrName = name, dsrKind = kind, dsrRange = range, dsrChildren = [] }

    parseRange r =
      let start = case KM.lookup "start" r of
            Just (Object p) -> parsePos p
            _ -> Position 0 0
          end = case KM.lookup "end" r of
            Just (Object p) -> parsePos p
            _ -> Position 0 0
      in Range start end

    parsePos p =
      let line = case KM.lookup "line" p of
            Just (Aeson.Number n) -> round n
            _ -> 0
          char = case KM.lookup "character" p of
            Just (Aeson.Number n) -> round n
            _ -> 0
      in Position line char

    dummyRange = Range (Position 0 0) (Position 0 0)

parseSymbolsFromResponse _ = []

-- | Extract call hierarchy (incoming calls) for a symbol
extractCallHierarchy :: LSPClient -> Text -> IO [CallHierarchyItem]
extractCallHierarchy client _name = do
  _ <- takeMVar (lspMessageId client)
  putMVar (lspMessageId client) 1
  pure []  -- placeholder

-- ───────────────────────────────────────────────
-- Symbol → Node/Edge conversion
-- ───────────────────────────────────────────────

symbolToNodes :: FilePath -> [DocumentSymbolResult] -> [Node]
symbolToNodes filePath symbols =
  [ Node
    { nodeId           = makeNodeId filePath (dsrName sym)
    , nodeLabel        = dsrName sym
    , nodeFileType     = CodeFile
    , nodeSourceFile   = T.pack filePath
    , nodeSourceLocation = Just $ T.pack ("L" ++ show (posLine (rangeStart (dsrRange sym))))
    , nodeLineEnd      = Just $ posLine (rangeEnd (dsrRange sym))
    , nodeKind         = Just $ symbolKindToText (dsrKind sym)
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }
  | sym <- symbols
  ]

-- | Build edges from the symbol tree (parent contains child).
-- We add:
--   1. A Contains edge from the file to each top-level symbol
--   2. Contains edges between parent symbols and their children (from the symbol hierarchy)
symbolTreeToEdges :: FilePath -> [DocumentSymbolResult] -> [Edge]
symbolTreeToEdges filePath flatSymbols =
  let -- File → symbol edges
      fileEdges =
        [ Edge
          { edgeSource        = T.pack (takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath)
          , edgeTarget        = makeNodeId filePath (dsrName sym)
          , edgeRelation      = Contains
          , edgeConfidence    = Extracted
          , edgeConfidenceScore = 1.0
          , edgeSourceFile    = T.pack filePath
          , edgeSourceLocation = Just $ T.pack ("L" ++ show (posLine (rangeStart (dsrRange sym))))
          , edgeWeight        = 1.0
          }
        | sym <- flatSymbols
        ]
      -- Parent → child symbol edges (based on the hierarchy)
      hierarchyEdges = buildHierarchyEdges filePath flatSymbols
  in fileEdges ++ hierarchyEdges

-- | Build parent→child Contains edges from the symbol hierarchy.
-- Since parseSymbolsFromResponse flattens the tree, we track parent context
-- during the recursive descent. However, since our flat list doesn't preserve
-- parent info, we reconstruct it from ranges: a parent's range contains a child's range.
buildHierarchyEdges :: FilePath -> [DocumentSymbolResult] -> [Edge]
buildHierarchyEdges filePath symbols =
  let -- For each pair, check if one's range contains the other
      -- A contains B if B's start is >= A's start AND B's end is <= A's end AND A != B
      containsPairs = [(parent, child)
                       | parent <- symbols
                       , child <- symbols
                       , dsrName parent /= dsrName child
                       , rangeContains (dsrRange parent) (dsrRange child)
                       ]
      -- Keep only direct parent→child (remove transitive containment)
      -- A is a direct parent of B if no C exists where A contains C and C contains B
      directPairs = [(p, c) | (p, c) <- containsPairs
                            , not (any (\m -> dsrName m /= dsrName p
                                          && dsrName m /= dsrName c
                                          && rangeContains (dsrRange p) (dsrRange m)
                                          && rangeContains (dsrRange m) (dsrRange c)) symbols)]
  in [ Edge
     { edgeSource        = makeNodeId filePath (dsrName parent)
     , edgeTarget        = makeNodeId filePath (dsrName child)
     , edgeRelation      = Contains
     , edgeConfidence    = Extracted
     , edgeConfidenceScore = 1.0
     , edgeSourceFile    = T.pack filePath
     , edgeSourceLocation = Just $ T.pack ("L" ++ show (posLine (rangeStart (dsrRange parent))))
     , edgeWeight        = 1.0
     }
     | (parent, child) <- directPairs]

-- | Check if one range fully contains another
rangeContains :: Range -> Range -> Bool
rangeContains outer inner =
  let outerStart = posLine (rangeStart outer) * 10000 + posCharacter (rangeStart outer)
      outerEnd   = posLine (rangeEnd outer) * 10000 + posCharacter (rangeEnd outer)
      innerStart = posLine (rangeStart inner) * 10000 + posCharacter (rangeStart inner)
      innerEnd   = posLine (rangeEnd inner) * 10000 + posCharacter (rangeEnd inner)
  in innerStart >= outerStart && innerEnd <= outerEnd

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

makeNodeId :: FilePath -> Text -> NodeId
makeNodeId filePath name =
  let stem = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      -- Include directory hash to prevent collisions (e.g., src/Types.hs vs app/Types.hs)
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      hashPrefix = T.pack $ show dirHash
  in hashPrefix <> T.pack "_" <> stem <> T.pack "_" <> name

-- | Create a stub node when LSP extraction fails
makeStubNode :: FilePath -> Node
makeStubNode filePath =
  let name = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      hashPrefix = T.pack $ show dirHash
      nodeId' = hashPrefix <> T.pack "_" <> name
  in Node
    { nodeId           = nodeId'
    , nodeLabel        = name
    , nodeFileType     = CodeFile
    , nodeSourceFile   = T.pack filePath
    , nodeSourceLocation = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Nothing
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }

-- | Convert LSP SymbolKind integer to a human-readable text label.
--   See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind
symbolKindToText :: Int -> Text
symbolKindToText k = case k of
  1  -> "File"
  2  -> "Module"
  3  -> "Namespace"
  4  -> "Package"
  5  -> "Class"
  6  -> "Method"
  7  -> "Property"
  8  -> "Field"
  9  -> "Constructor"
  10 -> "Enum"
  11 -> "Interface"
  12 -> "Function"
  13 -> "Variable"
  14 -> "Constant"
  15 -> "String"
  16 -> "Number"
  17 -> "Boolean"
  18 -> "Array"
  19 -> "Object"
  20 -> "Key"
  21 -> "Null"
  22 -> "EnumMember"
  23 -> "Struct"
  24 -> "Event"
  25 -> "Operator"
  26 -> "TypeParameter"
  _  -> "Unknown"

takeExtension :: FilePath -> String
takeExtension path =
  let reversed = reverse path
      ext = takeWhile (/= '.') reversed
  in if null ext then "" else '.' : reverse ext

-- ───────────────────────────────────────────────
-- Parse server capabilities from initialize response
-- ───────────────────────────────────────────────

-- | Parse ServerCapabilities from the initialize JSON-RPC response.
-- Defaults to False for any capability not explicitly advertised (safe fallback).
parseServerCapabilities :: Value -> ServerCapabilities
parseServerCapabilities (Object o) =
  let -- Navigate: result.capabilities
      mResult = KM.lookup "result" o
      mCaps = case mResult of
        Just (Object r) -> KM.lookup "capabilities" r
        _ -> Nothing
  in case mCaps of
       Just (Object caps) -> parseCapsObj caps
       _ -> defaultServerCapabilities
parseServerCapabilities _ = defaultServerCapabilities

parseCapsObj :: Aeson.Object -> ServerCapabilities
parseCapsObj caps = ServerCapabilities
  { scpDocumentSymbolProvider  = lookupBool caps "textDocument" "documentSymbolProvider"
  , scpReferencesProvider      = lookupBool caps "textDocument" "referencesProvider"
  , scpCallHierarchyProvider   = lookupBoolCaps caps "textDocument" "callHierarchyProvider"
  , scpDefinitionProvider      = lookupBool caps "textDocument" "definitionProvider"
  , scpWorkspaceSymbolProvider = lookupBoolCaps caps "workspace" "symbolProvider"
  }

defaultServerCapabilities :: ServerCapabilities
defaultServerCapabilities = ServerCapabilities
  { scpDocumentSymbolProvider  = False
  , scpReferencesProvider      = False
  , scpCallHierarchyProvider   = False
  , scpDefinitionProvider       = False
  , scpWorkspaceSymbolProvider  = False
  }

-- | Look up a boolean capability nested under a top-level key.
-- e.g. lookupBool caps "textDocument" "documentSymbolProvider"
lookupBool :: Aeson.Object -> Text -> Text -> Bool
lookupBool caps topKey subKey =
  case KM.lookup (AK.fromText topKey) caps of
    Just (Object td) ->
      case KM.lookup (AK.fromText subKey) td of
        Just (Aeson.Bool b)      -> b
        Just (Object _)          -> True  -- if it's an object, the capability exists
        Just (Aeson.Number _)     -> True  -- some servers use 0/1
        _ -> False
    _ -> False

-- | Same as lookupBool but also handles the capability being a nested object
lookupBoolCaps :: Aeson.Object -> Text -> Text -> Bool
lookupBoolCaps caps topKey subKey =
  case KM.lookup (AK.fromText topKey) caps of
    Just (Object td) ->
      case KM.lookup (AK.fromText subKey) td of
        Just (Aeson.Bool b)      -> b
        Just (Object _)          -> True  -- capability exists with options
        Just (Aeson.Number _)    -> True
        _ -> False
    _ -> False

-- ───────────────────────────────────────────────
-- Workspace symbol extraction
-- ───────────────────────────────────────────────

-- | Extract all symbols in the project using workspace/symbol.
-- Returns a list grouped by file URI for downstream consumption.
extractWorkspaceSymbols :: LSPClient -> IO (Either Text [SymbolInformation])
extractWorkspaceSymbols client = catch (do
  nextId <- takeMVar (lspMessageId client)
  putMVar (lspMessageId client) (nextId + 1)
  let req = lspWorkspaceSymbolWithId nextId ""
  sendLSPMessage (lspStdin client) req

  resp <- readLSPResponseForId (lspStdout client) nextId
  case resp of
    Left err -> pure $ Left $ T.pack $ "workspace/symbol failed: " ++ err
    Right val -> pure $ Right $ parseWorkspaceSymbolResponse val
  ) $ \(e :: SomeException) -> pure $ Left $ T.pack $ "workspace/symbol error: " ++ show e

-- | Parse workspace/symbol response into SymbolInformation list
parseWorkspaceSymbolResponse :: Value -> [SymbolInformation]
parseWorkspaceSymbolResponse (Object o) =
  case KM.lookup "result" o of
    Just (Array arr) -> mapMaybe parseSymInfo (V.toList arr)
    _ -> []
  where
    parseSymInfo (Object s) =
      let name = case KM.lookup "name" s of
            Just (Aeson.String t) -> t
            _ -> ""
          kind = case KM.lookup "kind" s of
            Just (Aeson.Number n) -> round n
            _ -> 0
          loc = case KM.lookup "location" s of
            Just (Object l) -> parseLocation l
            _ -> Location "" (Range (Position 0 0) (Position 0 0))
      in if T.null name then Nothing
         else Just SymbolInformation { siName = name, siKind = kind, siLocation = loc }
    parseSymInfo _ = Nothing

    parseLocation l =
      let uri = case KM.lookup "uri" l of
            Just (Aeson.String u) -> T.drop 7 u  -- strip "file://"
            _ -> ""
          range = case KM.lookup "range" l of
            Just (Object r) -> parseRange' r
            _ -> Range (Position 0 0) (Position 0 0)
      in Location uri range

    parseRange' r =
      let start = case KM.lookup "start" r of
            Just (Object p) -> parsePos' p
            _ -> Position 0 0
          end = case KM.lookup "end" r of
            Just (Object p) -> parsePos' p
            _ -> Position 0 0
      in Range start end

    parsePos' p =
      let line = case KM.lookup "line" p of
            Just (Aeson.Number n) -> round n
            _ -> 0
          char = case KM.lookup "character" p of
            Just (Aeson.Number n) -> round n
            _ -> 0
      in Position line char

parseWorkspaceSymbolResponse _ = []

-- | Convert workspace symbols to DocumentSymbolResult format
-- Groups by file URI, then creates nodes/edges per file.
workspaceSymbolsToDocumentSymbols :: [SymbolInformation] -> Map FilePath [DocumentSymbolResult]
workspaceSymbolsToDocumentSymbols syms =
  Map.fromListWith (++)
    [ (T.unpack (locUri (siLocation sym))
    , [ DocumentSymbolResult
        { dsrName = siName sym
        , dsrKind = siKind sym
        , dsrRange = locRange (siLocation sym)
        , dsrChildren = []
        }
      ]
    )
  | sym <- syms
  , not (T.null (locUri (siLocation sym)))
  ]