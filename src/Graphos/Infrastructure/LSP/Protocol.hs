-- | LSP Protocol types (JSON-RPC messages)
module Graphos.Infrastructure.LSP.Protocol
  ( -- * JSON-RPC types
    JSONRPCRequest(..)
  , JSONRPCResponse(..)
  , JSONRPCNotification(..)

    -- * LSP Message types
  , InitializeParams(..)
  , InitializeResult(..)
  , ServerCapabilities(..)
  , TextDocumentIdentifier(..)
  , Position(..)
  , Range(..)
  , Location(..)
  , DocumentSymbolParams(..)
  , DocumentSymbolResult(..)
  , ReferenceParams(..)
  , CallHierarchyItem(..)
  , CallHierarchyIncomingCallsParams(..)
  , SymbolInformation(..)

    -- * LSP methods
  , lspInitialize
  , lspInitialized
  , lspDocumentSymbol
  , lspDocumentSymbolWithId
  , lspWorkspaceSymbol
  , lspWorkspaceSymbolWithId
  , lspDidOpen
  , lspDidClose
  , lspReferences
  , lspReferencesWithId
  , lspCallHierarchyIncoming
  , lspCallHierarchyPrepareWithId
  , lspCallHierarchyIncomingWithId
  , lspShutdown
  , lspExit
  , rangeToJson

    -- * Document URIs
  , fileUri
  , uriToFilePath

    -- * Helpers
  , languageIdFromExt
  ) where

import Data.Aeson (ToJSON(..), Value, object, (.=))
import qualified Data.ByteString as BS
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- ───────────────────────────────────────────────
-- JSON-RPC types
-- ───────────────────────────────────────────────

data JSONRPCRequest = JSONRPCRequest
  { jrpcId      :: Int
  , jrpcMethod  :: Text
  , jrpcParams  :: Maybe Value
  } deriving (Eq, Show)

instance ToJSON JSONRPCRequest where
  toJSON req = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id"      .= jrpcId req
    , "method"  .= jrpcMethod req
    , "params"  .= jrpcParams req
    ]

data JSONRPCResponse = JSONRPCResponse
  { jrpcRspId   :: Int
  , jrpcResult :: Maybe Value
  , jrpcError  :: Maybe Value
  } deriving (Eq, Show)

data JSONRPCNotification = JSONRPCNotification
  { jrpcnMethod :: Text
  , jrpcnParams :: Maybe Value
  } deriving (Eq, Show)

instance ToJSON JSONRPCNotification where
  toJSON n = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "method"  .= jrpcnMethod n
    , "params"  .= jrpcnParams n
    ]

-- ───────────────────────────────────────────────
-- LSP types
-- ───────────────────────────────────────────────

data InitializeParams = InitializeParams
  { initProcessId     :: Int
  , initRootUri       :: Maybe Text
  , initCapabilities  :: ClientCapabilities
  } deriving (Eq, Show)

data ClientCapabilities = ClientCapabilities
  deriving (Eq, Show)

data InitializeResult = InitializeResult
  { initResultCapabilities :: ServerCapabilities
  } deriving (Eq, Show)

data ServerCapabilities = ServerCapabilities
  { scpDocumentSymbolProvider    :: Bool
  , scpReferencesProvider        :: Bool
  , scpCallHierarchyProvider     :: Bool
  , scpDefinitionProvider        :: Bool
  , scpWorkspaceSymbolProvider   :: Bool
  } deriving (Eq, Show)

data TextDocumentIdentifier = TextDocumentIdentifier
  { tdiUri :: Text
  } deriving (Eq, Show)

data Position = Position
  { posLine      :: Int
  , posCharacter :: Int
  } deriving (Eq, Show)

data Range = Range
  { rangeStart :: Position
  , rangeEnd   :: Position
  } deriving (Eq, Show)

data Location = Location
  { locUri   :: Text
  , locRange :: Range
  } deriving (Eq, Show)

data DocumentSymbolParams = DocumentSymbolParams
  { dspTextDocument :: TextDocumentIdentifier
  } deriving (Eq, Show)

data DocumentSymbolResult = DocumentSymbolResult
  { dsrName      :: Text
  , dsrKind      :: Int
  , dsrRange     :: Range
  , dsrSelectionRange :: Range
    -- ^ the symbol's name; requests about the symbol are asked here, since a
    -- server can answer nothing at the start of 'dsrRange' (e.g. @export@)
  , dsrChildren  :: [DocumentSymbolResult]
  } deriving (Eq, Show)

data ReferenceParams = ReferenceParams
  { refTextDocument :: TextDocumentIdentifier
  , refPosition     :: Position
  , refContext      :: ReferenceContext
  } deriving (Eq, Show)

data ReferenceContext = ReferenceContext
  { rcIncludeDeclaration :: Bool
  } deriving (Eq, Show)

data CallHierarchyItem = CallHierarchyItem
  { chiName           :: Text
  , chiKind           :: Int
  , chiUri            :: Text
  , chiRange          :: Range
  , chiSelectionRange :: Range
  } deriving (Eq, Show)

data CallHierarchyIncomingCallsParams = CallHierarchyIncomingCallsParams
  { chicItem :: CallHierarchyItem
  } deriving (Eq, Show)

-- | Result from workspace/symbol — flat list of symbols with location URIs
data SymbolInformation = SymbolInformation
  { siName     :: Text
  , siKind     :: Int
  , siLocation :: Location
  } deriving (Eq, Show)

-- ───────────────────────────────────────────────
-- LSP methods
-- ───────────────────────────────────────────────

-- | The @initialize@ request. Pure: the caller supplies the process id of the
-- running Graphos process (a server with a parent watchdog exits when that pid
-- is not alive) and the optional @initializationOptions@ of the session.
-- Every capability Graphos sends requests for is declared.
lspInitialize :: Int -> Maybe Value -> FilePath -> JSONRPCRequest
lspInitialize processId initOptions rootPath = JSONRPCRequest
  { jrpcId     = 1
  , jrpcMethod = "initialize"
  , jrpcParams = Just $ object $
      [ "processId" .= processId
      , "rootPath"  .= T.pack rootPath
      , "rootUri"   .= fileUri rootPath rootPath
      , "capabilities" .= object
        [ "textDocument" .= object
          [ "documentSymbol" .= object
            [ "dynamicRegistration" .= False
            , "hierarchicalDocumentSymbolSupport" .= True
            ]
          , "references" .= object
            [ "dynamicRegistration" .= False ]
          , "callHierarchy" .= object
            [ "dynamicRegistration" .= False ]
          , "publishDiagnostics" .= object []
          ]
        , "workspace" .= object
          [ "symbol" .= object
            [ "dynamicRegistration" .= False
            ]
          ]
        ]
      , "workspaceFolders" .= [object ["uri" .= fileUri rootPath rootPath, "name" .= ("root" :: Text)]]
      , "trace" .= ("off" :: Text)
      ] ++ maybe [] (\v -> ["initializationOptions" .= v]) initOptions
  }

-- ───────────────────────────────────────────────
-- Document URIs
-- ───────────────────────────────────────────────

-- | The absolute @file://@ URI of @path@. A relative path is resolved against
-- the session root; @.@ and @..@ segments are removed; characters outside the
-- unreserved set and @/@ are percent-encoded (UTF-8).
fileUri :: FilePath -> FilePath -> Text
fileUri root path =
  let absolute = case path of
        ('/':_) -> path
        _       -> root ++ "/" ++ path
  in "file://" <> percentEncode (normalisePath absolute)

-- | Remove empty, @.@ and @..@ segments of an absolute path.
normalisePath :: FilePath -> FilePath
normalisePath p = '/' : intercalate "/" (reverse (foldl step [] (splitSlash p)))
  where
    step acc seg = case seg of
      ""   -> acc
      "."  -> acc
      ".." -> drop 1 acc
      _    -> seg : acc
    splitSlash s = case break (== '/') s of
      (a, [])     -> [a]
      (a, _:rest) -> a : splitSlash rest

percentEncode :: FilePath -> Text
percentEncode = T.concat . map enc . BS.unpack . TE.encodeUtf8 . T.pack
  where
    enc w
      | unreserved w = T.singleton (toEnum (fromIntegral w))
      | otherwise    = T.pack ('%' : hex (w `div` 16) : [hex (w `mod` 16)])
    unreserved w = (w >= 0x41 && w <= 0x5A) || (w >= 0x61 && w <= 0x7A)
                || (w >= 0x30 && w <= 0x39) || w `elem` [0x2D, 0x2E, 0x5F, 0x7E, 0x2F]
    hex n = "0123456789ABCDEF" !! fromIntegral n

-- | The file path of a @file://@ URI (percent-decoded), or 'Nothing' for any
-- other scheme or an empty path.
uriToFilePath :: Text -> Maybe FilePath
uriToFilePath uri = do
  rest <- T.stripPrefix "file://" uri
  -- @file://host/path@ is not produced by language servers; keep the path part.
  let path = if "/" `T.isPrefixOf` rest then rest else T.dropWhile (/= '/') rest
  if T.null path then Nothing else Just (percentDecode path)

percentDecode :: Text -> FilePath
percentDecode = T.unpack . TE.decodeUtf8With (\_ _ -> Just '\xFFFD') . BS.pack . go . BS.unpack . TE.encodeUtf8
  where
    go (0x25 : a : b : rest)
      | Just x <- unhex a, Just y <- unhex b = (x * 16 + y) : go rest
    go (w : rest) = w : go rest
    go [] = []
    unhex w
      | w >= 0x30 && w <= 0x39 = Just (w - 0x30)
      | w >= 0x41 && w <= 0x46 = Just (w - 0x37)
      | w >= 0x61 && w <= 0x66 = Just (w - 0x57)
      | otherwise              = Nothing

-- | initialized notification (must be sent after initialize response)
lspInitialized :: JSONRPCNotification
lspInitialized = JSONRPCNotification
  { jrpcnMethod = "initialized"
  , jrpcnParams = Just $ object []
  }

lspDocumentSymbol :: Text -> JSONRPCRequest
lspDocumentSymbol uri = JSONRPCRequest
  { jrpcId     = 2
  , jrpcMethod = "textDocument/documentSymbol"
  , jrpcParams = Just $ object
      [ "textDocument" .= object ["uri" .= uri]
      ]
  }

lspReferences :: Text -> Int -> Int -> JSONRPCRequest
lspReferences uri line char = JSONRPCRequest
  { jrpcId     = 3
  , jrpcMethod = "textDocument/references"
  , jrpcParams = Just $ object
      [ "textDocument" .= object ["uri" .= uri]
      , "position"     .= object ["line" .= line, "character" .= char]
      , "context"      .= object ["includeDeclaration" .= True]
      ]
  }

-- | References request with custom message ID (for concurrent requests)
lspReferencesWithId :: Text -> Int -> Int -> Int -> JSONRPCRequest
lspReferencesWithId uri line char msgId = JSONRPCRequest
  { jrpcId     = msgId
  , jrpcMethod = "textDocument/references"
  , jrpcParams = Just $ object
      [ "textDocument" .= object ["uri" .= uri]
      , "position"     .= object ["line" .= line, "character" .= char]
      , "context"      .= object ["includeDeclaration" .= True]
      ]
  }

lspCallHierarchyIncoming :: Text -> Int -> JSONRPCRequest
lspCallHierarchyIncoming name id_ = JSONRPCRequest
  { jrpcId     = id_
  , jrpcMethod = "callHierarchy/incomingCalls"
  , jrpcParams = Just $ object
      [ "item" .= object ["name" .= name]
      ]
  }

-- | Call hierarchy prepare request with custom message ID
lspCallHierarchyPrepareWithId :: Text -> Int -> Int -> Int -> JSONRPCRequest
lspCallHierarchyPrepareWithId uri line char msgId = JSONRPCRequest
  { jrpcId     = msgId
  , jrpcMethod = "textDocument/prepareCallHierarchy"
  , jrpcParams = Just $ object
      [ "textDocument" .= object ["uri" .= uri]
      , "position"     .= object ["line" .= line, "character" .= char]
      ]
  }

-- | Call hierarchy incoming calls with custom message ID
lspCallHierarchyIncomingWithId :: CallHierarchyItem -> Int -> JSONRPCRequest
lspCallHierarchyIncomingWithId item msgId = JSONRPCRequest
  { jrpcId     = msgId
  , jrpcMethod = "callHierarchy/incomingCalls"
  , jrpcParams = Just $ object
      [ "item" .= object
          [ "name"           .= chiName item
          , "kind"           .= chiKind item
          , "uri"            .= chiUri item
          , "range"          .= rangeToJson (chiRange item)
          , "selectionRange" .= rangeToJson (chiSelectionRange item)
          ]
      ]
  }

rangeToJson :: Range -> Value
rangeToJson r = object
  [ "start" .= object ["line" .= posLine (rangeStart r), "character" .= posCharacter (rangeStart r)]
  , "end"   .= object ["line" .= posLine (rangeEnd r), "character" .= posCharacter (rangeEnd r)]
  ]

lspShutdown :: JSONRPCRequest
lspShutdown = JSONRPCRequest
  { jrpcId     = 999
  , jrpcMethod = "shutdown"
  , jrpcParams = Nothing
  }

-- | exit notification — must be sent after shutdown response
lspExit :: JSONRPCNotification
lspExit = JSONRPCNotification
  { jrpcnMethod = "exit"
  , jrpcnParams = Nothing
  }

-- | Document symbol request with custom ID
lspDocumentSymbolWithId :: Text -> Int -> JSONRPCRequest
lspDocumentSymbolWithId uri msgId = JSONRPCRequest
  { jrpcId     = msgId
  , jrpcMethod = "textDocument/documentSymbol"
  , jrpcParams = Just $ object
      [ "textDocument" .= object ["uri" .= uri]
      ]
  }

-- | workspace/symbol request — returns all symbols in the project
lspWorkspaceSymbol :: JSONRPCRequest
lspWorkspaceSymbol = JSONRPCRequest
  { jrpcId     = 100
  , jrpcMethod = "workspace/symbol"
  , jrpcParams = Just $ object ["query" .= ("" :: Text)]
  }

-- | workspace/symbol request with custom ID
lspWorkspaceSymbolWithId :: Int -> Text -> JSONRPCRequest
lspWorkspaceSymbolWithId msgId query = JSONRPCRequest
  { jrpcId     = msgId
  , jrpcMethod = "workspace/symbol"
  , jrpcParams = Just $ object ["query" .= query]
  }

-- | textDocument/didOpen notification - needs language ID and file text
lspDidOpen :: Text -> Text -> Text -> JSONRPCNotification
lspDidOpen uri langId fileText = JSONRPCNotification
  { jrpcnMethod = "textDocument/didOpen"
  , jrpcnParams = Just $ object
      [ "textDocument" .= object
          [ "uri"        .= uri
          , "languageId" .= langId
          , "version"    .= (1 :: Int)
          , "text"       .= fileText
          ]
      ]
  }

-- | textDocument/didClose notification
lspDidClose :: Text -> JSONRPCNotification
lspDidClose uri = JSONRPCNotification
  { jrpcnMethod = "textDocument/didClose"
  , jrpcnParams = Just $ object
      [ "textDocument" .= object ["uri" .= uri]
      ]
  }

-- | Map file extension to LSP language ID (default / hardcoded).
-- Prefer 'languageIdFromExtConfig' with a GraphosConfig for user-configurable mappings.
languageIdFromExt :: String -> Text
languageIdFromExt ext = case ext of
  ".py"    -> "python"
  ".pyw"   -> "python"
  ".hs"    -> "haskell"
  ".lhs"   -> "haskell"
  ".js"    -> "javascript"
  ".jsx"   -> "javascriptreact"
  ".ts"    -> "typescript"
  ".tsx"   -> "typescriptreact"
  ".go"    -> "go"
  ".rs"    -> "rust"
  ".c"     -> "c"
  ".cpp"   -> "cpp"
  ".h"     -> "c"
  ".hpp"   -> "cpp"
  ".java"  -> "java"
  ".cs"    -> "csharp"
  ".rb"    -> "ruby"
  ".php"   -> "php"
  ".swift" -> "swift"
  ".kt"    -> "kotlin"
  ".kts"   -> "kotlin"
  ".scala" -> "scala"
  ".lua"   -> "lua"
  ".zig"   -> "zig"
  ".ex"    -> "elixir"
  ".exs"   -> "elixir"
  ".dart"  -> "dart"
  ".vue"   -> "vue"
  ".svelte" -> "svelte"
  ".nix"   -> "nix"
  ".text"  -> "plaintext"
  ".raml"  -> "raml"
  _        -> "plaintext"