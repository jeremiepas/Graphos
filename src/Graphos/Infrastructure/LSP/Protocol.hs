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
  , lspCallHierarchyIncoming
  , lspShutdown
  , lspExit

    -- * Helpers
  , languageIdFromExt
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, object, (.=), (.:), withObject, (.:?), (.!=))
import Data.Text (Text)
import qualified Data.Text as T

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

lspInitialize :: FilePath -> JSONRPCRequest
lspInitialize rootPath = JSONRPCRequest
  { jrpcId     = 1
  , jrpcMethod = "initialize"
  , jrpcParams = Just $ object
      [ "processId" .= (42 :: Int)
      , "rootPath"  .= T.pack rootPath
      , "rootUri"   .= T.pack ("file://" ++ rootPath)
      , "capabilities" .= object
        [ "textDocument" .= object
          [ "documentSymbol" .= object
            [ "dynamicRegistration" .= False
            , "hierarchicalDocumentSymbolSupport" .= True
            ]
          , "publishDiagnostics" .= object []
          ]
        , "workspace" .= object
          [ "symbol" .= object
            [ "dynamicRegistration" .= False
            ]
          ]
        ]
      , "workspaceFolders" .= [object ["uri" .= T.pack ("file://" ++ rootPath), "name" .= ("root" :: Text)]]
      , "trace" .= ("off" :: Text)
      ]
  }

-- | initialized notification (must be sent after initialize response)
lspInitialized :: JSONRPCNotification
lspInitialized = JSONRPCNotification
  { jrpcnMethod = "initialized"
  , jrpcnParams = Just $ object []
  }

lspDocumentSymbol :: FilePath -> JSONRPCRequest
lspDocumentSymbol filePath = JSONRPCRequest
  { jrpcId     = 2
  , jrpcMethod = "textDocument/documentSymbol"
  , jrpcParams = Just $ object
      [ "textDocument" .= object ["uri" .= T.pack ("file://" ++ filePath)]
      ]
  }

lspReferences :: FilePath -> Int -> Int -> JSONRPCRequest
lspReferences filePath line char = JSONRPCRequest
  { jrpcId     = 3
  , jrpcMethod = "textDocument/references"
  , jrpcParams = Just $ object
      [ "textDocument" .= object ["uri" .= T.pack ("file://" ++ filePath)]
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
lspDocumentSymbolWithId :: FilePath -> Int -> JSONRPCRequest
lspDocumentSymbolWithId filePath msgId = JSONRPCRequest
  { jrpcId     = msgId
  , jrpcMethod = "textDocument/documentSymbol"
  , jrpcParams = Just $ object
      [ "textDocument" .= object ["uri" .= T.pack ("file://" ++ filePath)]
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
lspDidOpen :: FilePath -> Text -> Text -> JSONRPCNotification
lspDidOpen filePath langId fileText = JSONRPCNotification
  { jrpcnMethod = "textDocument/didOpen"
  , jrpcnParams = Just $ object
      [ "textDocument" .= object
          [ "uri"        .= T.pack ("file://" ++ filePath)
          , "languageId" .= langId
          , "version"    .= (1 :: Int)
          , "text"       .= fileText
          ]
      ]
  }

-- | textDocument/didClose notification
lspDidClose :: FilePath -> JSONRPCNotification
lspDidClose filePath = JSONRPCNotification
  { jrpcnMethod = "textDocument/didClose"
  , jrpcnParams = Just $ object
      [ "textDocument" .= object ["uri" .= T.pack ("file://" ++ filePath)]
      ]
  }

-- | Map file extension to LSP language ID
languageIdFromExt :: String -> Text
languageIdFromExt ext = case ext of
  ".py"   -> "python"
  ".pyw"  -> "python"
  ".hs"   -> "haskell"
  ".lhs"  -> "haskell"
  ".js"   -> "javascript"
  ".jsx"  -> "javascriptreact"
  ".ts"   -> "typescript"
  ".tsx"  -> "typescriptreact"
  ".go"   -> "go"
  ".rs"   -> "rust"
  ".c"    -> "c"
  ".cpp"  -> "cpp"
  ".h"    -> "c"
  ".hpp"  -> "cpp"
  ".java" -> "java"
  ".cs"   -> "csharp"
  ".rb"   -> "ruby"
  ".php"  -> "php"
  ".swift" -> "swift"
  ".kt"   -> "kotlin"
  ".kts"  -> "kotlin"
  ".scala" -> "scala"
  ".lua"  -> "lua"
  ".zig"  -> "zig"
  ".ex"   -> "elixir"
  ".exs"  -> "elixir"
  ".dart" -> "dart"
  ".vue"  -> "vue"
  ".svelte" -> "svelte"
  _       -> "plaintext"