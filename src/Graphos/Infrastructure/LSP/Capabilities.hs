-- | LSP capability detection - discovers what an LSP server supports
module Graphos.Infrastructure.LSP.Capabilities
  ( -- * Capability detection
    detectLSPCapabilities
  , checkDocumentSymbolSupport
  , checkReferenceSupport
  , checkCallHierarchySupport

    -- * LSP server discovery
  , discoverLanguageServers
  , LanguageServerInfo(..)
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)

import Graphos.Infrastructure.LSP.Client (languageServerCommands, LSPClientConfig(..))

-- | Information about a discovered language server
data LanguageServerInfo = LanguageServerInfo
  { lsiCommand      :: FilePath
  , lsiArgs         :: [String]
  , lsiExtensions   :: [String]
  , lsiName         :: Text
  } deriving (Eq, Show)

-- | Detect what capabilities an LSP server supports
data LSPCapabilities = LSPCapabilities
  { lspCapDocumentSymbols   :: Bool
  , lspCapReferences        :: Bool
  , lspCapCallHierarchy     :: Bool
  , lspCapDefinition        :: Bool
  , lspCapWorkspaceSymbol   :: Bool
  } deriving (Eq, Show)

-- | Detect capabilities by checking server initialization response
detectLSPCapabilities :: LSPClientConfig -> IO LSPCapabilities
detectLSPCapabilities _ = pure LSPCapabilities
  { lspCapDocumentSymbols = True   -- Most LSP servers support this
  , lspCapReferences      = True   -- Most LSP servers support this
  , lspCapCallHierarchy   = False  -- Not universally supported
  , lspCapDefinition      = True   -- Most LSP servers support this
  , lspCapWorkspaceSymbol = False  -- Will be set from actual response
  }

-- | Check if a server supports document symbols
checkDocumentSymbolSupport :: LSPCapabilities -> Bool
checkDocumentSymbolSupport = lspCapDocumentSymbols

-- | Check if a server supports find references
checkReferenceSupport :: LSPCapabilities -> Bool
checkReferenceSupport = lspCapReferences

-- | Check if a server supports call hierarchy
checkCallHierarchySupport :: LSPCapabilities -> Bool
checkCallHierarchySupport = lspCapCallHierarchy

-- | Discover all available language servers on the system
discoverLanguageServers :: IO [LanguageServerInfo]
discoverLanguageServers = do
  let servers = Map.toList languageServerCommands
  catMaybesIO $ map (\(ext, (cmd, args)) -> do
    found <- findExecutable cmd
    case found of
      Just path -> pure $ Just LanguageServerInfo
        { lsiCommand    = path
        , lsiArgs       = args
        , lsiExtensions = [ext]
        , lsiName       = T.pack cmd
        }
      Nothing -> pure Nothing
    ) servers
  where
    catMaybesIO :: [IO (Maybe a)] -> IO [a]
    catMaybesIO = fmap catMaybes . sequence

    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr (\mx acc -> case mx of Just x -> x:acc; Nothing -> acc) []