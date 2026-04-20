-- | LSP Client (re-export hub) — thin orchestrator that delegates to sub-modules.
-- Uses proper JSON-RPC over stdio protocol with Content-Length framing.
-- Shares one LSP connection per language server for efficiency.
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

import Graphos.Infrastructure.LSP.ServerMap
  ( languageServerCommands
  , findLSPServer
  , languageIdFromExt
  )
import Graphos.Infrastructure.LSP.Transport
  ( LSPClient(..)
  , LSPClientConfig(..)
  , defaultLSPConfig
  , connectToLSP
  , disconnectLSP
  )
import Graphos.Infrastructure.LSP.CapabilityParse
  ( parseServerCapabilities
  )
import Graphos.Infrastructure.LSP.Extraction
  ( extractViaLSP
  , extractDocumentSymbols
  , extractCallHierarchy
  , extractWorkspaceSymbols
  , workspaceSymbolsToDocumentSymbols
  , symbolToNodes
  , symbolTreeToEdges
  )