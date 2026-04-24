-- | LSP server capability parsing from initialize responses.
-- Handles both nested and flat capability formats from different LSP servers.
-- Some servers (e.g. typescript-language-server) send capabilities at the top level:
--   {"capabilities": {"documentSymbolProvider": true, "workspaceSymbolProvider": true}}
-- Others nest them under groups:
--   {"capabilities": {"textDocument": {"documentSymbolProvider": true}, "workspace": {"symbolProvider": true}}}
-- The LSP spec uses different key names depending on format:
--   Nested: workspace.symbolProvider  |  Flat: workspaceSymbolProvider
--   Nested: textDocument.documentSymbolProvider  |  Flat: documentSymbolProvider
-- We check both formats, preferring nested if present.
module Graphos.Infrastructure.LSP.CapabilityParse
  ( parseServerCapabilities
  , defaultServerCapabilities
  , ServerCapabilities(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AK (fromText)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

import Graphos.Infrastructure.LSP.Protocol

-- | Parse ServerCapabilities from the initialize JSON-RPC response.
-- Defaults to False for any capability not explicitly advertised (safe fallback).
parseServerCapabilities :: Value -> ServerCapabilities
parseServerCapabilities (Object o) =
  let mResult = KM.lookup "result" o
      mCaps = case mResult of
        Just (Object r) -> KM.lookup "capabilities" r
        _ -> Nothing
  in case mCaps of
       Just (Object caps) -> parseCapsObj caps
       _ -> defaultServerCapabilities
parseServerCapabilities _ = defaultServerCapabilities

parseCapsObj :: Aeson.Object -> ServerCapabilities
parseCapsObj caps = ServerCapabilities
  { scpDocumentSymbolProvider  = lookupCapability caps "textDocument" "documentSymbolProvider" "documentSymbolProvider"
  , scpReferencesProvider      = lookupCapability caps "textDocument" "referencesProvider" "referencesProvider"
  , scpCallHierarchyProvider   = lookupCapability caps "textDocument" "callHierarchyProvider" "callHierarchyProvider"
  , scpDefinitionProvider      = lookupCapability caps "textDocument" "definitionProvider" "definitionProvider"
  , scpWorkspaceSymbolProvider = lookupCapability caps "workspace" "symbolProvider" "workspaceSymbolProvider"
  }

-- | Default server capabilities (all disabled — safe fallback)
defaultServerCapabilities :: ServerCapabilities
defaultServerCapabilities = ServerCapabilities
  { scpDocumentSymbolProvider  = False
  , scpReferencesProvider      = False
  , scpCallHierarchyProvider   = False
  , scpDefinitionProvider       = False
  , scpWorkspaceSymbolProvider  = False
  }

-- | Look up a capability, checking nested, then flat format.
-- Priority:
--   1. Nested: caps.{topKey}.{subKey}        (e.g. caps.textDocument.documentSymbolProvider)
--   2. Flat:   caps.{flatKey}                 (e.g. caps.documentSymbolProvider)
--
-- The flat key name differs from the nested sub-key for workspace/symbol:
--   Nested: caps.workspace.symbolProvider    (sub-key = "symbolProvider")
--   Flat:   caps.workspaceSymbolProvider     (flat-key = "workspaceSymbolProvider")
lookupCapability :: Aeson.Object -> Text -> Text -> Text -> Bool
lookupCapability caps topKey subKey flatKey =
  case KM.lookup (AK.fromText topKey) caps of
    Just (Object td) ->
      case KM.lookup (AK.fromText subKey) td of
        Just (Aeson.Bool b)    -> b
        Just (Object _)        -> True    -- capability with options = enabled
        Just (Aeson.Number _)  -> True    -- some servers use 0/1
        _                      -> lookupFlat caps flatKey  -- nested key not found, try flat
    _ -> lookupFlat caps flatKey  -- no nested group, try flat

-- | Look up a capability at the top level of the capabilities object.
-- Handles: Bool, Object (options = enabled), Number (non-zero = enabled).
lookupFlat :: Aeson.Object -> Text -> Bool
lookupFlat caps key =
  case KM.lookup (AK.fromText key) caps of
    Just (Aeson.Bool b)    -> b
    Just (Object _)        -> True    -- capability with options = enabled
    Just (Aeson.Number n)  -> n /= 0  -- some servers use 0/1
    _                      -> False