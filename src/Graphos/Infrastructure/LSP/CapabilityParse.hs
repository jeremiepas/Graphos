-- | LSP server capability parsing from initialize responses.
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
  { scpDocumentSymbolProvider  = lookupBool caps "textDocument" "documentSymbolProvider"
  , scpReferencesProvider      = lookupBool caps "textDocument" "referencesProvider"
  , scpCallHierarchyProvider   = lookupBoolCaps caps "textDocument" "callHierarchyProvider"
  , scpDefinitionProvider      = lookupBool caps "textDocument" "definitionProvider"
  , scpWorkspaceSymbolProvider = lookupBoolCaps caps "workspace" "symbolProvider"
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

-- | Look up a boolean capability nested under a top-level key.
lookupBool :: Aeson.Object -> Text -> Text -> Bool
lookupBool caps topKey subKey =
  case KM.lookup (AK.fromText topKey) caps of
    Just (Object td) ->
      case KM.lookup (AK.fromText subKey) td of
        Just (Aeson.Bool b)      -> b
        Just (Object _)          -> True
        Just (Aeson.Number _)     -> True
        _ -> False
    _ -> False

-- | Same as lookupBool but also handles the capability being a nested object
lookupBoolCaps :: Aeson.Object -> Text -> Text -> Bool
lookupBoolCaps caps topKey subKey =
  case KM.lookup (AK.fromText topKey) caps of
    Just (Object td) ->
      case KM.lookup (AK.fromText subKey) td of
        Just (Aeson.Bool b)      -> b
        Just (Object _)          -> True
        Just (Aeson.Number _)    -> True
        _ -> False
    _ -> False