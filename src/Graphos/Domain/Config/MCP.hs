-- | MCP server configuration types.
-- MCPConfig and its defaults.
-- Pure data types — no IO.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config.MCP
  ( MCPConfig(..)
  , defaultMCPConfig
  , mergeMCPConfig
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, withObject, (.:?), (.!=))
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import Graphos.Domain.Config.Extraction (lowerFirst)
import GHC.Generics (Generic)

-- ───────────────────────────────────────────────
-- MCP Server Configuration
-- ───────────────────────────────────────────────

-- | Per-request limits for the MCP server.
-- All fields are optional in graphos.yaml — missing values fall back to defaults.
--
-- Defaults are chosen to comfortably exceed typical requests; the node cap
-- bounds worst-case expansions (e.g. a 40k-member community) and the timeout
-- is a safety net so a request never hangs.
data MCPConfig = MCPConfig
  { mcMaxRequestNodes :: Int   -- ^ Per-request cap on nodes returned (neighborhoods, community members, context)
  , mcRequestTimeout  :: Int   -- ^ Per-request wall-clock timeout in seconds
  } deriving (Eq, Show, Generic)

instance ToJSON MCPConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 3 }

instance FromJSON MCPConfig where
  parseJSON = withObject "MCPConfig" $ \v -> MCPConfig
    <$> v .:? "maxRequestNodes" .!= 5000
    <*> v .:? "requestTimeout"  .!= 30

defaultMCPConfig :: MCPConfig
defaultMCPConfig = MCPConfig
  { mcMaxRequestNodes = 5000
  , mcRequestTimeout  = 30
  }

-- | Merge two MCPConfig values: project overrides global.
-- A field in project is considered "explicit" if it differs from the default.
mergeMCPConfig :: MCPConfig -> MCPConfig -> MCPConfig
mergeMCPConfig global project = MCPConfig
  { mcMaxRequestNodes = if mcMaxRequestNodes project /= mcMaxRequestNodes defaultMCPConfig
                           then mcMaxRequestNodes project
                           else mcMaxRequestNodes global
  , mcRequestTimeout  = if mcRequestTimeout project /= mcRequestTimeout defaultMCPConfig
                           then mcRequestTimeout project
                           else mcRequestTimeout global
  }
