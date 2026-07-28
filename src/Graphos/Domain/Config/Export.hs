-- | Export configuration types.
-- Neo4j, Memgraph, and their defaults.
-- Pure data types — no IO.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config.Export
  ( Neo4jConfig(..)
  , defaultNeo4jConfig
  , MemgraphConfig(..)
  , defaultMemgraphConfig
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, withObject, (.:?), (.!=))
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import Graphos.Domain.Config.Extraction (lowerFirst)
import GHC.Generics (Generic)

-- ───────────────────────────────────────────────
-- Neo4j Configuration
-- ───────────────────────────────────────────────

-- | Neo4j connection configuration for Cypher export and push.
-- Used by --neo4j and --neo4j-push flags, overridable via graphos.yaml.
--
-- All fields except URI are optional in the YAML file (they have defaults).
-- The FromJSON instance uses .:? so partial neo4j sections are valid.
data Neo4jConfig = Neo4jConfig
  { neo4jUri          :: String  -- ^ Neo4j HTTP URI (e.g. "http://localhost:7474")
  , neo4jUser         :: String  -- ^ Username for authentication (e.g. "neo4j")
  , neo4jPassword     :: String  -- ^ Password for authentication
  , neo4jPushMode     :: String  -- ^ Push mode: "full", "subgraph", or "community" (default: "subgraph")
  , neo4jSubgraphSize :: Int     -- ^ Representatives per community for subgraph mode (default: 7)
  } deriving (Eq, Show, Generic)

instance ToJSON Neo4jConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 5 }

-- | Custom FromJSON: allows partial neo4j sections in graphos.yaml.
-- All fields are optional — missing values fall back to defaults.
instance FromJSON Neo4jConfig where
  parseJSON = withObject "Neo4jConfig" $ \v -> Neo4jConfig
    <$> v .:? "uri"           .!= "http://localhost:7474"
    <*> v .:? "user"          .!= "neo4j"
    <*> v .:? "password"      .!= "graphos_dev"
    <*> v .:? "pushMode"      .!= "subgraph"
    <*> v .:? "subgraphSize"  .!= 7

-- | Default Neo4j configuration for local development.
defaultNeo4jConfig :: Neo4jConfig
defaultNeo4jConfig = Neo4jConfig
  { neo4jUri          = "http://localhost:7474"
  , neo4jUser         = "neo4j"
  , neo4jPassword     = "graphos_dev"
  , neo4jPushMode     = "subgraph"
  , neo4jSubgraphSize = 7
  }

-- ───────────────────────────────────────────────
-- Memgraph Configuration
-- ───────────────────────────────────────────────

-- | Memgraph connection configuration for Bolt-protocol export and push.
-- Memgraph uses Bolt protocol (not HTTP) — the URI format is bolt://host:port.
-- All fields are optional in graphos.yaml — missing values fall back to defaults.
data MemgraphConfig = MemgraphConfig
  { mgUri          :: String  -- ^ Memgraph Bolt URI (e.g. "bolt://localhost:7688")
  , mgUser         :: String  -- ^ Username (often "" for Memgraph — no auth by default)
  , mgPassword     :: String  -- ^ Password (often "" for Memgraph — no auth by default)
  , mgPushMode     :: String  -- ^ Push mode: "full", "subgraph", or "community" (default: "subgraph")
  , mgSubgraphSize :: Int     -- ^ Representatives per community for subgraph mode (default: 7)
  } deriving (Eq, Show, Generic)

instance ToJSON MemgraphConfig where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lowerFirst . drop 2 }

instance FromJSON MemgraphConfig where
  parseJSON = withObject "MemgraphConfig" $ \v -> MemgraphConfig
    <$> v .:? "uri"           .!= "bolt://localhost:7688"
    <*> v .:? "user"          .!= ""
    <*> v .:? "password"      .!= ""
    <*> v .:? "pushMode"      .!= "subgraph"
    <*> v .:? "subgraphSize"  .!= 7

defaultMemgraphConfig :: MemgraphConfig
defaultMemgraphConfig = MemgraphConfig
  { mgUri          = "bolt://localhost:7688"
  , mgUser         = ""
  , mgPassword     = ""
  , mgPushMode     = "subgraph"
  , mgSubgraphSize = 7
  }