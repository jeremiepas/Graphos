-- | Declarative import-resolution configuration (custom-extractors 1.3).
--
-- A @resolve:@ entry maps a file extension to one of the built-in resolver
-- kinds, so users can replace hardcoded per-language import handling without
-- patching Graphos. Pure data types — no IO; the resolvers themselves live in
-- Infrastructure/Extract/ImportResolution.
{-# LANGUAGE DeriveGeneric #-}
module Graphos.Domain.Config.Resolve
  ( -- * Resolver kind
    ResolveKind(..)

    -- * Per-extension resolution config
  , ResolveConfig(..)

    -- * Defaults
  , defaultResolveConfigs
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), withObject, (.:), (.:?), (.!=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

-- | Built-in resolver strategies selectable in @resolve: { ".ext": { kind: … } }@.
--
--   * 'ResolvePackage'    — package/module dotted name → path under a source root
--     (Java/Kotlin/Scala/Go-style packages).
--   * 'ResolvePSR4'       — PHP PSR-4 namespace prefix → directory under a root.
--   * 'ResolveModulePath' — module name → path driven by a source-of-truth index
--     file (e.g. @settings.gradle@, @pom.xml@).
--   * 'ResolvePathPrefix' — import path → file by prefix concatenation (the
--     default relative-path behaviour, made explicit).
data ResolveKind
  = ResolvePackage
  | ResolvePSR4
  | ResolveModulePath
  | ResolvePathPrefix
  deriving (Eq, Show, Generic)

instance ToJSON ResolveKind where
  toJSON ResolvePackage    = "package"
  toJSON ResolvePSR4       = "psr4"
  toJSON ResolveModulePath = "module-path"
  toJSON ResolvePathPrefix = "path-prefix"

instance FromJSON ResolveKind where
  parseJSON (String "package")     = pure ResolvePackage
  parseJSON (String "psr4")        = pure ResolvePSR4
  parseJSON (String "module-path") = pure ResolveModulePath
  parseJSON (String "path-prefix") = pure ResolvePathPrefix
  parseJSON v = fail $ "Unknown resolve kind: " ++ show v
    ++ ". Expected package, psr4, module-path, or path-prefix"

-- | Resolution rules for one extension.
data ResolveConfig = ResolveConfig
  { rcKind          :: ResolveKind
  , rcRoots         :: [FilePath]       -- ^ source roots, relative to the config directory
  , rcSourceOfTruth :: Maybe FilePath   -- ^ index file naming modules (module-path only)
  } deriving (Eq, Show, Generic)

instance ToJSON ResolveConfig where
  toJSON rc = object $ concat
    [ [ "kind"  .= rcKind rc ]
    , [ "roots" .= rcRoots rc | not (null (rcRoots rc)) ]
    , maybe [] (\f -> [ "source_of_truth" .= f ]) (rcSourceOfTruth rc)
    ]

instance FromJSON ResolveConfig where
  parseJSON = withObject "ResolveConfig" $ \v -> ResolveConfig
    <$> v .:  "kind"
    <*> v .:? "roots" .!= []
    <*> v .:? "source_of_truth"

-- | Built-in resolve entries: none — declarative resolution is opt-in, so a
-- config without @resolve:@ behaves exactly as before.
defaultResolveConfigs :: Map String ResolveConfig
defaultResolveConfigs = Map.empty
