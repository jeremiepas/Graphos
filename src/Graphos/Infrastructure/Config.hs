-- | Configuration loader - reads graphos.yaml and merges with defaults.
-- This is the only module that performs IO for config loading.
-- All config types live in Domain.Config (pure).
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Config
  ( -- * Loading
    loadConfig
  , loadConfigFrom
  , loadConfigWithGlobal
  , globalConfigPath

    -- * Resolution helpers
  , findLSPServerFromConfig
  , languageIdFromConfig

    -- * Re-export domain types
  , module Graphos.Domain.Config
  ) where

import Control.Exception (catch, SomeException(..))
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Yaml (FromJSON(..), withObject, (.:?))
import System.Directory (doesFileExist, getHomeDirectory, getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))
import qualified Data.Yaml as Yaml

import Graphos.Domain.Config

-- ───────────────────────────────────────────────
-- Configuration file format (YAML)
-- ───────────────────────────────────────────────

-- | Intermediate type for parsing the YAML file.
-- User provides only overrides; defaults are merged.
data ConfigFile = ConfigFile
  { cfLsp            :: Maybe (Map String LSPServerConfig)
  , cfLanguageIds     :: Maybe (Map String Text)
  , cfFileExtensions  :: Maybe FileExtensionConfig
  , cfExtractors      :: Maybe (Map String ExtractorConfig)
  , cfNeo4j           :: Maybe Neo4jConfig
  , cfLabeling        :: Maybe LabelingConfig
  , cfObservability   :: Maybe ObservabilityConfig
  } deriving (Eq, Show)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \v -> ConfigFile
    <$> v .:? "lsp"
    <*> v .:?  "language_ids"
    <*> v .:? "file_extensions"
    <*> v .:? "extractors"
    <*> v .:? "neo4j"
    <*> v .:? "labeling"
    <*> v .:? "observability"

-- ───────────────────────────────────────────────
-- Loading
-- ───────────────────────────────────────────────

-- | Path to the global user config file.
-- Falls back to ~/.config/graphos/graphos.yaml (XDG_CONFIG_HOME/graphos/graphos.yaml).
globalConfigPath :: IO FilePath
globalConfigPath = do
  xdgDir <- catch (getXdgDirectory XdgConfig "graphos") (\(_ :: SomeException) -> do
    home <- getHomeDirectory
    pure $ home </> ".config" </> "graphos")
  pure $ xdgDir </> "graphos.yaml"

-- | Load Graphos configuration with the standard layered merge:
--   1. Built-in defaults
--   2. Global user config (~/.config/graphos/graphos.yaml)
--   3. Project config (./graphos.yaml)
--
-- Project values override global; global values fill in defaults.
-- This is the main entry point used by the CLI.
loadConfig :: IO GraphosConfig
loadConfig = loadConfigWithGlobal "graphos.yaml"

-- | Load with a custom project config path (e.g. for testing).
loadConfigWithGlobal :: FilePath -> IO GraphosConfig
loadConfigWithGlobal projectPath = do
  globalPath <- globalConfigPath
  globalCfg <- loadConfigFrom globalPath
  projectCfg <- loadConfigFrom projectPath
  if globalCfg == defaultGraphosConfig && projectCfg == defaultGraphosConfig
    then pure defaultGraphosConfig
    else do
      let merged = mergeGraphosConfig globalCfg projectCfg
      when (globalCfg /= defaultGraphosConfig) $
        putStrLn $ "[config] Global: " ++ globalPath
      when (projectCfg /= defaultGraphosConfig) $
        putStrLn $ "[config] Project: " ++ projectPath
      pure merged

-- | Load Graphos configuration from a specific file path.
-- Falls back to defaults if the file doesn't exist or has parse errors.
loadConfigFrom :: FilePath -> IO GraphosConfig
loadConfigFrom path = do
  exists <- doesFileExist path
  if not exists
    then pure defaultGraphosConfig
    else do
      result <- catch
        (do content <- BS.readFile path
            case Yaml.decodeEither' content of
              Right (cfg :: ConfigFile) -> pure $ Right cfg
              Left err -> pure $ Left $ "YAML parse error: " ++ show err
        )
        $ \(e :: SomeException) -> pure $ Left $ "Config read error: " ++ show e
      case result of
        Left err -> do
          putStrLn $ "[config] " ++ path ++ ": " ++ err ++ " — using defaults"
          pure defaultGraphosConfig
        Right cfgFile ->
          pure $ mergeConfig cfgFile defaultGraphosConfig

-- | Merge user config overrides onto defaults.
-- User values take precedence; missing fields fall back to defaults.
mergeConfig :: ConfigFile -> GraphosConfig -> GraphosConfig
mergeConfig cfgFile defaults = GraphosConfig
  { gcLsp = case cfLsp cfgFile of
      Just userLsp -> Map.union userLsp (gcLsp defaults)
      Nothing      -> gcLsp defaults
  , gcLanguageIds = case cfLanguageIds cfgFile of
      Just userIds -> Map.union userIds (gcLanguageIds defaults)
      Nothing      -> gcLanguageIds defaults
  , gcFileExtensions = case cfFileExtensions cfgFile of
      Just userExts -> userExts  -- full override for file extensions
      Nothing       -> gcFileExtensions defaults
  , gcExtractors = case cfExtractors cfgFile of
      Just userExt -> Map.union userExt (gcExtractors defaults)
      Nothing      -> gcExtractors defaults
  , gcNeo4j = case cfNeo4j cfgFile of
      Just neo4j  -> neo4j
      Nothing     -> gcNeo4j defaults
  , gcLabeling = case cfLabeling cfgFile of
      Just labeling -> labeling
      Nothing      -> gcLabeling defaults
  , gcObservability = case cfObservability cfgFile of
      Just obs  -> obs
      Nothing   -> gcObservability defaults
  }

-- ───────────────────────────────────────────────
-- Resolution helpers (replace hardcoded lookups)
-- ───────────────────────────────────────────────

-- | Find an LSP server for a file extension from the config.
-- Returns the command and args if found, Nothing otherwise.
findLSPServerFromConfig :: GraphosConfig -> String -> IO (Maybe (String, [String]))
findLSPServerFromConfig config ext =
  case Map.lookup ext (gcLsp config) of
    Just server -> do
      let cmd = lspCommand server
      if null cmd
        then pure Nothing  -- empty command = explicitly disabled
        else pure $ Just (cmd, lspArgs server)
    Nothing -> pure Nothing

-- | Look up a language ID for a file extension from the config.
-- Falls back to "plaintext" for unknown extensions.
languageIdFromConfig :: GraphosConfig -> String -> Text
languageIdFromConfig config ext =
  Map.findWithDefault "plaintext" ext (gcLanguageIds config)