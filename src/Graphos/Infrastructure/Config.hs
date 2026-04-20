-- | Configuration loader - reads graphos.yaml and merges with defaults.
-- This is the only module that performs IO for config loading.
-- All config types live in Domain.Config (pure).
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Config
  ( -- * Loading
    loadConfig
  , loadConfigFrom

    -- * Resolution helpers
  , findLSPServerFromConfig
  , languageIdFromConfig

    -- * Re-export domain types
  , module Graphos.Domain.Config
  ) where

import Control.Exception (catch, SomeException(..))
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Yaml (FromJSON(..), withObject, (.:?))
import System.Directory (doesFileExist)
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
  } deriving (Eq, Show)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \v -> ConfigFile
    <$> v .:? "lsp"
    <*> v .:?  "language_ids"
    <*> v .:? "file_extensions"

-- ───────────────────────────────────────────────
-- Loading
-- ───────────────────────────────────────────────

-- | Load Graphos configuration from the default path (./graphos.yaml).
-- Falls back to defaults if the file doesn't exist or has parse errors.
loadConfig :: IO GraphosConfig
loadConfig = loadConfigFrom "graphos.yaml"

-- | Load Graphos configuration from a specific file path.
-- Falls back to defaults if the file doesn't exist or has parse errors.
loadConfigFrom :: FilePath -> IO GraphosConfig
loadConfigFrom path = do
  exists <- doesFileExist path
  if not exists
    then do
      putStrLn $ "[config] No config file at " ++ path ++ " — using defaults"
      pure defaultGraphosConfig
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
          putStrLn $ "[config] " ++ err ++ " — using defaults"
          pure defaultGraphosConfig
        Right cfgFile -> do
          putStrLn $ "[config] Loaded " ++ path
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