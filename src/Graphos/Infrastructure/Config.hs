-- | Configuration loader - reads graphos.yaml and merges with defaults.
-- This is the only module that performs IO for config loading.
-- All config types live in Domain.Config (pure).
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Config
  ( -- * Loading
    loadConfig
  , loadConfigSilent
  , loadConfigFrom
  , loadConfigWithGlobal
  , globalConfigPath

    -- * Resolution helpers
  , findLSPServerFromConfig
  , resolveLSPServer
  , effectiveLSPServerCommands
  , languageIdFromConfig

    -- * Init / writing
  , generateDefaultConfig
  , writeConfigYaml
  , configDocComments
  , fullInitTemplate

    -- * Re-export domain types
  , module Graphos.Domain.Config
  ) where

import Control.Exception (catch, SomeException(..))
import Control.Monad (when)
import Data.Aeson (Value(..))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml (FromJSON(..), withObject, (.:?))
import System.Directory (doesFileExist, findExecutable, getHomeDirectory, getXdgDirectory, XdgDirectory(..))
import System.Exit (exitWith, ExitCode(..))
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import qualified Data.Yaml as Yaml

import Graphos.Infrastructure.FileSystem.AtomicWrite (writeStringFileAtomic)
import qualified Graphos.Infrastructure.LSP.ServerMap as ServerMap

import Graphos.Domain.Config ( PdfExtractionMode(..)
                             , GraphosConfig(..)
                             , Neo4jConfig(..)
                             , MemgraphConfig(..)
                             , LabelingConfig(..)
                             , ObservabilityConfig(..)
                               , EmbeddingConfig(..)
                               , defaultEmbeddingConfig
                               , SemanticEdgesConfig(..)
                               , VisionConfig(..)
                               , FileExtensionConfig(..)
                               , ExtractorConfig(..)
                               , Granularity(..)
                               , LSPServerConfig(..)
                               , IngestConfig(..)
                               , MemoryConfig(..)
                               , SourceConfig(..)
                               , ResolveConfig(..)
                               , configSectionErrors
                               , CacheConfig(..)
                               , validateEmbeddingConfig
   , defaultGraphosConfig
   , mergeGraphosConfig
   )

import Graphos.Domain.Config.Detection (DetectionConfig(..))
import Graphos.Domain.Config.Clustering (ClusteringConfig(..))

-- ───────────────────────────────────────────────
-- Configuration file format (YAML)
-- ───────────────────────────────────────────────

-- | Intermediate type for parsing the YAML file.
-- User provides only overrides; defaults are merged.
data ConfigFile = ConfigFile
  { cfLsp              :: Maybe (Map String LSPServerConfig)
  , cfLanguageIds       :: Maybe (Map String Text)
  , cfFileExtensions    :: Maybe FileExtensionConfig
  , cfExtractors        :: Maybe (Map String ExtractorConfig)
  , cfGranularity       :: Maybe Granularity
  , cfPdfExtraction     :: Maybe PdfExtractionMode
  , cfNeo4j             :: Maybe Neo4jConfig
  , cfMemgraph          :: Maybe MemgraphConfig
  , cfLabeling          :: Maybe LabelingConfig
  , cfObservability     :: Maybe ObservabilityConfig
  , cfEmbedding         :: Maybe EmbeddingConfig
  , cfSemanticEdges     :: Maybe SemanticEdgesConfig
  , cfVision            :: Maybe VisionConfig
   , cfIngest            :: Maybe IngestConfig
   , cfDetection         :: Maybe DetectionConfig
    , cfClustering        :: Maybe ClusteringConfig
   , cfMemory            :: Maybe MemoryConfig
   , cfSources           :: Maybe [SourceConfig]
   , cfResolve           :: Maybe (Map String ResolveConfig)
   , cfOutput            :: Maybe FilePath
   , cfCache             :: Maybe CacheConfig
   } deriving (Eq, Show)

instance FromJSON ConfigFile where
  parseJSON = withObject "ConfigFile" $ \v -> ConfigFile
    <$> v .:? "lsp"
    <*> v .:?  "language_ids"
    <*> v .:? "file_extensions"
    <*> v .:? "extractors"
    <*> v .:? "granularity"
    <*> v .:? "pdf_extraction"
    <*> v .:? "neo4j"
    <*> v .:? "memgraph"
    <*> v .:? "labeling"
    <*> v .:? "observability"
    <*> v .:? "embedding"
     <*> v .:? "semantic_edges"
     <*> v .:? "vision"
     <*> v .:? "ingest"
     <*> v .:? "detection"
      <*> v .:? "clustering"
     <*> v .:? "memory"
     <*> v .:? "sources"
     <*> v .:? "resolve"
     <*> v .:? "output"
     <*> v .:? "cache"

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
loadConfig = loadConfigWithGlobal' True "graphos.yaml"

-- | Like 'loadConfig' but without the "[config] ..." provenance lines.
-- Used before the RTS re-exec, where the child process loads (and reports)
-- the same config again moments later.
loadConfigSilent :: IO GraphosConfig
loadConfigSilent = loadConfigWithGlobal' False "graphos.yaml"

-- | Load with a custom project config path (e.g. for testing).
loadConfigWithGlobal :: FilePath -> IO GraphosConfig
loadConfigWithGlobal = loadConfigWithGlobal' True

loadConfigWithGlobal' :: Bool -> FilePath -> IO GraphosConfig
loadConfigWithGlobal' verbose projectPath = do
  globalPath <- globalConfigPath
  globalCfg <- loadConfigFrom globalPath
  projectCfg <- loadConfigFrom projectPath
  if globalCfg == defaultGraphosConfig && projectCfg == defaultGraphosConfig
    then pure defaultGraphosConfig
    else do
      let merged = mergeGraphosConfig globalCfg projectCfg
      when (verbose && globalCfg /= defaultGraphosConfig) $
        putStrLn $ "[config] Global: " ++ globalPath
      when (verbose && projectCfg /= defaultGraphosConfig) $
        putStrLn $ "[config] Project: " ++ projectPath
      pure merged

-- | Load Graphos configuration from a specific file path.
-- Falls back to defaults if the file doesn't exist or is not valid YAML,
-- EXCEPT for semantically invalid values, which fail the load outright and
-- name the offending key/entry:
--
--   * @embedding.batchSize@/@embedding.concurrency@ below 1;
--   * an inconsistent @extractors:@ entry (e.g. @mode: command@ without a
--     @command@ argv, @mode: tree-sitter@ without a grammar, or a field that
--     belongs to another mode);
--   * an inconsistent or unknown @resolve:@ entry (e.g. @kind: frobnicate@).
--
-- Validation runs on the raw parsed document ('configSectionErrors') so a
-- mistyped extractor/resolve entry is never silently dropped in favour of the
-- compiled-in defaults.
loadConfigFrom :: FilePath -> IO GraphosConfig
loadConfigFrom path = do
  exists <- doesFileExist path
  if not exists
    then pure defaultGraphosConfig
    else do
      result <- catch
        (do content <- BS.readFile path
            case Yaml.decodeEither' content of
              Right (value :: Value) -> pure $ Right value
              Left err -> pure $ Left $ "YAML parse error: " ++ show err
        )
        $ \(e :: SomeException) -> pure $ Left $ "Config read error: " ++ show e
      case result of
        Left err -> do
          putStrLn $ "[config] " ++ path ++ ": " ++ err ++ " — using defaults"
          pure defaultGraphosConfig
        Right value ->
          case configSectionErrors value of
            [] ->
              case (parseEither parseJSON value :: Either String ConfigFile) of
                Left err -> do
                  putStrLn $ "[config] " ++ path ++ ": YAML parse error: "
                    ++ err ++ " — using defaults"
                  pure defaultGraphosConfig
                Right cfgFile -> applyConfigFile cfgFile
            errs -> do
              mapM_ (\msg -> hPutStrLn stderr $ "[config] " ++ path ++ ": " ++ msg) errs
              hPutStrLn stderr $ "[config] " ++ path
                ++ ": extractor/resolve entries rejected before build"
              exitWith (ExitFailure 1)
  where
    cfEmbeddingOrDefault cfgFile = maybe defaultEmbeddingConfig id (cfEmbedding cfgFile)

    applyConfigFile cfgFile =
      case validateEmbeddingConfig (cfEmbeddingOrDefault cfgFile) of
        Left (key, val) -> do
          hPutStrLn stderr $ "[config] " ++ path ++ ": invalid value for key \""
            ++ key ++ "\": " ++ show val ++ " (must be >= 1)"
          exitWith (ExitFailure 1)
        Right () ->
          pure $ mergeConfig (resolveConfigPaths (takeDirectory path) cfgFile)
                             defaultGraphosConfig

-- | Resolve runtime-grammar paths from a config file relative to that file's
-- directory (custom-extractors 4.1). Absolute paths and @~@-prefixed paths are
-- left untouched: the loader expands @~@ and the OS handles absolutes.
resolveConfigPaths :: FilePath -> ConfigFile -> ConfigFile
resolveConfigPaths base cfgFile = cfgFile
  { cfExtractors = fmap (Map.map resolveEntry) (cfExtractors cfgFile) }
  where
    resolveEntry ec = ec
      { ecGrammarPath = fmap (resolve p) (ecGrammarPath ec)
      , ecQueries = fmap (resolve p) (ecQueries ec)
      }
    p = if null base then "." else base
    resolve dir path
      | isAbsolute path = path
      | take 1 path == "~" = path
      | otherwise = dir </> path

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
  , gcGranularity = case cfGranularity cfgFile of
      Just gran -> gran
      Nothing   -> gcGranularity defaults
  , gcPdfExtraction = case cfPdfExtraction cfgFile of
      Just mode -> mode
      Nothing   -> gcPdfExtraction defaults
  , gcNeo4j = case cfNeo4j cfgFile of
      Just neo4j  -> neo4j
      Nothing     -> gcNeo4j defaults
  , gcMemgraph = case cfMemgraph cfgFile of
      Just memgraph -> memgraph
      Nothing       -> gcMemgraph defaults
  , gcLabeling = case cfLabeling cfgFile of
      Just labeling -> labeling
      Nothing      -> gcLabeling defaults
  , gcObservability = case cfObservability cfgFile of
      Just obs  -> obs
      Nothing   -> gcObservability defaults
  , gcEmbedding = case cfEmbedding cfgFile of
       Just emb   -> emb
       Nothing    -> gcEmbedding defaults
  , gcSemanticEdges = case cfSemanticEdges cfgFile of
       Just se    -> se
       Nothing    -> gcSemanticEdges defaults
  , gcVision = case cfVision cfgFile of
      Just vision -> vision
      Nothing     -> gcVision defaults
  , gcIngest = case cfIngest cfgFile of
      Just ingest -> ingest
      Nothing     -> gcIngest defaults
  , gcDetection = case cfDetection cfgFile of
      Just det -> det
      Nothing   -> gcDetection defaults
   , gcClustering = case cfClustering cfgFile of
       Just cl -> cl
       Nothing  -> gcClustering defaults
  , gcMemory = case cfMemory cfgFile of
      Just mem -> mem
      Nothing  -> gcMemory defaults
  , gcSources = case cfSources cfgFile of
      Just srcs -> srcs
      Nothing   -> gcSources defaults
  , gcResolve = case cfResolve cfgFile of
      Just res -> Map.union res (gcResolve defaults)
      Nothing  -> gcResolve defaults
  , gcOutput = case cfOutput cfgFile of
      Just out -> Just out
      Nothing  -> gcOutput defaults
  , gcCache = case cfCache cfgFile of
      Just c  -> c
      Nothing -> gcCache defaults
  }

-- ───────────────────────────────────────────────
-- Init: generate and write a default config file (multi-source-graphs 2.3)
-- ───────────────────────────────────────────────

-- | Build the default 'GraphosConfig' (the same defaults the loader produces
-- when no graphos.yaml exists). The @init@ workflow and tests use this as the
-- round-trip baseline.
generateDefaultConfig :: IO GraphosConfig
generateDefaultConfig = pure defaultGraphosConfig

-- | Serialize a config as YAML and write it atomically at the given path.
-- The template's YAML key names intentionally mirror the documented sections
-- in the generated @graphos.yaml@ (camelCase per ToJSON field names).
writeConfigYaml :: FilePath -> GraphosConfig -> IO ()
writeConfigYaml path cfg =
  writeStringFileAtomic path (renderedYaml cfg <> "\n")

-- | YAML rendering of a config (ByteString → strict UTF-8 Text, with
-- trailing whitespace stripped so the file ends in exactly one newline).
renderedYaml :: GraphosConfig -> String
renderedYaml cfg = T.unpack (T.strip (TE.decodeUtf8 (Yaml.encode cfg)))

-- | Commented documentation appended to the machine-rendered defaults by
-- @graphos init@ (multi-source-graphs 2.3 / workflow 15). Every line is a
-- comment so the generated file still parses exactly as the rendered config;
-- the comments document the optional sections — including the multi-source
-- @sources:@ example and the @output:@ key.
configDocComments :: String
configDocComments = unlines
  [ ""
  , "# ──── Multi-source (named roots) ─────────────"
  , "# Graph multiple filesystem roots into ONE graph. Each source gets a name;"
  , "# every node is tagged with its source and node ids stay distinct across"
  , "# sources even for identical relative paths. Without `sources:`, Graphos"
  , "# graphs the single positional PATH argument as before."
  , "# sources:"
  , "#   - name: repoA              # unique, non-empty"
  , "#     path: ~/code/repoA       # must exist (~ and ${VAR} are expanded)"
  , "#     ignore: [\"vendor\", \"dist\"]  # extra ignores scoped to this source"
  , "#   - name: repoB"
  , "#     path: ~/code/repoB"
  , ""
  , "# ──── Output directory ────────────────────────"
  , "# Where graph.json, the HTML view, cache, manifest, and conversations are"
  , "# written. Default: graphos-out/. The CLI flag -o/--output overrides this."
  , "# output: my-graph-out"
  , ""
  , "# ──── Custom extractors ───────────────────────"
  , "# Besides lsp / tree-sitter / stub, an extension can run an external command"
  , "# that prints graph.json-shaped nodes/edges on stdout. The command is an"
  , "# argv list (no shell); {file} is replaced with the file path, {files} with"
  , "# every path in batch mode. Failures are recorded per file and never abort"
  , "# the build. Required: a non-empty `command` for `mode: command`."
  , "# extractors:"
  , "#   \".pl\":"
  , "#     mode: command"
  , "#     command: [\"my-perl-extractor\", \"{file}\"]"
  , "#     batch: false        # one invocation for all files ({files})"
  , "#     timeout: 60         # seconds per invocation (default 60)"
  , ""
  , "# ──── Runtime tree-sitter grammars ────────────"
  , "# A tree-sitter entry may load a user-supplied grammar shared library"
  , "# (ABI 9-11) plus optional definitions/imports/calls .scm queries. Relative"
  , "# paths resolve against this config file's directory. Required: `grammar`"
  , "# or `grammar_path` for `mode: tree-sitter`."
  , "# extractors:"
  , "#   \".php\":"
  , "#     mode: tree-sitter"
  , "#     grammar_path: ~/.local/lib/tree-sitter-php.so"
  , "#     queries: ~/.config/graphos/queries/php/"
  , ""
  , "# ──── Language server sessions ───────────────"
  , "# Each `lsp:` entry (used by `mode: lsp`) also takes session options; the"
  , "# values above are the defaults. initialization_options is sent verbatim"
  , "# in `initialize`; call_hierarchy is auto (when the server advertises it),"
  , "# on (always, until the server answers MethodNotFound) or off;"
  , "# call_hierarchy_max_symbols 0 asks every symbol of a file for its callers;"
  , "# max_restarts bounds the restarts of a server that dies during a build."
  , "# lsp:"
  , "#   \".ts\":"
  , "#     command: typescript-language-server"
  , "#     args: [\"--stdio\"]"
  , "#     language_id: typescript"
  , "#     initialization_options: { tsserver: { maxTsServerMemory: 6144 } }"
  , "#     timeout_seconds: 300"
  , "#     call_hierarchy: \"on\""
  , "#     call_hierarchy_max_symbols: 0"
  , "#     max_restarts: 1"
  , ""
  , "# ──── Declarative import resolution ───────────"
  , "# Map an extension to a built-in resolver kind: package, psr4, module-path,"
  , "# or path-prefix. roots are relative to the project (input) root; every kind"
  , "# accepts them, package/psr4 require them, module-path requires"
  , "# source_of_truth. An unknown kind fails config loading."
  , "# resolve:"
  , "#   \".java\": { kind: package, roots: [\"src/main/java\"] }"
  , "#   \".php\":  { kind: psr4, roots: [\"src\"] }"
  , "#   \".kt\":   { kind: module-path, source_of_truth: settings.gradle }"
  ]

-- | The full documented init template: the rendered default config followed
-- by 'configDocComments'. Pure so tests can assert on the exact template
-- content; @graphos init@ writes exactly this via 'writeConfigYaml' +
-- an append of 'configDocComments'.
fullInitTemplate :: GraphosConfig -> String
fullInitTemplate cfg = renderedYaml cfg <> "\n" <> configDocComments

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

-- | Resolve an LSP server for @ext@ honoring the merged config.
--
-- If @ext@ is present in 'gcLsp':
--   * an empty 'lspCommand' means explicitly disabled -> 'Nothing';
--   * otherwise resolve the command via 'findExecutable' (when possible,
--     matching legacy 'ServerMap.findLSPServer') and return @('cmd', 'lspArgs')@.
-- Otherwise fall back to 'ServerMap.findLSPServer'.
resolveLSPServer :: GraphosConfig -> String -> IO (Maybe (String, [String]))
resolveLSPServer config ext =
  case Map.lookup ext (gcLsp config) of
    Just server -> do
      let cmd = lspCommand server
      if null cmd
        then pure Nothing
        else do
          found <- findExecutable cmd
          pure $ Just (maybe cmd id found, lspArgs server)
    Nothing -> ServerMap.findLSPServer ext

-- | Effective (extension -> (command, args)) map merging config (winning)
-- over the built-in 'ServerMap.languageServerCommands'.
effectiveLSPServerCommands :: GraphosConfig -> Map String (String, [String])
effectiveLSPServerCommands config =
  Map.union
    (Map.map (\s -> (lspCommand s, lspArgs s)) (gcLsp config))
    ServerMap.languageServerCommands