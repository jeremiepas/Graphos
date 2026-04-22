-- | Pipeline configuration and detection types.
-- Pure data types with no IO dependencies.
module Graphos.Domain.Types.Pipeline
  ( -- * Configuration
    PipelineConfig(..)
  , EdgeDensity(..)
  , defaultConfig

    -- * Detection types
  , Detection(..)
  , FileCategory(..)
  ) where

import Data.Aeson (ToJSON(..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Graphos.Domain.Config (GraphosConfig, defaultGraphosConfig)

-- | Pipeline configuration
data PipelineConfig = PipelineConfig
  { cfgInputPath    :: FilePath
  , cfgOutputDir    :: FilePath
  , cfgDirected     :: Bool
  , cfgDeepMode    :: Bool
  , cfgNoViz        :: Bool
  , cfgUpdate       :: Bool
  , cfgClusterOnly  :: Bool
  , cfgObsidian     :: Bool
  , cfgObsidianDir  :: Maybe FilePath
  , cfgNeo4j        :: Bool
  , cfgNeo4jPush    :: Maybe Text  -- URI
  , cfgMCP          :: Maybe FilePath
  , cfgSVG          :: Bool
  , cfgGraphML      :: Bool
  , cfgWatch        :: Bool
  , cfgWiki         :: Bool
  , cfgVerbose      :: Bool  -- ^ --verbose: show DEBUG level logs
  , cfgDebug        :: Bool  -- ^ --debug: show TRACE level logs + internal details
  , cfgEdgeDensity  :: EdgeDensity  -- ^ how many inferred edges to add
  , cfgResolution   :: Double       -- ^ community resolution: higher = fewer larger communities (default: 1.0)
  , cfgMinCommSize  :: Int          -- ^ minimum community size; smaller ones get merged (default: 3)
  , cfgThreads      :: Int          -- ^ number of parallel extraction threads (default: 1)
  , cfgCommunityGraph :: Bool      -- ^ export community-level graph JSON for LLM navigation
  , cfgGraphosConfig :: GraphosConfig  -- ^ LSP servers, language IDs, file extensions (config-driven)
  } deriving (Eq, Show)

-- | Edge density level for inference
-- Controls how aggressively the pipeline infers additional edges between nodes.
data EdgeDensity
  = Sparse    -- ^ No inferred edges, only extracted ones
  | Normal    -- ^ Community bridge edges + transitive deps (default)
  | Dense     -- ^ All inferred edges: bridges + transitive + shared context
  | Maximum   -- ^ Dense + lower thresholds for shared context
  deriving (Eq, Show, Read)

-- | Default pipeline configuration
defaultConfig :: PipelineConfig
defaultConfig = PipelineConfig
  { cfgInputPath    = "."
  , cfgOutputDir    = "graphos-out"
  , cfgDirected     = False
  , cfgDeepMode    = False
  , cfgNoViz        = False
  , cfgUpdate       = False
  , cfgClusterOnly  = False
  , cfgObsidian     = False
  , cfgObsidianDir  = Nothing
  , cfgNeo4j        = False
  , cfgNeo4jPush    = Nothing
  , cfgMCP          = Nothing
  , cfgSVG          = False
  , cfgGraphML      = False
  , cfgWatch        = False
  , cfgWiki         = False
  , cfgVerbose      = False
  , cfgDebug        = False
  , cfgEdgeDensity  = Normal
  , cfgResolution   = 1.0
  , cfgMinCommSize  = 3
  , cfgThreads      = 1
  , cfgCommunityGraph = False
  , cfgGraphosConfig = defaultGraphosConfig
  }

-- | File detection result
data Detection = Detection
  { detectionTotalFiles  :: Int
  , detectionTotalWords  :: Int
  , detectionNeedsGraph   :: Bool
  , detectionWarning      :: Maybe Text
  , detectionFiles        :: Map FileCategory [FilePath]
  } deriving (Eq, Show)

-- | File categories
data FileCategory
  = CodeFiles
  | DocFiles
  | PaperFiles
  | ImageFiles
  | VideoFiles
  deriving (Eq, Show, Ord, Generic)

instance ToJSON FileCategory where
  toJSON CodeFiles  = "code"
  toJSON DocFiles   = "document"
  toJSON PaperFiles = "paper"
  toJSON ImageFiles = "image"
  toJSON VideoFiles = "video"