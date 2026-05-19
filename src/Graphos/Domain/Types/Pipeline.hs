-- | Pipeline configuration and detection types.
-- Pure data types with no IO dependencies.
module Graphos.Domain.Types.Pipeline
  ( -- * Configuration
    PipelineConfig(..)
  , EdgeDensity(..)
  , Neo4jPushMode(..)
  , Neo4jStreamingConfig(..)
  , defaultConfig

    -- * Checkpoint & resume
  , PipelineStep(..)
  , PipelineCheckpoint(..)

    -- * Detection types
  , Detection(..)
  , FileCategory(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject, withText)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Graphos.Domain.Config (GraphosConfig, defaultGraphosConfig)
import Graphos.Infrastructure.Observability (OtelConfig(..), defaultOtelConfig)

-- | Pipeline configuration
data PipelineConfig = PipelineConfig
  { cfgInputPath    :: FilePath
  , cfgOutputDir    :: FilePath
  , cfgDirected     :: Bool
  , cfgDeepMode    :: Bool
  , cfgNoViz        :: Bool
  , cfgUpdate       :: Bool
  , cfgClusterOnly  :: Bool
  , cfgNoCluster    :: Bool          -- ^ Skip clustering entirely (--no-cluster)
  , cfgLabel        :: Bool          -- ^ Use LLM to label communities (--label)
  , cfgObsidian     :: Bool
  , cfgObsidianDir  :: Maybe FilePath
  , cfgNeo4j        :: Bool
  , cfgNeo4jPush    :: Maybe Text  -- URI
  , cfgNeo4jPushMode :: Neo4jPushMode  -- ^ how much data to push to Neo4j (default: SubgraphPush for >10k nodes)
  , cfgNeo4jSubgraphSize :: Int   -- ^ representatives per community for subgraph mode (default: 7)
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
  , cfgMaxLeidenIterations :: Int   -- ^ max Leiden iterations before stopping (default: 50, lower for large graphs)
  , cfgThreads      :: Int          -- ^ number of parallel extraction threads (default: 1)
  , cfgCommunityGraph :: Bool      -- ^ export community-level graph JSON for LLM navigation
  , cfgGraphosConfig :: GraphosConfig  -- ^ LSP servers, language IDs, file extensions (config-driven)
  , cfgNeo4jStreaming :: Maybe Neo4jStreamingConfig  -- ^ Push nodes to Neo4j during extraction (streaming)
  , cfgMetricsPort   :: Maybe Int                   -- ^ Prometheus metrics server port (e.g. Just 9090)
  , cfgOtelEnabled   :: Bool                         -- ^ Enable OTLP export (--otel flag)
  , cfgOtelConfig     :: OtelConfig                   -- ^ OpenTelemetry configuration
  , cfgDebugTraceDir  :: Maybe FilePath               -- ^ Directory for debug trace JSONL files
  } deriving (Eq, Show)

-- | Edge density level for inference
-- Controls how aggressively the pipeline infers additional edges between nodes.
data EdgeDensity
  = Sparse    -- ^ No inferred edges, only extracted ones
  | Normal    -- ^ Community bridge edges + transitive deps (default)
  | Dense     -- ^ All inferred edges: bridges + transitive + shared context
  | Maximum   -- ^ Dense + lower thresholds for shared context
  deriving (Eq, Show, Read)

-- | Neo4j push mode — controls how much data is pushed to Neo4j.
--   FullPush pushes all nodes, edges, communities, and BELONGS_TO relationships.
--   SubgraphPush pushes communities + representative sub-graphs per community.
--   CommunityPush pushes communities + inter-community edges only (fastest).
data Neo4jPushMode
  = FullPush         -- ^ All nodes + edges + communities (current behavior)
  | SubgraphPush     -- ^ Communities + representative sub-graphs per community
  | CommunityPush    -- ^ Communities + inter-community edges only
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
  , cfgNoCluster    = False
  , cfgLabel        = False
  , cfgObsidian     = False
  , cfgObsidianDir  = Nothing
  , cfgNeo4j        = False
  , cfgNeo4jPush    = Nothing
  , cfgNeo4jPushMode = SubgraphPush
  , cfgNeo4jSubgraphSize = 7
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
  , cfgMaxLeidenIterations = 50
  , cfgThreads      = 1
  , cfgCommunityGraph = False
  , cfgGraphosConfig = defaultGraphosConfig
  , cfgNeo4jStreaming = Nothing
  , cfgMetricsPort   = Nothing
  , cfgOtelEnabled   = False
  , cfgOtelConfig     = defaultOtelConfig
  , cfgDebugTraceDir  = Nothing
  }

-- | Neo4j streaming push configuration — pushed node-by-node during extraction.
-- When provided, each file's extraction is pushed to Neo4j immediately
-- using MERGE (idempotent), giving real-time visibility in the graph database.
data Neo4jStreamingConfig = Neo4jStreamingConfig
  { neo4jsUri      :: Text  -- ^ Neo4j HTTP URI (e.g. "http://localhost:7474")
  , neo4jsUser     :: Text  -- ^ Username
  , neo4jsPassword :: Text  -- ^ Password
  } deriving (Eq, Show)

-- | File detection result
data Detection = Detection
  { detectionTotalFiles  :: Int
  , detectionTotalWords  :: Int
  , detectionNeedsGraph   :: Bool
  , detectionWarning      :: Maybe Text
  , detectionFiles        :: Map FileCategory [FilePath]
  } deriving (Eq, Show)

-- | Pipeline steps for checkpoint tracking
data PipelineStep
  = StepDetect
  | StepExtract
  | StepBuild
  | StepCluster
  | StepAnalyze
  | StepReport
  | StepExport
  deriving (Eq, Show, Read, Generic)

instance ToJSON PipelineStep where
  toJSON StepDetect  = "detect"
  toJSON StepExtract = "extract"
  toJSON StepBuild   = "build"
  toJSON StepCluster = "cluster"
  toJSON StepAnalyze = "analyze"
  toJSON StepReport  = "report"
  toJSON StepExport  = "export"

instance FromJSON PipelineStep where
  parseJSON = withText "PipelineStep" $ \t -> case t of
    "detect"  -> pure StepDetect
    "extract" -> pure StepExtract
    "build"   -> pure StepBuild
    "cluster" -> pure StepCluster
    "analyze" -> pure StepAnalyze
    "report"  -> pure StepReport
    "export"  -> pure StepExport
    _         -> fail $ "Unknown pipeline step: " ++ T.unpack t

-- | Pipeline checkpoint for resuming after failure.
-- Tracks which files have been extracted and pushed to Neo4j,
-- and which pipeline step we're on, so we can restart from that point.
data PipelineCheckpoint = PipelineCheckpoint
  { chkPipelineId    :: Text            -- ^ Unique ID for this pipeline run (timestamp-based)
  , chkCurrentStep   :: PipelineStep    -- ^ Which step we're on
  , chkCompletedSteps :: [PipelineStep]  -- ^ Steps that completed successfully
  , chkFilesExtracted :: [FilePath]      -- ^ Files already extracted
  , chkFilesPushedNeo4j :: [FilePath]   -- ^ Files whose nodes were pushed to Neo4j
  , chkStartedAt     :: Text            -- ^ ISO 8601 timestamp when pipeline started
  } deriving (Eq, Show, Generic)

instance ToJSON PipelineCheckpoint where
  toJSON chk = object
    [ "pipeline_id"        .= chkPipelineId chk
    , "current_step"       .= chkCurrentStep chk
    , "completed_steps"    .= chkCompletedSteps chk
    , "files_extracted"    .= chkFilesExtracted chk
    , "files_pushed_neo4j" .= chkFilesPushedNeo4j chk
    , "started_at"         .= chkStartedAt chk
    ]

instance FromJSON PipelineCheckpoint where
  parseJSON = withObject "PipelineCheckpoint" $ \v -> PipelineCheckpoint
    <$> v .: "pipeline_id"
    <*> v .: "current_step"
    <*> v .: "completed_steps"
    <*> v .: "files_extracted"
    <*> v .: "files_pushed_neo4j"
    <*> v .: "started_at"

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