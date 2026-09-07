-- | Main pipeline orchestration (re-export hub).
--
-- Full pipeline: detect → extract → build → cluster → infer → analyze → report → export
-- With --no-cluster: detect → extract → build → report → export (skip clustering)
--
-- Implementation lives in focused sub-modules:
--   - UseCase.Pipeline.Core        — full pipeline (runPipeline)
--   - UseCase.Pipeline.Incremental — --watch mode + single-file ingestion
module Graphos.UseCase.Pipeline
  ( runPipeline
  , runClusterOnlyPipeline
  , runIncrementalPipeline
  , runSingleFilePipeline
  , PipelineResult(..)
  , SingleFileResult(..)
  ) where

import Graphos.UseCase.Pipeline.Core
  ( runPipeline
  , runClusterOnlyPipeline
  , PipelineResult(..)
  )
import Graphos.UseCase.Pipeline.Incremental
  ( runIncrementalPipeline
  , runSingleFilePipeline
  , SingleFileResult(..)
  )