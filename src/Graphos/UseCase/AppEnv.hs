-- | Application environment aggregating all port interfaces.
-- A single record that can be passed through the pipeline, with concrete
-- implementations provided by Infrastructure.Wiring.
module Graphos.UseCase.AppEnv
  ( -- * Application environment
    AppEnv(..)
  ) where

import Graphos.UseCase.Port.ExtractionPort (ExtractionPort)
import Graphos.UseCase.Port.ExportPort (ExportPort)
import Graphos.UseCase.Port.FileSystemPort (FileSystemPort)
import Graphos.UseCase.Port.LoggingPort (LoggingPort)
import Graphos.UseCase.Port.ObservabilityPort (ObservabilityPort)
import Graphos.UseCase.Port.LLMPort (LLMPort)

-- | Application environment — the single dependency injection point.
-- Infrastructure.Wiring provides the production implementation;
-- tests provide mock implementations by replacing individual fields.
data AppEnv = AppEnv
  { extractionPort      :: ExtractionPort
  , exportPort          :: ExportPort
  , fileSystemPort      :: FileSystemPort
  , loggingPort         :: LoggingPort
  , observabilityPort    :: ObservabilityPort
  , llmPort              :: LLMPort
  }