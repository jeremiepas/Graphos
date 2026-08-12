<!--
  One PDCA step file for ONE task. Lives at tasks/<task-slug>/<step>.md
  where <step> is plan | do | check | act.
  tasks.md remains the tracked index; these files are the per-task journal.
  Retry: if Act is NOT OK, keep this file as the trace and add a new cycle
  under tasks/<task-slug>/attempt-N/ — never delete a prior attempt.
-->

# Task 4 — Create UseCase.Port.FileSystemPort, LoggingPort, ObservabilityPort, LLMPort — DO

**Task slug**: `04-usecase-remaining-ports`
**Attempt**: 1
**Status**: in-progress

## Summary

Created all 4 remaining port modules with their record types. Each port defines fields matching the corresponding Infrastructure function signatures, using only Domain and standard library types.

## Detail

### What was implemented

#### FileSystemPort (`src/Graphos/UseCase/Port/FileSystemPort.hs`, 22 lines)

**Record type `FileSystemPort`** with 4 fields:
- `fspLoadCheckpoint :: FilePath -> IO (Maybe PipelineCheckpoint)` — load pipeline checkpoint
- `fspSaveCheckpoint :: FilePath -> PipelineCheckpoint -> IO ()` — save checkpoint
- `fspClearCheckpoint :: FilePath -> IO ()` — clear checkpoint
- `fspLoadIgnorePatterns :: FilePath -> IO [AnnotatedPattern]` — load .gitignore patterns

**Known issue**: `FileSystemPort` currently imports `AnnotatedPattern` from `Infrastructure.FileSystem.Ignore`. This violates the port principle (ports should not import Infrastructure). This will be fixed in Task 8 when `AnnotatedPattern` is either moved to Domain or duplicated in the port.

#### LoggingPort (`src/Graphos/UseCase/Port/LoggingPort.hs`, 23 lines)

**Record type `LoggingPort`** with 5 fields:
- `lpLogTrace :: Text -> IO ()`
- `lpLogDebug :: Text -> IO ()`
- `lpLogInfo :: Text -> IO ()`
- `lpLogWarn :: Text -> IO ()`
- `lpLogError :: Text -> IO ()`

**Supporting type**: `LogLevel` enum (`LogTrace | LogDebug | LogInfo | LogWarn | LogError`) defined in the port module to avoid importing `Infrastructure.Logging (LogLevel(..))`.

#### ObservabilityPort (`src/Graphos/UseCase/Port/ObservabilityPort.hs`, 28 lines)

**Record type `ObservabilityPort`** with 5 fields:
- `opInitObservability :: OtelConfig -> Maybe Int -> FilePath -> IO ()` — init observability
- `opShutdownObservability :: IO ()` — shutdown/flush
- `opIncCounter :: Text -> Int64 -> IO ()` — increment metric counter
- `opSetGauge :: Text -> Double -> IO ()` — set metric gauge
- `opTraceEvent :: Text -> [(Text, Text)] -> IO ()` — write debug trace event

**Known issue**: `ObservabilityPort` imports `OtelConfig` from `Domain.Config`. This is acceptable (Domain import in UseCase), but `opInitObservability` is a no-op in the production wiring since `Main.hs` initializes observability before creating the port.

#### LLMPort (`src/Graphos/UseCase/Port/LLMPort.hs`, 47 lines)

**Record type `LLMPort`** with 5 fields:
- `lpCallLLM :: LabelingConfig -> Text -> IO (Either Text Text)` — call LLM for labeling
- `lpParseLabelsFromResponse :: Text -> Map CommunityId Text` — parse response (pure function)
- `lpGenerateEmbedding :: EmbeddingConfig -> Text -> IO (Either Text [Double])` — generate embeddings
- `lpAnalyzeImage :: VisionConfig -> LabelingConfig -> FilePath -> IO (Either Text ImageAnalysis)` — analyze image
- `lpValidateUrl :: Text -> Either Text Text` — validate URL (pure function)

**Supporting types** (defined in the port module to avoid Infrastructure imports):
- `ImageAnalysis` — mirrors `Infrastructure.LLM.Vision.ImageAnalysis` using port-local types
- `ImageKind` — enum mirroring `Vision.ImageKind` (Photo, Screenshot, Diagram, Resume, Chart, OtherKind)
- `Entity` — mirrors `Vision.Entity` with `entityLabel` and `entityType` fields

### Key decisions

1. **`AnnotatedPattern` still imported from Infrastructure**: This is a known violation that will be fixed in Task 8. Options: (a) move `AnnotatedPattern` to Domain, or (b) define a port-local type. Decision deferred to Task 8 since `UseCase.Detect` also imports it.

2. **`LogLevel` duplicated in port**: Rather than importing `Infrastructure.Logging (LogLevel(..))`, the port defines its own `LogLevel` enum. `Infrastructure.Wiring.productionLoggingPort` maps between them.

3. **Pure functions in ports**: `lpParseLabelsFromResponse` and `lpValidateUrl` are pure functions (no IO). This is correct — they're included in the port for decoupling, not because they need IO. They could be called without AppEnv but are included for consistency.

4. **`ImageAnalysis/ImageKind/Entity` in LLMPort**: These types are defined locally in the port module to avoid UseCase importing `Infrastructure.LLM.Vision`. Wiring converts between Infrastructure and Port types via `convertImageKind` and `convertEntity`.

### Concrete changes

- Created `src/Graphos/UseCase/Port/FileSystemPort.hs` (22 lines)
- Created `src/Graphos/UseCase/Port/LoggingPort.hs` (23 lines)
- Created `src/Graphos/UseCase/Port/ObservabilityPort.hs` (28 lines)
- Created `src/Graphos/UseCase/Port/LLMPort.hs` (47 lines)
- All modules compile successfully with `cabal build`

## Result

Pending — awaiting Check (Task 4.C).