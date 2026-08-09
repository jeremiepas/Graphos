{-# LANGUAGE ScopedTypeVariables #-}
-- | File and URL ingestion — single-file and URL ingestion for the Graphos pipeline.
--
-- Two ingestion modes:
--   1. URL ingest: fetch URLs and save as annotated markdown for extraction
--   2. File ingest: accept a single file path, auto-detect category, extract
--      entities, and optionally generate embeddings via local Ollama.
module Graphos.UseCase.Ingest
  ( -- * URL ingestion
    ingest
  , IngestResult(..)
  , detectUrlType

    -- * Single-file ingestion
  , ingestFile
  , FileIngestResult(..)

    -- * Category resolution helpers
  , resolveEmbedForCategory
  , resolveGranularityForCategory
  ) where

import Control.Exception (SomeException, catch)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, UTCTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeExtension, (</>))
import Network.HTTP.Client
  ( newManager, parseRequest, requestHeaders, responseBody, httpLbs, responseStatus
  , defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro, responseTimeoutNone
  )
import Network.HTTP.Types (statusCode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Crypto.Hash.SHA256 as SHA256

import Graphos.Domain.Types
  ( Node(..), Extraction(..), Granularity(..)
  , FileCategory(..), Detection(..)
   , IngestEmbedding(..), emptyIngestEmbedding, IngestIndex(..), emptyIngestIndex, addToIndex, isFileUpToDate
  , PipelineConfig(..), EmbeddingConfig(..)
  )
import Graphos.Domain.Config (GraphosConfig(..), gcFileExtensions, FileExtensionConfig(..), IngestConfig(..), IngestUrlConfig(..), IngestCategories(..), IngestCategoryConfig(..), FileEntry(..))
import Graphos.Infrastructure.Security (validateUrl)
import qualified Graphos.Infrastructure.LLM.Embedding as Emb
import qualified Graphos.UseCase.Extract as Extract
import Graphos.UseCase.AppEnv (AppEnv)
import Graphos.UseCase.IngestIndex (loadIndex)
import Graphos.Infrastructure.Logging (LogEnv, logInfo)
import qualified Data.Map.Strict as Map

-- ───────────────────────────────────────────────
-- URL Ingestion (existing)
-- ───────────────────────────────────────────────

-- | Result of ingesting a URL
data IngestResult = IngestResult
  { irPath     :: FilePath
  , irType     :: Text
  , irSummary  :: Text
  } deriving (Eq, Show)

-- | URL type auto-detection
data UrlType
  = TwitterUrl
  | ArxivUrl
  | PdfUrl
  | ImageUrl
  | YoutubeUrl
  | GenericWeb
  deriving (Eq, Show)

-- | Detect URL type from the URL string
detectUrlType :: Text -> UrlType
detectUrlType url
  | "twitter.com" `T.isInfixOf` url || "x.com" `T.isInfixOf` url = TwitterUrl
  | "arxiv.org" `T.isInfixOf` url = ArxivUrl
  | T.isSuffixOf ".pdf" url = PdfUrl
  | T.isSuffixOf ".png" url || T.isSuffixOf ".jpg" url || T.isSuffixOf ".webp" url = ImageUrl
  | "youtube.com" `T.isInfixOf` url || "youtu.be" `T.isInfixOf` url = YoutubeUrl
  | otherwise = GenericWeb

-- | Ingest a URL - fetch content and save as annotated markdown
-- For PDFs, downloads the actual content instead of creating a stub.
ingest :: IngestUrlConfig -> Text -> FilePath -> Maybe Text -> Maybe Text -> IO (Either Text IngestResult)
ingest urlCfg url rawDir author contributor =
  case validateUrl url of
    Left err -> pure (Left err)
    Right validUrl -> do
      let urlType = detectUrlType validUrl
          ext = typeToExt urlType
          filename = generateFilename validUrl ext
          filepath = rawDir </> filename
      createDirectoryIfMissing True rawDir

      now <- getCurrentTime
      let timestamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%m-%dT%H:%M:%SZ" now)
          frontmatter = buildFrontmatter validUrl timestamp author contributor

      -- For PDFs, download the actual content instead of a stub
      case urlType of
        PdfUrl -> do
          exists <- doesFileExist filepath
          if exists
            then pure (Right IngestResult
                  { irPath = filepath
                  , irType = typeToText urlType
                  , irSummary = "File already exists, skipped"
                  })
            else do
              result <- downloadFileWithConfig urlCfg validUrl filepath
              case result of
                Left err -> do
                  -- Retry once if configured
                  if iucRetry urlCfg > 0
                    then do
                      result2 <- downloadFileWithConfig urlCfg { iucRetry = 0 } validUrl filepath
                      case result2 of
                        Left err2 -> do
                          let stubContent = frontmatter <> "\n[PDF content - to be fetched]\n"
                          writeFile filepath (T.unpack stubContent)
                          pure (Right IngestResult
                            { irPath = filepath
                            , irType = typeToText urlType
                            , irSummary = "Download failed: " <> err2 <> " - saved stub"
                            })
                        Right _ -> pure (Right IngestResult
                          { irPath = filepath
                          , irType = typeToText urlType
                          , irSummary = "Downloaded PDF content (retry)"
                          })
                    else do
                      let stubContent = frontmatter <> "\n[PDF content - to be fetched]\n"
                      writeFile filepath (T.unpack stubContent)
                      pure (Right IngestResult
                        { irPath = filepath
                        , irType = typeToText urlType
                        , irSummary = "Download failed: " <> err <> " - saved stub"
                        })
                Right _ -> pure (Right IngestResult
                  { irPath = filepath
                  , irType = typeToText urlType
                  , irSummary = "Downloaded PDF content"
                  })
        _ -> do
          let stubContent = case urlType of
                TwitterUrl -> frontmatter <> "\n[Tweet content - to be fetched]\n"
                ArxivUrl   -> frontmatter <> "\n[arXiv abstract - to be fetched]\n"
                ImageUrl   -> frontmatter <> "\n[Image description - to be fetched]\n"
                YoutubeUrl -> frontmatter <> "\n[Video transcript - to be fetched]\n"
                GenericWeb -> frontmatter <> "\n[Webpage content - to be fetched]\n"
          exists <- doesFileExist filepath
          if exists
            then pure (Right IngestResult
                  { irPath = filepath
                  , irType = typeToText urlType
                  , irSummary = "File already exists, skipped"
                  })
            else do
              writeFile filepath (T.unpack stubContent)
              pure (Right IngestResult
                { irPath = filepath
                , irType = typeToText urlType
                , irSummary = "Saved stub file - populate with fetched content"
                })

-- ───────────────────────────────────────────────
-- Single-File Ingestion
-- ───────────────────────────────────────────────

-- | Result of single-file ingestion
data FileIngestResult = FileIngestResult
  { firPath       :: FilePath          -- ^ Path of the ingested file
  , firCategory   :: FileCategory      -- ^ Detected file category
  , firExtraction :: Extraction         -- ^ Extracted nodes and edges
  , firEmbeddings :: [IngestEmbedding]  -- ^ Generated embeddings (empty if disabled)
  , firIndex      :: IngestIndex        -- ^ Updated ingest index
  } deriving (Show)

-- | Resolve effective embed for a file category.
resolveEmbedForCategory :: Bool -> IngestCategories -> FileCategory -> Bool
resolveEmbedForCategory topLevel cats category =
  case categoryConfig of
    Just cfg -> maybe topLevel id (iccEmbed cfg)
    Nothing  -> topLevel
  where
    categoryConfig = case category of
      CodeFiles   -> icatCode cats
      DocFiles    -> icatDoc cats
      PaperFiles  -> icatPaper cats
      ImageFiles  -> icatImage cats
      VideoFiles  -> icatVideo cats
      OfficeFiles -> icatOffice cats

-- | Resolve effective granularity for a file category.
resolveGranularityForCategory :: Granularity -> IngestCategories -> FileCategory -> Granularity
resolveGranularityForCategory topLevel cats category =
  case categoryConfig of
    Just cfg -> maybe topLevel id (iccGranularity cfg)
    Nothing  -> topLevel
  where
    categoryConfig = case category of
      CodeFiles   -> icatCode cats
      DocFiles    -> icatDoc cats
      PaperFiles  -> icatPaper cats
      ImageFiles  -> icatImage cats
      VideoFiles  -> icatVideo cats
      OfficeFiles -> icatOffice cats

-- | Compute a SHA256 hex hash of a file's contents.
sha256File :: FilePath -> IO Text
sha256File path = do
  contents <- BS.readFile path
  pure $ T.pack $ BS8.unpack $ BS16.encode $ SHA256.hash contents

-- | Add or update a file entry in the index.
idxWithFileEntry :: FilePath -> FileEntry -> IngestIndex -> IngestIndex
idxWithFileEntry path entry idx = idx { iiFiles = Map.insert path entry (iiFiles idx) }

-- | Ingest a single file: detect category, extract entities, optionally generate embeddings.
--
-- This bypasses the full pipeline's directory scan — it processes exactly one file.
-- The extraction reuses the same extractors (LSP, tree-sitter, stub) configured
-- in graphos.yaml.
--
-- Uses the ingest configuration for embed/granularity defaults, deduplication,
-- and category-level overrides.
ingestFile :: AppEnv -> PipelineConfig -> FilePath -> LogEnv -> IO (Either Text FileIngestResult)
ingestFile appEnv config filePath env = do
  -- Verify file exists
  exists <- doesFileExist filePath
  if not exists
    then pure $ Left $ T.pack $ "File not found: " ++ filePath
    else do
      let graphosCfg = cfgGraphosConfig config
          ingestCfg  = cfgIngest config
          ext        = takeExtension filePath
          fec        = gcFileExtensions graphosCfg
          category   = detectFileCategory ext fec
          effectiveEmbed = resolveEmbedForCategory (icEmbed ingestCfg) (icCategories ingestCfg) category

      logInfo env $ T.pack $ "  Ingesting file: " ++ filePath ++ " (category: " ++ show category ++ ")"

      -- Deduplication check: skip if file unchanged
      existingIdx <- loadIndex (icIndexPath ingestCfg)
      fileHash <- sha256File filePath
      let skipDedup = icDeduplicate ingestCfg && isFileUpToDate filePath fileHash existingIdx
      if skipDedup
        then do
          logInfo env $ T.pack $ "  Skipping unchanged file (hash match): " ++ filePath
          pure $ Right FileIngestResult
            { firPath = filePath
            , firCategory = category
            , firExtraction = undefined
            , firEmbeddings = []
            , firIndex = existingIdx
            }
        else do
          -- Build a mini-detection for the single file
          let detection = Detection
                { detectionTotalFiles = 1
                , detectionTotalWords  = 0
                , detectionNeedsGraph = True
                , detectionWarning     = Nothing
                , detectionFiles       = Map.singleton category [filePath]
                }

          -- Extract entities from the single file
          extraction <- Extract.extractAll appEnv config detection

          let nodes = Map.elems (extractionNodes extraction)
              nodeCount = length nodes
              edgeCount = Map.size (extractionEdges extraction)

          logInfo env $ T.pack $ "  Extracted " ++ show nodeCount ++ " nodes, " ++ show edgeCount ++ " edges"

          -- Generate embeddings if enabled
          let embCfg = gcEmbedding graphosCfg
              embedEnabled = effectiveEmbed || embEnabled embCfg

          (embeddings, idx) <- if embedEnabled
            then do
              logInfo env $ T.pack $ "  Generating embeddings via " ++ embModel embCfg ++ "..."
              embs <- generateEmbeddingsForNodes embCfg nodes env
              now <- getCurrentTime
              entryHash <- sha256File filePath
              let entry = FileEntry { feHash = entryHash, feIngestedAt = T.pack $ show now }
                  idx' = idxWithFileEntry filePath entry $ foldr addToIndex emptyIngestIndex embs
              logInfo env $ T.pack $ "  Generated " ++ show (length embs) ++ " embeddings"
              pure (embs, idx')
            else do
              -- Store metadata-only entries (no vector) for index lookups
              now <- getCurrentTime
              entryHash <- sha256File filePath
              let entry = FileEntry { feHash = entryHash, feIngestedAt = T.pack $ show now }
                  metaEmbs = [emptyIngestEmbedding (nodeId n) (nodeSourceFile n) now | n <- nodes]
                  idx' = idxWithFileEntry filePath entry $ foldr addToIndex emptyIngestIndex metaEmbs
              pure (metaEmbs, idx')

          pure $ Right FileIngestResult
            { firPath = filePath
            , firCategory = category
            , firExtraction = extraction
            , firEmbeddings = embeddings
            , firIndex = idx
            }

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Detect file category from extension using config-driven FileExtensionConfig.
-- Falls back to DocFiles for unknown extensions.
detectFileCategory :: String -> FileExtensionConfig -> FileCategory
detectFileCategory ext fec
  | ext `elem` fecCode fec   = CodeFiles
  | ext `elem` fecDoc fec    = DocFiles
  | ext `elem` fecPaper fec  = PaperFiles
  | ext `elem` fecImage fec  = ImageFiles
  | ext `elem` fecVideo fec  = VideoFiles
  | ext `elem` fecOffice fec = OfficeFiles
  | otherwise                = DocFiles

-- | Generate embeddings for a list of extracted nodes.
-- Creates a text representation of each node and calls the embedding API.
generateEmbeddingsForNodes :: EmbeddingConfig -> [Node] -> LogEnv -> IO [IngestEmbedding]
generateEmbeddingsForNodes cfg nodes env = do
  now <- getCurrentTime
  let model = T.pack (embModel cfg)
  -- Process nodes sequentially to avoid overwhelming local Ollama
  mapM (embedNode cfg model now env) nodes

-- | Generate embedding for a single node.
embedNode :: EmbeddingConfig -> Text -> UTCTime -> LogEnv -> Node -> IO IngestEmbedding
embedNode cfg model ts _env node = do
  let inputText = nodeLabel node <> " " <> nodeSourceFile node
  result <- Emb.generateEmbedding cfg inputText
  case result of
    Left _err ->
      -- On failure, store a metadata-only entry (no vector)
      pure IngestEmbedding
        { ieNodeId     = nodeId node
        , ieVector     = []
        , ieSourceHash = nodeSourceFile node
        , ieTimestamp  = ts
        , ieModel      = model
        }
    Right vec ->
      pure IngestEmbedding
        { ieNodeId     = nodeId node
        , ieVector     = vec
        , ieSourceHash = nodeSourceFile node
        , ieTimestamp  = ts
        , ieModel      = model
        }

-- ───────────────────────────────────────────────
-- URL helpers (existing)
-- ───────────────────────────────────────────────

buildFrontmatter :: Text -> Text -> Maybe Text -> Maybe Text -> Text
buildFrontmatter url timestamp author contributor =
  T.unlines
    [ "---"
    , "source_url: " <> quoteWrap url
    , "captured_at: " <> quoteWrap timestamp
    , "author: " <> maybe "null" quoteWrap author
    , "contributor: " <> maybe "null" quoteWrap contributor
    , "---"
    ]
  where
    quoteWrap t = "\"" <> t <> "\""

generateFilename :: Text -> String -> String
generateFilename url ext =
  let -- Simple: use last path segment or domain
      cleaned = T.unpack $ T.map safeChar $ T.reverse $ T.take 40 $ T.reverse url
  in cleaned ++ ext
  where
    safeChar c
      | c `elem` ("/\\:*?" :: String) = '_'
      | c == '.' = '_'
      | otherwise = c

typeToExt :: UrlType -> String
typeToExt TwitterUrl = ".md"
typeToExt ArxivUrl   = ".md"
typeToExt PdfUrl     = ".pdf"
typeToExt ImageUrl   = ".png"
typeToExt YoutubeUrl = ".md"
typeToExt GenericWeb = ".md"

typeToText :: UrlType -> Text
typeToText TwitterUrl = "twitter"
typeToText ArxivUrl   = "arxiv"
typeToText PdfUrl     = "pdf"
typeToText ImageUrl   = "image"
typeToText YoutubeUrl = "youtube"
typeToText GenericWeb = "webpage"

-- ───────────────────────────────────────────────
-- URL download helper
-- ───────────────────────────────────────────────

-- | Download a file from a URL and save it to a local path.
-- Returns Left with error message on failure, Right () on success.
downloadFileWithConfig :: IngestUrlConfig -> Text -> FilePath -> IO (Either Text ())
downloadFileWithConfig urlCfg url destPath = do
  result <- catch
    (do let settings = defaultManagerSettings
              { managerResponseTimeout =
                  if iucTimeout urlCfg <= 0
                    then responseTimeoutNone
                    else responseTimeoutMicro (iucTimeout urlCfg * 1000000)
              }
        manager <- newManager settings
        request0 <- parseRequest (T.unpack url)
        let request = request0
              { requestHeaders =
                  [("User-Agent", BS8.pack (iucUserAgent urlCfg))]
                    ++ requestHeaders request0
              }
        response <- httpLbs request manager
        let status = statusCode (responseStatus response)
        if status >= 200 && status < 300
          then do
            LBS.writeFile destPath (responseBody response)
            pure (Right ())
          else pure (Left $ "HTTP " <> T.pack (show status) <> " downloading " <> url)
    )
    (\(e :: SomeException) -> pure (Left $ "Download failed: " <> T.pack (show e)))
  pure result