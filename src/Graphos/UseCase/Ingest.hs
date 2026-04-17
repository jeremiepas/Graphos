-- | URL Ingest - fetch URLs and save as annotated markdown for extraction
module Graphos.UseCase.Ingest
  ( ingest
  , IngestResult(..)
  , detectUrlType
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import Graphos.Infrastructure.Security (validateUrl)

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
-- Note: Full HTTP fetching requires http-conduit. This implementation
-- provides the scaffolding and YAML frontmatter generation.
-- For now, it creates a stub file that can be manually filled or
-- fetched by an external tool.
ingest :: Text -> FilePath -> Maybe Text -> Maybe Text -> IO (Either Text IngestResult)
ingest url rawDir author contributor =
  case validateUrl url of
    Left err -> pure (Left err)
    Right validUrl -> do
      let urlType = detectUrlType validUrl
          ext = typeToExt urlType
          filename = generateFilename validUrl ext
          filepath = rawDir </> filename
      createDirectoryIfMissing True rawDir

      now <- getCurrentTime
      let timestamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
          frontmatter = buildFrontmatter validUrl timestamp author contributor
          stubContent = case urlType of
            TwitterUrl -> frontmatter <> "\n[Tweet content - to be fetched]\n"
            ArxivUrl   -> frontmatter <> "\n[arXiv abstract - to be fetched]\n"
            PdfUrl     -> frontmatter <> "\n[PDF content - to be fetched]\n"
            ImageUrl   -> frontmatter <> "\n[Image description - to be fetched]\n"
            YoutubeUrl -> frontmatter <> "\n[Video transcript - to be fetched]\n"
            GenericWeb -> frontmatter <> "\n[Webpage content - to be fetched]\n"

      -- Check if file already exists
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
-- Helpers
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