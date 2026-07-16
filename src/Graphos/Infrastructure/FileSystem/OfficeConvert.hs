-- | Office file conversion - .docx, .pptx, .xlsx to markdown
-- Uses zip-archive + xml-conduit (no external deps like pandoc)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.FileSystem.OfficeConvert
  ( docxToMarkdown
  , pptxToMarkdown
  , xlsxToMarkdown
  , docToMarkdown
  , pptToMarkdown
  , docxExtractMediaPaths
  , pptxExtractMediaPaths
  , extractMediaFile
  ) where

import Codec.Archive.Zip
  ( Archive(..), Entry, toArchiveOrFail, eRelativePath, fromEntry )
import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.XML as X
import qualified Text.XML.Cursor as C
import System.Directory (doesFileExist)

-- | OOXML namespace URIs
wNs :: Text
wNs = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

aNs :: Text
aNs = "http://schemas.openxmlformats.org/drawingml/2006/main"

sNs :: Text
sNs = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"

-- | Construct a namespaced XML Name
nsName :: Text -> Text -> X.Name
nsName local ns = X.Name
  { X.nameLocalName = local
  , X.nameNamespace = Just ns
  , X.namePrefix = Nothing
  }

-- | w: namespace element names
wP, wT, wPStyle :: X.Name
wP = nsName "p" wNs
wT = nsName "t" wNs
wPStyle = nsName "pStyle" wNs

-- | a: namespace element names
aT :: X.Name
aT = nsName "t" aNs

-- | s: namespace element names
sRow, sC, sV :: X.Name
sRow = nsName "row" sNs
sC = nsName "c" sNs
sV = nsName "v" sNs

-- | w:val attribute name
wVal :: X.Name
wVal = nsName "val" wNs

-- ───────────────────────────────────────────────
-- DOCX extraction
-- ───────────────────────────────────────────────

-- | Convert .docx file content to markdown.
-- Parses word/document.xml, extracts paragraphs with heading styles.
docxToMarkdown :: FilePath -> IO (Either Text Text)
docxToMarkdown path = do
  result <- parseZipFile path
  case result of
    Left err -> pure (Left err)
    Right archive -> case findEntry "word/document.xml" archive of
      Nothing -> pure (Left $ "Missing word/document.xml in: " <> T.pack path)
      Just entry -> case parseXML (fromEntry entry) of
        Left err -> pure (Left $ "Failed to parse document.xml: " <> T.pack err)
        Right doc -> pure (Right $ extractDocxMarkdown doc)

-- | Extract markdown from a parsed DOCX document cursor.
extractDocxMarkdown :: C.Cursor -> Text
extractDocxMarkdown cursor = T.unlines $ concatMap extractParagraph paragraphs
  where
    paragraphs = cursor C.$// C.element wP

    extractParagraph para =
      let styleVal = getStyleVal para
          textContent = T.concat $ concatMap C.content (para C.$// C.element wT)
          trimmed = T.strip textContent
      in if T.null trimmed
         then []
         else [prefixForStyle styleVal trimmed]

    getStyleVal para =
      let pStyleCursors = para C.$/ C.element wPStyle
          allVals = concatMap (C.attribute wVal) pStyleCursors
      in case allVals of
           (v:_) -> v
           [] -> ""

    prefixForStyle style txt
      | "Title"     `T.isInfixOf` style = "# " <> txt
      | "Heading1"  `T.isInfixOf` style = "## " <> txt
      | "Heading2"  `T.isInfixOf` style = "### " <> txt
      | "Heading3"  `T.isInfixOf` style = "#### " <> txt
      | "Heading4"  `T.isInfixOf` style = "##### " <> txt
      | "Heading5"  `T.isInfixOf` style = "###### " <> txt
      | otherwise                       = txt

-- | Extract media file paths from a DOCX archive.
docxExtractMediaPaths :: FilePath -> IO [FilePath]
docxExtractMediaPaths path = do
  result <- parseZipFile path
  case result of
    Left _ -> pure []
    Right archive -> pure $ filter isMediaPath (entryPaths archive)
  where
    isMediaPath p = "word/media/" `isPrefixOf` p &&
                    length p > length ("word/media/" :: String)

-- ───────────────────────────────────────────────
-- PPTX extraction
-- ───────────────────────────────────────────────

-- | Convert .pptx file content to markdown.
-- Extracts text from each slide, producing ## Slide N headers.
pptxToMarkdown :: FilePath -> IO (Either Text Text)
pptxToMarkdown path = do
  result <- parseZipFile path
  case result of
    Left err -> pure (Left err)
    Right archive -> do
      let slidePaths = sort $ filter isSlide (entryPaths archive)
          slideTexts = map (extractSlideText archive) slidePaths
          markdown = T.unlines $ concatMap formatSlide (zip [1..] slideTexts)
      pure (Right markdown)
  where
    isSlide p = "ppt/slides/slide" `isPrefixOf` p && ".xml" `isSuffixOf` p
                && not ("_rels" `isInfixOf` p)

formatSlide :: (Int, Text) -> [Text]
formatSlide (n, text)
  | T.null (T.strip text) = []
  | otherwise = ["## Slide " <> T.pack (show n), "", T.strip text, ""]

extractSlideText :: Archive -> FilePath -> Text
extractSlideText archive slidePath = case findEntry slidePath archive of
  Nothing -> ""
  Just entry ->
    case parseXML (fromEntry entry) of
      Left _ -> ""
      Right doc -> T.concat $ concatMap C.content (doc C.$// C.element aT)

-- | Extract media file paths from a PPTX archive.
pptxExtractMediaPaths :: FilePath -> IO [FilePath]
pptxExtractMediaPaths path = do
  result <- parseZipFile path
  case result of
    Left _ -> pure []
    Right archive -> pure $ filter isMediaPath (entryPaths archive)
  where
    isMediaPath p = "ppt/media/" `isPrefixOf` p &&
                    length p > length ("ppt/media/" :: String)

-- ───────────────────────────────────────────────
-- XLSX extraction
-- ───────────────────────────────────────────────

-- | Convert .xlsx file content to markdown.
-- Extracts cell data from each worksheet, producing markdown tables.
xlsxToMarkdown :: FilePath -> IO (Either Text Text)
xlsxToMarkdown path = do
  result <- parseZipFile path
  case result of
    Left err -> pure (Left err)
    Right archive -> do
      let sheetPaths = sort $ filter isSheet (entryPaths archive)
      if null sheetPaths
        then pure (Right $ "# Spreadsheet: " <> T.pack path <> "\n\n[No worksheets found]")
        else do
          let sheetsMarkdown = map (extractSheetText archive) sheetPaths
              markdown = T.unlines $ concatMap formatSheet (zip [1..] sheetsMarkdown)
          pure (Right markdown)
  where
    isSheet p = "xl/worksheets/" `isPrefixOf` p && ".xml" `isSuffixOf` p
                && not ("_rels" `isInfixOf` p)

formatSheet :: (Int, [[Text]]) -> [Text]
formatSheet (n, rows)
  | null rows || all (all T.null) rows = ["## Sheet " <> T.pack (show n)]
  | otherwise = ["## Sheet " <> T.pack (show n), "", formatTable rows]

formatTable :: [[Text]] -> Text
formatTable [] = ""
formatTable rows = case paddedRows of
    []     -> ""
    (h:rs) -> T.unlines $ formatRow h : sep : map formatRow rs
  where
    maxCols = maximum (map length rows)
    paddedRows = map (\r -> r ++ replicate (maxCols - length r) "") rows
    sep = "|" <> T.intercalate "|" (replicate maxCols "---") <> "|"

formatRow :: [Text] -> Text
formatRow cells = "|" <> T.intercalate "|" cells <> "|"

extractSheetText :: Archive -> FilePath -> [[Text]]
extractSheetText archive sheetPath = case findEntry sheetPath archive of
  Nothing -> []
  Just entry ->
    case parseXML (fromEntry entry) of
      Left _ -> []
      Right doc ->
        let rows = doc C.$// C.element sRow
        in map extractRow rows

extractRow :: C.Cursor -> [Text]
extractRow row =
  let cells = row C.$/ C.element sC
  in map extractCell cells

extractCell :: C.Cursor -> Text
extractCell cell = T.concat $ concatMap C.content (cell C.$/ C.element sV)

-- ───────────────────────────────────────────────
-- Legacy format stubs
-- ───────────────────────────────────────────────

-- | Stub for legacy .doc format - recommends conversion to .docx.
docToMarkdown :: FilePath -> IO (Either Text Text)
docToMarkdown path = pure (Right $ "# Document: " <> T.pack path <> "\n\n[Legacy .doc format — convert to .docx for full extraction]")

-- | Stub for legacy .ppt format - recommends conversion to .pptx.
pptToMarkdown :: FilePath -> IO (Either Text Text)
pptToMarkdown path = pure (Right $ "# Presentation: " <> T.pack path <> "\n\n[Legacy .ppt format — convert to .pptx for full extraction]")

-- ───────────────────────────────────────────────
-- Media extraction
-- ───────────────────────────────────────────────

-- | Extract a media file from a ZIP archive as ByteString.
extractMediaFile :: FilePath -> FilePath -> IO (Either Text ByteString)
extractMediaFile archivePath mediaPath = do
  result <- parseZipFile archivePath
  case result of
    Left err -> pure (Left err)
    Right archive -> case findEntry mediaPath archive of
      Nothing -> pure (Left $ "Media not found: " <> T.pack mediaPath)
      Just entry -> pure (Right $ BL.toStrict $ fromEntry entry)

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Parse a ZIP file from disk.
parseZipFile :: FilePath -> IO (Either Text Archive)
parseZipFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Left $ "File not found: " <> T.pack path)
    else (do
      bytes <- BL.readFile path
      case toArchiveOrFail bytes of
        Right archive -> pure (Right archive)
        Left err -> pure (Left $ T.pack err)
      ) `catch` \(e :: SomeException) -> pure (Left $ "Error reading ZIP: " <> T.pack (show e))

-- | Parse XML from a lazy ByteString.
parseXML :: BL.ByteString -> Either String C.Cursor
parseXML bs = case X.parseLBS X.def bs of
  Right doc -> Right (C.fromDocument doc)
  Left err -> Left (show err)

-- | Find a specific entry in a ZIP archive by path.
findEntry :: FilePath -> Archive -> Maybe Entry
findEntry path archive = listToMaybe $ filter ((== path) . eRelativePath) (zEntries archive)

-- | Get all entry paths from a ZIP archive.
entryPaths :: Archive -> [FilePath]
entryPaths archive = map eRelativePath (zEntries archive)