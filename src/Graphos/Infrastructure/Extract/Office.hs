-- | Office file extraction - routes .docx/.pptx/.xlsx/.doc/.ppt
-- through OfficeConvert then through extractDocFile.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Extract.Office
  ( extractOfficeFile
  ) where

import Control.Exception (SomeException, catch)
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.FilePath (takeExtension, takeFileName)

import Graphos.Domain.Types
import Graphos.Infrastructure.FileSystem.OfficeConvert
  ( docxToMarkdown, pptxToMarkdown, xlsxToMarkdown
  , docToMarkdown, pptToMarkdown
  )
import Graphos.Infrastructure.Logging (LogEnv, logWarn)
import Graphos.Infrastructure.Extract.Markdown (extractDocFile)

-- | Extract an office file by converting it to markdown
-- and feeding through the existing document extraction pipeline.
extractOfficeFile :: PipelineConfig -> LogEnv -> FilePath -> IO Extraction
extractOfficeFile _config env filePath = catch (do
  let ext = map toLower (takeExtension filePath)
  mdResult <- case ext of
    ".docx" -> docxToMarkdown filePath
    ".pptx" -> pptxToMarkdown filePath
    ".xlsx" -> xlsxToMarkdown filePath
    ".doc"  -> docToMarkdown filePath
    ".ppt"  -> pptToMarkdown filePath
    _       -> pure (Left $ "Unsupported office format: " <> T.pack ext)

  case mdResult of
    Left err -> do
      logWarn env $ T.pack $ "  [office] Error converting " ++ filePath ++ ": " ++ T.unpack err
      pure (extractionFromLists [officeStubNode filePath] [])
    Right _md -> do
      extraction <- extractDocFile env filePath
      let nodes = Map.elems (extractionNodes extraction)
      if null nodes
        then pure (extractionFromLists [officeStubNode filePath] [])
        else pure extraction
  ) $ \(e :: SomeException) -> do
    logWarn env $ T.pack $ "  [office] Error processing " ++ filePath ++ ": " ++ show e
    pure (extractionFromLists [officeStubNode filePath] [])

-- | Create a stub node for an office file that couldn't be extracted.
officeStubNode :: FilePath -> Node
officeStubNode fp = Node
  { nodeId = T.pack fp
  , nodeLabel = T.pack (takeFileName fp)
  , nodeFileType = OfficeFile
  , nodeSourceFile = T.pack fp
  , nodeLineStart = Nothing
  , nodeLineEnd = Nothing
  , nodeSignature = Nothing
  , nodeCommunityId = Nothing
  , nodeKind = Just "File"
  , nodeDegree = Nothing
  , nodeIsBridge = Nothing
  , nodeExtra = Nothing
  }
