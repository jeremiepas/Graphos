-- | Tests for OfficeConvert module — DOCX/PPTX/XLSX to markdown conversion.
module Graphos.Infrastructure.FileSystem.OfficeConvertSpec where

import Test.Hspec
import qualified Data.Text as T

import Graphos.Infrastructure.FileSystem.OfficeConvert
  ( docxToMarkdown
  , pptxToMarkdown
  , xlsxToMarkdown
  , docToMarkdown
  , pptToMarkdown
  , docxExtractMediaPaths
  , pptxExtractMediaPaths
  )

spec :: Spec
spec = do
  describe "docToMarkdown" $ do
    it "returns a stub message for legacy .doc files" $ do
      result <- docToMarkdown "/tmp/test.doc"
      case result of
        Left _ -> expectationFailure "Expected Right for .doc stub"
        Right md -> T.isInfixOf "Legacy .doc format" md `shouldBe` True

  describe "pptToMarkdown" $ do
    it "returns a stub message for legacy .ppt files" $ do
      result <- pptToMarkdown "/tmp/test.ppt"
      case result of
        Left _ -> expectationFailure "Expected Right for .ppt stub"
        Right md -> T.isInfixOf "Legacy .ppt format" md `shouldBe` True

  describe "docxToMarkdown" $ do
    it "returns Left for non-existent file" $ do
      result <- docxToMarkdown "/tmp/nonexistent_file_abc123.docx"
      case result of
        Left err -> T.isInfixOf "not found" err `shouldBe` True
        Right _ -> expectationFailure "Expected Left for non-existent file"

  describe "pptxToMarkdown" $ do
    it "returns Left for non-existent file" $ do
      result <- pptxToMarkdown "/tmp/nonexistent_file_abc123.pptx"
      case result of
        Left _ -> pure ()  -- Expected
        Right _ -> expectationFailure "Expected Left for non-existent file"

  describe "xlsxToMarkdown" $ do
    it "returns Left for non-existent file" $ do
      result <- xlsxToMarkdown "/tmp/nonexistent_file_abc123.xlsx"
      case result of
        Left _ -> pure ()  -- Expected
        Right _ -> expectationFailure "Expected Left for non-existent file"

  describe "docxExtractMediaPaths" $ do
    it "returns empty list for non-existent file" $ do
      paths <- docxExtractMediaPaths "/tmp/nonexistent_file_abc123.docx"
      paths `shouldBe` []

  describe "pptxExtractMediaPaths" $ do
    it "returns empty list for non-existent file" $ do
      paths <- pptxExtractMediaPaths "/tmp/nonexistent_file_abc123.pptx"
      paths `shouldBe` []