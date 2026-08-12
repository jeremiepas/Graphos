-- | PDF structure types and pure parsing logic.
--
-- Parses pdftotext output into a hierarchical structure of sections and paragraphs.
-- This module is pure (no IO) and independently testable.
--
-- Section detection uses regex heuristics to detect:
--   * ALL CAPS lines ≥ 4 words → Level 1 (Title)
--   * Numbered sections (1., 1.1) → Level by dot count
--   * CHAP./TITRE prefix → Level 2
--   * seCT. prefix → Level 3
--   * § prefix → Level 4
--   * Lettered items (A., B.) → Level 5
--
-- TOC pages are detected and skipped using a heuristic:
--   ≥ 60% of non-empty lines contain dot-leaders + page references.
--
-- Three granularity levels map to the pipeline's Granularity type:
--   * GranularityFile (Small) → File + top-level titles
--   * GranularityFunction (Medium) → File + sections + subsections
--   * GranularityFine (Large) → All levels + paragraphs (default)
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphos.Domain.PdfStructure
  ( -- * Types
    PdfSectionLevel(..)
  , PdfSection(..)
  , PdfParagraph(..)
  , PdfStructure(..)

    -- * Parsing
  , parsePdfStructure
  , splitIntoPages
  , isTocPage
  , detectSectionLevel
  , splitParagraphs

    -- * Conversion
  , pdfStructureToExtraction
  , makePdfFileNode
  , makeContainsEdge
  ) where

import Data.Char (isAlpha, isAlphaNum, isSpace, isUpper)
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types

-- ───────────────────────────────────────────────
-- Types
-- ───────────────────────────────────────────────

-- | Section level in PDF hierarchy
data PdfSectionLevel
  = PdfTitleLevel      -- ^ Level 1: ALL CAPS titles, major headings
  | PdfChapterLevel    -- ^ Level 2: CHAP., TITRE, numbered chapters
  | PdfSectionLevel    -- ^ Level 3: seCT., numbered sections
  | PdfSubsectionLevel -- ^ Level 4: §, numbered subsections
  | PdfItemLevel       -- ^ Level 5: A., B., lettered items
  deriving (Eq, Show, Ord)

-- | A detected section in PDF text
data PdfSection = PdfSection
  { psLevel      :: !PdfSectionLevel
  , psTitle      :: !Text
  , psLineNum    :: !Int        -- ^ 1-based line number in extracted text
  , psChildren   :: ![PdfSection]
  , psParagraphs :: ![PdfParagraph]
  } deriving (Eq, Show)

-- | A paragraph of text between sections
data PdfParagraph = PdfParagraph
  { ppText      :: !Text
  , ppLineStart :: !Int      -- ^ 1-based start line
  , ppLineEnd   :: !Int      -- ^ 1-based end line
  } deriving (Eq, Show)

-- | The complete parsed structure of a PDF document
data PdfStructure = PdfStructure
  { psFileTitle      :: !Text
  , psTopSections    :: ![PdfSection]
  , psFileParagraphs :: ![PdfParagraph]  -- ^ paragraphs before any section
  , psTocSkipped     :: !Int              -- ^ number of TOC pages skipped
  } deriving (Eq, Show)

-- ───────────────────────────────────────────────
-- Parsing
-- ───────────────────────────────────────────────

-- | Parse pdftotext output into a PdfStructure, applying granularity.
parsePdfStructure :: Granularity -> Text -> PdfStructure
parsePdfStructure granularity rawText =
  let pages = splitIntoPages rawText
      (nonTocPages, tocSkipped) = filterTocPages pages
      allText = T.unlines nonTocPages
      linesWithNum = zip [(1::Int)..] (T.lines allText)
      sections = detectSections linesWithNum
      paragraphs = if granularity == GranularityFine
                   then splitParagraphs allText
                   else []
      filteredSections = applyGranularity granularity sections
  in PdfStructure
       { psFileTitle = extractFileTitle allText
       , psTopSections = filteredSections
       , psFileParagraphs = filter (\p -> ppLineStart p > 0 && not (T.null (ppText p))) paragraphs
       , psTocSkipped = tocSkipped
       }

-- | Split text into pages (separated by form feeds from pdftotext)
splitIntoPages :: Text -> [Text]
splitIntoPages text =
  let pages = T.splitOn "\x0c" text
  in filter (not . T.null . T.strip) pages

-- | Filter out TOC pages, returning (non-TOC pages, count of TOC pages skipped)
filterTocPages :: [Text] -> ([Text], Int)
filterTocPages pages =
  let (kept, skipped) = foldr (\p (ks, s) -> if isTocPage p then (ks, s + 1) else (p:ks, s)) ([], 0) pages
  in (kept, skipped)

-- | Detect whether a page is a table of contents page.
--
-- A page is classified as TOC when ALL of:
--   * ≥ 60% of non-empty lines contain dot-leaders (3+ consecutive dots)
--   * ≥ 30% of lines end with page references (ib. or numbers)
--   * < 20% of lines have paragraph-length text (> 100 chars without dots)
isTocPage :: Text -> Bool
isTocPage pageText =
  let allLines = T.lines pageText
      nonEmpty = filter (not . T.null . T.strip) allLines
      totalLines = length nonEmpty
  in if totalLines < 3
     then False
     else let dotLeaderLines = length $ filter hasDotLeaders nonEmpty
              pageRefLines = length $ filter hasPageRef nonEmpty
              longTextLines = length $ filter hasLongText nonEmpty
              dotRatio = fromIntegral dotLeaderLines / fromIntegral totalLines :: Double
              pageRefRatio = fromIntegral pageRefLines / fromIntegral totalLines :: Double
              longTextRatio = fromIntegral longTextLines / fromIntegral totalLines :: Double
          in dotRatio >= 0.6 && pageRefRatio >= 0.3 && longTextRatio < 0.2

-- | Check if a line contains dot leaders (3+ consecutive dots or Unicode ellipsis)
hasDotLeaders :: Text -> Bool
hasDotLeaders line = T.isInfixOf "..." line || T.any (== '\x2026') line || T.isInfixOf ". . ." line

-- | Check if a line ends with a page reference (ib. or trailing digits)
hasPageRef :: Text -> Bool
hasPageRef line =
  let stripped = T.stripEnd line
      last3 = T.takeEnd 3 stripped
      last4 = T.takeEnd 4 stripped
  in T.isSuffixOf "ib." stripped
     || T.isSuffixOf "ib." (T.toLower stripped)
     || (T.length last3 > 0 && T.all isPdfDigit last3)
     || (T.length last4 > 1 && T.all (\c -> isPdfDigit c || c == '.') last4 && T.any isPdfDigit last4)
  where isPdfDigit c = c >= '0' && c <= '9'

-- | Check if a line has long text without dots (paragraph-length)
hasLongText :: Text -> Bool
hasLongText line =
  let stripped = T.strip line
  in T.length stripped > 100 && not (hasDotLeaders line)

-- | Extract the file title from text (first ALL CAPS line, or first non-empty line)
extractFileTitle :: Text -> Text
extractFileTitle text =
  let allLines = T.lines text
      capsLines = filter isAllCapsTitle allLines
  in case capsLines of
       (title:_) -> T.strip title
       [] -> case filter (not . T.null . T.strip) allLines of
               (first:_) -> T.take 80 (T.strip first)
               [] -> "Untitled"

-- | Check if a line is an ALL CAPS title (≥ 4 words, predominantly uppercase)
isAllCapsTitle :: Text -> Bool
isAllCapsTitle line =
  let stripped = T.strip line
      words_ = T.words stripped
      wordCount = length words_
      alphaWords = filter (T.any isAlpha) words_
      allUpper = all (\w -> T.all (\c -> not (isAlpha c) || isUpper c) w) alphaWords
  in wordCount >= 4 && allUpper && T.length stripped > 10

-- ───────────────────────────────────────────────
-- Section detection
-- ───────────────────────────────────────────────

-- | Detect the section level of a line, if any.
detectSectionLevel :: Text -> Maybe PdfSectionLevel
detectSectionLevel line =
  let stripped = T.strip line
  in if detectTitreHeader stripped      then Just PdfChapterLevel
     else if detectChapHeader stripped    then Just PdfChapterLevel
     else if detectSectHeader stripped    then Just PdfSectionLevel
     else if detectParagraphSign stripped then Just PdfSubsectionLevel
     else if detectLetteredItem stripped  then Just PdfItemLevel
     else if detectNumberedSection stripped then Just PdfChapterLevel
     else if isAllCapsTitle stripped     then Just PdfTitleLevel
     else Nothing

-- | Detect TITRE / TITRE Ier. headers
detectTitreHeader :: Text -> Bool
detectTitreHeader line =
  let lower = T.toLower line
  in T.isPrefixOf "titre" lower && T.length (T.strip line) > 8

-- | Detect CHAP. / CHAPITRE headers
detectChapHeader :: Text -> Bool
detectChapHeader line =
  let lower = T.toLower line
  in T.isPrefixOf "chap" lower || T.isPrefixOf "chapitre" lower

-- | Detect seCT. / SECTION headers
detectSectHeader :: Text -> Bool
detectSectHeader line =
  let lower = T.toLower line
  in T.isPrefixOf "sect" lower || T.isPrefixOf "section" lower

-- | Detect § headers
detectParagraphSign :: Text -> Bool
detectParagraphSign line =
  let stripped = T.strip line
  in T.isPrefixOf "\xA7" stripped || T.isPrefixOf "§" stripped

-- | Detect lettered items (A., B., etc.)
detectLetteredItem :: Text -> Bool
detectLetteredItem line =
  let stripped = T.strip line
  in case T.uncons stripped of
       Just (c, rest) | c `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" :: String) ->
         let afterChar = T.stripStart rest
         in case T.uncons afterChar of
              Just ('.', _) -> True
              Just (' ', rest2) ->
                let nextWord = T.takeWhile (not . isSpace) (T.stripStart rest2)
                in T.length nextWord >= 2
              _ -> False
       _ -> False

-- | Detect numbered sections (1., 1.1, 2.3.4, etc.)
detectNumberedSection :: Text -> Bool
detectNumberedSection line =
  let stripped = T.strip line
  in case T.uncons stripped of
       Just (c, _) | c >= '0' && c <= '9' ->
         let prefix = T.takeWhile (\c' -> (c' >= '0' && c' <= '9') || c' == '.') (T.stripStart stripped)
             dotCount = T.length (T.filter (== '.') prefix)
         in dotCount >= 1 && T.length prefix >= 2
       _ -> False

-- | Check if a line is a section header
isSectionLine :: Text -> Bool
isSectionLine line = isJust' (detectSectionLevel line) || detectNumberedSection line

-- | Detect sections from lines and return them as a flat list
detectSections :: [(Int, Text)] -> [PdfSection]
detectSections linesWithNum =
  let sectionLines = filter (\(_, line) -> isSectionLine line) linesWithNum
  in map (\(n, line) -> makeSection n line) sectionLines

-- | Create a PdfSection from a line number and text
makeSection :: Int -> Text -> PdfSection
makeSection lineNum line =
  let level = case detectSectionLevel line of
                Just l  -> l
                Nothing -> PdfSectionLevel  -- default
      title = cleanSectionTitle line
  in PdfSection
       { psLevel = level
       , psTitle = title
       , psLineNum = lineNum
       , psChildren = []
       , psParagraphs = []
       }

-- | Clean a section title line into just the title text
cleanSectionTitle :: Text -> Text
cleanSectionTitle line =
  let stripped = T.strip line
      -- Remove trailing page numbers and dot leaders
      withoutPageRef = T.stripEnd $ T.dropWhileEnd (\c -> (c >= '0' && c <= '9') || c == '.' || c == ' ') stripped
      -- Remove common prefixes
      cleaned = removePrefixes withoutPageRef
  in T.strip cleaned

removePrefixes :: Text -> Text
removePrefixes t = foldl' (\acc prefix -> if T.isPrefixOf prefix (T.toLower acc)
                                            then T.strip (T.drop (T.length prefix) acc)
                                            else acc) t prefixes
  where prefixes = ["titre", "chap.", "chapitre", "sect.", "section", "\xA7", "§"]

-- ───────────────────────────────────────────────
-- Paragraph splitting
-- ───────────────────────────────────────────────

-- | Split text into paragraphs based on blank line separation.
-- Each paragraph is a group of consecutive non-blank lines.
splitParagraphs :: Text -> [PdfParagraph]
splitParagraphs text =
  let linesWithNum = zip [(1::Int)..] (T.lines text)
      groups = groupByBlankLines linesWithNum
   in [ PdfParagraph { ppText = T.unlines (map snd grp)
                     , ppLineStart = fst (safeHead grp)
                     , ppLineEnd = fst (safeLast grp)
                    }
     | grp <- groups
     , length grp >= 2  -- Only paragraphs with 2+ lines
     , T.length (T.unlines (map snd grp)) > 50  -- Filter out very short fragments
     , not (any isSectionLine (map snd grp))  -- Skip groups containing section headers
     ]

-- | Group lines by blank line separators
groupByBlankLines :: [(Int, Text)] -> [[(Int, Text)]]
groupByBlankLines [] = []
groupByBlankLines lines_ =
  let (nonBlank, rest1) = span (\(_, l) -> not (T.null (T.strip l))) lines_
      (_, rest2) = span (\(_, l) -> T.null (T.strip l)) rest1
  in if null nonBlank
     then groupByBlankLines rest2
     else nonBlank : groupByBlankLines rest2

-- ───────────────────────────────────────────────
-- Granularity application
-- ───────────────────────────────────────────────

-- | Apply granularity filtering to sections
applyGranularity :: Granularity -> [PdfSection] -> [PdfSection]
applyGranularity GranularityFile sections =
  -- Small: only top-level titles
  filter (\s -> psLevel s == PdfTitleLevel) sections
applyGranularity GranularityFunction sections =
  -- Medium: titles, chapters, and sections (no subsections or items)
  filter (\s -> psLevel s <= PdfSectionLevel) sections
applyGranularity GranularityFine sections =
  -- Large: all levels
  sections

-- ───────────────────────────────────────────────
-- Conversion to Extraction
-- ───────────────────────────────────────────────

-- | Convert a PdfStructure into an Extraction (nodes and edges).
pdfStructureToExtraction :: FilePath -> PdfStructure -> Extraction
pdfStructureToExtraction filePath struct =
  let fileNode = makePdfFileNode filePath (psFileTitle struct)
      (sectionNodes, sectionEdges) = convertSections filePath (psTopSections struct)
      (paraNodes, paraEdges) = convertParagraphs filePath (psFileParagraphs struct) fileNode
      allNodes = fileNode : sectionNodes ++ paraNodes
      allEdges = sectionEdges ++ paraEdges
  in extractionFromLists allNodes allEdges

-- | Create a file-level node for a PDF document
makePdfFileNode :: FilePath -> Text -> Node
makePdfFileNode filePath title =
  let name = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      nid = T.pack (show dirHash) <> "_paper_" <> name
  in Node
       { nodeId           = nid
       , nodeLabel        = if T.null title then name else title
       , nodeFileType     = PaperFile
       , nodeSourceFile   = T.pack filePath
       , nodeLineStart    = Just 1
       , nodeLineEnd      = Nothing
       , nodeSignature    = Nothing
       , nodeCommunityId  = Nothing
       , nodeDegree       = Nothing
       , nodeIsBridge     = Nothing
       , nodeExtra        = Nothing
       , nodeKind         = Just "File"
       }

-- | Convert sections to nodes and edges
convertSections :: FilePath -> [PdfSection] -> ([Node], [Edge])
convertSections filePath sections =
  let (nodes, edges) = foldl' (addSection filePath) ([], []) sections
  in (nodes, edges)
  where
    addSection :: FilePath -> ([Node], [Edge]) -> PdfSection -> ([Node], [Edge])
    addSection fp (ns, es) section =
      let sectionNode = makeSectionNode fp section
          containsEdge = makeContainsEdge (makePdfFileNode fp "") sectionNode
          (childNodes, childEdges) = convertChildren fp section sectionNode
      in (ns ++ [sectionNode] ++ childNodes, es ++ [containsEdge] ++ childEdges)

    convertChildren :: FilePath -> PdfSection -> Node -> ([Node], [Edge])
    convertChildren fp parentSection parentNode =
      let childSections = psChildren parentSection
          childResults = map (addChildSection fp parentNode) childSections
          allNodes = concatMap fst childResults
          allEdges = concatMap snd childResults
      in (allNodes, allEdges)

    addChildSection :: FilePath -> Node -> PdfSection -> ([Node], [Edge])
    addChildSection fp parentNode childSection =
      let sectionNode = makeSectionNode fp childSection
          containsEdge = makeContainsEdge parentNode sectionNode
          (grandchildNodes, grandchildEdges) = convertChildren fp childSection sectionNode
      in ([sectionNode] ++ grandchildNodes, [containsEdge] ++ grandchildEdges)

-- | Create a section node
makeSectionNode :: FilePath -> PdfSection -> Node
makeSectionNode filePath section =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      levelTag = case psLevel section of
                   PdfTitleLevel      -> "title"
                   PdfChapterLevel    -> "chap"
                   PdfSectionLevel    -> "sect"
                   PdfSubsectionLevel -> "subsect"
                   PdfItemLevel       -> "item"
      cleanTitle = T.filter (\c -> isAlphaNum c || c `elem` (" -'_/" :: String)) (psTitle section)
      nid = T.pack (show dirHash) <> "_" <> T.pack levelTag <> "_" <> cleanTitle
  in Node
       { nodeId           = nid
       , nodeLabel        = psTitle section
       , nodeFileType     = PaperFile
       , nodeSourceFile   = T.pack filePath
       , nodeLineStart    = Just (psLineNum section)
       , nodeLineEnd      = Nothing
       , nodeSignature    = Nothing
       , nodeCommunityId  = Nothing
       , nodeDegree       = Nothing
       , nodeIsBridge     = Nothing
       , nodeExtra        = Nothing
       , nodeKind         = Just (T.pack levelTag)
       }

-- | Convert paragraphs to nodes and edges
convertParagraphs :: FilePath -> [PdfParagraph] -> Node -> ([Node], [Edge])
convertParagraphs filePath paragraphs fileNode =
  let nodes = map (makeParagraphNode filePath) paragraphs
      edges = [makeContainsEdge fileNode n | n <- nodes]
  in (nodes, edges)

-- | Create a paragraph node
makeParagraphNode :: FilePath -> PdfParagraph -> Node
makeParagraphNode filePath para =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      nid = T.pack (show dirHash) <> "_para_" <> T.pack (show (ppLineStart para))
  in Node
       { nodeId           = nid
       , nodeLabel        = T.take 80 (T.strip (ppText para))
       , nodeFileType     = PaperFile
       , nodeSourceFile   = T.pack filePath
       , nodeLineStart    = Just (ppLineStart para)
       , nodeLineEnd      = Just (ppLineEnd para)
       , nodeSignature    = Nothing
       , nodeCommunityId  = Nothing
       , nodeDegree       = Nothing
       , nodeIsBridge     = Nothing
       , nodeExtra        = Nothing
       , nodeKind         = Just "Paragraph"
       }

-- | Create a Contains edge from parent to child
makeContainsEdge :: Node -> Node -> Edge
makeContainsEdge parent child =
  Edge
    { edgeId        = EdgeId (nodeId parent <> "->" <> nodeId child <> ":contains")
    , edgeSource    = nodeId parent
    , edgeTarget    = nodeId child
    , edgeRelation  = Contains
    , edgeConfidence = Confidence 1.0
    , edgeWeight    = 1.0
    , edgeExtra     = Nothing
    }

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Safe head that errors with a message
safeHead :: [a] -> a
safeHead [] = error "safeHead: empty list"
safeHead (x:_) = x

-- | Safe last that errors with a message
safeLast :: [a] -> a
safeLast [] = error "safeLast: empty list"
safeLast xs = foldl' (\_ x -> x) (safeHead xs) xs

-- | Check if a Maybe is Just
isJust' :: Maybe a -> Bool
isJust' (Just _) = True
isJust' Nothing   = False