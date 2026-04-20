-- | Markdown/document extraction — headers, tags, wikilinks.
-- Parses headers as nodes, wikilinks/links/tags as edges.
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Extract.Markdown
  ( extractDocFile
  , docFileNode
  , mkHeaderNode
  , mkTagNode
  , parseHeader
  , parseWikiLinks
  , extractTags
  ) where

import Control.Exception (SomeException, catch)
import Data.Char (isAlphaNum)
import Data.List (nub)
import qualified Data.Text as T

import Graphos.Domain.Types
import Graphos.Infrastructure.Logging (LogEnv, logDebug)
import Graphos.UseCase.Extract.Haskell (makeStubNode)

-- | Extract concepts and relationships from a document file.
extractDocFile :: LogEnv -> FilePath -> IO Extraction
extractDocFile env filePath = catch (do
  content <- readFile filePath
  let allNodes = docNodes filePath content
      allEdges = docEdges filePath content allNodes
  logDebug env $ T.pack $ "  [doc] " ++ filePath ++ " → " ++ show (length allNodes) ++ " nodes, " ++ show (length allEdges) ++ " edges"
  pure emptyExtraction
    { extractionNodes = allNodes
    , extractionEdges = allEdges
    }
  ) $ \(_ :: SomeException) -> do
    logDebug env $ T.pack $ "  [doc] " ++ filePath ++ " → stub (read error)"
    pure emptyExtraction { extractionNodes = [makeStubNode filePath] }

-- | Parse a document for nodes: file node, headers, tags
docNodes :: FilePath -> String -> [Node]
docNodes filePath content =
  let fileNode = docFileNode filePath
      headerNodes = docHeaderNodes filePath content
      tagNodes = docTagNodes filePath content
  in fileNode : headerNodes ++ tagNodes

-- | Create a file-level node for a document
docFileNode :: FilePath -> Node
docFileNode filePath =
  let name = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      nid = T.pack (show dirHash) <> T.pack "_doc_" <> name
  in Node
    { nodeId           = nid
    , nodeLabel        = name
    , nodeFileType     = DocumentFile
    , nodeSourceFile   = T.pack filePath
    , nodeSourceLocation = Just "L1"
    , nodeLineEnd      = Nothing
    , nodeKind         = Just "File"
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }

-- | Parse headers (## Title) as nodes
docHeaderNodes :: FilePath -> String -> [Node]
docHeaderNodes filePath content =
  [ mkHeaderNode filePath level titleText lineNum
  | (lineNum, line) <- zip [1..] (lines content)
  , Just (level, titleText) <- [parseHeader line]
  , level <= 4
  ]

-- | Parse a markdown header line, returns (level, title)
parseHeader :: String -> Maybe (Int, String)
parseHeader line =
  let trimmed = dropWhile (== ' ') line
  in case trimmed of
    '#':'#':'#':'#':rest -> Just (4, dropWhile (== ' ') rest)
    '#':'#':'#':rest     -> Just (3, dropWhile (== ' ') rest)
    '#':'#':rest         -> Just (2, dropWhile (== ' ') rest)
    '#':rest             -> Just (1, dropWhile (== ' ') rest)
    _                    -> Nothing

-- | Create a node for a header
mkHeaderNode :: FilePath -> Int -> String -> Int -> Node
mkHeaderNode filePath level title lineNum =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      cleanTitle = T.pack $ takeWhile (\c -> isAlphaNum c || c `elem` (" -'_/" :: String)) title
      nid = T.pack (show dirHash) <> T.pack "_h" <> T.pack (show level) <> T.pack "_" <> cleanTitle
  in Node
    { nodeId           = nid
    , nodeLabel        = cleanTitle
    , nodeFileType     = DocumentFile
    , nodeSourceFile   = T.pack filePath
    , nodeSourceLocation = Just (T.pack $ "L" ++ show lineNum)
    , nodeLineEnd      = Nothing
    , nodeKind         = Just "Header"
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }

-- | Parse tags (#tag or #tag/sub) as nodes
docTagNodes :: FilePath -> String -> [Node]
docTagNodes filePath content =
  let tags = nub $ extractTags content
  in [ mkTagNode filePath tag | tag <- tags ]

-- | Extract #tags from text (but exclude # headers)
extractTags :: String -> [String]
extractTags text =
  [ tag
  | (i, ch) <- zip [0..] text
  , ch == '#'
  , i > 0
  , let prev = if i > 0 then text !! (i-1) else ' '
  , prev == ' ' || prev == '\n' || prev == ','
  , let afterHash = takeWhile (\ch' -> isAlphaNum ch' || ch' `elem` ("_/-" :: String)) (drop (i+1) text)
  , not (null afterHash)
  , case afterHash of (c':_) -> c' `notElem` (" " :: String); [] -> True
  , let tag = afterHash
  , length tag >= 2
  ]

-- | Create a node for a tag
mkTagNode :: FilePath -> String -> Node
mkTagNode filePath tag =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      nid = T.pack (show dirHash) <> T.pack "_tag_" <> T.pack tag
  in Node
    { nodeId           = nid
    , nodeLabel        = T.pack $ "#" ++ tag
    , nodeFileType     = DocumentFile
    , nodeSourceFile   = T.pack filePath
    , nodeSourceLocation = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Just "Tag"
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }

-- | Build edges: file→header (contains), file→tag (tags), wikilinks (references)
docEdges :: FilePath -> String -> [Node] -> [Edge]
docEdges filePath content nodes =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      fileNid = T.pack (show dirHash) <> T.pack "_doc_" <> T.pack (takeWhile (/= '.') (reverse $ takeWhile (/= '/') $ reverse filePath))
      headerEdges = [ Edge
        { edgeSource        = fileNid
        , edgeTarget        = nodeId n
        , edgeRelation      = Contains
        , edgeConfidence    = Extracted
        , edgeConfidenceScore = 1.0
        , edgeSourceFile    = T.pack filePath
        , edgeSourceLocation = Nothing
        , edgeWeight        = 1.0
        }
        | n <- nodes
        , T.isInfixOf (T.pack "_h") (nodeId n)
        ]
      tagEdges = [ Edge
        { edgeSource        = fileNid
        , edgeTarget        = nodeId n
        , edgeRelation      = References
        , edgeConfidence    = Extracted
        , edgeConfidenceScore = 1.0
        , edgeSourceFile    = T.pack filePath
        , edgeSourceLocation = Nothing
        , edgeWeight        = 1.0
        }
        | n <- nodes
        , T.isInfixOf (T.pack "_tag_") (nodeId n)
        ]
      wikilinkEdges = [ Edge
        { edgeSource        = fileNid
        , edgeTarget        = T.pack target
        , edgeRelation      = References
        , edgeConfidence    = Extracted
        , edgeConfidenceScore = 0.8
        , edgeSourceFile    = T.pack filePath
        , edgeSourceLocation = Nothing
        , edgeWeight        = 0.8
        }
        | target <- parseWikiLinks content
        ]
  in headerEdges ++ tagEdges ++ wikilinkEdges

-- | Parse [[wikilinks]] from markdown content
parseWikiLinks :: String -> [String]
parseWikiLinks content =
  [ T.unpack $ T.strip $ T.takeWhile (/= '|') $ T.drop 2 t
  | t <- T.splitOn (T.pack "[[") (T.pack content)
  , T.isInfixOf (T.pack "]]") t
  , let linkText = T.takeWhile (/= ']') t
  , not (T.null linkText)
  ]