-- | Markdown/document extraction — headers, tags, wikilinks, code blocks.
-- Parses headers as nodes, wikilinks/links/tags as edges.
-- Uses strict IO to avoid exhausting file descriptors with many concurrent reads.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.Infrastructure.Extract.Markdown
  ( extractDocFile
  , docFileNode
  , mkHeaderNode
  , mkTagNode
  , parseHeader
  , parseWikiLinks
  , extractTags
  ) where

import Control.Exception (SomeException, catch, evaluate)
import Data.Char (isAlphaNum, isSpace)
import Data.List (nub)
import qualified Data.Text as T
import System.IO (withFile, IOMode(ReadMode), hGetContents')

import Graphos.Domain.Types
import Graphos.Domain.Graph (makeStubNode)
import Graphos.Infrastructure.Logging (LogEnv, logDebug)

-- | Extract concepts and relationships from a document file.
-- Uses strict IO (withFile + hGetContents') to avoid lazy file handle leaks
-- that cause EMFILE "read error" when processing 1000+ files concurrently.
extractDocFile :: LogEnv -> FilePath -> IO Extraction
extractDocFile env filePath = catch (do
  content <- withFile filePath ReadMode $ \h -> do
    raw <- hGetContents' h
    _ <- evaluate (length raw)  -- force full read before closing handle
    pure (T.pack raw)
  let allNodes = docNodes filePath content
      allEdges = docEdges filePath content allNodes
  logDebug env $ T.pack $ "  [doc] " ++ filePath ++ " → " ++ show (length allNodes) ++ " nodes, " ++ show (length allEdges) ++ " edges"
  pure (extractionFromLists allNodes allEdges)
  ) $ \(_ :: SomeException) -> do
  logDebug env $ T.pack $ "  [doc] " ++ filePath ++ " → stub (read error)"
  pure (extractionFromLists [makeStubNode filePath] [])

-- ───────────────────────────────────────────────
-- Node extraction
-- ───────────────────────────────────────────────

-- | Parse a document for nodes: file node, headers, tags
docNodes :: FilePath -> T.Text -> [Node]
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
      nid = T.pack (show dirHash) <> "_doc_" <> name
  in Node
     { nodeId           = nid
    , nodeLabel        = name
    , nodeFileType     = DocFile
    , nodeSourceFile   = T.pack filePath
    , nodeLineStart    = Just 1
    , nodeCommunityId  = Nothing
    , nodeDegree       = Nothing
    , nodeIsBridge     = Nothing
    , nodeExtra        = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Just "File"
    , nodeSignature    = Nothing
    }

-- ───────────────────────────────────────────────
-- Header parsing
-- ───────────────────────────────────────────────

-- | Parse headers (## Title) as nodes
docHeaderNodes :: FilePath -> T.Text -> [Node]
docHeaderNodes filePath content =
  [ mkHeaderNode filePath level titleText lineNum
  | (lineNum, line) <- zip [1..] (T.lines content)
  , Just (level, titleText) <- [parseHeader line]
  , level <= 4
  ]

-- | Parse a markdown header line, returns (level, title)
parseHeader :: T.Text -> Maybe (Int, T.Text)
parseHeader line =
  let trimmed = T.dropWhile (== ' ') line
  in case T.unpack trimmed of
    '#':'#':'#':'#':rest -> Just (4, T.strip (T.pack rest))
    '#':'#':'#':rest     -> Just (3, T.strip (T.pack rest))
    '#':'#':rest         -> Just (2, T.strip (T.pack rest))
    '#':rest             -> Just (1, T.strip (T.pack rest))
    _                    -> Nothing

-- | Create a node for a header
mkHeaderNode :: FilePath -> Int -> T.Text -> Int -> Node
mkHeaderNode filePath level title lineNum =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      cleanTitle = T.filter (\c -> isAlphaNum c || c `elem` (" -'_/" :: String)) title
      nid = T.pack (show dirHash) <> "_h" <> T.pack (show level) <> "_" <> cleanTitle
  in Node
    { nodeId           = nid
    , nodeLabel        = cleanTitle
    , nodeFileType     = DocFile
    , nodeSourceFile   = T.pack filePath
    , nodeLineStart    = Just lineNum
    , nodeCommunityId  = Nothing
    , nodeDegree       = Nothing
    , nodeIsBridge     = Nothing
    , nodeExtra        = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Just "Header"
    , nodeSignature    = Nothing
    }

-- ───────────────────────────────────────────────
-- Tag parsing (O(n) — scans Text directly)
-- ───────────────────────────────────────────────

-- | Parse #tags as nodes
docTagNodes :: FilePath -> T.Text -> [Node]
docTagNodes filePath content =
  let tags = nub $ extractTags content
  in [ mkTagNode filePath tag | tag <- tags ]

-- | Extract #tags from text (but exclude # headers).
-- O(n) scan over Text — no String conversion needed.
extractTags :: T.Text -> [T.Text]
extractTags text = go False text
  where
    go _ t | T.null t = []
    go prevNewline t
      | T.head t == '#'
      , prevNewline   = go False (T.tail t)  -- header, skip
      | T.head t == '#' = case takeTag (T.tail t) of
          (tag, rest)
            | T.length tag >= 2 -> tag : go False rest
            | otherwise -> go False rest
      | T.head t == '\n' = go True (T.tail t)
      | isSpace (T.head t) = go (T.head t == '\n') (T.tail t)
      | otherwise = go False (T.tail t)

    takeTag t = let tag = T.takeWhile (\c -> isAlphaNum c || c `elem` ("_/-" :: String)) t
                    rest = T.drop (T.length tag) t
                in (tag, rest)

-- | Create a node for a tag
mkTagNode :: FilePath -> T.Text -> Node
mkTagNode filePath tag =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      nid = T.pack (show dirHash) <> "_tag_" <> tag
  in Node
    { nodeId           = nid
    , nodeLabel        = "#" <> tag
    , nodeFileType     = DocFile
    , nodeSourceFile   = T.pack filePath
  , nodeLineStart    = Nothing
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Just "Tag"
    , nodeSignature    = Nothing
    }

-- ───────────────────────────────────────────────
-- Edge construction
-- ───────────────────────────────────────────────

-- | Build edges: file→header (contains), file→tag (tags), wikilinks (references)
docEdges :: FilePath -> T.Text -> [Node] -> [Edge]
docEdges filePath content nodes =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      fileNid = T.pack (show dirHash) <> "_doc_" <> T.pack (takeWhile (/= '.') (reverse $ takeWhile (/= '/') $ reverse filePath))
      headerEdges = [ Edge
        { edgeId        = EdgeId (fileNid <> "->" <> nodeId n <> ":contains")
        , edgeSource    = fileNid
        , edgeTarget    = nodeId n
        , edgeRelation  = Contains
        , edgeConfidence = Confidence 1.0
        , edgeWeight    = 1.0
        , edgeExtra     = Nothing
        }
        | n <- nodes
        , "_h" `T.isInfixOf` nodeId n
        ]
      tagEdges = [ Edge
        { edgeId        = EdgeId (fileNid <> "->" <> nodeId n <> ":references")
        , edgeSource    = fileNid
        , edgeTarget    = nodeId n
        , edgeRelation  = References
        , edgeConfidence = Confidence 1.0
        , edgeWeight    = 1.0
        , edgeExtra     = Nothing
        }
        | n <- nodes
        , "_tag_" `T.isInfixOf` nodeId n
        ]
      wikilinkEdges = [ Edge
        { edgeId        = EdgeId (fileNid <> "->" <> T.pack target <> ":references")
        , edgeSource    = fileNid
        , edgeTarget    = T.pack target
        , edgeRelation  = References
        , edgeConfidence = Confidence 0.8
        , edgeWeight    = 0.8
        , edgeExtra     = Nothing
        }
        | target <- parseWikiLinks content
        ]
  in headerEdges ++ tagEdges ++ wikilinkEdges

-- | Parse [[wikilinks]] from markdown content
parseWikiLinks :: T.Text -> [String]
parseWikiLinks content =
  [ T.unpack $ T.strip $ T.takeWhile (/= '|') $ T.drop 2 t
  | t <- T.splitOn "[[" content
  , "]]" `T.isInfixOf` t
  , let linkText = T.takeWhile (/= ']') t
  , not (T.null linkText)
  ]
