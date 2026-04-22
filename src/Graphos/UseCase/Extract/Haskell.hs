-- | Haskell stub extraction — parses module name, imports, and top-level declarations.
-- Used as fallback when HLS returns 0 symbols (e.g., not yet indexed).
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Extract.Haskell
  ( extractHaskellStub
  , makeStubNode
  , parseHaskellModule
  , parseHaskellImports
  , parseHaskellDecls
  , isTopLevelDecl
  , extractDeclName
  , extractImportName
  ) where

import Control.Exception (SomeException, catch)
import Data.Char (isAlphaNum)
import Data.List (find, isPrefixOf)
import qualified Data.Text as T

import Graphos.Domain.Types

-- | Create a stub node when no LSP is available
makeStubNode :: FilePath -> Node
makeStubNode filePath =
  let name = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      hashPrefix = T.pack $ show dirHash
      nodeId' = hashPrefix <> T.pack "_" <> name
  in Node
    { nodeId           = nodeId'
    , nodeLabel        = name
    , nodeFileType     = CodeFile
    , nodeSourceFile   = T.pack filePath
    , nodeSourceLocation = Nothing
    , nodeLineEnd      = Nothing
    , nodeKind         = Nothing
    , nodeSignature    = Nothing
    , nodeSourceUrl    = Nothing
    , nodeCapturedAt   = Nothing
    , nodeAuthor       = Nothing
    , nodeContributor  = Nothing
    }

-- | Haskell-aware stub extraction
extractHaskellStub :: FilePath -> IO Extraction
extractHaskellStub filePath = catch (do
  content <- readFile filePath
  let allNodes = haskellStubNodes filePath content
      edges = haskellStubEdges filePath allNodes
  pure emptyExtraction
    { extractionNodes = allNodes
    , extractionEdges = edges
    }
  ) $ \(_ :: SomeException) -> pure emptyExtraction
    { extractionNodes = [makeStubNode filePath] }

-- | Parse Haskell source for module name, imports, and top-level decl names
haskellStubNodes :: FilePath -> String -> [Node]
haskellStubNodes filePath content =
  let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      hashPrefix = T.pack $ show dirHash
      modName = parseHaskellModule content
      imports = parseHaskellImports content
      decls = parseHaskellDecls content
      modNode = case modName of
        Just mn ->
          [ Node
            { nodeId           = hashPrefix <> T.pack "_" <> T.pack mn
            , nodeLabel        = T.pack mn
            , nodeFileType     = CodeFile
            , nodeSourceFile   = T.pack filePath
            , nodeSourceLocation = Just "L1"
            , nodeLineEnd      = Nothing
            , nodeKind         = Just "Module"
            , nodeSignature    = Nothing
            , nodeSourceUrl    = Nothing
            , nodeCapturedAt   = Nothing
            , nodeAuthor       = Nothing
            , nodeContributor  = Nothing
            }
          ]
        Nothing -> []
      impNodes = [ Node
        { nodeId           = hashPrefix <> T.pack "_import_" <> T.pack imp
        , nodeLabel        = T.pack imp
        , nodeFileType     = CodeFile
        , nodeSourceFile   = T.pack filePath
        , nodeSourceLocation = Nothing
        , nodeLineEnd      = Nothing
        , nodeKind         = Just "Module"
        , nodeSignature    = Nothing
        , nodeSourceUrl    = Nothing
        , nodeCapturedAt   = Nothing
        , nodeAuthor       = Nothing
        , nodeContributor  = Nothing
        }
        | imp <- imports
        ]
      declNodes = [ Node
        { nodeId           = hashPrefix <> T.pack "_" <> T.pack decl
        , nodeLabel        = T.pack decl
        , nodeFileType     = CodeFile
        , nodeSourceFile   = T.pack filePath
        , nodeSourceLocation = Nothing
        , nodeLineEnd      = Nothing
        , nodeKind         = Nothing
        , nodeSignature    = Nothing
        , nodeSourceUrl    = Nothing
        , nodeCapturedAt   = Nothing
        , nodeAuthor       = Nothing
        , nodeContributor  = Nothing
        }
        | decl <- decls
        ]
  in modNode ++ impNodes ++ declNodes

-- | Build edges from Haskell stub nodes: module→import, module→decl
haskellStubEdges :: FilePath -> [Node] -> [Edge]
haskellStubEdges filePath nodes =
  let modNodeM = find (\n -> not ("_import_" `T.isInfixOf` nodeId n)) nodes
  in case modNodeM of
    Just mn ->
      let otherNodes = filter (\n -> nodeId n /= nodeId mn) nodes
      in [ Edge
        { edgeSource        = nodeId mn
        , edgeTarget        = nodeId other
        , edgeRelation      = Imports
        , edgeConfidence    = Ambiguous
        , edgeConfidenceScore = 0.7
        , edgeSourceFile    = T.pack filePath
        , edgeSourceLocation = nodeSourceLocation mn
        , edgeWeight        = 0.7
        }
        | other <- otherNodes
        ]
    Nothing -> []

{- | Parse the module name from a Haskell source file -}
parseHaskellModule :: String -> Maybe String
parseHaskellModule content =
  case [line | line <- lines content, "module " `isPrefixOf` dropWhile (== ' ') line] of
    (line:_) -> Just $ extractModuleName line
    [] -> Nothing
  where
    extractModuleName line =
      let afterModule = dropWhile (== ' ') $ drop 7 line
          name = takeWhile (\c -> isAlphaNum c || c `elem` ("._" :: String)) afterModule
      in if null name then "Main" else name

-- | Parse import declarations from a Haskell source file
parseHaskellImports :: String -> [String]
parseHaskellImports content =
  [ extractImportName line
  | line <- lines content
  , "import " `isPrefixOf` dropWhile (== ' ') line
  ]

-- | Extract import name from a line
extractImportName :: String -> String
extractImportName line =
  let trimmed = dropWhile (== ' ') line
      afterImport = dropWhile (== ' ') $ drop 7 trimmed
      isQualified = "qualified " `isPrefixOf` afterImport
      afterQual = if isQualified then dropWhile (== ' ') $ drop 9 afterImport else afterImport
      name = takeWhile (\c -> isAlphaNum c || c `elem` ("._" :: String)) afterQual
  in if null name then "Unknown" else name

-- | Parse top-level declaration names from a Haskell source file
parseHaskellDecls :: String -> [String]
parseHaskellDecls content =
  [ extractDeclName line
  | line <- lines content
  , isTopLevelDecl line
  ]

-- | Check if a line is a top-level declaration
isTopLevelDecl :: String -> Bool
isTopLevelDecl line =
  let trimmed = dropWhile (== ' ') line
  in not (null trimmed)
     && case trimmed of (c:_) -> c `notElem` ("-{-#" :: String); [] -> False
     && not ("module " `isPrefixOf` trimmed)
     && not ("import " `isPrefixOf` trimmed)
     && not ("where" `isPrefixOf` trimmed)
     && not ("deriving" `isPrefixOf` trimmed)
     && not ("else" `isPrefixOf` trimmed)
     && not ("then" `isPrefixOf` trimmed)
     && not ("in " `isPrefixOf` trimmed)
     && not ("do" == trimmed)
     && not ("let" `isPrefixOf` trimmed)
     && not ("=" == dropWhile (/= '=') trimmed)

-- | Extract declaration name from a line
extractDeclName :: String -> String
extractDeclName line =
  let trimmed = dropWhile (== ' ') line
      (_prefixLen, rest) = case trimmed of
        s | "data " `isPrefixOf` s -> (5 :: Int, drop 5 s)
          | "newtype " `isPrefixOf` s -> (8, drop 8 s)
          | "type " `isPrefixOf` s -> (5, drop 5 s)
          | "class " `isPrefixOf` s -> (6, drop 6 s)
          | "instance " `isPrefixOf` s -> (9, drop 9 s)
          | "type family " `isPrefixOf` s -> (12, drop 12 s)
          | "data family " `isPrefixOf` s -> (12, drop 12 s)
          | otherwise -> (0, s)
      nameRest = dropWhile (== ' ') rest
      name = takeWhile (\c -> isAlphaNum c || c `elem` ("'_" :: String)) nameRest
  in if null name
     then take 20 trimmed
     else name