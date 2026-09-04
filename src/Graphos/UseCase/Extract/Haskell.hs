-- | Haskell stub extraction — parses module name, imports, and top-level declarations.
-- Used as fallback when HLS returns 0 symbols (e.g., not yet indexed).
{-# LANGUAGE ScopedTypeVariables #-}
module Graphos.UseCase.Extract.Haskell
  ( extractHaskellStub
  , parseHaskellModule
  , parseHaskellImports
  , parseHaskellDecls
  , isTopLevelDecl
  , extractDeclName
  , extractImportName
  , haskellStubNodes
  , haskellStubEdges
  ) where

import Control.Exception (SomeException, catch)
import Data.Char (isAlpha, isAlphaNum)
import Data.List (find, isPrefixOf)
import Data.Bits ((.|.))
import qualified Data.Text as T
import Data.Text.Short (fromText, toText)

import Graphos.Domain.Types
import Graphos.Domain.Graph (makeStubNode)

-- | Haskell-aware stub extraction
extractHaskellStub :: FilePath -> IO Extraction
extractHaskellStub filePath = catch (do
  content <- readFile filePath
  let allNodes = haskellStubNodes filePath content
      edges = haskellStubEdges filePath allNodes
  pure (extractionFromLists allNodes edges)
  ) $ \(_ :: SomeException) -> pure (extractionFromLists [makeStubNode filePath] [])

-- | Build a canonical module node ID.
-- Module IDs are shared across all files that import or declare the same module
-- name, which enables cross-file import edges. The special module name 'Main' is
-- kept directory-scoped to avoid merging unrelated executable entry points.
canonicalModuleId :: T.Text -> FilePath -> NodeId
canonicalModuleId name filePath
  | name == T.pack "Main" =
      let dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
          dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      in T.pack (show dirHash) <> T.pack "_" <> name
  | otherwise = T.pack "mod_" <> name

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
          let mnText = T.pack mn
          in [ Node
               { nodeId           = canonicalModuleId mnText filePath
               , nodeLabel        = fromText mnText
               , nodeFileType     = CodeFile
        , nodeSourceFile   = fromText (T.pack filePath)
               , nodeLineStart    = Just 1
               , nodeCommunityId  = Nothing
               , nodeDegree       = Nothing
               , nodeIsBridge     = Nothing
               , nodeExtra        = Nothing
               , nodeLineEnd      = Nothing
               , nodeKind         = Just (fromText "Module")
               , nodeSignature    = Nothing
               , nodePresentBits  = bitNodeLineStart .|. bitNodeKind
               }
              ]
        Nothing -> []
      impNodes = [ Node
        { nodeId           = canonicalModuleId (T.pack imp) filePath
        , nodeLabel        = fromText (T.pack imp)
        , nodeFileType     = CodeFile
        , nodeSourceFile   = fromText (T.pack filePath)
   , nodeLineStart    = Nothing
   , nodeCommunityId  = Nothing
   , nodeDegree       = Nothing
   , nodeIsBridge     = Nothing
   , nodeExtra        = Nothing
        , nodeLineEnd      = Nothing
        , nodeKind         = Just (fromText "Module")
        , nodeSignature    = Nothing
        , nodePresentBits  = bitNodeKind
        }
        | imp <- imports
        ]
      declNodes = [ Node
        { nodeId           = hashPrefix <> T.pack "_" <> T.pack declName
        , nodeLabel        = fromText (T.pack declName)
        , nodeFileType     = CodeFile
        , nodeSourceFile   = fromText (T.pack filePath)
   , nodeLineStart    = Nothing
   , nodeCommunityId  = Nothing
   , nodeDegree       = Nothing
   , nodeIsBridge     = Nothing
   , nodeExtra        = Nothing
        , nodeLineEnd      = Nothing
        , nodeKind         = Just (fromText declKind')
        , nodeSignature    = Nothing
        , nodePresentBits  = bitNodeKind
        }
        | (declName, declKind') <- decls
        ]
  in modNode ++ impNodes ++ declNodes

-- | Build edges from Haskell stub nodes: module→import, module→decl.
-- Uses "_import_" in the node ID to distinguish import targets from declarations.
haskellStubEdges :: FilePath -> [Node] -> [Edge]
haskellStubEdges _filePath nodes =
  let modNodeM = find (\n -> nodeKind n == Just "Module") nodes
  in case modNodeM of
    Just mn ->
      let importNodes = filter (\n -> nodeKind n == Just "Module" && nodeId n /= nodeId mn) nodes
          declNodes = filter (\n -> nodeKind n /= Just "Module") nodes
          mkEdge rel other =
            Edge
              { edgeId        = EdgeId (nodeId mn <> "->" <> nodeId other <> ":" <> relName rel)
              , edgeSource    = nodeId mn
              , edgeTarget    = nodeId other
              , edgeRelation  = rel
              , edgeConfidence = Confidence 0.7
              , edgeWeight    = 0.7
              , edgeExtra     = Nothing
              }
      in map (mkEdge Imports) importNodes ++ map (mkEdge Contains) declNodes
    Nothing -> []
  where
    relName Imports  = "imports"
    relName Contains = "contains"
    relName r        = T.pack (show r)

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

-- | Parse top-level declarations from a Haskell source file.
-- Returns @(name, kind)@ pairs for valid declarations only.
parseHaskellDecls :: String -> [(String, T.Text)]
parseHaskellDecls content =
  [ (name, declKind line)
  | line <- lines content
  , isTopLevelDecl line
  , Just name <- [extractDeclName line]
  ]

-- | Check if a line is a top-level declaration.
-- Only column-0 lines beginning with a letter or '(' are considered.
-- This excludes indented continuations, guards ('|'), braces, string literals,
-- comments, pragmas, and control keywords.
isTopLevelDecl :: String -> Bool
isTopLevelDecl line =
  case dropWhile (== ' ') line of
    [] -> False
    trimmed@(c:_) ->
      (isAlpha c || c == '(')
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

-- | Extract declaration name from a line. Returns 'Nothing' when the line
-- does not start with a recognizable declaration form.
extractDeclName :: String -> Maybe String
extractDeclName line =
  let trimmed = dropWhile (== ' ') line
      (_prefixLen, rest) = case trimmed of
        s | "data family " `isPrefixOf` s -> (12 :: Int, drop 12 s)
          | "type family " `isPrefixOf` s -> (12, drop 12 s)
          | "newtype " `isPrefixOf` s -> (8, drop 8 s)
          | "data " `isPrefixOf` s -> (5, drop 5 s)
          | "type " `isPrefixOf` s -> (5, drop 5 s)
          | "class " `isPrefixOf` s -> (6, drop 6 s)
          | "instance " `isPrefixOf` s -> (9, drop 9 s)
          | otherwise -> (0, s)
      nameRest = dropWhile (== ' ') rest
      name = takeWhile (\c -> isAlphaNum c || c `elem` ("'_" :: String)) nameRest
  in if null name then Nothing else Just name

-- | Classify a declaration line into a node kind.
-- All identifier declarations that are not data/newtype/type/class/instance
-- are treated as functions (bindings or type signatures).
declKind :: String -> T.Text
declKind line
  | "data family " `isPrefixOf` trimmed = "Type"
  | "type family " `isPrefixOf` trimmed = "Type"
  | "data " `isPrefixOf` trimmed = "Type"
  | "newtype " `isPrefixOf` trimmed = "Type"
  | "type " `isPrefixOf` trimmed = "Type"
  | "class " `isPrefixOf` trimmed = "Class"
  | "instance " `isPrefixOf` trimmed = "Instance"
  | otherwise = "Function"
  where trimmed = dropWhile (== ' ') line
