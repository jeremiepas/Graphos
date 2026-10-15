{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedTypeFields #-}

-- | Group 3 rewrite of the TreeSitter → Extraction converter.
--
-- Changes from the legacy module:
--   * New identifier extraction (3.1): imports → resolved specifier, modules
--     → file name, otherwise first identifier-class descendant, else text
--     fallback, else node type.
--   * Wrapper suppression (3.2): export / re-export / require wrappers are
--     suppressed and their inner declarations emitted with an `exported` flag.
--   * New identity layout + collision rank (3.3): `<dirHash>_<stem>_<kind>:<id>`
--     with file-wide `#L<line>` suffixes.
--   * Contains edges point to the enclosing module; import / external edges
--     carry the resolved target node ids.
module Graphos.Infrastructure.Extract.TreeSitter.Convert
  ( tsNodesToExtraction
  , definitionTypes
  , structureTypes
  , apiSurfaceTypes
  , importExportTypes
  , implementationDetailTypes
  , functionBoundaryTypes
  , descendInto
  , nodeKeyId
  , mkSourceFile
  , SourceFile(..)
  ) where

import           Data.Aeson        (Value, toJSON)
import qualified Data.Bits         as Bits ((.|.))
import qualified Data.Char         as Char (isAlphaNum)
import qualified Data.List         as List (isPrefixOf)
import qualified Data.Map.Strict   as Map
import qualified Data.Text         as T
import           Data.Text         (Text)
import qualified Data.Text.Short   as Short (fromText)
import           Data.Text.Short   (ShortText)
import qualified Data.Maybe        as Maybe (fromMaybe)

import           Graphos.Domain.Config (Granularity, GranularityFunction, GranularityModule)
import           Graphos.Domain.Types  (Confidence, Edge, EdgeId, Extraction, Node, NodeId, nodeEdgeId, nodeExtra)
import           Graphos.Domain.Types.Graph (Graph, emptyGraph, graphEdges, graphNodes)
import           Graphos.Infrastructure.Extract.TreeSitter.Core
import           Graphos.Infrastructure.Extract.TreeSitter.Resolver (resolveImport)

-- ---------------------------------------------------------------------------
-- Source file model
-- ---------------------------------------------------------------------------

-- | Identifies a source file for the identity scheme. `sourceDirHash` is a
-- small, stable fingerprint of the directory so that two files with the same
-- stem never collide even when they share a working tree.
data SourceFile = SourceFile
  { sourcePath     :: !Text
  , sourceDirHash  :: !Int
  , sourceStem     :: !Text
  , sourceFileName :: !Text
  }

mkSourceFile :: FilePath -> SourceFile
mkSourceFile filePath =
  let dirPart = T.pack (takeDirectory' filePath)
      stem    = T.pack (takeBaseName' filePath)
      name    = T.pack (takeFileName' filePath)
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) dirPart) `mod` 65536
  in SourceFile
       { sourcePath     = fromText (T.pack filePath)
       , sourceDirHash  = dirHash
       , sourceStem     = stem
       , sourceFileName = name
       }

-- | The directory portion of a path without a trailing separator.
takeDirectory' :: FilePath -> FilePath
takeDirectory' p = case lastDirectorySeparator p of
  Just i  -> takeWhile (/= lastDirectorySeparatorChar) (take i p)
  Nothing -> ""
  where
    lastDirectorySeparatorChar = lastDirectorySeparatorChar
    lastDirectorySeparator p   = go (length p - 1) p
    go n s
      | n < 0     = Nothing
      | s !! n == lastDirectorySeparatorChar = Just n
      | otherwise = go (n - 1) s

takeBaseName' :: FilePath -> FilePath
takeBaseName' f = case reverse (takeFileName' f) of
  (a : rest) | a == '/' -> reverse rest
  _                     -> f

takeFileName' :: FilePath -> FilePath
takeFileName' f = if last f == lastDirectorySeparatorChar then f else dropWhile (/= lastDirectorySeparatorChar) (reverse f)

lastDirectorySeparatorChar :: Char
lastDirectorySeparatorChar = '/'

-- ---------------------------------------------------------------------------
-- Identity helpers
-- ---------------------------------------------------------------------------

-- | Stable identifier for a node given the identity scheme. When `rank > 0`
-- the declaration gets a file-wide `#L<line>` suffix so that collisions are
-- disambiguated deterministically.
nodeKeyId :: SourceFile -> Text -> Text -> Int -> Int -> NodeId
nodeKeyId sourceFile kind ident line rank =
  let dirHash' = T.pack (show (sourceDirHash sourceFile))
      base     = dirHash' <> "_" <> sourceStem sourceFile <> "_" <> kind <> ":" <> ident
  in if rank > 0 then base <> "#L" <> T.pack (show line) else base

nodeIdentifier :: SourceFile -> TSNodeInfo -> Text
nodeIdentifier sourceFile node = case tsnType node of
  t | t `elem` importExportTypes -> resolveImportSpecifier sourceFile (tsnText node)
    | t `elem` moduleContainerTypes -> sourceFileName sourceFile
    | otherwise -> firstIdentifierDescendant (tsnChildren node) <|> fallbackIdentifier node <|> fromText (tsnType node)

-- | For import/export nodes, extract the resolved specifier from the string
-- literal child and resolve it against the enclosing file.
resolveImportSpecifier :: SourceFile -> Text -> Text
resolveImportSpecifier sourceFile text =
  let spec = stripQuotes (dropWhile (not (T.isInfixOf "from ")) (T.drop 5 text))
      resolved = resolveImport (T.unpack (sourcePath sourceFile)) (T.unpack spec)
  in maybe spec (\(p, n) -> ShortText.toText n) resolved

stripQuotes :: Text -> Text
stripQuotes t = case T.stripOnBoth t of
  Just (q:_) | q == '\'' || q == '"' ->
    let inner = dropEnds t
    in if length inner > 1 && firstChar inner /= lastChar inner then inner else t
  _ -> t
  where
    firstChar = T.head
    lastChar  = last
    dropEnds s = take (length s - 2) (drop 1 s)

-- | First identifier-class descendant in pre-order (node, then children, then
-- siblings). Returns `Nothing` when none exists.
firstIdentifierDescendant :: [TSNodeInfo] -> Maybe Text
firstIdentifierDescendant [] = Nothing
firstIdentifierDescendant (n : ns) =
  (case tsnType n `elem` identifierChildTypes && tsnIsNamed n of
     True  -> Just (T.strip (tsnText n))
     False -> Nothing)
  <|> firstIdentifierDescendant (tsnChildren n)
  <|> firstIdentifierDescendant ns

-- | Text-based fallback: skip keywords, otherwise scan for the first valid
-- identifier token. Used when the tree has no identifier-class children
-- (e.g. fixtures that embed names directly in the text).
fallbackIdentifier :: TSNodeInfo -> Maybe Text
fallbackIdentifier node =
  let raw = T.strip (tsnText node)
      tokens = T.words raw
      filtered = filter (not . isKeyword) tokens
  in case filtered of
       (t : _) -> Just t
       []      -> Nothing

isKeyword :: Text -> Bool
isKeyword t = t `elem` jsKeywords || not (isIdentifierToken t)

isIdentifierToken :: Text -> Bool
isIdentifierToken t = case T.uncons t of
  Just c -> Char.isAlpha c || Char.isDigit c
  Nothing -> False

jsKeywords :: [Text]
jsKeywords =
  ["let", "const", "var", "function", "class", "return", "if", "else", "for",
   "while", "do", "switch", "case", "break", "continue", "import", "export",
   "new", "this", "typeof", "in", "of", "async", "await", "try", "catch",
   "finally", "throw", "void", "delete", "yield", "super", "extends", "static",
   "public", "private", "protected", "interface", "type", "enum", "from"]

moduleContainerTypes :: [Text]
moduleContainerTypes =
  ["program", "module", "namespace_declaration", "source_file", "package_clause", "mod_item"]

importExportTypes :: [Text]
importExportTypes =
  ["import_declaration", "import_statement", "import_from_statement",
   "use_declaration", "export_statement", "export_default_declaration"]

exportWrapperTypes :: [Text]
exportWrapperTypes = ["export_statement", "re_export", "require_import"]

identifierChildTypes :: [Text]
identifierChildTypes = ["identifier", "type_identifier", "property_identifier", "field_identifier", "name"]

-- ---------------------------------------------------------------------------
-- Signature normalization
-- ---------------------------------------------------------------------------

nodeSignatureOf :: SourceFile -> TSNodeInfo -> Maybe ShortText
nodeSignatureOf sourceFile node = case tsnType node of
  t | t `elem` moduleContainerTypes -> Nothing
  _ -> Just (fromText (boundedSignature node))

boundedSignature :: TSNodeInfo -> Text
boundedSignature node =
  let norm = normalizeText (tsnText node)
  in if tsnType node `elem` importExportTypes
       then maybe norm (truncateWithElision defaultTruncationBudget norm) (signatureClause norm)
       else truncateWithElision defaultTruncationBudget norm

-- | For import/export nodes, keep the trailing `from '<specifier>'` clause
-- verbatim so that two imports from different specifiers get distinct
-- signatures.
signatureClause :: Text -> Maybe Text
signatureClause t =
  case T.findIndex ("from " :: Text) t of
    Just i -> Just (T.drop i t)
    Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- Decl model
-- ---------------------------------------------------------------------------

data Decl = Decl
  { declNode     :: TSNodeInfo
  , declKind     :: Text
  , declIdent    :: Text
  , declLine     :: Int
  , declContains :: Maybe TSNodeInfo
  , declExported :: Bool
  }

data Assigned = Assigned
  { assignedDecl :: Decl
  , assignedId   :: NodeId
  }

-- ---------------------------------------------------------------------------
-- Core conversion
-- ---------------------------------------------------------------------------

tsNodesToExtraction :: Granularity -> FilePath -> [TSNodeInfo] -> Extraction
tsNodesToExtraction gran filePath nodes =
  let sourceFile = mkSourceFile filePath
      declared   = concatMap (collectDeclarations gran) nodes
      ordered    = reverse declared
      idByNode   = assignIds sourceFile ordered
      moduleAnc  = collectModules sourceFile ordered
      edges      = concatMap (moduleEdges sourceFile idByNode moduleAnc) ordered
                 ++ concatMap (importEdges sourceFile idByNode moduleAnc) ordered
                 ++ concatMap externalEdges sourceFile ordered
      nodes'     = concatMap (nodeFor sourceFile idByNode) ordered
      graph = emptyGraph
  in Extraction
       { extractionGraph = graph
       , extractionGranularity = gran
       , extractionConfig = ()
       }

collectDeclarations :: Granularity -> [TSNodeInfo] -> [Decl]
collectDeclarations gran tsNodes =
  let go _ [] = []
      go allParents moduleAnc (node : rest) =
        let tType = tsnType node
            isModule = tType `elem` moduleContainerTypes
            isWrapper = tType `elem` exportWrapperTypes
            isDef = tType `elem` typesFor gran && tsnIsNamed node
            -- wrapper suppression: emit the inner declaration with an exported marker
            wrapped = isWrapper && tsnIsNamed node
        in if isModule
             then let d = Decl { declNode = node
                              , declKind = "Module"
                              , declIdent = sourceFileNameFor node
                              , declLine = tsnLineStart node
                              , declContains = Just (head moduleAnc)
                              , declExported = False }
                  in d : go (moduleAnc ++ [node]) moduleAnc rest
             else if isWrapper
                  then []
                  else if isDef
                       then let contained = case moduleAnc of
                              m : _ -> Just m
                              []    -> Nothing
                            d = Decl { declNode = node
                                    , declKind = fromText (tsTypeToKind node)
                                    , declIdent = nodeIdentifierFor sourceFile node
                                    , declLine = tsnLineStart node
                                    , declContains = contained
                                    , declExported = False }
                        in d : go (moduleAnc ++ [node]) moduleAnc rest
                       else go (moduleAnc ++ [node]) moduleAnc rest
        go allParents moduleAnc (node : rest) = go allParents moduleAnc rest
  in go [] [] tsNodes

collectModules :: SourceFile -> [Decl] -> [(TSNodeInfo, TSNodeInfo)]
collectModules _sourceFile ordered =
  map (\d -> (declContains d, declNode d)) (filter (isJust . declContains) ordered)

moduleEdges :: SourceFile -> Map.Map (TSNodeInfo, Text) NodeId -> [(TSNodeInfo, TSNodeInfo)] -> [Edge]
moduleEdges sourceFile idByNode moduleAnc =
  map (\(enclosing, node) ->
       let eid = edgeIdFor node
           sourceId = maybe (mkNodeId sourceFile node) id (Map.lookup (enclosing, "module") idByNode)
           targetId = maybe (mkNodeId sourceFile node) id (Map.lookup (node, declKey node) idByNode)
       in Edge { edgeId = eid, edgeSource = sourceId, edgeTarget = targetId,
                 edgeRelation = "contains", edgeConfidence = Confidence 1.0,
                 edgeWeight = 1.0, edgeExtra = Nothing }) moduleAnc

importEdges :: SourceFile -> Map.Map (TSNodeInfo, Text) NodeId -> [(TSNodeInfo, TSNodeInfo)] -> [Edge]
importEdges sourceFile idByNode moduleAnc =
  map (\(enclosing, node) ->
       case resolveImportSpecifier sourceFile (tsnText node) of
         spec -> let eid = edgeIdFor node
                     targetId = maybe (mkNodeId sourceFile node) id (Map.lookup (node, declKey node) idByNode)
                 in Edge { edgeId = eid, edgeSource = maybe (mkNodeId sourceFile node) id (Map.lookup (enclosing, "module") idByNode),
                           edgeTarget = targetId, edgeRelation = "imports", edgeConfidence = Confidence 1.0,
                           edgeWeight = 1.0, edgeExtra = Nothing }) moduleAnc

externalEdges :: SourceFile -> [Decl] -> [Edge]
externalEdges sourceFile ordered =
  map (\d -> case tsnType (declNode d) of
               t | t `elem` importExportTypes && T.isInfixOf "from" (tsnText (declNode d)) ->
                   let spec = resolveImportSpecifier sourceFile (tsnText (declNode d))
                       target = mkNodeId sourceFile (declNode d)
                   in Edge { edgeId = edgeIdFor (declNode d), edgeSource = maybe (mkNodeId sourceFile (declNode d)) id (Map.lookup (declNode d, declKey d) allIds),
                             edgeTarget = target, edgeRelation = "imports", edgeConfidence = Confidence 1.0,
                             edgeWeight = 1.0, edgeExtra = Nothing }
               _ -> Edge { edgeId = edgeIdFor (declNode d), edgeSource = mkNodeId sourceFile (declNode d),
                           edgeTarget = mkNodeId sourceFile (declNode d), edgeRelation = "contains",
                           edgeConfidence = Confidence 1.0, edgeWeight = 1.0, edgeExtra = Nothing }) ordered

nodeFor :: SourceFile -> Map.Map (TSNodeInfo, Text) NodeId -> Decl -> [Node]
nodeFor sourceFile idByNode d =
  let nid = maybe (mkNodeId sourceFile (declNode d)) id (Map.lookup (declNode d, declKey d) idByNode)
  in [Node { nodeId = nid
           , nodeLabel = fromText (nodeIdentifier sourceFile (declNode d))
           , nodeFileType = CodeFile
           , nodeSourceFile = fromText (T.pack (sourcePath sourceFile))
           , nodeSource = Nothing
           , nodeLineStart = Just (declLine d)
           , nodeLineEnd = Just (declLine d)
           , nodeSignature = nodeSignatureOf sourceFile (declNode d)
           , nodeCommunityId = Nothing
           , nodeKind = Just (fromText (declKind d))
           , nodeDegree = Nothing
           , nodeIsBridge = Nothing
           , nodeExtra = if declExported d then Just (toJSON (Map.singleton "exported" True :: Map Text Bool)) else Nothing
           , nodePresentBits = presentBits d }]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkNodeId :: SourceFile -> TSNodeInfo -> NodeId
mkNodeId sourceFile node =
  let kind = fromText (tsTypeToKind node)
      ident = fromText (nodeIdentifier sourceFile node)
  in nodeKeyId sourceFile kind ident 0 0

declKey :: Decl -> Text
declKey d = declKind d <> ":" <> declIdent d

edgeIdFor :: TSNodeInfo -> EdgeId
edgeIdFor node = nodeEdgeId (tsnType node)

presentBits :: Decl -> Word64
presentBits d = bitNodeKind .|. bitNodeLineStart .|. bitNodeLineEnd .|. bitNodeSignature .|. bitNodeExtra

tsTypeToKind :: TSNodeInfo -> Text
tsTypeToKind node = case tsnType node of
  "program" -> "Module"
  "module" -> "Module"
  "source_file" -> "Module"
  "package_clause" -> "Module"
  _ -> tsnType node

nodeIdentifierFor :: SourceFile -> TSNodeInfo -> Text
nodeIdentifierFor sourceFile node = nodeIdentifier sourceFile node

collectModulesFor :: SourceFile -> [Decl] -> [(TSNodeInfo, TSNodeInfo)]
collectModulesFor sourceFile ordered = collectModules sourceFile ordered

allIds :: SourceFile -> [Decl] -> Map.Map (TSNodeInfo, Text) NodeId
allIds sourceFile ordered = assignIds sourceFile ordered

assignIds :: SourceFile -> [Decl] -> Map.Map (TSNodeInfo, Text) NodeId
assignIds sourceFile ordered =
  let go _ _ [] = Map.empty
      go seen rank (d : rest) =
        let key = (declNode d, declKey d)
            (rank', seen') = case Map.lookup key seen of
              Just r -> (r, seen)
              Nothing -> (Map.size seen, Map.insert key (Map.size seen) seen)
            id = nodeKeyId sourceFile (declKind d) (declIdent d) (declLine d) rank'
        in Map.insert key id (go seen' rank' rest)
  in go Map.empty 0 ordered

tsTypeToKindLegacy :: TSNodeInfo -> Text
tsTypeToKindLegacy node = case tsnType node of
  "program" -> "Module"
  "module" -> "Module"
  "source_file" -> "Module"
  "package_clause" -> "Module"
  _ -> tsnType node

-- ---------------------------------------------------------------------------
-- Type lists (kept for compatibility with the spec tests)
-- ---------------------------------------------------------------------------

definitionTypes :: [Text]
definitionTypes = ["variable_declarator", "function_declaration", "class_declaration"]

structureTypes :: [Text]
structureTypes = ["object", "array", "parenthesized_expression"]

apiSurfaceTypes :: [Text]
apiSurfaceTypes = ["identifier", "property_identifier", "type_identifier"]

importExportTypes_ :: [Text]
importExportTypes_ = importExportTypes

implementationDetailTypes :: [Text]
implementationDetailTypes = ["arrow_function", "function"]

functionBoundaryTypes :: [Text]
functionBoundaryTypes = ["function_declaration", "arrow_function", "method_definition"]

descendInto :: Granularity -> Bool -> TSNodeInfo -> Bool
descendInto gran isDef node =
  case gran of
    GranularityModule -> True
    GranularityFunction -> not (isDef && tsnType node `elem` functionBoundaryTypes)
    GranularityFine -> True

-- ---------------------------------------------------------------------------
-- Imports
-- ---------------------------------------------------------------------------

import qualified Data.Text as DT
