-- | Convert tree-sitter AST nodes into Graphos domain types.
-- Pure conversion — no IO.
--
-- Node volume is controlled by 'Granularity':
--
--   * 'GranularityFine'     — all whitelisted definition types, full recursion.
--   * 'GranularityFunction' — structure + API surface only; recursion STOPS at
--     function/method/constructor boundaries, so nothing inside a function
--     body produces a node regardless of its AST type.
--   * 'GranularityFile'     — only the root module/structure node(s).
{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitter.Convert
  ( tsNodesToExtraction
  , tsNodeToGraphNodes
  , tsNodeToGraphEdges
  , definitionTypes
  , structureTypes
  , apiSurfaceTypes
  , importExportTypes
  , implementationDetailTypes
  , functionBoundaryTypes
    , makeNodeId
    , fileModuleNode
    , tsNodeLabel
    , tsNodeUntruncatedLabel
    -- | Group 3 node-identity helpers (AVI-984): stable identity scheme
    --   <dirHash>_<stem>_<kind>:<ident> with file-wide collision ranks.
    , SourceFile(..)
    , mkSourceFile
    , nodeKeyId
   ) where


import Data.List (isPrefixOf)
import System.FilePath (takeExtension)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Data.Maybe (fromMaybe)
import Data.Aeson (toJSON)

import Graphos.Domain.Config (Granularity(..))
import Graphos.Domain.Types
  ( Node(..), Edge(..), Extraction(..), extractionFromLists
  , NodeId, FileType(..), Relation(..), Confidence(..), EdgeId(..)
  , bitNodeKind, bitNodeLineStart, bitNodeLineEnd
  )
import Data.Bits ((.|.))
import Graphos.Infrastructure.Extract.TreeSitter.Core (TSNodeInfo(..), normalizeText, defaultTruncationBudget, truncateWithElision)
import Graphos.Infrastructure.Extract.TreeSitter.Resolver (resolveImport)

-- | Tier 1 — file/module structure. Emitted at every granularity level.
structureTypes :: [String]
structureTypes =
  [ "module", "program", "source_file"
  , "package_clause"        -- Go
  , "mod_item"              -- Rust
  , "document"              -- JSON, YAML
  , "translation_unit"      -- C++
  , "stream"                -- YAML
  ]

-- | Tier 2 — API surface: definitions visible from outside a function body.
-- Emitted at 'GranularityFunction' and 'GranularityFine'.
apiSurfaceTypes :: [String]
apiSurfaceTypes =
  [ "function_declaration", "function_definition", "function"
  , "method_declaration", "method_definition"
  , "class_declaration", "class_definition", "class"
  , "interface_declaration", "interface_definition", "interface"
  , "type_declaration", "type_alias_declaration", "type_definition"
  , "type_alias", "type_item", "type_identifier"
  , "enum_declaration", "enum_definition", "enum"
  , "enum_item"
  , "variable_declaration", "lexical_declaration", "variable_declarator"
  , "var_declaration"
  , "const_declaration", "const_item"
  , "let_declaration"
  , "import_declaration", "import_statement", "import_from_statement"
  , "export_statement", "export_default_declaration"
  , "property_signature", "property_definition", "property_declaration"
  , "arrow_function", "generator_function_declaration"
  , "abstract_class_declaration", "abstract_interface_declaration"
  , "constructor", "field_declaration"
  , "trait", "trait_item", "impl_block", "impl_item"
  , "data_type", "new_type", "type_synonym"
  , "decorated_definition"
    -- Rust
  , "function_item", "struct_item", "use_declaration"
  , "static_item", "extern_item", "attribute_item"
    -- Haskell
  , "declarations", "instance_declaration", "pattern_declaration", "type_signature"
    -- Ruby
  , "method", "singleton_method"
    -- C++
  , "class_specifier", "struct_specifier", "union_specifier", "enum_specifier"
  , "namespace_definition", "template_declaration", "alias_declaration"
  , "declaration", "preproc_include"
    -- SQL
  , "create_table_statement", "create_function_statement", "create_type_statement"
  , "create_domain_statement", "create_index_statement", "create_schema_statement"
  , "create_sequence"
  ]

importExportTypes :: [String]
importExportTypes =
  [ "import_declaration", "import_statement", "import_from_statement"
  , "export_statement", "export_default_declaration"
  ]

-- | Tier 3 — implementation detail: statements, parameters, locals, JSON
-- values. Emitted only at 'GranularityFine'.
implementationDetailTypes :: [String]
implementationDetailTypes =
  [ "decorator"
  , "expression_statement", "assignment", "augmented_assignment"
  , "return_statement", "parameter", "default_parameter", "typed_parameter"
  , "for_statement", "while_statement", "if_statement"
  , "with_statement", "try_statement", "except_clause"
    -- JSON values: every pair/object/array of every JSON file
  , "object", "array", "pair"
    -- SQL DML
  , "select_statement", "insert_statement", "update_statement", "delete_statement"
  ]

-- | AST types that delimit a function body. At 'GranularityFunction' the
-- walker does not descend past these, making function bodies opaque.
functionBoundaryTypes :: [String]
functionBoundaryTypes =
  [ "function_declaration", "function_definition", "function"
  , "method_declaration", "method_definition"
  , "arrow_function", "generator_function_declaration"
  , "constructor"
  , "function_item"       -- Rust
  , "method", "singleton_method"  -- Ruby
  , "create_function_statement"   -- SQL
  ]

-- | All node types that can represent definitions (fine level).
-- Kept for backward compatibility and as the fine-level whitelist.
definitionTypes :: [String]
definitionTypes = structureTypes ++ apiSurfaceTypes ++ implementationDetailTypes

-- | Whitelist for a granularity level.
typesFor :: Granularity -> [String]
typesFor GranularityFine     = definitionTypes
typesFor GranularityFunction = structureTypes ++ apiSurfaceTypes
typesFor GranularityFile     = structureTypes

-- | Whether to recurse into a node's children at the given level.
-- @isDef@ tells whether the current node was emitted as a definition.
descendInto :: Granularity -> Bool -> TSNodeInfo -> Bool
descendInto GranularityFine     _     _    = True
descendInto GranularityFile     _     _    = False
descendInto GranularityFunction isDef node =
  not (isDef && tsnType node `elem` functionBoundaryTypes)

-- | Convert a tree of TSNodeInfo into an Extraction at a given granularity.
tsNodesToExtraction :: Granularity -> FilePath -> [TSNodeInfo] -> Extraction
tsNodesToExtraction gran filePath nodes =
  let results = map (\node -> (tsNodeToGraphNodes gran filePath node, tsNodeToGraphEdges gran filePath Nothing node)) nodes
      graphNodes = concatMap fst results
      extraNodes = concatMap (snd . snd) results
      graphEdges = concatMap (fst . snd) results
  in extractionFromLists (graphNodes ++ extraNodes) graphEdges

-- | Convert a TSNodeInfo and its children into Graphos Nodes and Edges.
tsNodeToGraphNodes :: Granularity -> FilePath -> TSNodeInfo -> [Node]
tsNodeToGraphNodes gran filePath node =
  let isDef = tsnType node `elem` typesFor gran && tsnIsNamed node
      self = [makeNode filePath node | isDef]
      children = if descendInto gran isDef node
        then concatMap (tsNodeToGraphNodes gran filePath) (tsnChildren node)
        else map (makeNode filePath) (opaqueBodyUses gran isDef node)
  in self ++ children

-- | Convert a TSNodeInfo tree into Graphos Edges (Contains parent→child).
tsNodeToGraphEdges :: Granularity -> FilePath -> Maybe Text -> TSNodeInfo -> ([Edge], [Node])
tsNodeToGraphEdges gran filePath parentLabel node =
  let myLabel = tsNodeLabel node
      isDef = tsnType node `elem` typesFor gran && tsnIsNamed node
      tType = tsnType node
      
      containsEdge p targetId = Edge
            { edgeId        = EdgeId (makeNodeId filePath p <> "->" <> targetId <> ":contains")
            , edgeSource    = makeNodeId filePath p
            , edgeTarget    = targetId
            , edgeRelation  = Contains
            , edgeConfidence = Confidence 1.0
            , edgeWeight    = 1.0
            , edgeExtra     = Nothing
            }

      -- Parent → child edge (Contains)
      myEdges = case (parentLabel, isDef) of
        (Just p, True) ->
          [ containsEdge p (if tType `elem` perOccurrenceTypes
                              then defNodeId filePath node
                              else makeNodeId filePath myLabel) ]
        _ -> []

      -- Function → `use` edges for `use` items inside an opaque function body
      bodyUseEdges = [ containsEdge myLabel (defNodeId filePath u) | u <- opaqueBodyUses gran isDef node ]

      -- Imports/Exports edges
      -- Go/Rust/Ruby imports are resolved with repository context in
      -- Graphos.Infrastructure.Extract.TreeSitter.Imports; Go's
      -- @import_declaration@ must not go through the TypeScript specifier path.
      (importEdges, importNodes) = if tType `elem` importExportTypes && takeExtension filePath /= ".go"
        then case extractSpecifier node of
                Just (targetPath, targetName) ->
                  let targetId = if "external:" `isPrefixOf` targetPath
                                 then makeNodeId "external" targetName
                                 else makeNodeId targetPath targetName
                      edge = Edge
                        { edgeId        = EdgeId (makeNodeId filePath (fromMaybe "" parentLabel) <> "->" <> targetId <> ":imports")
                        , edgeSource    = makeNodeId filePath (fromMaybe "" parentLabel)
                        , edgeTarget    = targetId
                        , edgeRelation  = Imports
                        , edgeConfidence = Confidence 1.0
                        , edgeWeight    = 1.0
                        , edgeExtra     = if tType /= "import_declaration" && tType /= "import_statement" && tType /= "import_from_statement"
                                           then Just (toJSON (T.pack "re-export"))
                                           else Nothing
                        }
                      targetNode = Node
                        { nodeId           = targetId
                        , nodeLabel        = fromText targetName
                        , nodeFileType     = CodeFile
                        , nodeSourceFile   = fromText (T.pack targetPath)
                        , nodeSource       = Nothing
                        , nodeLineStart    = Nothing
                        , nodeLineEnd      = Nothing
                        , nodeSignature    = Nothing
                        , nodeCommunityId  = Nothing
                        , nodeKind         = Just (fromText "External")
                        , nodeDegree       = Nothing
                        , nodeIsBridge     = Nothing
                        , nodeExtra        = Nothing
                        , nodePresentBits  = bitNodeKind
                        }
                  in ([edge], [targetNode])
                Nothing -> ([], [])
        else ([], [])

      -- Recurse into children
      (childEdges, childNodes) = 
        if not (descendInto gran isDef node)
        then ([], [])
        else if isDef
             then foldr (\child (es, ns) -> 
                    let (e, n) = tsNodeToGraphEdges gran filePath (Just myLabel) child
                    in (e ++ es, n ++ ns)) ([], []) (tsnChildren node)
             else foldr (\child (es, ns) -> 
                    let (e, n) = tsNodeToGraphEdges gran filePath parentLabel child
                    in (e ++ es, n ++ ns)) ([], []) (tsnChildren node)

  in (myEdges ++ bodyUseEdges ++ importEdges ++ childEdges, importNodes ++ childNodes)

      where
        extractSpecifier :: TSNodeInfo -> Maybe (FilePath, Text)
        extractSpecifier n =
          let tailText = extractTail (tsnText n)
          in if T.null tailText
             then Nothing
             else let raw = T.strip (T.drop (T.length "from ") tailText)
                      rawNoSemi = if T.null raw || T.last raw /= ';' then raw else T.init raw
                      specifier = stripQuotes (T.stripEnd rawNoSemi)
                  in if T.null specifier
                     then Nothing
                     else resolveImport filePath (T.unpack specifier)

-- | Strip surrounding single/double quotes/backticks from a specifier.
stripQuotes :: Text -> Text
stripQuotes t0
  | T.null t0 = t0
  | h `elem` quotes && T.last t0 `elem` quotes && T.length t0 >= 2 = T.init (T.tail t0)
  | h `elem` quotes = T.tail t0
  | otherwise = t0
  where
    h = T.head t0
    quotes :: [Char]
    quotes = "'\"`"


-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | AST types that get one node per occurrence: the same text repeated in a
-- file (a Rust @use super::*;@ at module level and again in @mod tests@) is
-- several items, so the id carries the start line.
perOccurrenceTypes :: [String]
perOccurrenceTypes = ["use_declaration"]

-- | Node id of a definition node.
defNodeId :: FilePath -> TSNodeInfo -> NodeId
defNodeId filePath node
  | tsnType node `elem` perOccurrenceTypes =
      makeNodeId filePath (tsNodeUntruncatedLabel node) <> "#L" <> T.pack (show (tsnStartRow node + 1))
  | otherwise = makeNodeId filePath (tsNodeUntruncatedLabel node)

-- | The file's module node (the root @source_file@/@program@/… node), the
-- source of the file's import edges. When the grammar could not parse the
-- file and the root is an @ERROR@ node, the root is still returned, as a
-- @Module@, so that the imports read from the partial tree have a source.
fileModuleNode :: FilePath -> [TSNodeInfo] -> Maybe Node
fileModuleNode filePath nodes =
  case filter tsnIsNamed nodes of
    (n : _)
      | tsnType n `elem` structureTypes -> Just (makeNode filePath n)
      | otherwise -> Just (makeNode filePath n) { nodeKind = Just (fromText "Module") }
    [] -> Nothing

-- | Import items (Rust @use@) inside a function body that the walker does
-- not enter at 'GranularityFunction'. They still get a node each, contained
-- by the function, so a body-local @use@ is not lost.
opaqueBodyUses :: Granularity -> Bool -> TSNodeInfo -> [TSNodeInfo]
opaqueBodyUses GranularityFunction True node
  | tsnType node `elem` functionBoundaryTypes = concatMap uses (tsnChildren node)
  where
    uses n | tsnType n == "use_declaration" && tsnIsNamed n = [n]
           | otherwise = concatMap uses (tsnChildren n)
opaqueBodyUses _ _ _ = []

-- | Make a Graphos Node from a TSNodeInfo.
makeNode :: FilePath -> TSNodeInfo -> Node
makeNode filePath node = Node
  { nodeId           = defNodeId filePath node
  , nodeLabel        = fromText (tsNodeLabel node)
  , nodeFileType     = CodeFile
  , nodeSourceFile   = fromText (T.pack filePath)
  , nodeSource       = Nothing
  , nodeLineStart    = Just (tsnStartRow node + 1)
  , nodeCommunityId  = Nothing
  , nodeDegree       = Nothing
  , nodeIsBridge     = Nothing
  , nodeExtra        = Nothing
  , nodeLineEnd      = Just (tsnEndRow node + 1)
           , nodeKind         = Just (fromText (T.pack $ tsTypeToKind (tsnType node)))
  , nodeSignature    = Nothing
  , nodePresentBits  = bitNodeLineStart .|. bitNodeLineEnd .|. bitNodeKind
  }

-- | Get the untruncated, normalized label for a node.
tsNodeUntruncatedLabel :: TSNodeInfo -> Text
tsNodeUntruncatedLabel node =
  let raw = let t = tsnText node in if T.null t then T.pack (tsnType node) else t
  in normalizeText raw

-- | Get a display label for a node — use text if available, otherwise type.
-- For imports/exports, preserves the specifier via middle-elision.
tsNodeLabel :: TSNodeInfo -> Text
tsNodeLabel node =
  let untruncated = tsNodeUntruncatedLabel node
      tType = tsnType node
  in if tType `elem` importExportTypes && T.length untruncated > defaultTruncationBudget
     then truncateWithElision defaultTruncationBudget untruncated (extractTail untruncated)
     else untruncated

-- | Extract the 'from <specifier>' part of a declaration.
extractTail :: Text -> Text
extractTail t =
  case T.breakOnEnd "from " t of
    (prefix, suffix) | not (T.null prefix) -> "from " <> suffix
    _ -> ""

-- | Convert tree-sitter type to human-readable kind.
tsTypeToKind :: String -> String
tsTypeToKind t = case t of
  -- Functions / Methods
  "function_declaration" -> "Function"
  "function" -> "Function"
  "function_definition" -> "Function"
  "function_item" -> "Function"
  "method_declaration" -> "Method"
  "method_definition" -> "Method"
  "arrow_function" -> "Function"
  "generator_function_declaration" -> "Function"
  "constructor" -> "Constructor"
  -- Classes / Interfaces / Traits
  "class_declaration" -> "Class"
  "class_definition" -> "Class"
  "class" -> "Class"
  "abstract_class_declaration" -> "Class"
  "interface_declaration" -> "Interface"
  "interface_definition" -> "Interface"
  "interface" -> "Interface"
  "trait" -> "Trait"
  "trait_item" -> "Trait"
  "impl_block" -> "Impl"
  "impl_item" -> "Impl"
  "struct_item" -> "Struct"
  -- Types / Enums
  "type_declaration" -> "Type"
  "type_alias_declaration" -> "Type"
  "type_definition" -> "Type"
  "type_alias" -> "Type"
  "type_item" -> "Type"
  "type_synonym" -> "Type"
  "data_type" -> "Type"
  "new_type" -> "Type"
  "enum_declaration" -> "Enum"
  "enum_definition" -> "Enum"
  "enum" -> "Enum"
  "enum_item" -> "Enum"
  -- Variables / Constants
  "variable_declaration" -> "Variable"
  "lexical_declaration" -> "Variable"
  "variable_declarator" -> "Variable"
  "var_declaration" -> "Variable"
  "const_declaration" -> "Constant"
  "const_item" -> "Constant"
  "let_declaration" -> "Variable"
  "static_item" -> "Constant"
  -- Imports / Exports
  "import_declaration" -> "Import"
  "import_statement" -> "Import"
  "import_from_statement" -> "Import"
  "use_declaration" -> "Import"
  "export_statement" -> "Export"
  "export_default_declaration" -> "Export"
  -- Properties / Fields
  "property_signature" -> "Property"
  "property_definition" -> "Property"
  "property_declaration" -> "Property"
  "field_declaration" -> "Field"
  "parameter" -> "Parameter"
  "default_parameter" -> "Parameter"
  "typed_parameter" -> "Parameter"
  "decorator" -> "Decorator"
  -- Python-specific
  "decorated_definition" -> "Definition"
  "expression_statement" -> "Statement"
  "assignment" -> "Assignment"
  "augmented_assignment" -> "Assignment"
  "return_statement" -> "Return"
  "for_statement" -> "Loop"
  "while_statement" -> "Loop"
  "if_statement" -> "Conditional"
  "with_statement" -> "Context"
  "try_statement" -> "Try"
  "except_clause" -> "Except"
  -- Go-specific
  "package_clause" -> "Module"
  "type_identifier" -> "Type"
  -- Rust-specific
  "extern_item" -> "Extern"
  "attribute_item" -> "Attribute"
  "mod_item" -> "Module"
  -- Haskell-specific
  "declarations" -> "Declarations"
  "instance_declaration" -> "Instance"
  "pattern_declaration" -> "Pattern"
  "type_signature" -> "Signature"
  -- Ruby-specific
  "method" -> "Method"
  "singleton_method" -> "Method"
  -- C++-specific
  "translation_unit" -> "Module"
  "class_specifier" -> "Class"
  "struct_specifier" -> "Struct"
  "union_specifier" -> "Struct"
  "enum_specifier" -> "Enum"
  "namespace_definition" -> "Namespace"
  "template_declaration" -> "Template"
  "alias_declaration" -> "Type"
  "declaration" -> "Declaration"
  "preproc_include" -> "Import"
  -- SQL-specific
  "create_table_statement" -> "Table"
  "create_function_statement" -> "Function"
  "create_type_statement" -> "Type"
  "create_domain_statement" -> "Type"
  "create_index_statement" -> "Index"
  "create_schema_statement" -> "Schema"
  "create_sequence" -> "Sequence"
  "select_statement" -> "Query"
  "insert_statement" -> "Query"
  "update_statement" -> "Query"
  "delete_statement" -> "Query"
  -- YAML
  "stream" -> "Document"
  -- General
  "module" -> "Module"
  "program" -> "Module"
  "source_file" -> "Module"
  -- JSON
  "document" -> "Document"
  "object" -> "Object"
  "array" -> "Array"
  "pair" -> "Property"
  _ -> t

-- | Create a node ID from file path and name.
makeNodeId :: FilePath -> Text -> NodeId
makeNodeId filePath name =
  let normalizedName = normalizeText name
      stem = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      hashPrefix = T.pack $ show dirHash
      safeName = T.filter (\c -> c /= '"' && c /= '\'' && c /= '`') normalizedName
   in hashPrefix <> "_" <> stem <> "_" <> safeName

-- ---------------------------------------------------------------------------
-- Group 3 node identity (AVI-984 / AVI-973)
--
-- Stable identity scheme: "<dirHash>_<stem>_<kind>:<ident>" with an optional
-- file-wide "#L<line>" suffix for collision rank. These helpers are layered on
-- top of the proven conversion functions above; they do not alter the tested
-- 'makeNodeId' behaviour.
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
       { sourcePath     = T.pack filePath
       , sourceDirHash  = dirHash
       , sourceStem     = stem
       , sourceFileName = name
       }

-- | Directory portion of a path (everything before its final '/').
takeDirectory' :: FilePath -> FilePath
takeDirectory' p =
  case break (== '/') (reverse p) of
    (_, '/' : dirRest) -> reverse dirRest
    _                  -> ""

-- | File name portion of a path (its final component).
takeFileName' :: FilePath -> FilePath
takeFileName' p = reverse (fst (break (== '/') (reverse p)))

-- | File stem (file name without its final extension).
takeBaseName' :: FilePath -> FilePath
takeBaseName' f =
  let fn = reverse (takeFileName' f)
      (stem, _) = break (== '.') fn
  in if null stem then fn else reverse stem

-- | Stable identifier for a node under the Group 3 identity scheme. When
-- `rank > 0` the declaration gets a file-wide `#L<line>` suffix so that
-- collisions are disambiguated deterministically.
nodeKeyId :: SourceFile -> Text -> Text -> Int -> Int -> NodeId
nodeKeyId sourceFile kind ident line rank =
  let dirHash' = T.pack (show (sourceDirHash sourceFile))
      base     = dirHash' <> "_" <> sourceStem sourceFile <> "_" <> kind <> ":" <> ident
  in if rank > 0 then base <> "#L" <> T.pack (show line) else base
