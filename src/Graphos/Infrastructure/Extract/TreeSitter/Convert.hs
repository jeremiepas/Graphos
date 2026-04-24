-- | Convert tree-sitter AST nodes into Graphos domain types.
-- Pure conversion — no IO.
{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitter.Convert
  ( tsNodesToExtraction
  , tsNodeToGraphNodes
  , tsNodeToGraphEdges
  , definitionTypes
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Types
  ( Node(..), Edge(..), Extraction(..), emptyExtraction
  , NodeId, FileType(..), Relation(..), Confidence(..)
  )
import Graphos.Infrastructure.Extract.TreeSitter.Core (TSNodeInfo(..))

-- | Tree-sitter node types that represent meaningful definitions.
-- Covers: TypeScript/JSX, Python, Go, Rust, Haskell, JSON.
definitionTypes :: [String]
definitionTypes =
  [ -- Cross-language (TypeScript, Python, Go, Haskell, etc.)
    "module", "program", "source_file"
  , "function_declaration", "function_definition", "function"
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
    -- Python-specific
  , "decorated_definition", "decorator"
  , "expression_statement", "assignment", "augmented_assignment"
  , "return_statement", "parameter", "default_parameter", "typed_parameter"
  , "for_statement", "while_statement", "if_statement"
  , "with_statement", "try_statement", "except_clause"
    -- Go-specific
  , "package_clause"
    -- Rust-specific
  , "function_item", "struct_item", "mod_item", "use_declaration"
  , "static_item", "extern_item", "attribute_item"
    -- Haskell-specific
  , "declarations", "instance_declaration", "pattern_declaration", "type_signature"
    -- JSON
  , "document", "object", "array", "pair"
  ]

-- | Convert a tree of TSNodeInfo into an Extraction.
tsNodesToExtraction :: FilePath -> [TSNodeInfo] -> Extraction
tsNodesToExtraction filePath nodes =
  let graphNodes = concatMap (tsNodeToGraphNodes filePath) nodes
      graphEdges = concatMap (tsNodeToGraphEdges filePath Nothing) nodes
  in emptyExtraction
    { extractionNodes = graphNodes
    , extractionEdges = graphEdges
    }

-- | Convert a TSNodeInfo and its children into Graphos Nodes.
tsNodeToGraphNodes :: FilePath -> TSNodeInfo -> [Node]
tsNodeToGraphNodes filePath node
  | tsnType node `elem` definitionTypes && tsnIsNamed node =
      [makeNode filePath node] ++ concatMap (tsNodeToGraphNodes filePath) (tsnChildren node)
  | otherwise = concatMap (tsNodeToGraphNodes filePath) (tsnChildren node)

-- | Convert a TSNodeInfo tree into Graphos Edges (Contains parent→child).
tsNodeToGraphEdges :: FilePath -> Maybe Text -> TSNodeInfo -> [Edge]
tsNodeToGraphEdges filePath parentLabel node =
  let myLabel = tsNodeLabel node
      isDef = tsnType node `elem` definitionTypes && tsnIsNamed node
      -- Parent → child edge
      myEdges = case (parentLabel, isDef) of
        (Just p, True) ->
          [ Edge
            { edgeSource        = makeNodeId filePath p
            , edgeTarget        = makeNodeId filePath myLabel
            , edgeRelation      = Contains
            , edgeConfidence    = Extracted
            , edgeConfidenceScore = 1.0
            , edgeSourceFile    = T.pack filePath
            , edgeSourceLocation = Just $ T.pack $ "L" ++ show (tsnStartRow node + 1)
            , edgeWeight        = 1.0
            }
          ]
        _ -> []
      -- Recurse into children
      childEdges = if isDef
        then concatMap (tsNodeToGraphEdges filePath (Just myLabel)) (tsnChildren node)
        else concatMap (tsNodeToGraphEdges filePath parentLabel) (tsnChildren node)
  in myEdges ++ childEdges

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Make a Graphos Node from a TSNodeInfo.
makeNode :: FilePath -> TSNodeInfo -> Node
makeNode filePath node = Node
  { nodeId           = makeNodeId filePath (tsNodeLabel node)
  , nodeLabel        = tsNodeLabel node
  , nodeFileType     = CodeFile
  , nodeSourceFile   = T.pack filePath
  , nodeSourceLocation = Just $ T.pack $ "L" ++ show (tsnStartRow node + 1)
  , nodeLineEnd      = Just (tsnEndRow node + 1)
  , nodeKind         = Just $ T.pack $ tsTypeToKind (tsnType node)
  , nodeSignature    = Nothing
  , nodeSourceUrl    = Nothing
  , nodeCapturedAt   = Nothing
  , nodeAuthor       = Nothing
  , nodeContributor  = Nothing
  }

-- | Get a display label for a node — use text if available, otherwise type.
tsNodeLabel :: TSNodeInfo -> Text
tsNodeLabel node =
  let raw = let t = tsnText node in if T.null t then T.pack (tsnType node) else t
  in T.filter (\c -> c /= '\n' && c /= '\r') raw

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
  let stem = T.pack $ takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse filePath
      dirPart = reverse $ dropWhile (/= '/') $ reverse filePath
      dirHash = abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) (0 :: Int) (T.pack dirPart) `mod` 65536)
      hashPrefix = T.pack $ show dirHash
      safeName = T.filter (\c -> c /= '\n' && c /= '\r' && c /= '"' && c /= '\'' && c /= '`') name
  in hashPrefix <> "_" <> stem <> "_" <> safeName