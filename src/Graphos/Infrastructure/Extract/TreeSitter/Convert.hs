-- | Convert tree-sitter AST nodes into Graphos domain types.
-- Pure conversion — no IO.
--
-- Node identity (D1) is derived from the file path, the node kind and the
-- declared identifier:
--
--   * id          = `<dirHash>_<stem>_<kind>:<identifier>`
--   * label       = identifier (file name for modules, resolved specifier for
--                   imports / re-exports)
--   * signature   = normalized declaration text under the truncation budget
--
-- Identifier-less wrapper nodes (`export_statement`, `decorated_definition`)
-- are suppressed; their inner declaration is emitted once and flagged
-- exported. See the `extraction-quality` / `identifier-resolution` specs.
{-# LANGUAGE OverloadedStrings #-}
module Graphos.Infrastructure.Extract.TreeSitter.Convert
  ( tsNodesToExtraction
  , makeNodeId
  , mkSourceFile
  , tsNodeLabel
  , tsNodeSignature
  , extractSpecifier
  , lastQuoted
  , nodeIdentifier
  , definitionTypes
  , structureTypes
  , apiSurfaceTypes
  , importExportTypes
  , implementationDetailTypes
  , functionBoundaryTypes
    ) where

import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Data.Maybe (fromMaybe)
import Data.Aeson (object, (.=), toJSON)
import Data.Word (Word64)

import qualified Data.Map.Strict as Map

import Graphos.Domain.Config (Granularity(..))
import Graphos.Domain.Types
  ( Node(..), Edge(..), Extraction(..), extractionFromLists
  , NodeId, FileType(..), Relation(..), Confidence(..), EdgeId(..)
  , bitNodeKind, bitNodeLineStart, bitNodeLineEnd
  , bitNodeSignature, bitNodeExtra )
import Data.Bits ((.|.))
import Graphos.Infrastructure.Extract.TreeSitter.Core
  ( TSNodeInfo(..), SourceFile(..), mkSourceFile
  , normalizeText, sourceDirHash, sourceStem, sourceFilePathStr
  , extractIdentifier, truncateWithElision )
import Graphos.Infrastructure.Extract.TreeSitter.Resolver (resolveImport)

-- ───────────────────────────────────────────────
-- Whitelisted AST types (unchanged tiers)
-- ───────────────────────────────────────────────

-- | Tier 1 — file/module structure. Emitted at every granularity level.
structureTypes :: [String]
structureTypes =
  [ "module", "program", "source_file"
  , "package_clause"        -- Go
  , "mod_item"              -- Rust
  , "document"              -- JSON
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
  ]

-- | AST types that carry no identifier of their own and must never be emitted
-- as a node in their own right. Their single inner declaration is emitted
-- once (flagged exported); callers recurse through them unchanged.
wrapperTypes :: [String]
wrapperTypes = [ "export_statement", "decorated_definition" ]

-- | AST types whose identifier is the file name rather than a declared name.
moduleTypes :: [String]
moduleTypes =
  [ "module", "program", "source_file"
  , "package_clause", "mod_item"
  ]

-- | All node types that can represent definitions (fine level).
definitionTypes :: [String]
definitionTypes = structureTypes ++ apiSurfaceTypes ++ implementationDetailTypes

-- | Whitelist for a granularity level.
typesFor :: Granularity -> [String]
typesFor GranularityFunction = structureTypes ++ apiSurfaceTypes
typesFor GranularityFine     = definitionTypes
typesFor GranularityFile     = structureTypes

-- | Whether to recurse into a node's children at the given level.
-- @isDef@ tells whether the current node was emitted as a definition.
descendInto :: Granularity -> Bool -> TSNodeInfo -> Bool
descendInto GranularityFile     _     _    = False
descendInto GranularityFunction isDef node =
  not (isDef && tsnType node `elem` functionBoundaryTypes)
descendInto GranularityFine     _     _    = True

-- ───────────────────────────────────────────────
-- Identifier, label, signature (D1 / labels)
-- ───────────────────────────────────────────────

-- | The identifier component shared by a node's id and its label:
--
--   * module nodes        — the file stem
--   * import / re-export  — the resolved specifier
--   * everything else     — the first named identifier-class child
nodeIdentifier :: SourceFile -> Granularity -> TSNodeInfo -> Maybe Text
nodeIdentifier sf _ node =
  case tsnType node of
    t | t `elem` moduleTypes       -> Just (sourceStem sf)
      | t `elem` importExportTypes -> Just $ fromMaybe "" (extractSpecifier sf node)
      | otherwise                  -> extractIdentifier node

-- | Display label for a node: its identifier (falling back to the normalized
-- node text when no identifier resolves).
tsNodeLabel :: SourceFile -> Granularity -> TSNodeInfo -> Text
tsNodeLabel sf gran node =
  fromMaybe (normalizeText (tsnText node)) (nodeIdentifier sf gran node)

-- | Normalized declaration text stored in `nodeSignature`:
--
--   * module nodes        — 'Nothing' (a module node carries no signature)
--   * import / re-export  — the normalized declaration with the trailing
--                           `from '<specifier>'` clause preserved, eliding the
--                           middle only when it exceeds the budget
--   * everything else     — the normalized declaration, elided in the middle
--                           only when it exceeds the budget
tsNodeSignature :: Int -> TSNodeInfo -> Maybe Text
tsNodeSignature budget node =
  case tsnType node of
    t | t `elem` moduleTypes -> Nothing
      | t `elem` importExportTypes ->
          Just $ sigWithTail budget (normalizeText (tsnText node))
               (extractImportTail (tsnText node))
      | otherwise ->
          let norm = normalizeText (tsnText node)
          in if T.length norm <= budget then Just norm
               else Just (T.take budget norm)
  where
    sigWithTail :: Int -> Text -> Text -> Text
    sigWithTail b raw tailText
      | T.length raw <= b = raw
      | T.null tailText = T.take b raw
      | T.length tailText > (b - 3) = T.take b raw
      | otherwise = truncateWithElision b raw tailText

-- | Extract the trailing `from '<specifier>'` clause of an import declaration
-- for signature preservation. Returns "" when absent.
extractImportTail :: Text -> Text
extractImportTail t =
  let norm = normalizeText t
      (_, tailText) = T.breakOn "from " norm
  in if T.null tailText then "" else tailText

-- | Resolve the module specifier carried by an import-like declaration.
--
-- Prefers the declaration's string-literal child (the structured form the
-- `import-resolution` spec requires); falls back to the last quoted token in
-- the node text for fixtures that carry no children. Never scans arbitrary
-- text for the `from ` keyword, so template literals inside a body yield
-- nothing.
extractSpecifier :: SourceFile -> TSNodeInfo -> Maybe Text
extractSpecifier _ node =
  case findStringLiteral (tsnChildren node) of
    Just s -> Just (stripQuotes (T.strip s))
    Nothing ->
      let t = normalizeText (tsnText node)
      in case lastQuoted t of
           Just q -> Just (stripQuotes (T.strip q))
           Nothing -> Nothing

-- | First named child whose type names a string literal.
findStringLiteral :: [TSNodeInfo] -> Maybe Text
findStringLiteral = go
  where
    isStringType ty = any (`T.isPrefixOf` T.toLower (T.pack ty))
                      [ "string", "simple_string", "quote", "fragment" ]
    go [] = Nothing
    go (c : cs)
      | tsnIsNamed c && isStringType (tsnType c) = Just (tsnText c)
      | otherwise = go cs

-- | The last single- or double-quoted token in a declaration (backticks are
-- excluded: they denote template literals, never module specifiers).
lastQuoted :: Text -> Maybe Text
lastQuoted t0
  | T.null t0 = Nothing
  | otherwise = scan (T.length t0 - 1)
  where
    scan i
      | i < 0     = Nothing
      | otherwise = case T.unpack t0 !! i of
          '\'' -> Just (quoteToken '\'' t0)
          '"'  -> Just (quoteToken '"' t0)
          _    -> scan (i - 1)
    quoteToken q t = T.reverse (T.takeWhile (/= q) (T.drop 1 (T.reverse t)))

-- | Strip matching surrounding single / double quotes from a specifier.
stripQuotes :: Text -> Text
stripQuotes t0
  | T.null t0 = t0
  | h `elem` qs && T.last t0 `elem` qs && T.length t0 >= 2 = T.init (T.tail t0)
  | h `elem` qs = T.tail t0
  | otherwise = t0
  where
    h = T.head t0
    qs :: [Char]
    qs = "'\""

-- ───────────────────────────────────────────────
-- Node identity (new layout)
-- ───────────────────────────────────────────────

-- | New node-id layout: `<dirHash>_<stem>_<kind>:<identifier>`.
--
-- The `#L<line>` overload suffix is NOT applied here; it is appended by the
-- per-file collision pass in 'tsNodesToExtraction' only for repeated base ids.
makeNodeId :: SourceFile -> Text -> Text -> NodeId
makeNodeId sf kind ident =
  sourceDirHash sf <> "_" <> sourceStem sf <> "_" <> kind <> ":" <> safeIdent
  where
    safeIdent = T.take 256 (T.filter (\c -> not (c `elem` badChars)) ident)
    badChars = [ '"', '\'', '`', '/', '\\', '\n', '\r' ]

-- ───────────────────────────────────────────────
-- Two-phase traversal: collect raw nodes, then resolve collisions
-- ───────────────────────────────────────────────

-- | A node candidate with its base id, before collision resolution.
data RawNode = RawNode
  { rnNode     :: Node            -- nodeId = base id (replaced after resolution)
  , rnBaseId   :: NodeId
  , rnLine     :: Int             -- start row (0-based) for `#L` suffix
  }

-- | A containment edge referencing base ids of source and target.
data ContainEdge = ContainEdge { ceSource :: NodeId, ceTarget :: NodeId }

-- | A resolved import target used to materialize an edge endpoint node.
data ImportTarget = ImportTarget
  { itPath       :: FilePath      -- resolved path (or "external:...")
  , itName       :: Text          -- resolved name
  , itIsReExport :: Bool
  }

-- | Accumulator threaded through a single tree traversal.
type Acc = ([RawNode], [ContainEdge], [(NodeId, ImportTarget)])

-- | Convert tree-sitter AST nodes into an Extraction at a given granularity.
--
-- Phase 1 walks the tree collecting raw node candidates plus containment and
-- import edges (keyed by base id). Phase 2 resolves in-file base-id collisions
-- to assign final ids, then threads those final ids through every edge so that
-- `contains` endpoints equal the exact emitted node ids.
tsNodesToExtraction :: SourceFile -> Granularity -> Int -> [TSNodeInfo] -> Extraction
tsNodesToExtraction sf gran budget nodes =
  let (raws, contains, imports) =
        foldr (\n (r, c, i) ->
          let (ns, ces, ies) = traverseNode sf gran budget Nothing False n
          in (r ++ ns, c ++ ces, i ++ ies)) ([], [], []) nodes

      -- Phase 2: resolve per-file collisions → (RawNode, finalId)
      resolved = resolveCollisions raws
      idAt     = Map.fromList [ (rnBaseId r, fid) | (r, fid) <- resolved ]
      remap x  = case Map.lookup x idAt of Just f -> f; Nothing -> x
      graphNodes = [ rnFinalNode r fid | (r, fid) <- resolved ]
                 ++ importTargetNodes [ t | (_src, t) <- imports ]
      edges = [ mkContainEdge (remap (ceSource e)) (remap (ceTarget e))
              | e <- contains ]
            ++ [ mkImportEdge (remap src) tgt | (src, tgt) <- imports ]
  in extractionFromLists graphNodes edges

-- | Given a file's raw nodes, assign each a final id: the first occurrence
-- (by ascending start line) of a base id keeps the plain base id; every
-- subsequent occurrence carries `#L<line>`. Deterministic and independent of
-- display labels.
resolveCollisions :: [RawNode] -> [(RawNode, NodeId)]
resolveCollisions raws =
  concatMap resolveGroup
    (groupBy (\a b -> rnBaseId a == rnBaseId b) (sortBy (comparing rnBaseId) raws))
  where
    resolveGroup rs =
      let sorted = sortBy (comparing rnLine) rs
          go [] = []
          go (r : rest)
            | null rest  = [ (r, rnBaseId r) ]
            | otherwise  = ( r, rnBaseId r )
                          : [ (r', rnBaseId r' <> "#L" <> T.pack (show (rnLine r')))
                            | r' <- rest ]
      in go sorted

-- ───────────────────────────────────────────────
-- Traversal
-- ───────────────────────────────────────────────

-- | Walk one node and its (descendable) children.
--
-- @parentEmitted@ is the base id of the nearest ancestor that was emitted as a
-- node; it threads unchanged through suppressed wrappers so an inner
-- declaration keeps its containment edge from the same ancestor. @isExported@
-- records whether the node lies within a suppressed `export` / `@decorator`
-- chain and is flagged on the emitted node.
traverseNode :: SourceFile -> Granularity -> Int -> Maybe NodeId -> Bool -> TSNodeInfo
             -> ([RawNode], [ContainEdge], [(NodeId, ImportTarget)])
traverseNode sf gran budget parentEmitted isExported node =
  let tType   = tsnType node
      isDef   = tType `elem` typesFor gran && tsnIsNamed node
      ident   = nodeIdentifier sf gran node
      kind    = T.pack (tsTypeToKind tType)
      baseId  = maybe "" (\i -> makeNodeId sf kind i) ident
      sig     = tsNodeSignature budget node
      marker  = if isExported then Just (object ["exported" .= (True :: Bool)]) else Nothing
      parentBase = fromMaybe "" parentEmitted
      containEdge = if T.null baseId then [] else [ContainEdge parentBase baseId]
      childAcc = if descendInto gran True node
                 then foldChildren sf gran budget (Just baseId) False (tsnChildren node)
                 else ([], [], [])
      (csns, cces, cims) = childAcc
  in if not isDef || tType `elem` wrapperTypes
        -- Suppress wrappers and non-definition nodes; recurse unchanged.
        then foldChildren sf gran budget parentEmitted
               (isExported || tType `elem` wrapperTypes)
               (tsnChildren node)
        else
          let emitNode = Node
                { nodeId           = baseId
                , nodeLabel        = fromText (fromMaybe "" ident)
                , nodeFileType     = CodeFile
                , nodeSourceFile   = fromText (T.pack (sourceFilePathStr sf))
                , nodeSource       = Nothing
                , nodeLineStart    = Just (tsnStartRow node + 1)
                , nodeLineEnd      = Just (tsnEndRow node + 1)
                , nodeSignature    = fmap fromText sig
                , nodeCommunityId  = Nothing
                , nodeDegree       = Nothing
                , nodeIsBridge     = Nothing
                , nodeExtra        = marker
                , nodeKind         = Just (fromText kind)
                , nodePresentBits  = presentBits sig isExported
                }
              rawNode = RawNode { rnNode = emitNode, rnBaseId = baseId, rnLine = tsnStartRow node }
              importParts = importPartsFor sf parentBase tType node
          in (rawNode : csns, containEdge ++ cces, importParts ++ cims)

-- | Build the (source id, ImportTarget) pair for an import/export node, using
-- the enclosing emitted node's id as the edge source.
importPartsFor :: SourceFile -> Text -> String -> TSNodeInfo -> [(NodeId, ImportTarget)]
importPartsFor sf parentBase _tType node =
  case extractSpecifier sf node of
    Just spec ->
      let impPath = case resolveImport (sourceFilePathStr sf) (T.unpack spec) of
                      Just (p, _) -> p
                      Nothing -> "external:" ++ T.unpack spec
          impName = case resolveImport (sourceFilePathStr sf) (T.unpack spec) of
                      Just (_, n) -> Just n
                      Nothing -> Just spec
      in [ (parentBase, ImportTarget
            { itPath = impPath
            , itName = fromMaybe spec impName
            , itIsReExport = T.isInfixOf (T.pack "export") (T.toLower (normalizeText (tsnText node))) }) ]
    Nothing -> []

-- | Fold over children, threading a parent base id and exported flag.
foldChildren :: SourceFile -> Granularity -> Int -> Maybe NodeId -> Bool -> [TSNodeInfo]
             -> Acc
foldChildren sf gran budget parentEmitted isExported children =
  foldr (\c (ns, ce, im) ->
          let (n, c_, i_) = traverseNode sf gran budget parentEmitted isExported c
          in (ns ++ n, ce ++ c_, im ++ i_)) ([], [], []) children

-- ───────────────────────────────────────────────
-- Edge + endpoint-node construction
-- ───────────────────────────────────────────────

rnFinalNode :: RawNode -> NodeId -> Node
rnFinalNode r fid = (rnNode r) { nodeId = fid }

mkContainEdge :: NodeId -> NodeId -> Edge
mkContainEdge src tgt =
  Edge
    { edgeId        = EdgeId (src <> "->" <> tgt <> ":contains")
    , edgeSource    = src
    , edgeTarget    = tgt
    , edgeRelation  = Contains
    , edgeConfidence = Confidence 1.0
    , edgeWeight    = 1.0
    , edgeExtra     = Nothing
    }

mkImportEdge :: NodeId -> ImportTarget -> Edge
mkImportEdge src tgt =
  Edge
    { edgeId        = EdgeId (src <> "->" <> targetId <> ":imports")
    , edgeSource    = src
    , edgeTarget    = targetId
    , edgeRelation  = Imports
    , edgeConfidence = Confidence 1.0
    , edgeWeight    = 1.0
    , edgeExtra     = if itIsReExport tgt
                      then Just (toJSON (T.pack "re-export"))
                      else Nothing
    }
  where
    targetId = makeNodeId (mkSourceFile (itPath tgt) "") "Module" (itName tgt)

-- | Materialize import-target endpoint nodes so edges have valid endpoints.
-- Relative imports resolve to the target file's module node; package / builtin
-- specifiers resolve to a single canonical external node identity.
importTargetNodes :: [ImportTarget] -> [Node]
importTargetNodes targets =
  [ let p = itPath t
        knd = fromText (if "external:" `T.isPrefixOf` T.pack p
                         then "External" else "Module")
    in Node
        { nodeId           = makeNodeId (mkSourceFile p "") "Module" (itName t)
        , nodeLabel        = fromText (itName t)
        , nodeFileType     = CodeFile
        , nodeSourceFile   = fromText (T.pack p)
        , nodeSource       = Nothing
        , nodeLineStart    = Nothing
        , nodeLineEnd      = Nothing
        , nodeSignature    = Nothing
        , nodeCommunityId  = Nothing
        , nodeDegree       = Nothing
        , nodeIsBridge     = Nothing
        , nodeExtra        = if itIsReExport t
                             then Just (toJSON (T.pack "re-export"))
                             else Nothing
        , nodeKind         = Just knd
        , nodePresentBits  = bitNodeKind
        }
  | t <- targets ]

-- ───────────────────────────────────────────────
-- Helpers
-- ───────────────────────────────────────────────

-- | Presence bit-field for an emitted node.
presentBits :: Maybe Text -> Bool -> Word64
presentBits sig isExported =
  bitNodeLineStart .|. bitNodeLineEnd .|. bitNodeKind
  .|. maybe 0 (\_ -> bitNodeSignature) sig
  .|. (if isExported then bitNodeExtra else 0)

-- | Convert tree-sitter type to human-readable kind.
tsTypeToKind :: String -> String
tsTypeToKind t = case t of
  "function_declaration" -> "Function"
  "function" -> "Function"
  "function_definition" -> "Function"
  "function_item" -> "Function"
  "method_declaration" -> "Method"
  "method_definition" -> "Method"
  "arrow_function" -> "Function"
  "generator_function_declaration" -> "Function"
  "constructor" -> "Constructor"
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
  "variable_declaration" -> "Variable"
  "lexical_declaration" -> "Variable"
  "variable_declarator" -> "Variable"
  "var_declaration" -> "Variable"
  "const_declaration" -> "Constant"
  "const_item" -> "Constant"
  "let_declaration" -> "Variable"
  "static_item" -> "Constant"
  "import_declaration" -> "Import"
  "import_statement" -> "Import"
  "import_from_statement" -> "Import"
  "use_declaration" -> "Import"
  "export_statement" -> "Export"
  "export_default_declaration" -> "Export"
  "property_signature" -> "Property"
  "property_definition" -> "Property"
  "property_declaration" -> "Property"
  "field_declaration" -> "Field"
  "parameter" -> "Parameter"
  "default_parameter" -> "Parameter"
  "typed_parameter" -> "Parameter"
  "decorator" -> "Decorator"
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
  "package_clause" -> "Module"
  "type_identifier" -> "Type"
  "extern_item" -> "Extern"
  "attribute_item" -> "Attribute"
  "mod_item" -> "Module"
  "declarations" -> "Declarations"
  "instance_declaration" -> "Instance"
  "pattern_declaration" -> "Pattern"
  "type_signature" -> "Signature"
  "module" -> "Module"
  "program" -> "Module"
  "source_file" -> "Module"
  "document" -> "Document"
  "object" -> "Object"
  "array" -> "Array"
  "pair" -> "Property"
  _ -> t
