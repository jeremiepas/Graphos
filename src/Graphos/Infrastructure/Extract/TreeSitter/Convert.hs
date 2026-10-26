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
  , specifierStringTypes
  , extractSpecifier
  , rawSpecifier
  , implementationDetailTypes
  , functionBoundaryTypes
    , makeNodeId
    , tsNodeLabel
    , tsNodeUntruncatedLabel
    -- | Group 3 node-identity helpers (AVI-984): stable identity scheme
    --   <dirHash>_<stem>_<kind>:<ident> with file-wide collision ranks.
    , SourceFile(..)
    , mkSourceFile
    , nodeKeyId
    ) where


import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Aeson (toJSON)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Set (Set)

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
  , "type_alias", "type_item"
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

-- | AST node types that carry a module specifier as their direct string child
-- ('string' / 'string_fragment'). 'rawSpecifier' gates specifier extraction on
-- the presence of one of these as a DIRECT child, so a node whose text merely
-- contains the substring `from ` (for example a template literal inside an
-- export statement) never yields a specifier.
specifierStringTypes :: [String]
specifierStringTypes = [ "string", "string_fragment" ]

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

-- ---------------------------------------------------------------------------
-- Group 4 call edges (AVI-1447)
--
-- A file-wide context threaded through 'tsNodeToGraphEdges' that carries the
-- file's import bindings and local definition names, used for best-effort,
-- name-based callee resolution. The threaded "inside a call" flag suppresses
-- calls nested inside a call's arguments (only the top-level call of an
-- enclosing function is emitted).
-- ---------------------------------------------------------------------------

-- | File-wide context for `calls` edge emission.
data CallsCtx = CallsCtx
  { callsCtxImports  :: !(Map Text NodeId)     -- ^ import binding name -> External node id
  , callsCtxLocal    :: !(Set Text)            -- ^ local definition names
  , callsCtxInside   :: !Bool                  -- ^ threaded: inside a call subtree
  }

emptyCallsCtx :: CallsCtx
emptyCallsCtx = CallsCtx Map.empty Set.empty False

-- | Build a file-wide 'CallsCtx' from a root node: collect the file's import
-- bindings and local definition names. `callsCtxInside` starts `False`; it is
-- threaded down the recursion by 'tsNodeToGraphEdges'.
collectCallsCtx :: FilePath -> TSNodeInfo -> CallsCtx
collectCallsCtx filePath node =
  emptyCallsCtx
    { callsCtxImports  = Map.fromList (concatMap importBindings (tsnChildren node))
    , callsCtxLocal    = Set.fromList (mapMaybe firstIdentOrTypeName (tsnChildren node))
    }
  where
    -- | Identifier characters, hoisted for reuse by 'bindingNames'.
    identChars :: String
    identChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$"

    -- | Import binding name(s) -> External node id. Returns one pair per
    -- binding in the import (fixes multi-binding destructured imports such as
    -- `import { a, b }`, where the old single-binding version registered only
    -- the first). Every binding collapses onto the same resolved External node.
    importBindings :: TSNodeInfo -> [(Text, NodeId)]
    importBindings n = case extractSpecifier filePath n of
          Just (targetPath, targetName) ->
            let targetId = if "external:" `isPrefixOf` targetPath
                            then makeNodeId "external" targetName
                            else makeNodeId targetPath targetName
            in map (\b -> (b, targetId)) (bindingNames n)
          Nothing -> []

    -- | Local binding name(s) of an import node: every identifier used in this
    -- file to refer to an imported symbol (best-effort, name-based).
    --
    -- For `import Foo` this returns `[Foo]`; for `{ a, b }` destructured
    -- imports it returns both bindings. Best-effort: extract every identifier
    -- run in the import text preceding the `from <specifier>` (the specifier is
    -- a string literal, so it never contributes identifiers).
    bindingNames :: TSNodeInfo -> [Text]
    bindingNames n =
      let txt      = normalizeText (tsnText n)
          afterImp = T.drop (T.length "import") txt
          body     = T.strip (fst (T.breakOnEnd "from" afterImp))
      in identifierRuns body

    -- | All maximal identifier runs in a text (e.g. the bindings of an import).
    identifierRuns :: Text -> [Text]
    identifierRuns t =
      let t' = T.dropWhile (`notElem` identChars) t
      in case T.uncons t' of
           Just (c, rest)
             | c `elem` identChars ->
                 let run = T.cons c (T.takeWhile (`elem` identChars) rest)
                 in run : identifierRuns rest
           _ -> []

    -- | Local definition name (skips imports, which carry a long specifier
    -- text). Returns the binding name of a definition node when it is named
    -- and is not itself an import/export specifier.
    firstIdentOrTypeName :: TSNodeInfo -> Maybe Text
    firstIdentOrTypeName n =
      let t = tsnType n
      in if t `elem` typesFor GranularityFine
             && tsnIsNamed n
             && t `notElem` importExportTypes
           then Just (tsNodeLabel n)
           else Nothing

-- | Build a single `calls` edge from an enclosing-definition source id to a
-- resolved callee target id, matching the LSP `parseIncomingCall` shape.
callsRelation :: NodeId -> NodeId -> Edge
callsRelation srcId tgtId =
  Edge
    { edgeId        = EdgeId (srcId <> "->call:" <> tgtId)
    , edgeSource    = srcId
    , edgeTarget    = tgtId
    , edgeRelation  = Calls
    , edgeConfidence = Confidence 0.9
    , edgeWeight    = 0.9
    , edgeExtra     = Nothing
    }

-- | Resolve the callee of a `call_expression`/`new_expression` to a `calls`
-- edge, or drop the edge (best-effort, name-based resolution).
--
--   * same-file callee that is a local definition -> in-file node
--   * imported callee -> matching `External` node id
--   * anything else (e.g. a global such as `Date`) -> drop
resolveCalls :: FilePath -> CallsCtx -> Text -> TSNodeInfo -> Maybe Edge
resolveCalls filePath ctx enclosingDef node =
  case calleeName node of
    Nothing -> Nothing
    Just calleeText ->
      let srcId = makeNodeId filePath enclosingDef
          tgtId = resolveCalleeTarget filePath ctx calleeText
      in case tgtId of
           Just calleeId -> Just (callsRelation srcId calleeId)
           Nothing -> Nothing

-- | Resolve a callee name to a target node id.
resolveCalleeTarget :: FilePath -> CallsCtx -> Text -> Maybe NodeId
resolveCalleeTarget filePath ctx callee
  -- imported callee -> the External node produced by resolveImport/extractSpecifier
  | callee `Map.member` imports = case Map.lookup callee imports of
                                       Just calleeId -> Just calleeId
                                       Nothing -> Nothing
  -- same-file callee that is a local definition -> in-file node
  | callee `Set.member` local = Just (makeNodeId filePath callee)
  -- unresolvable global (e.g. Date) -> drop
  | otherwise = Nothing
  where
    imports = callsCtxImports ctx
    local   = callsCtxLocal ctx

-- | The callee name of a call/new node: the first `identifier` in its subtree
-- (the class name for `new Foo()`, the receiver for `receiver.method()`).
calleeName :: TSNodeInfo -> Maybe Text
calleeName node = firstIdentifierInList (tsnChildren node)

-- | First `identifier` node text in a preorder DFS of a single node's subtree.
firstIdentifierIn :: TSNodeInfo -> Maybe Text
firstIdentifierIn n = case tsnType n of
  "identifier" | not (T.null (tsnText n)) -> Just (tsnText n)
  _ -> case tsnChildren n of
         (k : rest) -> case firstIdentifierIn k of
                          Just r -> Just r
                          Nothing -> firstIdentifierInList rest
         [] -> Nothing

-- | First `identifier` node text across a list of nodes (preorder).
firstIdentifierInList :: [TSNodeInfo] -> Maybe Text
firstIdentifierInList xs = case xs of
  (k : rest) -> case firstIdentifierIn k of
                  Just r -> Just r
                  Nothing -> firstIdentifierInList rest
  [] -> Nothing
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
  let results = map (\node ->
        (tsNodeToGraphNodes gran filePath node, tsNodeToGraphEdges gran filePath Nothing node)) nodes
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
        else []
  in self ++ children

-- | Convert a TSNodeInfo tree into Graphos Edges (Contains parent→child).
--
-- Public entry point: build the file-wide 'CallsCtx' from the given root node
-- and thread it through the internal recursion ('tsNodeToGraphEdges''). The
-- public signature is intentionally unchanged from the pre-call-edge version so
-- that callers (including the Hspec suite) need no `ctx` argument.
tsNodeToGraphEdges :: Granularity -> FilePath -> Maybe Text -> TSNodeInfo -> ([Edge], [Node])
tsNodeToGraphEdges gran filePath parentLabel node =
  tsNodeToGraphEdges' gran filePath (collectCallsCtx filePath node) parentLabel (tsNodeLabel node) node

-- | Internal recursion: threads the file-wide 'CallsCtx' down the tree. The
-- @callsCtxInside@ flag flips to 'True' whenever the current node is a call
-- (or we are already inside one), suppressing calls nested inside a call's
-- arguments.
tsNodeToGraphEdges' :: Granularity -> FilePath -> CallsCtx -> Maybe Text -> Text -> TSNodeInfo -> ([Edge], [Node])
tsNodeToGraphEdges' gran filePath ctx parentLabel enclosingDef node =
  let myLabel = tsNodeLabel node
      isDef = tsnType node `elem` typesFor gran && tsnIsNamed node
      tType = tsnType node
      isCall = tType == "call_expression" || tType == "new_expression"
      insideCall = callsCtxInside ctx
      -- The enclosing definition label threads through statement-level (non
      -- boundary) nodes unchanged; it only updates when we descend into a
      -- function/method/constructor boundary. This is the source of `calls`
      -- edges (spec: enclosing function name, or `module` for module-level).
      childDef = if tType `elem` functionBoundaryTypes then myLabel else enclosingDef

      -- Parent → child edge (Contains)
      myEdges = case (parentLabel, isDef) of
        (Just p, True) ->
          [ Edge
            { edgeId        = EdgeId (makeNodeId filePath p <> "->" <> makeNodeId filePath myLabel <> ":contains")
            , edgeSource    = makeNodeId filePath p
            , edgeTarget    = makeNodeId filePath myLabel
            , edgeRelation  = Contains
            , edgeConfidence = Confidence 1.0
            , edgeWeight    = 1.0
            , edgeExtra     = Nothing
            }
          ]
        _ -> []

      -- Calls edge (top-level call of an enclosing function only).
      callsEdges =
        if isCall && not insideCall
           then case resolveCalls filePath ctx enclosingDef node of
                  Just e -> [e]
                  Nothing -> []
           else []

      -- Imports/Exports edges
      (importEdges, importNodes) = if tType `elem` importExportTypes
        then case extractSpecifier filePath node of
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

      -- Recurse into children with a threaded call context. The child context
      -- flips `callsCtxInside` on whenever the current node is a call (or we are
      -- already inside one), so calls nested inside a call's arguments are
      -- suppressed by the `callsEdges` guard above.
      childCtx = ctx { callsCtxInside = insideCall || isCall }

      (childEdges, childNodes) =
        if not (descendInto gran isDef node)
        then ([], [])
        else if isDef
             then foldr (\child (es, ns) ->
                       let (e, n) = tsNodeToGraphEdges' gran filePath childCtx (Just myLabel) childDef child
                       in (e ++ es, n ++ ns)) ([], []) (tsnChildren node)
             else foldr (\child (es, ns) ->
                       let (e, n) = tsNodeToGraphEdges' gran filePath childCtx parentLabel childDef child
                       in (e ++ es, n ++ ns)) ([], []) (tsnChildren node)

  in (myEdges ++ callsEdges ++ importEdges ++ childEdges, importNodes ++ childNodes)

-- | Resolve a `from <specifier>` import to its canonical target path and name.
--
-- The specifier is read from the declaration's DIRECT string-literal child
-- ('string' / 'string_fragment') and retained verbatim, independently of any
-- label truncation applied for display. A node whose text merely contains the
-- substring `from ` (for example a template literal inside an export statement)
-- yields 'Nothing', because no string child is a direct child of such a node.
extractSpecifier :: FilePath -> TSNodeInfo -> Maybe (FilePath, Text)
extractSpecifier filePath n =
  rawSpecifier n >>= \spec -> resolveImport filePath (T.unpack spec)

-- | The module specifier carried by a declaration's DIRECT string child, if any.
-- Returns 'Nothing' for an empty / whitespace-only specifier (enforcing that a
-- bare string literal never resolves to a target, and that scanning node text
-- for `from ` can never produce a specifier).
rawSpecifier :: TSNodeInfo -> Maybe Text
rawSpecifier n =
  listToMaybe
    [ stripQuotes (T.strip body)
    | child <- tsnChildren n
    , tsnType child `elem` specifierStringTypes
    , let body = tsnText child ] >>= \spec ->
    if T.null (T.strip spec) then Nothing else Just spec

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

-- | Make a Graphos Node from a TSNodeInfo.
makeNode :: FilePath -> TSNodeInfo -> Node
makeNode filePath node = Node
  { nodeId           = makeNodeId filePath (tsNodeUntruncatedLabel node)
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
      specTail = maybe "" (\s -> "from '" <> s <> "'") (rawSpecifier node)
  in if tType `elem` importExportTypes && T.length untruncated > defaultTruncationBudget
     then truncateWithElision defaultTruncationBudget untruncated specTail
     else untruncated

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
