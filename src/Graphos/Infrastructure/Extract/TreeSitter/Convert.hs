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
  , makeNode
  , tsNodeLabel
  , tsNodeSignature
  , nodeIdentifier
  , definitionTypes
  , structureTypes
  , apiSurfaceTypes
  , wrapperTypes
  , importExportTypes
  , implementationDetailTypes
  , functionBoundaryTypes
    ) where

import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Data.Maybe (fromMaybe, isJust)
import Data.Aeson (object, (.=))
import Data.Word (Word64)
import System.FilePath (takeFileName)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
  , extractIdentifier, defaultTruncationBudget, truncateText, truncateWithElision , nodeIdentityBudget )
import Graphos.Infrastructure.Extract.TreeSitter.Resolver (resolveImport)

-- ───────────────────────────────────────────────
-- Whitelisted AST types (unchanged tiers)
-- ───────────────────────────────────────────────

-- | Tier 1 — file/module structure. Emitted at every granularity level.
structureTypes :: [String]
structureTypes =
  [ "module", "program", "source_file", "translation_unit"
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

  , "constructor", "field_declaration"
  , "trait", "trait_item", "impl_block", "impl_item"
  , "data_type", "new_type", "type_synonym"
  , "decorated_definition"
  , "namespace_definition", "namespace"
  , "class_specifier", "struct_specifier"
  , "method"
  , "singleton_method"
  , "create_table", "create_index"
  , "create_table_stmt", "create_index_stmt"
  , "create_table_statement", "create_index_statement"
  , "preproc_include"
     -- Rust

  , "function_item", "struct_item", "use_declaration"
  , "static_item", "extern_item", "attribute_item"
     -- Haskell
  , "declarations", "instance_declaration", "pattern_declaration", "type_signature"
     -- C# / PHP / Perl (AVI-1765) — node names verified against
     -- vendored parser.c (ABI 11) and tree-sitter-php 0.5.0.2 vendor/src/parser.c.
     -- C#: record_declaration does NOT exist in ABI 11 grammar (grep 0 hits);
     --      records were added after vendored revision. Not claimed.
  , "namespace_declaration"       -- C#: namespace N { } (parser.c: "namespace_declaration")
  , "using_directive"             -- C#: using System;   (parser.c: "using_directive")
  , "constructor_declaration"     -- C#: ctor            (parser.c: "constructor_declaration")
  , "destructor_declaration"      -- C#: ~T()            (parser.c: "destructor_declaration")
  , "struct_declaration"          -- C#: struct S { }    (parser.c: "struct_declaration")
  , "delegate_declaration"        -- C#                  (parser.c: "delegate_declaration")
  , "enum_member_declaration"     -- C# enum member      (parser.c: "enum_member_declaration")
  , "trait_declaration"           -- PHP: trait T { }    (tree-sitter-php: "trait_declaration")
  , "subroutine"                  -- Perl: sub foo { }   (parser.c: only "subroutine", no "subroutine_declaration")
  , "package_statement"           -- Perl: package Foo;  (parser.c: "package_statement")
  , "use_constant_statement"      -- Perl: use Foo; / use constant (parser.c: "use_constant_statement")
  , "use_parent_statement"        -- Perl: use parent 'Base'; (parser.c: "use_parent_statement")
  , "require_statement"           -- Perl: require 'x';  (parser.c: "require_statement")
  ]

importExportTypes :: [String]
importExportTypes =
  [ "import_declaration", "import_statement", "import_from_statement"
  , "export_statement", "export_default_declaration"
  , "using_directive"             -- C#: using System;
  , "use_constant_statement"      -- Perl: use Foo; (see apiSurfaceTypes comment)
  , "use_parent_statement"        -- Perl: use parent 'Base';
  , "require_statement"           -- Perl: require 'x';
  , "include_expression"          -- PHP: include 'file.php' (tree-sitter-php: "include_expression")
  , "include_once_expression"     -- PHP: include_once 'file.php'
  , "require_expression"          -- PHP: require 'file.php' (tree-sitter-php: "require_expression")
  , "require_once_expression"     -- PHP: require_once 'file.php'
  , "namespace_use_declaration"   -- PHP: use Foo\Bar;
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
  , "select", "select_stmt", "select_statement"
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
  , "subroutine"                  -- Perl: parser.c "subroutine"
  , "use_constant_statement"      -- Perl
  , "use_parent_statement"        -- Perl
  , "require_statement"           -- Perl
  , "constructor_declaration"     -- C#
  ]

-- | AST types that carry no identifier of their own and must never be emitted
-- as a node in their own right. Their single inner declaration is emitted
-- once (flagged exported); callers recurse through them unchanged.
wrapperTypes :: [String]
wrapperTypes = [ "export_statement", "decorated_definition" ]

-- | AST types whose identifier is the file name rather than a declared name.
moduleTypes :: [String]
moduleTypes =
  [ "module", "program", "source_file", "translation_unit"
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
    t | t `elem` moduleTypes       -> Just (truncateText defaultTruncationBudget (T.pack (takeFileName (sourceFilePathStr sf))))
      | t `elem` importExportTypes -> Just (truncateText defaultTruncationBudget (fromMaybe "" (extractSpecifier sf node)))
      | otherwise                  -> fmap (truncateText defaultTruncationBudget) (extractIdentifier node)

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
tsNodeSignature :: SourceFile -> Granularity -> Int -> TSNodeInfo -> Maybe Text
tsNodeSignature _sf _gran budget node =
  case tsnType node of
    t | t `elem` moduleTypes -> Nothing
      | t `elem` importExportTypes ->
          Just $ sigWithTail budget (normalizeText (tsnText node))
               (extractImportTail (tsnText node))
      | otherwise ->
          let norm = normalizeText (tsnText node)
          in if T.length norm <= budget then Just norm
               else Just (truncateText budget norm)
  where
    sigWithTail :: Int -> Text -> Text -> Text
    sigWithTail b raw tailText
      | T.length raw <= b = raw
      | T.null tailText   = truncateWithElision b raw raw
      | otherwise         = truncateWithElision b raw tailText

-- | Extract the trailing `from '<specifier>'` clause of an import declaration
-- for signature preservation. Returns "" when absent.
extractImportTail :: Text -> Text
extractImportTail t =
  let norm = normalizeText t
      (_, tailClause) = T.breakOn "from " norm
  in if T.null tailClause then "" else tailClause

-- | Resolve the module specifier carried by an import-like declaration.
--
-- Prefers the declaration's string-literal child (the structured form the
-- `import-resolution` spec requires); falls back to the last quoted token in
-- the node text for fixtures that carry no children. Never scans arbitrary
-- text for the `from ` keyword, so template literals inside a body yield
-- nothing.
extractSpecifier :: SourceFile -> TSNodeInfo -> Maybe Text
extractSpecifier _ node
  | tsnType node `notElem` importExportTypes = Nothing
  | otherwise =
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
lastQuoted t =
  case T.unsnoc t of
    Just (_, q) | q == '\'' || q == '"' ->
      let headPart = T.init t
          prev = lastQuoteIdx q headPart
      in case prev of
           Just j  -> Just (T.drop j t)
           Nothing -> Nothing
    _ -> Nothing

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
    safeIdent = T.filter (\c -> not (c `elem` badChars)) ident
    badChars = [ '"', '\'', '`', '/', '\\', '\n', '\r' ]

-- ───────────────────────────────────────────────
makeNode :: FilePath -> TSNodeInfo -> Node
makeNode filePath node =
  let sf = mkSourceFile filePath ""
      kindText = T.pack (tsTypeToKind (tsnType node))
      kind = fromText kindText
      line = tsnStartRow node + 1
      rawIdent = fromMaybe (normalizeText (tsnText node)) (extractIdentifier node)
      identText = normalizeText rawIdent
      ident = truncateText (nodeIdentityBudget - 3) identText
      declText = normalizeText (tsnText node)
      isModuleNode = tsTypeToKind (tsnType node) == "Module"
      isImportExport = tsnType node `elem` importExportTypes
      sigText
        | isModuleNode = Nothing
        | isImportExport = Just (truncateWithElision defaultTruncationBudget declText (extractImportTail declText))
        | otherwise = Just (truncateText defaultTruncationBudget declText)
  in Node
       { nodeId           = makeNodeId sf kindText ident
       , nodeLabel        = fromText ident
       , nodeFileType     = CodeFile
       , nodeSourceFile   = fromText (T.pack filePath)
       , nodeSource       = Nothing
       , nodeLineStart    = Just line
       , nodeLineEnd      = Just (tsnEndRow node + 1)
       , nodeSignature    = fmap fromText sigText
       , nodeCommunityId  = Nothing
       , nodeDegree       = Nothing
       , nodeIsBridge     = Nothing
       , nodeExtra        = Nothing
       , nodeKind         = Just kind
       , nodePresentBits  = bitNodeLineStart .|. bitNodeLineEnd .|. bitNodeKind .|.
                             if isJust sigText then bitNodeSignature else 0
       }

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
      resolved = resolveCollisions sf raws
      idAt     = Map.fromList [ (rnBaseId r, fid) | (r, fid) <- resolved ]
      remap x  = case Map.lookup x idAt of Just f -> f; Nothing -> x
      graphNodes = [ rnFinalNode r fid | (r, fid) <- resolved ]
                 ++ importTargetNodes sf [ t | (_src, t) <- imports ]
      nodeIds = Set.fromList (map nodeId graphNodes)
      callEdges = [ mkCallEdge s' t'
                  | (s, t) <- callEdgePairs sf gran nodes
                  , let s' = remap s
                  , let t' = remap t
                  , s' `Set.member` nodeIds, t' `Set.member` nodeIds ]
      edges = [ mkContainEdge (remap (ceSource e)) (remap (ceTarget e))
              | e <- contains ]
            ++ [ mkImportEdge sf (remap src) tgt | (src, tgt) <- imports ]
            ++ callEdges
  in extractionFromLists graphNodes edges

-- | Given a file's raw nodes, assign each a final id: the first occurrence
-- (by ascending start line) of a base id keeps the plain base id; every
-- subsequent occurrence carries `#L<line>`. Deterministic and independent of
-- display labels.
resolveCollisions :: SourceFile -> [RawNode] -> [(RawNode, NodeId)]
resolveCollisions _sf raws =
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
      sig     = tsNodeSignature sf gran budget node
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
                , nodeLabel        = fromText (tsNodeLabel sf gran node)
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
importPartsFor sf parentBase tType node =
  case extractSpecifier sf node of
    Just spec ->
      let impPath = case resolveImport (sourceFilePathStr sf) (T.unpack spec) of
                      Just (p, _) -> p
                      Nothing -> "external:" ++ T.unpack spec
          impName = case resolveImport (sourceFilePathStr sf) (T.unpack spec) of
                      Just (_, n) -> n
                      Nothing -> spec
      in [ (parentBase, ImportTarget
            { itPath = impPath
            , itName = impName
            , itIsReExport = not (tType `elem` ["import_declaration", "import_statement", "import_from_statement"])
                              || T.isInfixOf "export" (tsnText node) }) ]
    Nothing -> []

-- | Fold over children, threading a parent base id and exported flag.
foldChildren :: SourceFile -> Granularity -> Int -> Maybe NodeId -> Bool -> [TSNodeInfo]
             -> Acc
foldChildren sf gran budget parentEmitted isExported children =
  foldr (\c (ns, ce, im) ->
          let (n, c_, i_) = traverseNode sf gran budget parentEmitted isExported c
          in (ns ++ n, ce ++ c_, im ++ i_)) ([], [], []) children

-- ───────────────────────────────────────────────
-- Call edges (AVI-1447)
-- ───────────────────────────────────────────────

-- | File-wide context for best-effort, name-based `calls` resolution.
data CallsCtx = CallsCtx
  { callsCtxImports :: Map.Map Text NodeId  -- ^ import binding name -> import target node id
  , callsCtxLocal   :: Map.Map Text NodeId  -- ^ top-level definition name -> base node id
  }

-- | AST types that denote a call.
callTypes :: [String]
callTypes = [ "call_expression", "new_expression" ]

-- | Id of the node a resolved import points at (matches 'mkImportEdge' and
-- 'importTargetNodes').
importTargetIdOf :: SourceFile -> TSNodeInfo -> Maybe NodeId
importTargetIdOf sf node = do
  spec <- extractSpecifier sf node
  let fp = sourceFilePathStr sf
      (p, name) = case resolveImport fp (T.unpack spec) of
        Just (rp, rn) -> (rp, rn)
        Nothing       -> ("external:" ++ T.unpack spec, spec)
  pure (makeNodeId (mkSourceFile p "") "Module" name)

-- | Base node id of an emitted definition node.
definitionBaseId :: SourceFile -> Granularity -> TSNodeInfo -> Maybe NodeId
definitionBaseId sf gran node =
  fmap (makeNodeId sf (T.pack (tsTypeToKind (tsnType node)))) (nodeIdentifier sf gran node)

-- | Collect a file's import bindings and top-level definition names.
collectCallsCtx :: SourceFile -> Granularity -> [TSNodeInfo] -> CallsCtx
collectCallsCtx sf gran roots = CallsCtx
  { callsCtxImports = Map.fromList (concatMap importBindings top)
  , callsCtxLocal   = Map.fromList
      [ (name, nid)
      | n <- top
      , tsnIsNamed n
      , tsnType n `elem` typesFor gran
      , tsnType n `notElem` importExportTypes
      , tsnType n `notElem` wrapperTypes
      , tsnType n `notElem` moduleTypes
      , Just name <- [nodeIdentifier sf gran n]
      , Just nid  <- [definitionBaseId sf gran n] ]
  }
  where
    children = concatMap tsnChildren roots
    top = children ++ [ k | c <- children, tsnType c `elem` wrapperTypes, k <- tsnChildren c ]

    importBindings n
      | tsnType n `elem` importExportTypes =
          case importTargetIdOf sf n of
            Just tid -> [ (b, tid) | b <- bindingNames n ]
            Nothing  -> []
      | otherwise = []

    -- Every identifier run in the import text before `from <specifier>`.
    bindingNames n =
      let txt  = normalizeText (tsnText n)
          body = T.strip (fst (T.breakOnEnd "from" (T.drop (T.length "import") txt)))
      in identifierRuns body

    identChars :: String
    identChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_$"

    identifierRuns t =
      let t' = T.dropWhile (`notElem` identChars) t
      in case T.uncons t' of
           Just (c, rest) | c `elem` identChars ->
             (T.cons c (T.takeWhile (`elem` identChars) rest))
               : identifierRuns (T.dropWhile (`elem` identChars) rest)
           _ -> []

-- | (enclosing definition id, callee target id) for every resolvable top-level
-- call. Calls nested inside a call's arguments are excluded; unresolvable
-- callees (globals) are dropped.
callEdgePairs :: SourceFile -> Granularity -> [TSNodeInfo] -> [(NodeId, NodeId)]
callEdgePairs sf gran roots = concatMap (walk Nothing False) roots
  where
    ctx = collectCallsCtx sf gran roots

    walk enc inside n =
      let tType   = tsnType n
          isCall  = tType `elem` callTypes
          emitted = tsnIsNamed n && tType `elem` typesFor gran && tType `notElem` wrapperTypes
          enc'    = if emitted && (tType `elem` functionBoundaryTypes || tType `elem` moduleTypes)
                      then maybe enc Just (definitionBaseId sf gran n)
                      else enc
          here    = [ (src, tgt)
                    | isCall, not inside
                    , Just src <- [enc]
                    , Just callee <- [firstIdentifierInList (tsnChildren n)]
                    , Just tgt <- [resolveCallee callee] ]
          kids    = if descendInto gran emitted n
                      then concatMap (walk enc' (inside || isCall)) (tsnChildren n)
                      else []
      in here ++ kids

    resolveCallee callee =
      case Map.lookup callee (callsCtxImports ctx) of
        Just i  -> Just i
        Nothing -> Map.lookup callee (callsCtxLocal ctx)

-- | First `identifier` text in a preorder walk of a list of nodes.
firstIdentifierInList :: [TSNodeInfo] -> Maybe Text
firstIdentifierInList [] = Nothing
firstIdentifierInList (k : rest) =
  case firstIdentifierIn k of
    Just r  -> Just r
    Nothing -> firstIdentifierInList rest
  where
    firstIdentifierIn n
      | tsnType n == "identifier" && not (T.null (tsnText n)) = Just (tsnText n)
      | otherwise = firstIdentifierInList (tsnChildren n)

mkCallEdge :: NodeId -> NodeId -> Edge
mkCallEdge src tgt =
  Edge
    { edgeId        = EdgeId (src <> "->call:" <> tgt)
    , edgeSource    = src
    , edgeTarget    = tgt
    , edgeRelation  = Calls
    , edgeConfidence = Confidence 0.9
    , edgeWeight    = 0.9
    , edgeExtra     = Nothing
    }

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

mkImportEdge :: SourceFile -> NodeId -> ImportTarget -> Edge
mkImportEdge _sf src tgt =
  Edge
    { edgeId        = EdgeId (src <> "->" <> targetId <> ":imports")
    , edgeSource    = src
    , edgeTarget    = targetId
    , edgeRelation  = Imports
    , edgeConfidence = Confidence 1.0
    , edgeWeight    = 1.0
    , edgeExtra     = if itIsReExport tgt
                      then Just (object ["re-export" .= (True :: Bool)])
                      else Nothing
    }
  where
    targetId = makeNodeId (mkSourceFile (itPath tgt) "") "Module" (itName tgt)

-- | Materialize import-target endpoint nodes so edges have valid endpoints.
-- Relative imports resolve to the target file's module node; package / builtin
-- specifiers resolve to a single canonical external node identity.
importTargetNodes :: SourceFile -> [ImportTarget] -> [Node]
importTargetNodes _sf targets =
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
                             then Just (object ["re-export" .= (True :: Bool)])
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


-- | Index of the last occurrence of a character in a text, 0-based.
lastQuoteIdx :: Char -> Text -> Maybe Int
lastQuoteIdx q t =
  let go i acc
        | i < 0     = acc
        | T.index t i == q = go (i - 1) (Just i)
        | otherwise        = go (i - 1) acc
  in go (T.length t - 1) Nothing

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
  "namespace_definition" -> "Namespace"
  "namespace" -> "Namespace"
  "class_specifier" -> "Class"
  "struct_specifier" -> "Struct"
  "method" -> "Method"
  "singleton_method" -> "Method"
  "create_table" -> "Table"
  "create_index" -> "Index"
  "select" -> "Query"
  "select_statement" -> "Query"
  "create_table_stmt" -> "Table"
  "create_table_statement" -> "Table"
  "create_index_stmt" -> "Index"
  "create_index_statement" -> "Index"
  "select_stmt" -> "Query"
  "preproc_include" -> "Import"
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
  -- `type_identifier` is intentionally *not* mapped here (AVI-1440). A bare
  -- type reference — the `type_identifier` node under a field annotation like
  -- `role: UserRole`, or the declared name of an `interface`/`type`/`enum` —
  -- is a *reference*, not a declaration, so it produces no node. Only declared
  -- types yield `kind: "Type"`: `type_declaration`, `type_alias_declaration`,
  -- `type_definition`, `type_alias`, `type_item`, `type_synonym`,
  -- `data_type`, `new_type`, and (C++) `alias_declaration`.
  -- Rust-specific
  "extern_item" -> "Extern"
  "attribute_item" -> "Attribute"
  "mod_item" -> "Module"
  "declarations" -> "Declarations"
  "instance_declaration" -> "Instance"
  "pattern_declaration" -> "Pattern"
  "type_signature" -> "Signature"
  "module" -> "Module"
  "translation_unit" -> "Module"
  "program" -> "Module"
  "source_file" -> "Module"
  "document" -> "Document"
  "object" -> "Object"
  "array" -> "Array"
  "pair" -> "Property"
  -- C# / PHP / Perl (AVI-1765) — verified node names (see apiSurfaceTypes)
  "namespace_declaration" -> "Namespace"
  "using_directive" -> "Import"
  "constructor_declaration" -> "Constructor"
  "destructor_declaration" -> "Destructor"
  "struct_declaration" -> "Struct"
  "delegate_declaration" -> "Delegate"
  "enum_member_declaration" -> "EnumMember"
  "trait_declaration" -> "Trait"
  "subroutine" -> "Function"
  "package_statement" -> "Module"
  "use_constant_statement" -> "Import"
  "use_parent_statement" -> "Import"
  "require_statement" -> "Import"
  "include_expression" -> "Import"
  "include_once_expression" -> "Import"
  "require_expression" -> "Import"
  "require_once_expression" -> "Import"
  "namespace_use_declaration" -> "Import"
  _ -> t
