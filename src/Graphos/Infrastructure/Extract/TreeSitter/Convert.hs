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

import Data.Char (isAlpha)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)
import Data.Maybe (fromMaybe, isJust, mapMaybe, listToMaybe)
import Data.Aeson (object, (.=), Value(..), toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
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
  , extractIdentifier, defaultTruncationBudget, truncateText, truncateWithElision , nodeIdentityBudget
  , childByField, childrenByField )
import Graphos.Infrastructure.Extract.TreeSitter.Resolver (resolveImport)

-- ───────────────────────────────────────────────
-- Whitelisted AST types (unchanged tiers)
-- ───────────────────────────────────────────────

-- | Tier 1 — file/module structure. Emitted at every granularity level.
structureTypes :: [String]
structureTypes =
  [ "module", "program", "source_file", "translation_unit"
  , "compilation_unit"      -- C#
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
  -- `type_identifier` deliberately excluded (AVI-1440 /
  -- extraction-semantic-enrichment): it is a bare type *reference* in a
  -- method/field/param type position; the few declarations whose own name is
  -- a `type_identifier` (e.g. TS `interface`/`type`) are identified via their
  -- wrapping `interface_declaration`/`type_alias_declaration` node, never via
  -- this bare node type itself.
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
  , "namespace_use_declaration"   -- PHP: use Foo\Bar;
  , "include_expression"          -- PHP: include 'f.php' — the import forms are
  , "include_once_expression"     --      expressions, nested in an
  , "require_expression"          --      expression_statement, so they are
  , "require_once_expression"     --      whitelisted in their own right
  , "subroutine"                  -- Perl: anonymous `sub { }` (a named `sub f { }`
                                  --       parses as "function_definition")
  , "package_statement"           -- Perl: package Foo;  (parser.c: "package_statement")
  , "use_no_statement"            -- Perl: use Foo; / no Foo; (parser.c: "use_no_statement")
  , "use_constant_statement"      -- Perl: use constant PI => 3;
  , "use_parent_statement"        -- Perl: use parent 'Base'; (parser.c: "use_parent_statement")
  , "require_statement"           -- Perl: require Foo::Bar; (a quoted `require 'x'`
                                  --       does not parse at this ABI)
  ]

importExportTypes :: [String]
importExportTypes =
  [ "import_declaration", "import_statement", "import_from_statement"
  , "export_statement", "export_default_declaration"
  , "using_directive"             -- C#: using System;
  , "use_no_statement"            -- Perl: use Foo; (see apiSurfaceTypes comment)
  , "use_constant_statement"      -- Perl: use constant PI => 3;
  , "use_parent_statement"        -- Perl: use parent 'Base';
  , "require_statement"           -- Perl: require Foo::Bar;
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
  , "subroutine"                  -- Perl: anonymous sub
  , "use_no_statement"            -- Perl
  , "use_constant_statement"      -- Perl
  , "use_parent_statement"        -- Perl
  , "require_statement"           -- Perl
  , "constructor_declaration"     -- C#
  , "method", "singleton_method"  -- Ruby (node-types.json: distinct from
                                  -- "method_declaration"/"method_definition";
                                  -- omitted here made every Ruby call's
                                  -- enclosing scope resolve to the module
                                  -- instead of the method — extraction-semantic-enrichment)
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
  , "compilation_unit"
  , "package_clause", "mod_item"
  -- A YAML/JSON `document` *is* its file, so it names itself after the file
  -- like any other root. Without this it has no identifier child, so every
  -- YAML and JSON file in a corpus shared the node id "" and collapsed into
  -- a single surviving node (AVI-1765).
  , "document"
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
      | t `elem` importExportTypes ->
          Just (truncateText defaultTruncationBudget
                  (fromMaybe (resolverOwnedLabel node) (extractSpecifier sf node)))
      | otherwise                  -> fmap (truncateText defaultTruncationBudget) (extractIdentifier node)

-- | Label for an import whose edge belongs to a repository-context resolver
-- ("Graphos.Infrastructure.Extract.TreeSitter.Imports"), not to
-- 'importPartsFor': the specifier is unquoted, so 'extractSpecifier'
-- deliberately yields nothing (emitting an edge here would duplicate the
-- resolver's), but the node still needs a label instead of the empty string.
resolverOwnedLabel :: TSNodeInfo -> Text
resolverOwnedLabel node
  | tsnType node `elem` resolverOwnedImportTypes =
      fromMaybe "" (bareSpecifier (normalizeText (tsnText node)))
  | otherwise = ""

-- | Java and Kotlin @import a.b.C;@ — resolved in Imports.hs.
resolverOwnedImportTypes :: [String]
resolverOwnedImportTypes = ["import_declaration"]

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
          let norm = T.stripEnd (headerOnly node (normalizeText (tsnText node)))
          in if T.length norm <= budget then Just norm
               else Just (truncateText budget norm)
  where
    sigWithTail :: Int -> Text -> Text -> Text
    sigWithTail b raw tailText
      | T.length raw <= b = raw
      | T.null tailText   = truncateWithElision b raw raw
      | otherwise         = truncateWithElision b raw tailText

-- | Truncate a normalized declaration's text to its *header* — up to (not
-- including) its body — for any grammar whose body is reachable via the
-- `body` field (java, python, typescript, go, rust, php, c/cpp all expose
-- one). Keeps annotations/decorators/modifiers/name/params/return type in
-- the signature while dropping the body text that used to swallow the
-- truncation budget. Falls back to the full text when no `body` field is
-- present — notably Ruby's `method`/`singleton_method`, whose statements are
-- direct, field-less children rather than a single body node (verified via
-- node-types.json; no reliable field-free truncation point exists there).
headerOnly :: TSNodeInfo -> Text -> Text
headerOnly node norm =
  case childByField "body" node of
    Just bodyField ->
      let bodyNorm = normalizeText (tsnText bodyField)
      in if T.null bodyNorm then norm else fst (T.breakOn bodyNorm norm)
    Nothing -> norm

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
-- the node text for fixtures that carry no children, then to the unquoted
-- form used by 'bareSpecifierTypes'. Never scans arbitrary text for the
-- `from ` keyword, so template literals inside a body yield nothing.
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
           Nothing
             | tsnType node `elem` bareSpecifierTypes -> bareSpecifier t
             | otherwise -> Nothing

-- | Import forms whose grammar writes the specifier unquoted (AVI-1765):
-- C# @using System.Text;@, PHP @use Foo\Bar;@, Perl @use Foo;@. Without a
-- specifier these nodes are emitted as @Import@ nodes with no @imports@
-- edge, which the `import-resolution` spec forbids.
bareSpecifierTypes :: [String]
bareSpecifierTypes =
  [ "using_directive"             -- C#
  , "namespace_use_declaration"   -- PHP
  , "use_no_statement"            -- Perl
  , "use_constant_statement"      -- Perl
  , "use_parent_statement"        -- Perl
  , "require_statement"           -- Perl
  ]

-- | Import-like forms that bring a module in rather than re-export it.
-- Everything else in 'importExportTypes' is a re-export (TypeScript
-- @export ... from@).
plainImportTypes :: [String]
plainImportTypes =
  [ "import_declaration", "import_statement", "import_from_statement"
  , "include_expression", "include_once_expression"
  , "require_expression", "require_once_expression"
  ] ++ bareSpecifierTypes

-- | Specifier of an unquoted import: drop the leading keyword and its
-- modifiers (@static@, @function@, @const@), follow a C# alias directive
-- (@using Alias = Namespace.Type;@) to its right-hand side, and keep the
-- dotted / backslashed path up to an alias, a group brace or the terminator.
bareSpecifier :: Text -> Maybe Text
bareSpecifier raw =
  case afterAlias (dropWhile isKeyword (T.words (T.map unSemi raw))) of
    (w : _) | let s = trimSeps (T.takeWhile (`notElem` ("{(,=" :: String)) w)
            , startsIdent s -> Just s
    _ -> Nothing
  where
    unSemi c = if c == ';' then ' ' else c
    isKeyword w = T.toLower w `elem`
      [ "use", "no", "using", "require", "include", "import"
      , "static", "function", "const", "global" ]
    afterAlias ws = case break (== "=") ws of
      (_, _ : rhs@(_ : _)) -> rhs
      _                    -> ws
    trimSeps = T.dropWhileEnd (`elem` ("\\.:" :: String))
    startsIdent s = case T.uncons s of
      Just (c, _) -> isAlpha c || c == '_' || c == '\\'
      Nothing     -> False

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

-- | A containment edge referencing a (base id, start line) key for both
-- source and target — the same key 'resolveCollisions' disambiguates
-- duplicate base ids by, so two sibling overloads sharing a base id each
-- keep their own distinct containment edge instead of collapsing onto
-- whichever overload happens to resolve last.
data ContainEdge = ContainEdge { ceSource :: (NodeId, Int), ceTarget :: (NodeId, Int) }

-- | A resolved import target used to materialize an edge endpoint node.
data ImportTarget = ImportTarget
  { itPath       :: FilePath      -- resolved path (or "external:...")
  , itName       :: Text          -- resolved name
  , itIsReExport :: Bool
  }

-- | Accumulator threaded through a single tree traversal.
type Acc = ([RawNode], [ContainEdge], [((NodeId, Int), ImportTarget)])

-- | Convert tree-sitter AST nodes into an Extraction at a given granularity.
--
-- Phase 1 walks the tree collecting raw node candidates plus containment and
-- import edges (keyed by base id + start line). Phase 2 resolves in-file
-- base-id collisions to assign final ids, then threads those final ids
-- through every edge so that `contains` endpoints equal the exact emitted
-- node ids — disambiguated per (base id, line) so overloaded siblings each
-- keep their own edge instead of one silently shadowing the other.
tsNodesToExtraction :: SourceFile -> Granularity -> Int -> [TSNodeInfo] -> Extraction
tsNodesToExtraction sf gran budget nodes =
  let (raws, contains, imports) =
        foldr (\n (r, c, i) ->
          let (ns, ces, ies) = traverseNode sf gran budget Nothing False n
          in (r ++ ns, c ++ ces, i ++ ies)) ([], [], []) nodes

      -- Phase 2: resolve per-file collisions → (RawNode, finalId)
      resolved = resolveCollisions sf raws
      -- Text-keyed map: last-base-id-instance-wins. Used only where an exact
      -- per-instance key isn't available (call-edge resolution is already
      -- best-effort/name-based, so this ambiguity is accepted there).
      idAt     = Map.fromList [ (rnBaseId r, fid) | (r, fid) <- resolved ]
      remap x  = case Map.lookup x idAt of Just f -> f; Nothing -> x
      -- (base id, line)-keyed map: exact per-instance resolution, used for
      -- `contains`/`imports` so duplicate base ids never collide.
      idAt2    = Map.fromList [ ((rnBaseId r, rnLine r), fid) | (r, fid) <- resolved ]
      remap2 (bid, ln) = case Map.lookup (bid, ln) idAt2 of Just f -> f; Nothing -> bid
      -- Annotations (extraction-semantic-enrichment): keyed by the same
      -- (base id, line) pair as the collision-safe remaps above, so a node's
      -- annotations attach to the exact raw node they were collected from.
      annMap = Map.fromList
        [ ((bid, tsnStartRow n), anns)
        | (n, anns) <- concatMap (definitionsWithAnnotations gran) nodes
        , not (null anns)
        , Just bid <- [definitionBaseId sf gran n] ]
      -- Notable strings (extraction-semantic-enrichment): canonical env/
      -- config/topic nodes, added to the node set before 'nodeIds' is
      -- computed so their reference edges validate like any other edge.
      notables = notableTriples sf gran nodes
      graphNodes = [ rnFinalNode annMap r fid | (r, fid) <- resolved ]
                 ++ importTargetNodes sf [ t | (_src, t) <- imports ]
                 ++ notableNodes notables
      nodeIds = Set.fromList (map nodeId graphNodes)
      callEdges = [ mkCallEdge s' t'
                  | (s, t) <- callEdgePairs sf gran nodes
                  , let s' = remap s
                  , let t' = remap t
                  , s' `Set.member` nodeIds, t' `Set.member` nodeIds ]
      annRefEdges = [ mkReferencesEdge 1.0 s' tid
                    | (srcKey, tid) <- annotationRefPairs sf gran nodes
                    , let s' = remap2 srcKey
                    , s' `Set.member` nodeIds, tid `Set.member` nodeIds ]
      notableRefEdges = [ mkReferencesEdge 0.9 s' (notablePrefix kind <> name)
                        | (srcKey, kind, name) <- notables
                        , let s' = remap2 srcKey
                        , s' `Set.member` nodeIds ]
      edges = [ mkContainEdge (remap2 (ceSource e)) (remap2 (ceTarget e))
              | e <- contains ]
            ++ [ mkImportEdge sf (remap2 src) tgt | (src, tgt) <- imports ]
            ++ callEdges
            ++ annRefEdges
            ++ notableRefEdges
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
-- @parentEmitted@ is the (base id, start line) of the nearest ancestor that
-- was emitted as a node — the line disambiguates which exact instance of a
-- possibly-duplicated base id is the parent, so two sibling overloads never
-- shadow each other's containment edge. It threads unchanged through
-- suppressed wrappers so an inner declaration keeps its containment edge
-- from the same ancestor. @isExported@ records whether the node lies within
-- a suppressed `export` / `@decorator` chain and is flagged on the emitted
-- node.
traverseNode :: SourceFile -> Granularity -> Int -> Maybe (NodeId, Int) -> Bool -> TSNodeInfo
             -> ([RawNode], [ContainEdge], [((NodeId, Int), ImportTarget)])
traverseNode sf gran budget parentEmitted isExported node =
  let tType   = tsnType node
      isDef   = tType `elem` typesFor gran && tsnIsNamed node
      ident   = nodeIdentifier sf gran node
      kind    = T.pack (tsTypeToKind tType)
      baseId  = maybe "" (\i -> makeNodeId sf kind i) ident
      thisLine = tsnStartRow node
      sig     = tsNodeSignature sf gran budget node
      marker  = if isExported then Just (object ["exported" .= (True :: Bool)]) else Nothing
      parentKey = fromMaybe ("", 0) parentEmitted
      containEdge = if T.null baseId then [] else [ContainEdge parentKey (baseId, thisLine)]
      childAcc = if descendInto gran True node
                 then foldChildren sf gran budget (Just (baseId, thisLine)) False (tsnChildren node)
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
              importParts = importPartsFor sf parentKey tType node
          in (rawNode : csns, containEdge ++ cces, importParts ++ cims)

-- | Build the (source key, ImportTarget) pair for an import/export node, using
-- the enclosing emitted node's (base id, line) as the edge source.
importPartsFor :: SourceFile -> (NodeId, Int) -> String -> TSNodeInfo -> [((NodeId, Int), ImportTarget)]
importPartsFor sf parentKey tType node =
  case extractSpecifier sf node of
    Just spec ->
      let impPath = case resolveImport (sourceFilePathStr sf) (T.unpack spec) of
                      Just (p, _) -> p
                      Nothing -> "external:" ++ T.unpack spec
          impName = case resolveImport (sourceFilePathStr sf) (T.unpack spec) of
                      Just (_, n) -> n
                      Nothing -> spec
      in [ (parentKey, ImportTarget
            { itPath = impPath
            , itName = impName
            , itIsReExport = not (tType `elem` plainImportTypes)
                              || T.isInfixOf "export" (tsnText node) }) ]
    Nothing -> []

-- | Fold over children, threading a parent (base id, line) key and exported flag.
foldChildren :: SourceFile -> Granularity -> Int -> Maybe (NodeId, Int) -> Bool -> [TSNodeInfo]
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

-- | AST types that denote a call, across every supported grammar's own
-- vocabulary (extraction-semantic-enrichment). Verified against each pinned
-- grammar's `node-types.json` (java 0.7.0.3, python 0.9.0.4, ruby 0.5.0.4, go
-- 0.5.0.3, rust 0.1.0.2, php 0.5.0.2, typescript 0.5.0.3) except c-sharp and
-- perl, which have no vendored `node-types.json` and are confirmed only at
-- the type-name-string-presence level (grep against `parser.c`, the AVI-1765
-- bar). Safe to keep as one flat list rather than a per-grammar dispatch
-- table: tree-sitter node-type strings are each grammar's own vocabulary, and
-- none of the names below is reused by another supported grammar for a
-- non-call construct, so a file's AST only ever matches the entries that
-- belong to its own grammar.
callTypes :: [String]
callTypes =
  [ "call_expression", "new_expression"    -- typescript/tsx/javascript, go, rust, c, cpp
  , "call"                                 -- python; ruby (no-paren dotted form, ambiguous
                                            -- with attribute access — see 'method_call')
  , "method_call"                          -- ruby: the real invocation node
  , "method_invocation", "object_creation_expression"  -- java (+ .kt via java grammar)
  , "invocation_expression"                -- c-sharp (type name only, see note above)
  , "function_call_expression", "member_call_expression"
  , "scoped_call_expression"               -- php
  ]

-- | The callee's name for a call-denoting node. Tries each known grammar
-- field in turn — exactly one will be present, since a node belongs to
-- exactly one grammar's vocabulary — applying 'rightmostName' to resolve a
-- member/qualified callee down to its final segment. Falls back to the
-- first-identifier-in-preorder heuristic when no known field matches
-- (field-less grammars/node types: PHP's `object_creation_expression`,
-- c-sharp pending independent field-id verification, perl).
calleeNameOf :: TSNodeInfo -> Maybe Text
calleeNameOf node =
  case listToMaybe (mapMaybe (`childByField` node) calleeFields) of
    Just field -> Just (rightmostName field)
    Nothing    -> firstIdentifierInList (tsnChildren node)
  where
    calleeFields = ["function", "name", "type", "method", "constructor"]

-- | The rightmost name-like leaf under a callee-field subtree: for a member
-- or qualified callee (`svc.save`, `new a.b.C()`, Ruby's `obj.foo` nested
-- inside `method_call.method`) this is the final segment (`save`, `C`,
-- `foo`); for a bare identifier it is the node's own text. Prefers a known
-- "final segment" field (TS `member_expression.property`, Java
-- `field_access.field`, Ruby `call.method`/`method_call.method`, PHP
-- `scoped_call_expression.name`) before falling back to the last named
-- child positionally, which holds for every verified grammar since a
-- member/qualified expression always writes its receiver/qualifier before
-- its final segment.
rightmostName :: TSNodeInfo -> Text
rightmostName node
  | tsnType node `elem` nameLeafTypes = tsnText node
  | otherwise = case listToMaybe (mapMaybe (`childByField` node) rightmostFields) of
      Just field -> rightmostName field
      Nothing -> case filter tsnIsNamed (tsnChildren node) of
        []     -> tsnText node
        cs     -> rightmostName (last cs)
  where
    nameLeafTypes = [ "identifier", "property_identifier", "field_identifier"
                     , "type_identifier", "name", "constant" ]
    -- "attribute" covers Python's `attribute.attribute` field (`obj.attr`).
    rightmostFields = ["property", "field", "method", "name", "attribute"]

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

-- | Every definition node reachable in the tree, mirroring the exact
-- emission rules 'traverseNode' uses (wrapper unwrapping, opaque function
-- bodies at coarser granularities) so the index matches precisely what ends
-- up emitted as graph nodes. Needed so a call from inside a class method can
-- resolve to a *sibling* method — not only to a top-level function — which
-- is the normal shape of a call in Java/C#/PHP/Kotlin.
allDefinitions :: Granularity -> TSNodeInfo -> [TSNodeInfo]
allDefinitions gran = go
  where
    go n =
      let tType = tsnType n
          isDef = tsnIsNamed n && tType `elem` typesFor gran
      in if not isDef || tType `elem` wrapperTypes
           then concatMap go (tsnChildren n)
           else n : if descendInto gran True n then concatMap go (tsnChildren n) else []

-- | Collect a file's import bindings and every definition name (top-level or
-- nested inside a class/namespace/impl/module).
--
-- KNOWN GAP: @callsCtxImports@ is populated only via 'extractSpecifier' —
-- the generic "quoted specifier" path TS/JS/Python import syntax exposes.
-- Java/Kotlin/C#/PHP/Perl import declarations carry an *unquoted* path
-- (@import a.b.C;@) that 'extractSpecifier' deliberately returns 'Nothing'
-- for (see its doc comment): their real resolution lives entirely in the
-- separate, repository-aware @Imports.hs@ pipeline, which this calls/
-- references machinery does not consult. Practically: an *imported* callee
-- (`new ClientData()` where `ClientData` comes from another file) or an
-- *imported* annotation (`@GET` from `javax.ws.rs.GET`) never resolves for
-- these five grammars in production — only same-file resolution does. Both
-- 'callEdgePairs' and 'annotationRefPairs' inherit this limitation; wiring
-- @Imports.hs@'s resolved bindings into this map is unimplemented.
collectCallsCtx :: SourceFile -> Granularity -> [TSNodeInfo] -> CallsCtx
collectCallsCtx sf gran roots = CallsCtx
  { callsCtxImports = Map.fromList (concatMap importBindings top)
  , callsCtxLocal   = Map.fromList
      [ (name, nid)
      | n <- concatMap (allDefinitions gran) roots
      , tsnType n `notElem` importExportTypes
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
                    , Just callee <- [calleeNameOf n]
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
-- Annotations / decorators / attributes (extraction-semantic-enrichment)
-- ───────────────────────────────────────────────

-- | One captured annotation/decorator: its name and the normalized text of
-- its argument list, if any.
data Annotation = Annotation { annName :: Text, annArgs :: Maybe Text }

-- | Java `annotation`/`marker_annotation`: both expose `name` as a field
-- (verified via `tree-sitter-java` 0.7.0.3 `node-types.json`); `annotation`
-- additionally exposes `arguments`.
javaAnnotation :: TSNodeInfo -> Annotation
javaAnnotation a =
  Annotation
    { annName = maybe (tsnText a) tsnText (childByField "name" a)
    , annArgs = fmap (normalizeText . tsnText) (childByField "arguments" a)
    }

-- | Python and TypeScript decorators expose no `name` field (verified: only
-- Python's carries an `arguments` field, and only on the call-shaped form;
-- TypeScript's carries no field at all) — parsed from the decorator's own
-- source text instead (`@name`, `@name(args)`, `@a.b.name(args)`), which is
-- robust across both grammars without assuming either one's internal
-- decorator shape.
parseDecoratorText :: Text -> (Text, Maybe Text)
parseDecoratorText raw =
  let t = T.dropWhile (== '@') (T.strip raw)
      (namePart, argsPart) = T.break (== '(') t
      segments = T.splitOn "." (T.strip namePart)
      name = case segments of [] -> T.strip namePart; xs -> last xs
      args = if T.null argsPart then Nothing else Just (T.strip argsPart)
  in (name, args)

decoratorAnnotation :: TSNodeInfo -> Annotation
decoratorAnnotation dec =
  let (name, args) = parseDecoratorText (normalizeText (tsnText dec))
  in Annotation name args

-- | Annotations carried directly by a Java declaration's `modifiers` child
-- (confirmed via `node-types.json`: `modifiers.children` explicitly allows
-- `annotation`/`marker_annotation`).
javaModifierAnnotations :: TSNodeInfo -> [Annotation]
javaModifierAnnotations node =
  [ javaAnnotation a
  | m <- tsnChildren node, tsnType m == "modifiers"
  , a <- tsnChildren m, tsnType a `elem` ["annotation", "marker_annotation"] ]

-- | TypeScript class decorators: a repeatable `decorator` field directly on
-- `class_declaration` (confirmed `multiple: true` via `node-types.json`).
-- Method/property-level TS decorators are not covered here — unlike the
-- class case, their exact grammar placement was not verified this pass.
tsClassDecorators :: TSNodeInfo -> [Annotation]
tsClassDecorators node = map decoratorAnnotation (childrenByField "decorator" node)

-- | (definition, its annotations) pairs for every definition reachable in
-- the tree. Handles both self-contained carriers (Java modifiers, TS class
-- decorator field) and the one wrapper-derived case this pass covers:
-- Python's `decorated_definition`, whose `decorator` children precede its
-- `definition` field and must be carried down to whichever inner
-- declaration ends up emitted — mirroring how 'traverseNode' threads
-- @isExported@ through that same wrapper. PHP carries no annotations here:
-- its pinned grammar (`tree-sitter-php` 0.5.0.2) has no attribute-syntax
-- node at all. Capped at 16 annotations per node.
definitionsWithAnnotations :: Granularity -> TSNodeInfo -> [(TSNodeInfo, [Annotation])]
definitionsWithAnnotations gran = go []
  where
    go pending n =
      let tType = tsnType n
          isDef = tsnIsNamed n && tType `elem` typesFor gran
      in if tType == "decorated_definition"
           then let decs = [ decoratorAnnotation d | d <- tsnChildren n, tsnType d == "decorator" ]
                in concatMap (go (pending ++ decs)) (tsnChildren n)
           else if not isDef || tType `elem` wrapperTypes
             then concatMap (go pending) (tsnChildren n)
             else (n, take 16 (pending ++ javaModifierAnnotations n ++ tsClassDecorators n))
                  : if descendInto gran True n then concatMap (go []) (tsnChildren n) else []

-- | Strip a `(...)`-wrapped argument list's outer parens, then any
-- surrounding quotes, yielding a clean scalar value for simple
-- single-argument annotations (`@Path("/x")`, `@GET`).
unwrapSingleArg :: Text -> Text
unwrapSingleArg raw =
  stripQuotes (T.strip (T.dropWhileEnd (== ')') (T.dropWhile (== '(') (T.strip raw))))

-- | The value bound to a named argument in a parenthesized argument list
-- (`cron = "0 0 * * * *"` inside `@Scheduled(cron = "0 0 * * * *")`). A
-- best-effort, name-based split — like the rest of this extractor's
-- annotation handling, not a full expression parser.
namedArg :: Text -> Text -> Maybe Text
namedArg key argsRaw =
  let body = T.dropWhileEnd (== ')') (T.dropWhile (== '(') (T.strip argsRaw))
      parts = T.splitOn "," body
      matching = [ v0 | p <- parts
                       , let (k, v0) = T.breakOn "=" p
                       , T.strip k == key, not (T.null v0) ]
  in case matching of
       (v : _) -> Just (stripQuotes (T.strip (T.drop 1 v)))
       []      -> Nothing

-- | Well-known annotation families distilled to flat, queryable
-- `nodeExtra` keys. Scoped to the JAX-RS / Spring vocabulary verified
-- against the motivating Java corpus (route, httpVerb, cron, fixedDelay,
-- topics, configKeys); Python/TypeScript decorator-based routing
-- equivalents (flask/fastapi, NestJS) are a documented gap, not implemented
-- this pass. Unknown annotations are captured (by 'definitionsWithAnnotations')
-- but not distilled.
-- | `${a.b.c}` / `${a.b.c:default}` placeholder -> config key `a.b.c`.
-- Shared by the `configKeys` nodeExtra distillation and the canonical
-- `config:` node pass (notable-string-extraction) so both stay in sync.
configKeysOf :: Text -> [Text]
configKeysOf t =
  case T.stripPrefix "${" t of
    Just rest ->
      let key = fst (T.breakOn ":" (fst (T.breakOn "}" rest)))
      in [key | not (T.null key)]
    Nothing -> []

distillAnnotations :: [Annotation] -> [(Text, Value)]
distillAnnotations = concatMap distillOne
  where
    distillOne (Annotation name margs) = case name of
      "Path" -> maybe [] (\a -> [("route", String (unwrapSingleArg a))]) margs
      "RequestMapping" -> maybe [] (\a -> [("route", String (unwrapSingleArg a))]) margs
      "GetMapping" -> ("httpVerb", String "GET") : routeFrom margs
      "PostMapping" -> ("httpVerb", String "POST") : routeFrom margs
      "PutMapping" -> ("httpVerb", String "PUT") : routeFrom margs
      "DeleteMapping" -> ("httpVerb", String "DELETE") : routeFrom margs
      "PatchMapping" -> ("httpVerb", String "PATCH") : routeFrom margs
      "GET" -> [("httpVerb", String "GET")]
      "POST" -> [("httpVerb", String "POST")]
      "PUT" -> [("httpVerb", String "PUT")]
      "DELETE" -> [("httpVerb", String "DELETE")]
      "PATCH" -> [("httpVerb", String "PATCH")]
      "Scheduled" -> maybe [] (\a -> catMaybesKV
                       [ fmap (\v -> ("cron", String v)) (namedArg "cron" a)
                       , fmap (\v -> ("fixedDelay", String v)) (namedArg "fixedDelay" a) ]) margs
      "KafkaListener" -> maybe [] (\a -> [("topics", toJSON [unwrapSingleArg a])]) margs
      "RabbitListener" -> maybe [] (\a -> [("topics", toJSON [unwrapSingleArg a])]) margs
      "Value" -> case margs of
        Just a | keys@(_:_) <- configKeysOf (unwrapSingleArg a) -> [("configKeys", toJSON keys)]
        _ -> []
      _ -> []
    routeFrom = maybe [] (\a -> [("route", String (unwrapSingleArg a))])
    catMaybesKV = foldr (\mkv acc -> maybe acc (: acc) mkv) []

-- | Merge distilled keys and the raw `annotations` array into a node's
-- existing `nodeExtra` (which may already carry an `exported`/`re-export`
-- marker) without discarding it.
mergeAnnotationExtra :: [Annotation] -> Maybe Value -> Maybe Value
mergeAnnotationExtra [] existing = existing
mergeAnnotationExtra anns existing =
  let base = case existing of Just (Object o) -> o; _ -> KM.empty
      annsJson = toJSON
        [ object (("name" .= annName a) : maybe [] (\x -> ["args" .= x]) (annArgs a))
        | a <- anns ]
      withAnns = KM.insert (Key.fromText "annotations") annsJson base
      withDistilled = foldr (\(k, v) acc -> KM.insert (Key.fromText k) v acc) withAnns (distillAnnotations anns)
  in Just (Object withDistilled)

-- | (definition's (base id, line) key, resolved annotation-import id) pairs
-- — one `references` edge per annotation whose name resolves through the
-- file's import bindings (reusing the same best-effort import index the
-- `calls` resolver builds). Unresolvable annotation names emit no edge.
annotationRefPairs :: SourceFile -> Granularity -> [TSNodeInfo] -> [((NodeId, Int), NodeId)]
annotationRefPairs sf gran roots =
  let ctx = collectCallsCtx sf gran roots
  in [ ((bid, tsnStartRow n), tid)
     | (n, anns) <- concatMap (definitionsWithAnnotations gran) roots
     , Just bid <- [definitionBaseId sf gran n]
     , Annotation { annName = name } <- anns
     , Just tid <- [Map.lookup name (callsCtxImports ctx)] ]

-- ───────────────────────────────────────────────
-- Notable strings: env / config / topic (extraction-semantic-enrichment)
-- ───────────────────────────────────────────────

-- | The three canonical rendezvous-node families a gated string literal can
-- resolve to. Ids carry no directory hash — the same name from any file in
-- the corpus is the same node (`env:CLIENTS_TABLE`, not one per reader).
data NotableKind = EnvVar | ConfigKey | Topic deriving (Eq, Ord, Show)

notablePrefix :: NotableKind -> Text
notablePrefix EnvVar    = "env:"
notablePrefix ConfigKey = "config:"
notablePrefix Topic     = "topic:"

notableNodeKind :: NotableKind -> Text
notableNodeKind EnvVar    = "Env"
notableNodeKind ConfigKey = "Config"
notableNodeKind Topic     = "Topic"

-- | Environment-variable read forms, matched by normalized node text prefix
-- — best-effort and name-based, consistent with the rest of this file's
-- call/annotation resolution rather than a full per-grammar AST model.
-- Covers: @System.getenv("X")@ (java), @os.getenv("X")@ / @os.environ["X"]@
-- (python), @process.env.X@ (javascript/typescript — a member access, not a
-- call), @std::env::var("X")@ (rust), @getenv("X")@ (c/cpp/perl),
-- @ENV["X"]@ (ruby), @$_ENV["X"]@ (php).
envVarName :: TSNodeInfo -> Maybe Text
envVarName n =
  let txt = normalizeText (tsnText n)
  in if any (`T.isPrefixOf` txt) callPrefixes then firstStringArgOf n
     else case lookupPrefix txt memberPrefixes of
            Just p  -> memberSuffix txt p
            Nothing -> case lookupPrefix txt subscriptPrefixes of
              Just p  -> subscriptKey txt p
              Nothing -> Nothing
  where
    callPrefixes = ["System.getenv(", "os.getenv(", "os.environ.get(", "std::env::var(", "getenv("]
    memberPrefixes = ["process.env."]
    subscriptPrefixes = ["ENV[", "$_ENV[", "os.environ["]
    lookupPrefix t ps = case filter (`T.isPrefixOf` t) ps of (p : _) -> Just p; [] -> Nothing
    -- The string literal lives inside the call's `arguments` field value
    -- (an `argument_list`/`argument_list`-shaped node), not as a direct
    -- child of the call itself; fall back to scanning the call's own direct
    -- children for grammars/node shapes with no `arguments` field.
    firstStringArgOf node =
      let cs = maybe (tsnChildren node) tsnChildren (childByField "arguments" node)
      in fmap (stripQuotes . T.strip) (findStringLiteral cs)
    identChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
    memberSuffix t prefix =
      let name = T.takeWhile (`elem` identChars) (T.drop (T.length prefix) t)
      in if T.null name then Nothing else Just name
    subscriptKey t prefix =
      let key = T.takeWhile (/= ']') (T.drop (T.length prefix) t)
      in if T.null key then Nothing else Just (stripQuotes (T.strip key))

-- | (enclosing definition's (base id, line) key, env var name) pairs for
-- every environment-read reachable anywhere in the file. Unlike
-- 'callEdgePairs' this walk ignores the granularity's opaque-body gating
-- entirely, so a `System.getenv(...)` deep in a method body still surfaces
-- even when that method's body would otherwise be opaque at Function
-- granularity.
envRefPairs :: SourceFile -> Granularity -> [TSNodeInfo] -> [((NodeId, Int), Text)]
envRefPairs sf gran roots = concatMap (walk Nothing) roots
  where
    walk enc n =
      let tType = tsnType n
          emitted = tsnIsNamed n && tType `elem` typesFor gran && tType `notElem` wrapperTypes
          enc' = if emitted && (tType `elem` functionBoundaryTypes || tType `elem` moduleTypes)
                   then fmap (\bid -> (bid, tsnStartRow n)) (definitionBaseId sf gran n)
                   else enc
          here = [ (key, name) | Just key <- [enc'], Just name <- [envVarName n] ]
      in here ++ concatMap (walk enc') (tsnChildren n)

-- | (definition's (base id, line) key, kind, name) triples for config keys
-- (`@Value`) and topics (`@KafkaListener`/`@RabbitListener`) — reusing the
-- same annotation data 'distillAnnotations' derives from, so the
-- human-readable `nodeExtra` keys and these graph nodes/edges never drift
-- apart.
annotationNotablePairs :: SourceFile -> Granularity -> [TSNodeInfo] -> [((NodeId, Int), NotableKind, Text)]
annotationNotablePairs sf gran roots =
  [ ((bid, tsnStartRow n), kind, name)
  | (n, anns) <- concatMap (definitionsWithAnnotations gran) roots
  , Just bid <- [definitionBaseId sf gran n]
  , a@Annotation { annName = annNm } <- anns
  , (kind, name) <- case annNm of
      "Value" -> [ (ConfigKey, k) | Just args <- [annArgs a], k <- configKeysOf (unwrapSingleArg args) ]
      "KafkaListener" -> topicOf a
      "RabbitListener" -> topicOf a
      _ -> [] ]
  where
    topicOf a = [ (Topic, t) | Just args <- [annArgs a], let t = unwrapSingleArg args, not (T.null t) ]

-- | Every (key, kind, name) triple — env-reads plus annotation-derived
-- config/topic refs — bounded to 32 distinct (kind, name) pairs per file
-- (notable-string-extraction spec); names outside 1-80 characters are
-- ignored. Over-cap entries are dropped silently: this module is pure (no
-- IO), so the spec's "logged at debug level" is not implemented here.
notableTriples :: SourceFile -> Granularity -> [TSNodeInfo] -> [((NodeId, Int), NotableKind, Text)]
notableTriples sf gran roots =
  let envs = [ (k, EnvVar, n) | (k, n) <- envRefPairs sf gran roots ]
      all' = envs ++ annotationNotablePairs sf gran roots
      sized = [ t | t@(_, _, n) <- all', let len = T.length n, len >= 1, len <= 80 ]
      keptKeys = Set.fromList (take 32 (ordNubOn (\(_, k, n) -> (k, n)) sized))
  in [ t | t@(_, k, n) <- sized, (k, n) `Set.member` keptKeys ]
  where
    ordNubOn f = go Set.empty
      where go _ [] = []
            go seen (x : xs)
              | f x `Set.member` seen = go seen xs
              | otherwise = f x : go (Set.insert (f x) seen) xs

-- | One canonical node per distinct (kind, name) — corpus-global identity,
-- no per-file directory hash, so every reader of the same env var/config
-- key/topic converges on the same node.
notableNodes :: [((NodeId, Int), NotableKind, Text)] -> [Node]
notableNodes triples =
  [ Node
      { nodeId = notablePrefix kind <> name
      , nodeLabel = fromText name
      , nodeFileType = CodeFile
      , nodeSourceFile = fromText ""
      , nodeSource = Nothing
      , nodeLineStart = Nothing
      , nodeLineEnd = Nothing
      , nodeSignature = Nothing
      , nodeCommunityId = Nothing
      , nodeDegree = Nothing
      , nodeIsBridge = Nothing
      , nodeExtra = Nothing
      , nodeKind = Just (fromText (notableNodeKind kind))
      , nodePresentBits = bitNodeKind
      }
  | (kind, name) <- Set.toList (Set.fromList [ (k, n) | (_, k, n) <- triples ]) ]

-- | @confidence@ is 1.0 for an annotation resolving to its imported symbol
-- (structural: the import binding is exact) and 0.9 for a notable-string
-- reference (best-effort, name-based — matches the `calls` resolver's own
-- confidence for the same reason).
mkReferencesEdge :: Double -> NodeId -> NodeId -> Edge
mkReferencesEdge conf src tgt =
  Edge
    { edgeId        = EdgeId (src <> "->ref:" <> tgt)
    , edgeSource    = src
    , edgeTarget    = tgt
    , edgeRelation  = References
    , edgeConfidence = Confidence conf
    , edgeWeight    = conf
    , edgeExtra     = Nothing
    }

-- ───────────────────────────────────────────────
-- Edge + endpoint-node construction
-- ───────────────────────────────────────────────

rnFinalNode :: Map.Map (NodeId, Int) [Annotation] -> RawNode -> NodeId -> Node
rnFinalNode annMap r fid =
  let anns = Map.findWithDefault [] (rnBaseId r, rnLine r) annMap
      base = rnNode r
      extra' = mergeAnnotationExtra anns (nodeExtra base)
  in base
       { nodeId = fid
       , nodeExtra = extra'
       , nodePresentBits = if isJust extra' then nodePresentBits base .|. bitNodeExtra else nodePresentBits base
       }

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
  "compilation_unit" -> "Module"
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
  "use_no_statement" -> "Import"
  "use_constant_statement" -> "Import"
  "use_parent_statement" -> "Import"
  "require_statement" -> "Import"
  "include_expression" -> "Import"
  "include_once_expression" -> "Import"
  "require_expression" -> "Import"
  "require_once_expression" -> "Import"
  "namespace_use_declaration" -> "Import"
  _ -> t
