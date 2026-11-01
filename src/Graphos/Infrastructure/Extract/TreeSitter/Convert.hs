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
  , applyCollisionRanks
  , definitionTypes
  , structureTypes
  , apiSurfaceTypes
  , importExportTypes
  , implementationDetailTypes
  , functionBoundaryTypes
    , makeNodeId
    , makeNode
    , fileModuleNode
    , tsNodeLabel
    , tsNodeUntruncatedLabel
    , nodeIdentityBudget
    -- | Node-identity helpers: stable `<dirHash>_<stem>_<kind>:<ident>`
    -- scheme (AVI-1452). 'nodeKeyId' emits the seed id (rank 0); the
    -- deterministic in-file collision pass appends `#L<line>` ranks later.
    , SourceFile(..)
    , mkSourceFile
    , nodeKeyId
    ) where


import Data.List (isPrefixOf, find, sortBy, groupBy)
import Data.Ord (comparing)
import System.FilePath (takeExtension)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText, ShortText)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Aeson (toJSON)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)

import Graphos.Domain.Config (Granularity(..))
import Graphos.Domain.Types
  ( Node(..), Edge(..), Extraction(..), extractionFromLists
  , NodeId, FileType(..), Relation(..), Confidence(..), EdgeId(..)
  , bitNodeKind, bitNodeLineStart, bitNodeLineEnd, bitNodeSignature
  )
import Data.Bits ((.|.))
import Graphos.Infrastructure.Extract.TreeSitter.Core (TSNodeInfo(..), normalizeText, defaultTruncationBudget, truncateWithElision, truncateText, nodeIdentityBudget)
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
            ( tsNodeToGraphNodes gran filePath node
            , tsNodeToGraphEdges gran filePath Nothing Nothing node )) nodes
        graphNodes = concatMap fst results
        extraNodes = concatMap (snd . snd) results
        graphEdges = concatMap (fst . snd) results
        -- Deterministic in-file collision pass: append `#L<line>` to colliding
        -- node ids and rewire contains edges so the bounded ids stay
        -- consistent. Applied to the node/edge lists *before*
        -- 'extractionFromLists' so true in-file collisions (same seed id) are
        -- disambiguated, not collapsed by Map.fromList.
        (finalNodes, finalEdges) = applyCollisionRanks (graphNodes ++ extraNodes) graphEdges
    in extractionFromLists finalNodes finalEdges

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
--
-- | Convert a TSNodeInfo tree into Graphos Edges (Contains parent→child).
--
-- Threads the parent node's bounded seed id ('parentId') so a contains edge
-- references the exact seed id of the parent node, and a file-wide 'CallsCtx'
-- (built from the root by 'collectCallsCtx') carries the file's import bindings
-- and local definition names for best-effort `calls` edge emission. Both are
-- threaded down the recursion together. In-file collisions are resolved later
-- by 'applyCollisionRanks', which appends `#L<line>` and rewires contains edges.
tsNodeToGraphEdges
  :: Granularity -> FilePath -> Maybe NodeId -> Maybe Text -> TSNodeInfo -> ([Edge], [Node])
tsNodeToGraphEdges gran filePath parentId parentLabel node =
  tsNodeToGraphEdges' gran filePath (collectCallsCtx filePath node) parentId parentLabel (tsNodeLabel node) node

-- | Internal recursion carrying the call context, the parent's seed id and the
-- enclosing definition label down the tree.
tsNodeToGraphEdges'
  :: Granularity -> FilePath -> CallsCtx -> Maybe NodeId -> Maybe Text -> Text -> TSNodeInfo -> ([Edge], [Node])
tsNodeToGraphEdges' gran filePath ctx parentId parentLabel enclosingDef node =
  let myLabel = tsNodeLabel node
      isDef     = tsnType node `elem` typesFor gran && tsnIsNamed node
      tType     = tsnType node
      myId      = seedIdFor filePath node
      isCall    = tType == "call_expression" || tType == "new_expression"
      insideCall = callsCtxInside ctx
      -- The enclosing definition label threads through statement-level (non
      -- boundary) nodes unchanged; it only updates when we descend into a
      -- function/method/constructor boundary. This is the source of `calls`
      -- edges (spec: enclosing function name, or `module` for module-level).
      childDef = if tType `elem` functionBoundaryTypes then myLabel else enclosingDef

      containsEdge source target = Edge
        { edgeId        = EdgeId (source <> "->" <> target <> ":contains")
        , edgeSource    = source
        , edgeTarget    = target
        , edgeRelation  = Contains
        , edgeConfidence = Confidence 1.0
        , edgeWeight    = 1.0
        , edgeExtra     = Nothing
        }

      -- Parent → child edge (Contains). References the parent's seed id
      -- ('parentId') and this node's seed id ('myId').
      myEdges = case (parentId, isDef) of
        (Just p, True) -> [ containsEdge p myId ]
        _              -> []

      -- Calls edge (top-level call of an enclosing function only).
      callsEdges =
        if isCall && not insideCall
           then case resolveCalls filePath ctx enclosingDef node of
                  Just e -> [e]
                  Nothing -> []
           else []

      -- Function → `use` edges for `use` items inside an opaque function body.
      bodyUseEdges = [ containsEdge myId (seedIdFor filePath u) | u <- opaqueBodyUses gran isDef node ]

      -- Imports/Exports edges. Go/Rust/Ruby imports are resolved with
      -- repository context in
      -- Graphos.Infrastructure.Extract.TreeSitter.Imports; Go's
      -- @import_declaration@ must not use the TypeScript specifier path.
      importEdges
        | tType `elem` importExportTypes && takeExtension filePath /= ".go" =
            case extractSpecifier filePath node of
              Just (targetPath, targetName) ->
                [ Edge
                    { edgeId        = EdgeId (importSource <> "->" <> importTargetId targetPath targetName <> ":imports")
                    , edgeSource    = importSource
                    , edgeTarget    = importTargetId targetPath targetName
                    , edgeRelation  = Imports
                    , edgeConfidence = Confidence 1.0
                    , edgeWeight    = 1.0
                    , edgeExtra     = if tType /= "import_declaration" && tType /= "import_statement" && tType /= "import_from_statement"
                                      then Just (toJSON (T.pack "re-export"))
                                      else Nothing
                    } ]
              Nothing -> []
          | otherwise = []

      importNodes
        | tType `elem` importExportTypes && takeExtension filePath /= ".go" =
            case extractSpecifier filePath node of
              Just (targetPath, targetName) ->
                [ Node
                    { nodeId           = importTargetId targetPath targetName
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
                    } ]
              Nothing -> []
          | otherwise = []

      -- Recurse into children with a threaded call context. The child context
      -- flips `callsCtxInside` on whenever the current node is a call (or we are
      -- already inside one), so calls nested inside a call's arguments are
      -- suppressed by the `callsEdges` guard above. The parent id threads down
      -- as 'Just myId' for a definition's children, staying 'parentId' for
      -- intervening (non-definition) nodes.
      childCtx = ctx { callsCtxInside = insideCall || isCall }

      (childEdges, childNodes)
        | not (descendInto gran isDef node) = ([], [])
        | isDef = foldr (\child (es, ns) ->
              let (e, n) = tsNodeToGraphEdges' gran filePath childCtx (Just myId) (Just myLabel) childDef child
              in (e ++ es, n ++ ns)) ([], []) (tsnChildren node)
        | otherwise = foldr (\child (es, ns) ->
              let (e, n) = tsNodeToGraphEdges' gran filePath childCtx parentId parentLabel childDef child
              in (e ++ es, n ++ ns)) ([], []) (tsnChildren node)

    in (myEdges ++ callsEdges ++ bodyUseEdges ++ importEdges ++ childEdges, importNodes ++ childNodes)
  where
    -- | Source id of an import edge: the parent's seed id when available, else
    -- the legacy path-scheme id of the enclosing module.
    importSource :: NodeId
    importSource = case parentId of
      Just p   -> p
      Nothing  -> makeNodeId filePath (fromMaybe "" parentLabel)

    importTargetId :: FilePath -> Text -> NodeId
    importTargetId targetPath targetName
      | "external:" `isPrefixOf` targetPath = makeNodeId "external" targetName
      | otherwise = makeNodeId targetPath targetName

-- | Extract the import/export specifier: the direct @string@ child of the
-- declaration node — i.e. the import path (the token after @from@).
--
-- Sourcing the specifier from the AST rather than scanning the whole node text
-- is what keeps template literals and prose that merely contain the word
-- @from@ (issue #77) from producing bogus @imports@ edges. A template literal's
-- text lives under a nested @template_string@ node, so it is never a direct
-- @string@ child of an import/export declaration and can never be mistaken for
-- a specifier.
extractSpecifier :: FilePath -> TSNodeInfo -> Maybe (FilePath, Text)
extractSpecifier filePath n =
  case firstChildOfType "string" (tsnChildren n) of
    Just child ->
      let specifier = stripQuotes (T.strip (tsnText child))
      in if T.null specifier
           then Nothing
           else resolveImport filePath (T.unpack specifier)
    Nothing -> Nothing

-- | First direct child of the given type, if any.
firstChildOfType :: String -> [TSNodeInfo] -> Maybe TSNodeInfo
firstChildOfType t = find (\c -> tsnType c == t)

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
--
-- Emits a bounded node identity: the id is the seed of the
-- `<dirHash>_<stem>_<kind>:<ident>` scheme (rank 0, no `#L` suffix — the
-- deterministic collision pass in 'tsNodesToExtraction' appends that later
-- once it has sibling context). The label and the signature are both the
-- declaration text bounded by 'nodeIdentityBudget', so the per-node size of
-- graph.json stays bounded (task 3.3, AVI-1452). The 'nodeSignature' present
-- bit is set so 'computePresentBits' / the JGF FromJSON round-trip preserves it.
makeNode :: FilePath -> TSNodeInfo -> Node
makeNode filePath node =
    let sf = mkSourceFile filePath
        line = tsnStartRow node + 1
        text' = tsNodeUntruncatedLabel node
        -- 'truncateText' appends a 3-char elision "...", so reserve it and keep
        -- the final <ident> — and the label/signature derived from it — within
        -- nodeIdentityBudget (AVI-1452). See Core.truncateText.
        elisionBudget = 3
        ident = truncateText (nodeIdentityBudget - elisionBudget) text'
        kindText = T.pack (tsTypeToKind (tsnType node))
        kind  = fromText kindText
    in Node
         { nodeId           = nodeKeyId sf kindText ident line 0
         , nodeLabel        = fromText ident
         , nodeFileType     = CodeFile
         , nodeSourceFile   = fromText (T.pack filePath)
         , nodeSource       = Nothing
         , nodeLineStart    = Just line
         , nodeLineEnd      = Just (tsnEndRow node + 1)
         , nodeKind         = Just kind
         , nodeSignature    = Just (fromText ident)
         , nodePresentBits  = bitNodeLineStart .|. bitNodeLineEnd .|. bitNodeKind .|. bitNodeSignature
         , nodeCommunityId  = Nothing
         , nodeDegree       = Nothing
         , nodeIsBridge     = Nothing
         , nodeExtra        = Nothing
         }

-- | Seed id (rank 0, no `#L` suffix) of a node under the bounded-identity
-- scheme. Threads a parent's exact seed id down the recursion and references a
-- child's seed id in a contains edge before 'applyCollisionRanks' may later
-- append a `#L<line>` suffix to disambiguate in-file collisions.
seedIdFor :: FilePath -> TSNodeInfo -> NodeId
seedIdFor filePath node =
    let sf     = mkSourceFile filePath
        kindText = T.pack (tsTypeToKind (tsnType node))
        -- Reserve elision space so the final <ident> stays within
        -- nodeIdentityBudget (see makeNode).
        ident    = truncateText (nodeIdentityBudget - 3) (tsNodeUntruncatedLabel node)
        line     = tsnStartRow node + 1
    in nodeKeyId sf kindText ident line 0

-- | Deterministic in-file collision pass for the bounded-identity scheme.
--
-- The rank-0 seed id emitted by 'makeNode' / 'seedIdFor' is
-- `<dirHash>_<stem>_<kind>:<ident>` and does NOT carry the start line, so two
-- definitions in one file that normalise to the same text share an id and are
-- collapsed by 'extractionFromLists'. This pass restores them: nodes are
-- grouped by their (shared) seed id, and within each colliding group the
-- first occurrence (by `(seedId, lineStart)`) keeps its bare seed id while
-- later occurrences get a `#L<lineStart>` suffix. Colliding nodes sit on
-- distinct lines, so the suffixes are unique and the run-to-run order is
-- irrelevant (criterion c). Only nodes that actually share a seed id are
-- suffixed; a unique-seed node (e.g. the module node that is the source of an
-- import edge) keeps its bare id so the edge needs no rewiring (criterion d).
-- Import/specifier stub nodes (no start line) are excluded from the pass.
applyCollisionRanks :: [Node] -> [Edge] -> ([Node], [Edge])
applyCollisionRanks nodes edges =
    let withRank  = concatMap rankFile (Map.toAscList byFile)
        remap     = Map.fromList [ (seedId, newId) | (seedId, _, newId) <- withRank ]
        newNodes  = map (\(_, n, _) -> n) withRank
                    ++ [ n | n <- nodes, not (isJust (nodeLineStart n)) ]
        newEdges  = map (remapContains remap) edges
    in (newNodes, newEdges)
  where
    -- Group-3 nodes (with a start line), grouped by source file. File order is
    -- ascending (Map.toAscList), so the rank assignment below is deterministic.
    byFile :: Map.Map ShortText [Node]
    byFile = Map.fromListWith (++)
           [ (nodeSourceFile n, [n]) | n <- nodes, isJust (nodeLineStart n) ]

    -- (originalSeed, nodeWithNewId, newId). Nodes are grouped by their seed id:
    -- only nodes that actually collide (share the same rank-0 seed id) receive
    -- a `#L<lineStart>` suffix. A unique-seed node stays at rank 0 with its
    -- bare id, so edges referencing it (e.g. an import edge from the module
    -- node) need no rewiring. Within each seed group, nodes are stably sorted
    -- by (seedId, lineStart) so input order is irrelevant (criterion c).
    rankFile :: (ShortText, [Node]) -> [(NodeId, Node, NodeId)]
    rankFile (_, ns) =
      concatMap rankGroup
        (groupBy sameSeed (sortBy (comparing key) keyed))
      where
        keyed = [ (nodeId n, fromMaybe 0 (nodeLineStart n), n) | n <- ns ]
        key (seed, line, _) = (seed, line)

        -- Same colliding group iff the rank-0 seed ids match.
        sameSeed :: (NodeId, Int, Node) -> (NodeId, Int, Node) -> Bool
        sameSeed (a, _, _) (b, _, _) = a == b

        rankGroup :: [(NodeId, Int, Node)] -> [(NodeId, Node, NodeId)]
        rankGroup ranked = zipWith assign [0 ..] ranked
          where
            assign :: Int -> (NodeId, Int, Node) -> (NodeId, Node, NodeId)
            assign i (seed, line, n)
              | i > 0     = (seed, n { nodeId = newId }, newId)
              | otherwise = (seed, n, seed)
              where
                newId = seed <> "#L" <> T.pack (show line)

    -- Rewire only Contains edges: their endpoints are Group-3 seed ids that may
    -- have been suffixed. Everything else (Imports/external stubs) is left
    -- as-is, falling back to the original id via findWithDefault.
    remapContains :: Map.Map NodeId NodeId -> Edge -> Edge
    remapContains remap e =
      case edgeRelation e of
        Contains ->
          let s = Map.findWithDefault (edgeSource e) (edgeSource e) remap
              t = Map.findWithDefault (edgeTarget e) (edgeTarget e) remap
          in e { edgeSource = s, edgeTarget = t,
                 edgeId   = EdgeId (s <> "->" <> t <> ":contains") }
        _ -> e

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

-- | File stem (file name without its final extension), matching
-- 'System.FilePath.takeBaseName': only the final extension is removed, and a
-- dotfile such as @.bashrc@ (no dot past position 0) is left untouched.
takeBaseName' :: FilePath -> FilePath
takeBaseName' f =
  let fn  = takeFileName' f
      ext = takeExtension fn
  in if null ext then fn
       else let stem = take (length fn - length ext) fn
            in if null stem then fn else stem

-- | Stable identifier for a node under the AVI-1452 bounded-identity scheme.
-- When `rank > 0` the declaration gets a file-wide `#L<line>` suffix so that
-- in-file collisions are disambiguated deterministically.
nodeKeyId :: SourceFile -> Text -> Text -> Int -> Int -> NodeId
nodeKeyId sourceFile kind ident line rank =
  let dirHash' = T.pack (show (sourceDirHash sourceFile))
      base     = dirHash' <> "_" <> sourceStem sourceFile <> "_" <> kind <> ":" <> ident
  in if rank > 0 then base <> "#L" <> T.pack (show line) else base
