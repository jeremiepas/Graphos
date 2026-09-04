-- | Evaluation of the read-only openCypher/GQL subset against the
-- in-memory property graph, plus the mutation evaluator for the write
-- clause subset (openspec change opencypher-write-mutations).
--
-- The read evaluator is a forward-walk over the graph: each MATCH path is
-- anchored at its first node, and the remaining relationship / node
-- patterns are walked forward from there. Multiple comma-separated paths
-- are combined by a Cartesian product of their bindings, with shared
-- variables AND-ed (a binding is kept only when every path agrees on
-- the value of a shared variable).
--
-- The mutation evaluator reuses the MATCH binding enumeration, folds the
-- write operations left-to-right over the graph for every matched
-- binding, and reports a MutationSummary. Model reconciliation rules
-- (labels, properties, closed relation vocabulary, upsert) live in the
-- Graph.Mutation helpers.
--
-- Pure — no IO, fully testable.
module Graphos.Domain.Query.Cypher.Eval
  ( -- * Result
    CypherResult(..)

    -- * Mutation results
  , MutationSummary(..)
  , MutationResult(..)

    -- * Evaluation
  , evaluate
  , evaluateStatement
  ) where

import Data.Aeson (Value(..), ToJSON(..))
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText)

import Text.Regex.TDFA (makeRegexM, matchTest, Regex)

import Graphos.Domain.Types (NodeId, Node(..), Edge(..), EdgeId(..), textToRelation, FileType(..), Confidence(..), relationToText)
import Graphos.Domain.Graph (Graph, gNodes, gEdges)
import Graphos.Domain.Graph.Mutation
  ( MutationSummary(..)
  , emptyMutationSummary
  , putNode
  , addNodeLabel
  , removeNodeLabel
  , setNodeProp
  , removeNodeProp
  , nodeExtraLabels
  , deleteEdge
  , putEdgeUpsert
  , setEdgeProp
  , removeEdgeProp
  , deleteEdgesTouching
  , rebuildAdjacency
  )
import Graphos.Domain.Graph.Index (GraphIndex)
import Graphos.Domain.Query.Cypher.AST
import Graphos.Domain.Query.Cypher.Mapping

-- ───────────────────────────────────────────────
-- Result
-- ───────────────────────────────────────────────

-- | A query result: column names, projected rows, and a truncation flag.
data CypherResult = CypherResult
  { crColumns   :: [Text]
    -- ^ Column names, in RETURN order.
  , crRows      :: [[Value]]
    -- ^ Projected rows.
  , crTruncated :: Bool
    -- ^ True when the result was capped by the budget.
  } deriving (Eq, Show)

-- ───────────────────────────────────────────────
-- Bindings
-- ───────────────────────────────────────────────

-- | The value bound to a variable.
data Bound
  = BNode NodeId
  | BEdge Edge
  | BPath [Edge]
  | BCounter Int
    -- ^ Mutation summary counter (implicit RETURN binding).
  deriving (Eq, Show)

-- | A variable-to-value binding.
type Binding = Map Text Bound

-- ───────────────────────────────────────────────
-- Entry point
-- ───────────────────────────────────────────────

-- | Evaluate a query against a graph, bounded by @budget@ rows.
evaluate :: Int -> CypherQuery -> Graph -> GraphIndex -> CypherResult
evaluate budget q g _idx =
  let (bindings, truncated) = takeCap budget (enumerateBindings g (cqPatterns q))
      filtered = case cqWhere q of
        Nothing -> bindings
        Just p  -> [ b | b <- bindings, evalPredicate b g p ]
      rc = cqReturn q
      allCounts = all isCountItem (rcItems rc)
      pairs = if allCounts
        then case filtered of
          []      -> []
          (b:_)   -> [ (b, projectRow g rc filtered b) ]
        else [ (b, projectRow g rc filtered b) | b <- filtered ]
      distinctPairs = if rcDistinct rc then dedupPairs pairs else pairs
      ordered = orderByBindings g (rcOrderBy rc) distinctPairs
      skipped = case rcSkip rc of
        Nothing -> ordered
        Just n  -> drop n ordered
      limited = case rcLimit rc of
        Nothing -> skipped
        Just n  -> take n skipped
  in CypherResult (columnNames rc) [ row | (_, row) <- limited ] truncated
  where
    isCountItem (RICount _ _) = True
    isCountItem _             = False

-- ───────────────────────────────────────────────
-- Mutation evaluation
-- ───────────────────────────────────────────────

-- | A mutation result: the mutated graph, the summary, and the optional
-- RETURN projection.
data MutationResult = MutationResult
  { mrGraph     :: Graph
  , mrSummary   :: MutationSummary
  , mrResult    :: CypherResult
    -- ^ RETURN rows (empty columns when there is no RETURN).
  } deriving (Eq, Show)

-- | Evaluate a statement. Read statements delegate to 'evaluate';
-- mutation statements fold their operations over each matched binding.
evaluateStatement :: Int -> CypherStatement -> Graph -> GraphIndex -> Either Text MutationResult
evaluateStatement budget (ReadStatement q) g idx =
  let r = evaluate budget q g idx
  in Right (MutationResult g emptyMutationSummary r)
evaluateStatement budget (MutStatement m) g idx = evaluateMutation budget m g idx

-- | Evaluate a mutation: bind MATCH paths (budget-capped, as reads),
-- fold ops over the graph per binding, then project the optional RETURN
-- against the post-mutation graph. The summary counters are exposed as
-- implicit RETURN bindings (@nodes_created@, @rels_created@,
-- @rels_upserted@, @properties_set@, @properties_removed@,
-- @nodes_deleted@, @rels_deleted@).
evaluateMutation :: Int -> Mut -> Graph -> GraphIndex -> Either Text MutationResult
evaluateMutation budget m g0 idx =
  let baseBindings = case mMatch m of
        Nothing -> [Map.empty]
        Just q  ->
          let (bs, _) = takeCap budget (enumerateBindings g0 (cqPatterns q))
              filtered = case cqWhere q of
                Nothing -> bs
                Just p  -> [ b | b <- bs, evalPredicate b g0 p ]
          in if null filtered then [Map.empty] else filtered
  in case foldOps g0 baseBindings (mOps m) of
       Left err -> Left err
       Right (g1, summary) ->
         let g2 = rebuildAdjacency g1
             cr = case mReturn m of
               Nothing -> CypherResult [] [] False
               Just rc -> projectReturn budget rc g2 idx summary baseBindings
         in Right (MutationResult g2 summary cr)

-- | Fold the write operations over the graph. When there is a MATCH
-- prefix, each binding enumerates one application pass; without a MATCH
-- the ops apply once against an empty binding. Returns the final graph
-- and summary.
foldOps :: Graph -> [Binding] -> [MutOp] -> Either Text (Graph, MutationSummary)
foldOps g bindings ops
  | null ops = Right (g, emptyMutationSummary)
  | otherwise =
      foldEither (\(gAcc, sAcc) b -> foldOpsOne b (gAcc, sAcc) ops)
                 (g, emptyMutationSummary) bindings

-- | Apply every op for one binding, threading the graph + summary.
foldOpsOne :: Binding -> (Graph, MutationSummary) -> [MutOp] -> Either Text (Graph, MutationSummary)
foldOpsOne _ acc [] = Right acc
foldOpsOne b (g, s) (op:rest) = case applyOp b g s op of
  Left err -> Left err
  Right (g', s') -> foldOpsOne b (g', s') rest

-- | Strict left fold over an Either.
foldEither :: (a -> b -> Either e a) -> a -> [b] -> Either e a
foldEither _ acc [] = Right acc
foldEither f acc (x:xs) = case f acc x of
  Left e   -> Left e
  Right a' -> foldEither f a' xs

-- | Apply a single write operation for one binding.
applyOp :: Binding -> Graph -> MutationSummary -> MutOp -> Either Text (Graph, MutationSummary)
applyOp b g s op = case op of
  MCreate pats -> createPatterns b g s pats
  MMerge pat ons -> mergePattern b g s pat ons
  MSet items -> Right (foldl' (applySetFor b) (g, s) items)
  MRemove items -> Right (foldl' (applyRemoveFor b) (g, s) items)
  MDelete detach vs -> deleteVars b g s detach vs

-- ───────────────────────────────────────────────
-- CREATE
-- ───────────────────────────────────────────────

createPatterns :: Binding -> Graph -> MutationSummary -> [PatternElem] -> Either Text (Graph, MutationSummary)
createPatterns b g s pats = do
  (_, (g', s')) <- createSeq b (g, s) pats
  Right (g', s')
  where
    createSeq b' acc [] = Right (b', acc)
    createSeq b' acc (NodePatE np : rest) = do
      (b'', acc') <- createNode acc b' np
      createSeq b'' acc' rest
    createSeq b' acc (RelPatE rp : rest) = do
      acc' <- createRel acc b' rp
      createSeq b' acc' rest

-- | Resolve a pattern's variable: an existing binding wins; otherwise
-- the node must already exist (e.g. an anchor from a previous pattern in
-- the same CREATE) or a fresh node is created. Returns the augmented
-- binding so chained patterns see created nodes.
createNode :: (Graph, MutationSummary) -> Binding -> NodePat -> Either Text (Binding, (Graph, MutationSummary))
createNode (g, s) b np =
  let var = npVar np
  in case Map.lookup var b of
    Just (BNode nid) ->
      case Map.lookup nid (gNodes g) of
        Just n -> Right (b, applyNodeProps (g, s) np n)
        Nothing -> Left ("unbound variable in CREATE pattern: " <> var)
    _ ->
      -- Fresh node (or an anonymous pattern): build from the pattern.
      let props = map (\(k, e) -> (k, evalExpr b g e)) (Map.toList (npProps np))
          mId = lookup "id" props >>= asText
          lbl = case npLabels np of
            (l:_) -> Just l
            []    -> Nothing
      in case mId of
        Just nid ->
          if Map.member nid (gNodes g)
            then Left ("node already exists: " <> nid)
            else Right (Map.insert var (BNode nid) b, insertFreshNode (g, s) nid lbl props)
        Nothing ->
          let nid = generatedId g var
          in Right (Map.insert var (BNode nid) b, insertFreshNode (g, s) nid lbl props)

insertFreshNode :: (Graph, MutationSummary) -> NodeId -> Maybe Text -> [(Text, Value)] -> (Graph, MutationSummary)
insertFreshNode (g, s) nid lbl props =
  let n0 = freshNode nid lbl
      n1 = foldl' (\n (k, v) -> setNodeProp k v n) n0 props
      s' = s { msNodesCreated = msNodesCreated s + 1 }
  in (putNode n1 g, s')

-- | A fresh node from a CREATE pattern: label set (when the pattern
-- carries one), everything else defaulted.
freshNode :: NodeId -> Maybe Text -> Node
freshNode nid lbl = Node
  { nodeId          = nid
  , nodeLabel       = fromText nid
  , nodeFileType    = CodeFile
  , nodeSourceFile  = fromText ""
  , nodeLineStart   = Nothing
  , nodeLineEnd     = Nothing
  , nodeSignature   = Nothing
  , nodeCommunityId = Nothing
  , nodeKind        = fromText <$> lbl
  , nodeDegree      = Nothing
  , nodeIsBridge    = Nothing
  , nodeExtra       = Nothing
  , nodePresentBits = 0
  }

-- | Apply a CREATE node pattern's property constraints to an existing
-- (bound) node, counting properties set.
applyNodeProps :: (Graph, MutationSummary) -> NodePat -> Node -> (Graph, MutationSummary)
applyNodeProps (g, s) np n =
  let props = [ (k, evalExpr Map.empty g e) | (k, e) <- Map.toList (npProps np) ]
      n' = foldl' (\acc (k, v) -> setNodeProp k v acc) n props
      s' = s { msPropertiesSet = msPropertiesSet s + length props }
  in (putNode n' g, s')

-- | Create a relationship pattern: both endpoints must resolve to bound
-- or previously-created nodes. An existing (source, target) pair is
-- upserted.
createRel :: (Graph, MutationSummary) -> Binding -> RelPat -> Either Text (Graph, MutationSummary)
createRel (g, s) b rp = do
  src <- resolveEndpoint (rpFrom rp)
  tgt <- resolveEndpoint (rpTo rp)
  let rel = case rpTypes rp of
        (t:_) -> textToRelation (T.toLower t)
        []    -> Nothing
  case rel of
    Nothing -> Left ("CREATE relationships require a relationship type")
    Just r ->
      let props = [ (k, evalExpr b g e) | (k, e) <- Map.toList (rpProps rp) ]
          eid = EdgeId (src <> "->" <> tgt <> ":" <> relationToText r)
          e0 = Edge
            { edgeId         = eid
            , edgeSource     = src
            , edgeTarget     = tgt
            , edgeRelation   = r
            , edgeWeight     = 1.0
            , edgeConfidence = Confidence 1.0
            , edgeExtra      = Nothing
            }
          e1 = foldl' (\acc (k, v) -> setEdgeProp k v acc) e0 props
          (g1, created) = putEdgeUpsert e1 g
          s' = if created
                 then s { msRelsCreated = msRelsCreated s + 1
                        , msPropertiesSet = msPropertiesSet s + length props }
                 else s { msRelsUpserted = msRelsUpserted s + 1
                        , msPropertiesSet = msPropertiesSet s + length props }
      in Right (g1, s')
  where
    resolveEndpoint var = case Map.lookup var b of
      Just (BNode nid)
        | Map.member nid (gNodes g) -> Right nid
        | otherwise -> Left ("unbound node in CREATE pattern: " <> var)
      _ -> Left ("unbound variable in CREATE pattern: " <> var)

-- | MERGE: match on (label, id) first, then on the full pattern property
-- map; create when nothing matches, then apply ON CREATE / ON MATCH SET.
mergePattern :: Binding -> Graph -> MutationSummary -> PatternElem -> [OnClause] -> Either Text (Graph, MutationSummary)
mergePattern b g s pat ons = case pat of
  NodePatE np -> mergeNode b g s np ons
  RelPatE rp -> mergeRel b g s rp ons

mergeNode :: Binding -> Graph -> MutationSummary -> NodePat -> [OnClause] -> Either Text (Graph, MutationSummary)
mergeNode b g s np ons =
  let props = map (\(k, e) -> (k, evalExpr b g e)) (Map.toList (npProps np))
      lbl = case npLabels np of
        (l:_) -> Just l
        []    -> Nothing
      -- Match: (label, id) when the pattern constrains id, else the full
      -- property map over nodes carrying the label.
      matched :: Maybe Node
      matched = case lookup "id" props >>= asText of
        Just nid  -> Map.lookup nid (gNodes g)
        Nothing -> case candidates of
          (n:_) -> Just n
          []    -> Nothing
      candidates = [ n | n <- Map.elems (gNodes g)
                   , labelMatches lbl n
                   , all (\(k, v) -> nodePropertyFull n k == Just v) props ]
      matchSetItems = [ it | OnMatch its <- ons, it <- its ]
  in case matched of
    Just n ->
      let b' = Map.insert (npVar np) (BNode (nodeId n)) b
      in Right (foldl' (applySetFor b') (g, s) matchSetItems)
    Nothing ->
      let mId = lookup "id" props >>= asText
          nid = fromMaybe (generatedId g (npVar np)) mId
      in if Map.member nid (gNodes g)
           then Left ("node already exists: " <> nid)
           else
             let n0 = freshNode nid lbl
                 n1 = foldl' (\acc (k, v) -> setNodeProp k v acc) n0 props
                 s1 = s { msNodesCreated = msNodesCreated s + 1 }
                 b' = Map.insert (npVar np) (BNode nid) b
                 onCreateItems = [ it | OnCreate its <- ons, it <- its ]
                 (g2, s2) = foldl' (applySetFor b')
                              (putNode n1 g, s1) onCreateItems
             in Right (g2, s2)

-- | Does a node carry the pattern's primary label (or the pattern has
-- none, matching any node)?
labelMatches :: Maybe Text -> Node -> Bool
labelMatches Nothing _ = True
labelMatches (Just l) n =
  case nodeCypherLabel n of
    Just lbl -> lbl == l
    Nothing  -> l `elem` nodeExtraLabels n

-- | Full property lookup: model fields, then extra-labels, then the
-- extra object (via Mapping).
nodePropertyFull :: Node -> Text -> Maybe Value
nodePropertyFull = nodeProperty

-- | MERGE over a relationship pattern: upsert semantics (the model has
-- no parallel edges), then ON clauses.
mergeRel :: Binding -> Graph -> MutationSummary -> RelPat -> [OnClause] -> Either Text (Graph, MutationSummary)
mergeRel b g s rp ons = do
  (g1, s1) <- createRel (g, s) b rp
  let setItems = [ it | OnMatch its <- ons, it <- its ]
  Right (foldl' (applySetFor b) (g1, s1) setItems)

-- | Apply a SET item against the graph for the given binding.
applySetFor :: Binding -> (Graph, MutationSummary) -> SetItem -> (Graph, MutationSummary)
applySetFor b (g, s) it = case it of
  SetProp v p e ->
    let val = evalExpr b g e
    in case Map.lookup v b of
      Just (BNode nid) -> case Map.lookup nid (gNodes g) of
        Just n ->
          let n' = setNodeProp p val n
          in (putNode n' g, s { msPropertiesSet = msPropertiesSet s + 1 })
        Nothing -> (g, s)
      Just (BEdge e0) ->
        let e' = setEdgeProp p val e0
        in (g { gEdges = Map.insert (edgeSource e0, edgeTarget e0) e' (gEdges g) }
           , s { msPropertiesSet = msPropertiesSet s + 1 })
      _ -> (g, s)
  SetLabel v l ->
    case Map.lookup v b of
      Just (BNode nid) -> case Map.lookup nid (gNodes g) of
        Just n ->
          let n' = addNodeLabel l n
          in (putNode n' g, s { msPropertiesSet = msPropertiesSet s + 1 })
        Nothing -> (g, s)
      _ -> (g, s)

-- | Apply a REMOVE item against the graph for the given binding.
applyRemoveFor :: Binding -> (Graph, MutationSummary) -> RemoveItem -> (Graph, MutationSummary)
applyRemoveFor b (g, s) it = case it of
  RemoveProp v p ->
    case Map.lookup v b of
      Just (BNode nid) -> case Map.lookup nid (gNodes g) of
        Just n ->
          let n' = removeNodeProp p n
          in (putNode n' g, s { msPropertiesRemoved = msPropertiesRemoved s + 1 })
        Nothing -> (g, s)
      Just (BEdge e0) ->
        let e' = removeEdgeProp p e0
        in (g { gEdges = Map.insert (edgeSource e0, edgeTarget e0) e' (gEdges g) }
           , s { msPropertiesRemoved = msPropertiesRemoved s + 1 })
      _ -> (g, s)
  RemoveLabel v l ->
    case Map.lookup v b of
      Just (BNode nid) -> case Map.lookup nid (gNodes g) of
        Just n ->
          let n' = removeNodeLabel l n
          in (putNode n' g, s { msPropertiesRemoved = msPropertiesRemoved s + 1 })
        Nothing -> (g, s)
      _ -> (g, s)

-- | DELETE / DETACH DELETE over the bound variables. Without DETACH, a
-- node that would leave dangling references (i.e. retains relationships
-- to nodes that are not deleted) is an error. Returns the graph and the
-- updated summary.
deleteVars :: Binding -> Graph -> MutationSummary -> Bool -> [Text] -> Either Text (Graph, MutationSummary)
deleteVars b g s detach vs =
  let nodeIds = [ nid | v <- vs, Just (BNode nid) <- [Map.lookup v b] ]
      edgeBinds = [ e | v <- vs, Just (BEdge e) <- [Map.lookup v b] ]
      pathBinds = [ es | v <- vs, Just (BPath es) <- [Map.lookup v b] ]
      boundEdges = edgeBinds ++ concat pathBinds
      nodeSet = Map.fromList [ (nid, ()) | nid <- nodeIds ]
      edgesDyingByNode =
        [ e | ((a, c), e) <- Map.toList (gEdges g)
        , Map.member a nodeSet || Map.member c nodeSet ]
      offenders = case detach of
        True  -> []
        False -> [ nid | nid <- nodeIds, hasNonSelfEdges nid ]
  in case offenders of
    (nid:_) -> Left ("cannot delete node with relationships (use DETACH DELETE): " <> nid)
    []      ->
      let g1 = foldl' (\acc e -> deleteEdge (edgeSource e, edgeTarget e) acc) g boundEdges
          (g2, edgesRemoved) =
            foldl' step (g1, length edgesDyingByNode) nodeIds
          step (gAcc, removed) nid =
            let (gAfter, removedHere) = deleteEdgesTouching nid gAcc
            in (gAfter { gNodes = Map.delete nid (gNodes gAfter) }, removed + removedHere)
          s' = s { msNodesDeleted = msNodesDeleted s + length nodeIds
                 , msRelsDeleted  = msRelsDeleted s + edgesRemoved }
      in Right (g2, s')
  where
    hasNonSelfEdges nid =
      any (\((a, c), _) -> (a == nid || c == nid) && a /= c)
          (Map.toList (gEdges g))

-- | Generated node id for id-less CREATE/MERGE patterns.
generatedId :: Graph -> Text -> Text
generatedId g var =
  let n = Map.size (gNodes g)
  in "gen-" <> var <> "-" <> T.pack (show n)

-- | Extract text from a value.
asText :: Value -> Maybe Text
asText (String t) = Just t
asText _          = Nothing

-- | Project the mutation RETURN against the post-mutation graph. Each
-- matched binding projects one row (post-mutation state); the summary
-- counters are available as implicit variables, and a mutation without
-- MATCH yields a single row from the summary binding alone.
projectReturn :: Int -> ReturnClause -> Graph -> GraphIndex -> MutationSummary -> [Binding] -> CypherResult
projectReturn budget rc g _idx summary bindings =
  let summaryBinding = Map.fromList
        [ ("nodes_created",      BCounter (msNodesCreated summary))
        , ("rels_created",       BCounter (msRelsCreated summary))
        , ("rels_upserted",      BCounter (msRelsUpserted summary))
        , ("properties_set",     BCounter (msPropertiesSet summary))
        , ("properties_removed", BCounter (msPropertiesRemoved summary))
        , ("nodes_deleted",      BCounter (msNodesDeleted summary))
        , ("rels_deleted",       BCounter (msRelsDeleted summary))
        ]
      base = if null bindings || null (mconcat bindings)
               then [summaryBinding]
               else [ Map.union b summaryBinding | b <- bindings ]
      (rows, truncated) = takeCap budget [ projectRow g rc [] b | b <- base ]
  in CypherResult (columnNames rc) rows truncated

-- | Bindings that resolve through summary counters (RETURN projections).
-- Only counters are supported: a counter resolves for property refs too
-- (e.g. @RETURN nodes_created@ works via refValue's VarRef path? no —
-- counters are plain values).
--
-- We extend 'Bound' with a counter variant so the existing expression
-- evaluator resolves summary counters without special-casing.

-- ───────────────────────────────────────────────
-- Enumeration
-- ───────────────────────────────────────────────

-- | Enumerate all bindings for a list of pattern elements, combining
-- the per-path bindings by Cartesian product.
enumerateBindings :: Graph -> [PatternElem] -> [Binding]
enumerateBindings g elems = combinePaths g (decomposePaths elems)

-- | Split a flat pattern list into paths. A new path starts at a node
-- pattern that is either first or preceded by another node pattern
-- (two nodes in a row, with no relationship between them).
decomposePaths :: [PatternElem] -> [[PatternElem]]
decomposePaths elems = reverse (go Nothing [] elems)
  where
    go :: Maybe PatternElem -> [[PatternElem]] -> [PatternElem] -> [[PatternElem]]
    go _ acc [] = acc
    go prev acc (x:xs)
      | isPathStart prev x = go (Just x) ((x : []) : acc) xs
      | otherwise          = go (Just x) (extendLast acc x) xs
    isPathStart :: Maybe PatternElem -> PatternElem -> Bool
    isPathStart prev (NodePatE _) = case prev of
      Nothing       -> True
      Just (NodePatE _) -> True
      Just (RelPatE _)  -> False
    isPathStart _ _ = False
    extendLast :: [[PatternElem]] -> PatternElem -> [[PatternElem]]
    extendLast acc x = case acc of
      []     -> [[x]]
      (p:ps) -> (reverse (x : reverse p) : ps)

-- | Combine per-path bindings by Cartesian product, AND-ing shared
-- variables.
combinePaths :: Graph -> [[PatternElem]] -> [Binding]
combinePaths g paths = foldr combineOne [Map.empty] (map (pathBindings g) paths)
  where
    combineOne :: [Binding] -> [Binding] -> [Binding]
    combineOne pathBs acc =
      [ merged | b <- acc, pb <- pathBs, Just merged <- [mergeBinding b pb] ]

-- | Compute the bindings for a single path (a sequence of node and
-- relationship patterns).
pathBindings :: Graph -> [PatternElem] -> [Binding]
pathBindings g (NodePatE np0 : rest) =
  [ b
  | nid0 <- nodeCandidates g np0
  , b <- walk g (Map.singleton (npVar np0) (BNode nid0)) nid0 rest
  ]
pathBindings _ _ = []

-- | Walk a well-formed path: (RelPatE, NodePatE)* after the first node.
walk :: Graph -> Binding -> NodeId -> [PatternElem] -> [Binding]
walk _g binding _cur [] = [binding]
walk g binding cur (RelPatE rp : NodePatE np2 : xs) =
  [ b2
  | (es, nid2) <- relPaths g rp cur
  , nodeMatches g np2 nid2
  , let b1 = case rpVar rp of
           Nothing -> binding
           Just v  -> Map.insert v (BPath es) binding
       , let b1' = Map.insert (npVar np2) (BNode nid2) b1
       , b2 <- walk g b1' nid2 xs
  ]
walk _ _ _ _ = []

-- ───────────────────────────────────────────────
-- Node candidates
-- ───────────────────────────────────────────────

-- | All node ids matching a node pattern (label + property constraints).
nodeCandidates :: Graph -> NodePat -> [NodeId]
nodeCandidates g np =
  [ nid
  | (nid, n) <- Map.toList (gNodes g)
  , nodeMatchesLabel np n
  , nodeMatchesProps g np nid
  ]

-- | Does a node match a node pattern (all labels + all property
-- constraints)?
nodeMatches :: Graph -> NodePat -> NodeId -> Bool
nodeMatches g np nid =
  case Map.lookup nid (gNodes g) of
    Just n  -> nodeMatchesLabel np n && nodeMatchesProps g np nid
    Nothing -> False

-- | Does a node carry all the labels in the pattern? The primary kind
-- is checked first; pattern labels that are not the primary kind are
-- consulted against the node's extra labels (SET-written).
nodeMatchesLabel :: NodePat -> Node -> Bool
nodeMatchesLabel np n =
  all (\l -> labelHas l) (npLabels np)
  where
    primary = nodeCypherLabel n
    extras  = nodeExtraLabels n
    labelHas l = primary == Just l || l `elem` extras

-- | Does a node satisfy the pattern's property constraints?
nodeMatchesProps :: Graph -> NodePat -> NodeId -> Bool
nodeMatchesProps g np nid =
  case Map.lookup nid (gNodes g) of
    Just n  -> all (\(k, e) -> nodeProperty n k == Just (evalExpr Map.empty g e)) (Map.toList (npProps np))
    Nothing -> False

-- ───────────────────────────────────────────────
-- Relationship traversal
-- ───────────────────────────────────────────────

-- | Single-edge hops from @start@ matching the relationship pattern.
-- Returns (edge, nextNodeId) pairs.
hop :: Graph -> RelPat -> NodeId -> [(Edge, NodeId)]
hop g rp start =
  [ (e, next)
  | (e, next) <- incident
  , edgeMatchesType rp e
  , edgeMatchesProps g rp e
  ]
  where
    incident :: [(Edge, NodeId)]
    incident = case rpDir rp of
      DirRight ->
        [ (e, edgeTarget e) | ((s, _), e) <- Map.toList (gEdges g), s == start ]
      DirLeft ->
        [ (e, edgeSource e) | ((_, t), e) <- Map.toList (gEdges g), t == start ]
      DirUndirected ->
        [ (e, edgeTarget e) | ((s, _), e) <- Map.toList (gEdges g), s == start ]
        ++ [ (e, edgeSource e) | ((_, t), e) <- Map.toList (gEdges g), t == start ]

-- | Variable-length paths from @start@ following the relationship
-- pattern for hrMin..hrMax hops. Returns (edgePath, endNodeId) pairs.
relPaths :: Graph -> RelPat -> NodeId -> [([Edge], NodeId)]
relPaths g rp start =
  [ (es, nid)
  | d <- [hrMin (rpHops rp) .. hrMax (rpHops rp)]
  , (es, nid) <- pathsOfLength g rp start d
  ]

-- | All paths of exactly @d@ hops from @start@.
pathsOfLength :: Graph -> RelPat -> NodeId -> Int -> [([Edge], NodeId)]
pathsOfLength _g _rp start 0 = [([], start)]
pathsOfLength g rp start d =
  [ (e : es, nid2)
  | (e, nid1) <- hop g rp start
  , (es, nid2) <- pathsOfLength g rp nid1 (d - 1)
  ]

-- | Does an edge match the relationship type constraint?
edgeMatchesType :: RelPat -> Edge -> Bool
edgeMatchesType rp e =
  case rpTypes rp of
    []    -> True
    ts    -> T.toLower (edgeCypherType e) `elem` map T.toLower ts

-- | Does an edge satisfy the relationship property constraints?
edgeMatchesProps :: Graph -> RelPat -> Edge -> Bool
edgeMatchesProps g rp e =
  all (\(k, expr) -> edgeProperty e k == Just (evalExpr Map.empty g expr)) (Map.toList (rpProps rp))

-- ───────────────────────────────────────────────
-- Binding merge
-- ───────────────────────────────────────────────

-- | Merge two bindings, checking that shared variables agree.
mergeBinding :: Binding -> Binding -> Maybe Binding
mergeBinding a b =
  let shared = Map.intersection a b
      agree = all (\(k, v) -> Map.lookup k b == Just v) (Map.toList shared)
  in if agree then Just (Map.union a b) else Nothing

-- ───────────────────────────────────────────────
-- Budget
-- ───────────────────────────────────────────────

-- | Cap a list to @n@ elements, reporting whether it was truncated.
takeCap :: Int -> [a] -> ([a], Bool)
takeCap n xs = (take n xs, case drop n xs of [] -> False; _ -> True)

-- ───────────────────────────────────────────────
-- WHERE
-- ───────────────────────────────────────────────

-- | Evaluate a predicate against a binding.
evalPredicate :: Binding -> Graph -> Predicate -> Bool
evalPredicate b g (PAnd p q) = evalPredicate b g p && evalPredicate b g q
evalPredicate b g (POr p q) = evalPredicate b g p || evalPredicate b g q
evalPredicate b g (PNot p) = not (evalPredicate b g p)
evalPredicate b g (PIsNull ref isNull) = isNullRef b g ref == isNull
evalPredicate b g (PCompare ref op e) = evalCompare b g ref op e
evalPredicate b g (PIn ref vals) =
  case refValue b g ref of
    Nothing   -> False
    Just Null -> False
    Just rv   -> any (\e -> valueEq rv (evalExpr b g e)) vals
evalPredicate b g (PStartsWith ref e) =
  case (refValue b g ref, evalExpr b g e) of
    (Just (String s), String t) -> T.isPrefixOf t s
    _                           -> False
evalPredicate b g (PContains ref e) =
  case (refValue b g ref, evalExpr b g e) of
    (Just (String s), String t) -> T.isInfixOf t s
    _                           -> False
evalPredicate b g (PRegex ref e) =
  case (refValue b g ref, evalExpr b g e) of
    (Just (String s), String t) ->
      case makeRegexM (T.unpack t) :: Maybe Regex of
        Nothing -> False
        Just re -> matchTest re (T.unpack s)
    _ -> False

-- | Evaluate a comparison predicate.
evalCompare :: Binding -> Graph -> PropRef -> CompareOp -> Expr -> Bool
evalCompare b g ref op e =
  case refValue b g ref of
    Nothing   -> False
    Just Null -> False
    Just rv   -> compareValue rv op (evalExpr b g e)

-- | Compare two non-null values.
compareValue :: Value -> CompareOp -> Value -> Bool
compareValue rv op ev =
  case ev of
    Null -> False
    _    -> case op of
      OpEq  -> rv == ev
      OpNeq -> rv /= ev
      OpLt  -> valueOrder rv ev == Just LT
      OpLe  -> case valueOrder rv ev of { Just LT -> True; Just EQ -> True; _ -> False }
      OpGt  -> valueOrder rv ev == Just GT
      OpGe  -> case valueOrder rv ev of { Just GT -> True; Just EQ -> True; _ -> False }

-- | Is a reference null (unbound, unknown property, or explicit null)?
isNullRef :: Binding -> Graph -> PropRef -> Bool
isNullRef b g ref =
  case refValue b g ref of
    Nothing   -> True
    Just Null -> True
    Just _    -> False

-- | Resolve a reference to a value.
refValue :: Binding -> Graph -> PropRef -> Maybe Value
refValue b g (VarRef v) =
  case Map.lookup v b of
    Just (BNode nid) -> case Map.lookup nid (gNodes g) of
      Just n  -> Just (toJSON (nodeProperties n))
      Nothing -> Nothing
    Just (BEdge e)   -> Just (toJSON (edgeProperties e))
    Just (BPath es)  -> Just (toJSON (map (toJSON . edgeProperties) es))
    Just (BCounter n) -> Just (toJSON n)
    Nothing         -> Nothing
refValue b g (PropRef v p) =
  case Map.lookup v b of
    Just (BNode nid) -> case Map.lookup nid (gNodes g) of
      Just n  -> nodeProperty n p
      Nothing -> Nothing
    Just (BEdge e)   -> edgeProperty e p
    Just (BPath _)   -> Nothing
    Just (BCounter n) -> Just (toJSON n)
    Nothing         -> Nothing

-- | Evaluate an expression to a value.
evalExpr :: Binding -> Graph -> Expr -> Value
evalExpr b g (EVar v) = fromMaybe Null (refValue b g (VarRef v))
evalExpr b g (EProp v p) = fromMaybe Null (refValue b g (PropRef v p))
evalExpr _ _ (EStr s) = toJSON s
evalExpr _ _ (ENum d) = toJSON d
evalExpr _ _ (EBool b) = toJSON b
evalExpr _ _ ENull = Null
evalExpr _ _ (ECount _) = Null

-- | Equality for values (null is never equal to anything).
valueEq :: Value -> Value -> Bool
valueEq Null _ = False
valueEq _ Null = False
valueEq a b = a == b

-- | Ordering for values (only same-type scalars are comparable).
valueOrder :: Value -> Value -> Maybe Ordering
valueOrder (Number a) (Number b) = Just (compare a b)
valueOrder (String a) (String b) = Just (compare a b)
valueOrder (Bool a)   (Bool b)   = Just (compare a b)
valueOrder _          _          = Nothing

-- ───────────────────────────────────────────────
-- Projection
-- ───────────────────────────────────────────────

-- | Project a binding to a row of values. The full filtered binding
-- list is passed so that count items can be evaluated.
projectRow :: Graph -> ReturnClause -> [Binding] -> Binding -> [Value]
projectRow g rc allBs binding = [ itemValue item binding | item <- rcItems rc ]
  where
    itemValue :: ReturnItem -> Binding -> Value
    itemValue item bnd = case item of
      RIExpr e _    -> evalExpr bnd g e
      RICount arg _ -> toJSON (countValue arg allBs) :: Value

-- | Column names for a RETURN clause.
columnNames :: ReturnClause -> [Text]
columnNames rc = [ colName item | item <- rcItems rc ]
  where
    colName (RIExpr e alias) = fromMaybe (exprName e) alias
    colName (RICount arg alias) = fromMaybe (countName arg) alias

-- | Count value for a count item.
countValue :: Maybe Text -> [Binding] -> Int
countValue Nothing bs = length bs
countValue (Just v) bs = length [ b | b <- bs, isJust (Map.lookup v b) ]

-- | Name for a count item.
countName :: Maybe Text -> Text
countName Nothing = "count(*)"
countName (Just v) = "count(" <> v <> ")"

-- | Name for an expression (used as a default column name).
exprName :: Expr -> Text
exprName (EVar v) = v
exprName (EProp v p) = v <> "." <> p
exprName (EStr _) = "''"
exprName (ENum _) = "0"
exprName (EBool _) = "false"
exprName ENull = "null"
exprName (ECount _) = "count"

-- ───────────────────────────────────────────────
-- DISTINCT / ORDER BY
-- ───────────────────────────────────────────────

-- | Deduplicate (binding, row) pairs by row.
dedupPairs :: [(Binding, [Value])] -> [(Binding, [Value])]
dedupPairs pairs = go pairs []
  where
    go [] _ = []
    go ((b, row):rest) seen
      | row `elem` seen = go rest seen
      | otherwise       = (b, row) : go rest (row:seen)

-- | Sort (binding, row) pairs by ORDER BY items.
orderByBindings :: Graph -> [OrderItem] -> [(Binding, [Value])] -> [(Binding, [Value])]
orderByBindings _ [] pairs = pairs
orderByBindings g items pairs =
  sortBy (\(a, _) (b, _) -> orderCompare g items a b) pairs

-- | Compare two bindings by a list of order items.
orderCompare :: Graph -> [OrderItem] -> Binding -> Binding -> Ordering
orderCompare g items a b =
  foldl combineCmp EQ [ cmpItem item | item <- items ]
  where
    cmpItem :: OrderItem -> Ordering
    cmpItem item =
      let va = refValue a g (oiRef item)
          vb = refValue b g (oiRef item)
          c  = compareOrder va vb
      in if oiDesc item then flipOrdering c else c
    flipOrdering LT = GT
    flipOrdering GT = LT
    flipOrdering EQ = EQ

-- | Combine a list of comparisons into a single ordering (first
-- non-EQ wins).
combineCmp :: Ordering -> Ordering -> Ordering
combineCmp acc c = if acc == EQ then c else acc

-- | Compare two Maybe Values for ordering.
compareOrder :: Maybe Value -> Maybe Value -> Ordering
compareOrder Nothing Nothing = EQ
compareOrder Nothing _       = LT
compareOrder _       Nothing = GT
compareOrder (Just a) (Just b) =
  case valueOrder a b of
    Just o -> o
    Nothing -> compare a b
