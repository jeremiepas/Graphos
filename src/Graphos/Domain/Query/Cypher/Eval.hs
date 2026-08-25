-- | Evaluation of the read-only openCypher/GQL subset against the
-- in-memory property graph.
--
-- The evaluator is a forward-walk over the graph: each MATCH path is
-- anchored at its first node, and the remaining relationship / node
-- patterns are walked forward from there. Multiple comma-separated paths
-- are combined by a Cartesian product of their bindings, with shared
-- variables AND-ed (a binding is kept only when every path agrees on
-- the value of a shared variable).
--
-- Pure — no IO, fully testable.
module Graphos.Domain.Query.Cypher.Eval
  ( -- * Result
    CypherResult(..)

    -- * Evaluation
  , evaluate
  ) where

import Data.Aeson (Value(..), ToJSON(..))
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.TDFA (makeRegexM, matchTest, Regex)

import Graphos.Domain.Types (NodeId, Node(..), Edge(..))
import Graphos.Domain.Graph (Graph, gNodes, gEdges)
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

-- | Does a node carry all the labels in the pattern?
nodeMatchesLabel :: NodePat -> Node -> Bool
nodeMatchesLabel np n =
  let lbl = nodeCypherLabel n
  in all (\l -> lbl == Just l) (npLabels np)

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
    Nothing         -> Nothing
refValue b g (PropRef v p) =
  case Map.lookup v b of
    Just (BNode nid) -> case Map.lookup nid (gNodes g) of
      Just n  -> nodeProperty n p
      Nothing -> Nothing
    Just (BEdge e)   -> edgeProperty e p
    Just (BPath _)   -> Nothing
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
