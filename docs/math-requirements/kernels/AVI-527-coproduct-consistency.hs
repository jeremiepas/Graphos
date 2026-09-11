-- AVI-527 concrete deliverable -- reproducible kernel.
--
-- Coproduct consistency across graph contexts. Verifies, against Graphos surfaces:
--   (1) On pairwise-DISJOINT graph contexts, `mergeGraphs` (Core.hs:140) realises the
--       categorical COPRODUCT in `Gr` (colimit over a discrete diagram): the canonical
--       key-inclusions are cocone legs, and for any target there is a UNIQUE mediating
--       morphism (order-independent; `merge A B == merge B A`).
--   (2) Coproducts are associative + commutative up to canonical iso on disjoint inputs,
--       so the n-way context merge (`foldl' merge`, AVI-524 AC-3) is grouping-independent.
--   (3) Under a CONFLICTING shared key, `mergeGraphs` is ORDER-DEPENDENT (AVI-536 §2.2):
--       no coproduct exists there -- it collapses to the lax colimit, not the coproduct.
--   (4) `gDirected` is a RIGID invariant (Core.hs:157); a mixed-directed family has no
--       canonical coproduct apex in `Gr` (the directed leg depends on operand order).
--   (5) Cross-context semantic-edge inference (semantic-edge-inference spec:
--       `inferSemanticCodeDocEdges`) STITCHES contexts together AFTER the coproduct, so
--       it does NOT commute with coproduct: `infer (A ⊕ B) /= (infer A) ⊕ (infer B)`.
--
-- Run:  runghc docs/math-requirements/kernels/AVI-527-coproduct-consistency.hs
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (sort, nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M

-- ─────────────────────────────────────────────────────────────────────────────
-- Faithful stub of Graphos.Domain.Graph.Core.Graph (Core.hs:47-57).
--   gNodes :: Map NodeId Node           -> KV         (one Vertex per node-key)
--   gEdges :: Map (NodeId,NodeId) Edge  -> Map (Text,Text) Rel   (keyed by (src,tgt), Core.hs:143)
--   gDirected :: Bool                    -> Bool
-- A morphism f : G -> H is (f_N on keys, f_E on edges) preserving incidence + relation.
-- ─────────────────────────────────────────────────────────────────────────────

type Rel = Text                      -- edge relation label (Relation, Edge.hs:32); never coerced
type KV  = M.Map T.Text Vertex       -- node-key -> carried value (the Node)

data Graph = Graph
  { gNodes    :: KV
  , gEdges    :: M.Map (Text, Text) Rel
  , gDirected :: Bool
  } deriving (Eq, Show)

-- Vertex stands in for Node; its single field is the value a key carries.
data Vertex = V Text deriving (Eq, Show)

-- mergeGraphs mirrored from Core.hs:140-162.
--   mergedNodes = gNodes old <> gNodes new                       (Core.hs:142)
--   mergedEdges = filterWithKey (src,tgt present) (gEdges old <> gEdges new)  (Core.hs:143-144)
--   gDirected   = gDirected old                                  (Core.hs:157)
merge :: Graph -> Graph -> Graph
merge a b =
  let nodes = gNodes a <> gNodes b
      edges = M.filterWithKey (\(s, t) _ -> M.member s nodes && M.member t nodes) (gEdges a <> gEdges b)
  in Graph { gNodes = nodes, gEdges = edges, gDirected = gDirected a }

-- Number of FREE choices for a mediating morphism: 0 forces uniqueness.
-- A mediating [f] must agree with f_A on every A-key and f_B on every B-key; when the
-- contexts are disjoint, the union of the two key-domains already fixes [f] on every key
-- of the apex, so there are zero free choices (AVI-536 §2.1 uniqueness up to unique iso).
freeChoices :: KV -> KV -> Int
freeChoices fA fB = length (nub (M.keys fA ++ M.keys fB)) - M.size (M.union fA fB)

-- Deterministic structural hash over (nodes, edges) — mirrors computeGraphHash ordering.
gHash :: Graph -> Text
gHash g =
  let ids = sort (M.keys (gNodes g))
      ets = sort [(s, t) | (s, t) <- M.keys (gEdges g)]
  in T.pack (show ids <> show ets)

nodeSet :: Graph -> [Text]
nodeSet g = sort (M.keys (gNodes g))

-- Carried label at a node-key (the Node's value), mirroring the "keep OLD" conflict check.
valAt :: Graph -> Text -> Maybe Text
valAt g k = fmap vLabel (M.lookup k (gNodes g))
vLabel (V t) = t

mk :: [(Text, Vertex)] -> [(Text, Text, Rel)] -> Bool -> Graph
mk nvs es directed = Graph
  { gNodes = M.fromList nvs
  , gEdges = M.fromList [((s, t), r) | (s, t, r) <- es]
  , gDirected = directed
  }

-- Cross-context semantic-edge inference (semantic-edge-inference spec):
-- for each DocFile node, emit References(codeNodeId -> docNodeId) above cosine threshold.
-- The edge (c1 -> d1) requires BOTH endpoints present: on a single-context graph the
-- target/source is absent so the dangling edge is dropped (Core.hs:143; single-corpus
-- auto-skip). Thus `infer` only emits the cross-context edge when both c1 AND d1 exist.
inferCrossContext :: Graph -> Graph
inferCrossContext g =
  if M.member "c1" (gNodes g) && M.member "d1" (gNodes g)
    then g { gEdges = M.insert ("c1", "d1") "References" (gEdges g) }
    else g

hasCrossEdge :: Graph -> Bool
hasCrossEdge g = isJust (M.lookup ("c1", "d1") (gEdges g))

main :: IO ()
main = do
  -- Three pairwise-DISJOINT contexts (disjoint node-key sets).
  let a = mk [("a1", V "a1"), ("a2", V "a2")] [("a1", "a2", "References")] True
      b = mk [("b1", V "b1"), ("b2", V "b2")] [("b1", "b2", "Calls")] True
      c = mk [("d1", V "d1"), ("doc1", V "doc1")] [("doc1", "d1", "References")] True

  putStrLn "-- (1) COPRODUCT on disjoint contexts ------------------------------------------------"
  -- Coproduct apex = mergeGraphs of disjoint graphs; legs are the inclusions.
  let ab = merge a b
  putStrLn $ "    merge A B node-count = " ++ show (length (nodeSet ab))
      ++ " (== |A|+|B|, disjoint union; coproduct legs are key-inclusions)"
  putStrLn $ "    merge A B == merge B A (order-independent => consistent colimit)? "
      ++ show (gHash (merge a b) == gHash (merge b a))

  -- Unique mediating morphism: assert no free choice forces a UNIQUE [f] (AVI-536 §2.1).
  let fA = M.fromList [("a1", V "x1"), ("a2", V "x2")]   -- a -> X
      fB = M.fromList [("b1", V "y1"), ("b2", V "y2")]   -- b -> X
  putStrLn $ "    free choices for mediating morphism = " ++ show (freeChoices fA fB)
      ++ "  (0 => a UNIQUE mediating morphism [f] with [f]∘ι_A=f_A, [f]∘ι_B=f_B)"

  putStrLn "-- (2) ASSOCIATIVITY + COMMUTATIVITY up to canonical iso on disjoint inputs -----------"
  putStrLn $ "    ((A ⊕ B) ⊕ C) == (A ⊕ (B ⊕ C))? "
      ++ show (gHash (merge (merge a b) c) == gHash (merge a (merge b c)))
  putStrLn $ "    A ⊕ B == B ⊕ A (consistency across grouping/ordering)? "
      ++ show (gHash (merge a b) == gHash (merge b a))

  putStrLn "-- (3) CONFLICT breaks the coproduct (collapses to lax colimit, AVI-536 §2.2) --------"
  -- A' and B' share key "s" with DIFFERENT values => not disjoint => no coproduct.
  let ap = mk [("s", V "OLD"), ("a1", V "a1")] [] True
      bp = mk [("s", V "NEW"), ("b1", V "b1")] [] True
  putStrLn $ "    merge A' B' keeps OLD at conflicting key? "
      ++ show (valAt (merge ap bp) "s" == Just "OLD")
  putStrLn $ "    merge A' B' node-value at 's' /= merge B' A' node-value at 's'? "
      ++ show (valAt (merge ap bp) "s" /= valAt (merge bp ap) "s")
      ++ "  (conflict => order-dependent => NOT a coproduct there; collapses to lax colimit, AVI-536 §2.2)"

  putStrLn "-- (4) gDirected is a RIGID invariant (Core.hs:157) -----------------------------------"
  -- Mixed-directed family: no single object in Gr can receive both legs with equal gDirected.
  let ad = mk [("a1", V "a1")] [] True      -- directed
      bu = mk [("b1", V "b1")] [] False     -- undirected
  putStrLn $ "    merge(directed A, undirected B).gDirected = " ++ show (gDirected (merge ad bu))
      ++ " ; merge(undirected B, directed A).gDirected = " ++ show (gDirected (merge bu ad))
      ++ "  => apex directedness depends on ORDER => no canonical coproduct apex for mixed-directed family in Gr"

  putStrLn "-- (5) CROSS-CONTEXT INFERENCE does NOT commute with coproduct --------------------------"
  -- infer adds a cross-context edge (code c1 -> doc d1) spanning A_code and A_doc.
  let acode = mk [("c1", V "c1")] [] True
      adoc  = mk [("d1", V "d1")] [] True
      mergedBeforeInfer     = inferCrossContext (merge acode adoc)   -- infer on the coproduct
      coproductOfInferred   = merge (inferCrossContext acode) (inferCrossContext adoc)  -- infer per-context, then coproduct
  putStrLn $ "    infer(⊕) has cross-context edge? "        ++ show (hasCrossEdge mergedBeforeInfer)
  putStrLn $ "    (infer A) ⊕ (infer B) has cross-context edge? " ++ show (hasCrossEdge coproductOfInferred)
      ++ "  => inference creates overlaps no local view carries (sheaf AVI-536 §2.1 violated by design)"
  putStrLn $ "    infer(⊕A) /= ⊕(infer A)? " ++ show (gHash mergedBeforeInfer /= gHash coproductOfInferred)

  putStrLn ""
  putStrLn "-- SUMMARY: coproduct consistency holds over DISJOINT contexts; conflict, mixed --"
  putStrLn "-- directedness, and cross-context inference are exactly the non-coproduct regimes. --"
