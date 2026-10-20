{-# LANGUAGE StrictData #-}
-- | Change-impact analysis, mirroring the Lean model
-- (openspec/changes/intent-graph-verification/lean/IntentGraph.lean) Change
-- namespace, CodeGraph.impact / frontier, the discharge discipline and the
-- admissibility gate.
--
-- A change is a before/after pair of projections; the node correspondence μ is
-- the identity on NodeId (the stable id supplied by GraphOS). The touched set is
-- removed ∪ added ∪ changed (attributes or outgoing edges) and extends the
-- baseline "Graphos.Domain.Graph.Diff" diff with the changed clause; the impact
-- set is the reflexive-transitive closure of the touched set under reverse
-- dependency edges ('depLabels' — notably not @tests@); the frontier is impact
-- minus touched, and every frontier node is an obligation that must be
-- discharged by an ack or a registered rule.
module Graphos.Domain.Impact
  ( -- * Change
    Change(..)
  , touched
  , depLabels
  , impact
  , frontier
    -- * Discharge discipline
  , Rule
  , Discharge(..)
  , stableSignature
  , discharged
  , openObligations
  , complete
    -- * Admissibility gate
  , admissible
  , assess
  , FailedConjunction(..)
  , headWitness
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (nub)

import Graphos.Domain.Graph.Core (Graph (gNodes, gEdges))
import Graphos.Domain.Graph.Diff (graphDiff)
import Graphos.Domain.Types.Graph (GraphDiff (..))
import Graphos.Domain.Types.Node
  ( Node, nodeKind, nodeLabel, nodeSignature
  , nodeSourceFile, nodeLineStart, nodeLineEnd, nodeBodyHash )
import Graphos.Domain.Types.Edge
  ( Edge, edgeRelation, Relation (Calls, Implements, References, Maps, DependsOn) )
import Graphos.Domain.IntentCheck
  ( SpecDocument, Verdict, docSpecs, evaluateSpec, holds, witnesses )
import Data.Text.Short (toText)
import Data.Text (Text)

-- | A change is a before / after pair of projections.
data Change = Change
  { before :: Graph
  , after  :: Graph
  } deriving (Eq, Show)

-- | Identity attributes shared by touched-detection and the stableSignature
-- rule. The key includes the body hash ('bodyHash'): a body-only edit moves
-- the key and therefore counts as touched (mirror of Lean's @n₁ ≠ n₂@). The
-- 'stableSignature' rule then deliberately ignores the body — the whole
-- point of the body/signature split.
stableNodeKey :: Node -> (Text, Maybe Text, Maybe Text, Text, Maybe Int, Maybe Int, Maybe Text)
stableNodeKey n =
  ( toText (nodeLabel n)
  , fmap toText (nodeKind n)
  , fmap toText (nodeSignature n)
  , toText (nodeSourceFile n)
  , nodeLineStart n
  , nodeLineEnd n
  , bodyHash n
  )

-- | Body identity carried under @body_hash@ in 'nodeExtra' (optional field from
-- the graph-json-contract). Absent ⇒ Nothing.
bodyHash :: Node -> Maybe Text
bodyHash n = fmap toText (nodeBodyHash n)

-- | Outgoing edges of a node.
outEdges :: Graph -> Text -> [Edge]
outEdges g v = [ e | ((s, _), e) <- Map.toList (gEdges g), s == v ]

sameEdgeSet :: [Edge] -> [Edge] -> Bool
sameEdgeSet e1 e2 = all (`elem` e2) e1 && all (`elem` e1) e2

-- | The touched set: removed ∪ added ∪ changed (attributes or out-edge set).
-- The removed / added parts come from the baseline 'graphDiff' this module
-- extends; "changed" (stable-key move, incl. body hash, or out-edge set move)
-- is the extra clause required by the Lean model's touched definition.
touched :: Change -> [Text]
touched c = nub (removed ++ added ++ changed)
  where
    d = graphDiff (before c) (after c)
    removed = Map.keys (diffRemovedNodes d)
    added   = Map.keys (diffAddedNodes d)
    changed = [ nid
              | (nid, nb) <- Map.toList (gNodes (before c))
              , Just na  <- [Map.lookup nid (gNodes (after c))]
              , stableNodeKey nb /= stableNodeKey na
                || not (sameEdgeSet (outEdges (before c) nid) (outEdges (after c) nid))
              ]

-- | Labels that propagate a change from a target to its source: if @a --ℓ--> b
-- and @b@ changes, then @a@ is impacted. @tests@ is deliberately excluded.
depLabels :: [Relation]
depLabels = [Calls, Implements, References, Maps, DependsOn]

-- | Reverse-dependency predecessors of @v@ under the dependency labels.
impactedBy :: Graph -> Text -> [Text]
impactedBy g v =
  [ s | ((s, d), e) <- Map.toList (gEdges g), d == v, edgeRelation e `elem` depLabels ]

-- | The impact set: D⁻¹-closure of the touched set, computed on the before
-- graph. Fuel = |V| saturates (every simple path has ≤ |V| edges).
impact :: Graph -> [Text] -> [Text]
impact g t = Set.toList (go (Set.fromList (nub t)))
  where
    go acc = let acc' = Set.union acc (Set.fromList (concatMap (impactedBy g) (Set.toList acc)))
             in if acc' == acc then acc else go acc'

-- | The frontier: impacted but unmodified — the omission surface to review.
frontier :: Graph -> [Text] -> [Text]
frontier g t = Set.toList (Set.difference (Set.fromList (impact g t)) (Set.fromList (nub t)))

-- | A non-affectation rule: @r c v@ certifies that frontier node @v@ is not
-- affected by change @c@.
type Rule = Change -> Text -> Bool

-- | Discharge means for a given change: human acks plus admitted rules.
data Discharge = Discharge
  { acks  :: [Text]
  , rules :: [Rule]
  }

-- | The shipped body-only rule: a frontier node is safely unaffected iff every
-- touched direct dependency kept its signature, kind and name (body may change).
stableSignature :: Rule
stableSignature c v = all keep deps
  where
    b = before c
    a = after c
    t = Set.fromList (touched c)
    deps = [ d
           | ((s, d), e) <- Map.toList (gEdges b)
           , s == v
           , edgeRelation e `elem` depLabels
           , d `Set.member` t ]
    keep d = case (Map.lookup d (gNodes b), Map.lookup d (gNodes a)) of
      (Just nb, Just na) -> stableIdentity nb na
      _                  -> False

-- | Signature / kind / name equality (body excluded — the whole point of the
-- rule).
stableIdentity :: Node -> Node -> Bool
stableIdentity n1 n2 =
  fmap toText (nodeSignature n1) == fmap toText (nodeSignature n2)
  && nodeKind n1 == nodeKind n2
  && toText (nodeLabel n1) == toText (nodeLabel n2)

-- | An obligation is discharged by an explicit ack or an admitted rule.
discharged :: Change -> Discharge -> Text -> Bool
discharged c d v = v `elem` acks d || any (\r -> r c v) (rules d)

-- | Obligations remaining after discharge: frontier nodes with no ack and no
-- rule clearing them.
openObligations :: Change -> Discharge -> [Text]
openObligations c d = [ v | v <- frontier (before c) (touched c), not (discharged c d v) ]

-- | The change is complete iff every frontier node is discharged.
complete :: Change -> Discharge -> Bool
complete c d = all (discharged c d) (frontier (before c) (touched c))

-- | A failed admissibility conjunct, named for the report.
data FailedConjunction
  = Uncomplete [Text]                     -- open obligations remain
  | NewSpecViolated (Maybe Text) Text     -- designated new spec violated, witness
  | Regression (Maybe Text) Text          -- a validated spec now violated, witness
  deriving (Eq, Show)

-- | First witness of a verdict ("" when none).
headWitness :: Verdict -> Text
headWitness v = case witnesses v of
  (w:_) -> w
  []    -> ""

-- | Admissibility = completeness ∧ the designated new spec holds on the after
-- projection ∧ no other validated spec regresses. Returns (admissible,
-- failed conjuncts).
assess :: Change -> Discharge -> SpecDocument -> Maybe Text -> (Bool, [FailedConjunction])
assess c d doc newSpecName = (null failures, failures)
  where
    after' = after c
    results = [ (nm, v) | (nm, s) <- docSpecs doc, let v = evaluateSpec after' s ]
    failures =
      (if complete c d then [] else [Uncomplete (openObligations c d)])
      ++ newSpecFailure
      ++ regressions
    newSpecFailure = case newSpecName of
      Just nm -> case [ v | (n, v) <- results, n == Just nm ] of
        (v:_) -> if holds v then [] else [NewSpecViolated (Just nm) (headWitness v)]
        []    -> [NewSpecViolated (Just nm) ""]
      Nothing -> []
    regressions =
      [ Regression nm (headWitness v)
      | (nm, v) <- results, nm /= newSpecName, not (holds v) ]

-- | Convenience boolean form of 'assess'.
admissible :: Change -> Discharge -> SpecDocument -> Maybe Text -> Bool
admissible c d doc ns = let (_, failures) = assess c d doc ns in null failures