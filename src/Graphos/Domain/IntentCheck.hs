{-# LANGUAGE StrictData #-}
-- | Decidable intent-spec checker, mirroring the Lean model
-- (openspec/changes/intent-graph-verification/lean/IntentGraph.lean) clause for
-- clause. All quantifiers are bounded by the node set, so evaluation is the
-- decision procedure: its soundness is the Lean theorem `check_sound`.
--
-- Four patterns compose with `and` / `not`:
--   * reaches   — a named node of one kind reaches a node of another kind via edges of one label
--   * absent    — no node of a kind whose name starts with a prefix (modulo a whitelist)
--   * covered   — every node of a kind is reached via a label by a node of another kind
--   * parallel  — total two-way correspondence between two kinds via a label
--
-- Kinds are matched against the graph nodes' @nodeKind@ text; an unknown kind
-- simply matches no nodes. Evaluation uses no model and is deterministic.
module Graphos.Domain.IntentCheck
  ( -- * Spec language
    Spec(..)
  , Verdict(..)
  , witnesses
    -- * Evaluation
  , reachable
  , evaluateSpec
  , evaluate
  , allHolds
  , holds
    -- * Spec-file parsing (JSON: {name?, specs:[Spec...]})
  , SpecDocument(..)
  , parseSpecDocument
  ) where

import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text.Short (toText)

import Graphos.Domain.Graph.Core (Graph (gNodes, gEdges))
import Graphos.Domain.Types.Node (Node, nodeId, nodeKind, nodeLabel)
import Graphos.Domain.Types.Edge (Relation, edgeRelation)

-- | A spec pattern. The discriminator lives in @"kind"@; node-kind parameters
-- use distinct field names to avoid clashing with @covered@'s node kind.
data Spec
  = Reaches Text Text Relation Text      -- fromKind fromName rel toKind
  | Absent  Text Text [Text]             -- nodeKind prefix whitelist
  | Covered Text Relation Text           -- nodeKind rel toKind
  | Parallel Text Text Relation          -- fromKind toKind rel
  | And [Spec]                           -- conjunction of sub-specs
  | Not Spec
  deriving (Eq, Show)

instance ToJSON Spec where
  toJSON (Reaches fk fn r tk) = object
    [ "kind"     .= ("reaches" :: Text)
    , "fromKind" .= fk
    , "fromName" .= fn
    , "rel"      .= r
    , "toKind"   .= tk
    ]
  toJSON (Absent n pk wl) = object
    [ "kind"      .= ("absent" :: Text)
    , "nodeKind"  .= n
    , "prefix"    .= pk
    , "whitelist" .= wl
    ]
  toJSON (Covered nk r tk) = object
    [ "kind"     .= ("covered" :: Text)
    , "nodeKind" .= nk
    , "rel"      .= r
    , "toKind"   .= tk
    ]
  toJSON (Parallel fk tk r) = object
    [ "kind"     .= ("parallel" :: Text)
    , "fromKind" .= fk
    , "toKind"   .= tk
    , "rel"      .= r
    ]
  toJSON (And subs) = object
    [ "kind"    .= ("and" :: Text)
    , "specs"   .= subs
    ]
  toJSON (Not s) = object
    [ "kind"    .= ("not" :: Text)
    , "spec"    .= s
    ]

-- | A verdict carries the witnessing node ids: satisfied nodes on success,
-- failing nodes on violation.
data Verdict = Satisfied [Text] | Violated [Text]
  deriving (Eq, Show)

witnesses :: Verdict -> [Text]
witnesses (Satisfied ws) = ws
witnesses (Violated ws)  = ws

-- | A verdict holds when there is no violation witness.
holds :: Verdict -> Bool
holds (Satisfied _) = True
holds (Violated ws) = null ws

-- | Edges of a given label leaving @u@.
edgesFrom :: Graph -> Relation -> Text -> [Text]
edgesFrom g rel u =
  [ d | ((s, d), e) <- Map.toList (gEdges g), s == u, edgeRelation e == rel ]

-- | Edges of a given label entering @w@.
edgesTo :: Graph -> Relation -> Text -> [Text]
edgesTo g rel w =
  [ s | ((s, d), e) <- Map.toList (gEdges g), d == w, edgeRelation e == rel ]

nodeById :: Graph -> Text -> Maybe Node
nodeById g id' = Map.lookup id' (gNodes g)

-- | Reflexive-transitive closure under a single label (fuel = |V| suffices:
-- every simple path has ≤ |V| edges, so saturation is reached). Includes start.
reachable :: Graph -> Relation -> Text -> Set.Set Text
reachable g rel start = go (Set.singleton start)
  where
    go acc = let acc' = Set.union acc (Set.fromList (concatMap (edgesFrom g rel) (Set.toList acc)))
             in if acc' == acc then acc else go acc'

nodesOfKind :: Graph -> Text -> [Node]
nodesOfKind g k = [ n | n <- Map.elems (gNodes g), fmap toText (nodeKind n) == Just k ]

-- | Evaluate one spec against a graph. The decision procedure mirrors `sat`.
evaluateSpec :: Graph -> Spec -> Verdict
evaluateSpec g = go
  where
    labelEq :: Node -> Text -> Bool
    labelEq n t = toText (nodeLabel n) == t

    reachVia :: Graph -> Relation -> Text -> Text -> Bool
    reachVia gr rel s t = t `Set.member` (reachable gr rel s)

    go (Reaches fromK fromName rel toK) =
      let us = [ n | n <- nodesOfKind g fromK, labelEq n fromName ]
          bad = [ n | n <- us, not (any (\w -> reachVia g rel (nodeId n) (nodeId w)) (nodesOfKind g toK)) ]
      in if null us then Violated [fromName]
         else if null bad then Satisfied []
         else Violated (map nodeId bad)

    go (Covered nodeK rel toK) =
      let good n = any (\w -> reachVia g rel (nodeId w) (nodeId n)) (nodesOfKind g toK)
          bad = [ n | n <- nodesOfKind g nodeK, not (good n) ]
      in if null bad then Satisfied [] else Violated [nodeId n | n <- bad]

    go (Parallel fromK toK rel) =
      let as     = nodesOfKind g fromK
          bs     = nodesOfKind g toK
          kindOf w k = case nodeById g w of
                             Just n  -> fmap toText (nodeKind n) == Just k
                             Nothing -> False
          fwdBad = [ n | n <- as, not (any (\w -> kindOf w toK) (edgesFrom g rel (nodeId n))) ]
          bckBad = [ n | n <- bs, not (any (\s -> kindOf s fromK) (edgesTo g rel (nodeId n))) ]
          bad    = fwdBad ++ bckBad
      in if null bad then Satisfied [] else Violated [nodeId n | n <- bad]

    go (Absent nodeK pre wl) =
      let bad = [ n
                | n <- nodesOfKind g nodeK
                , T.isPrefixOf pre (toText (nodeLabel n))
                , not (toText (nodeLabel n) `elem` wl) ]
      in if null bad then Satisfied [] else Violated [nodeId n | n <- bad]

    go (And ss) =
      let vs = map go ss
      in if all satisfied vs
           then Satisfied []
           else Violated (concatMap witnesses vs)
      where
        satisfied (Satisfied _) = True
        satisfied (Violated _)  = False

    go (Not s) = case go s of
      Violated _  -> Satisfied []
      Satisfied _ -> Violated []

-- | Evaluate a whole document; returns one verdict per (named) spec.
evaluate :: Graph -> SpecDocument -> [(Maybe Text, (Spec, Verdict))]
evaluate g doc = map (\(nm, s) -> (nm, (s, evaluateSpec g s))) (docSpecs doc)

-- | Every spec holds (no violation) on the graph.
allHolds :: Graph -> SpecDocument -> Bool
allHolds g doc = all (\(_, (_, v)) -> holds v) (evaluate g doc)

-- | Parsed intent-spec document: an optional document name plus a list of
-- specs, each carrying an optional per-spec name (used by `--new-spec`).
data SpecDocument = SpecDocument
  { docName  :: Maybe Text
  , docSpecs :: [(Maybe Text, Spec)]
  } deriving (Eq, Show)

instance FromJSON Spec where
  parseJSON = withObject "Spec" $ \v -> do
    k <- v .: "kind"
    case k of
      "reaches"  -> Reaches <$> v .: "fromKind" <*> v .: "fromName"
                          <*> v .: "rel" <*> v .: "toKind"
      "absent"   -> Absent <$> v .: "nodeKind" <*> v .: "prefix"
                          <*> (v .:? "whitelist" .!= [])
      "covered"  -> Covered <$> v .: "nodeKind" <*> v .: "rel" <*> v .: "toKind"
      "parallel" -> Parallel <$> v .: "fromKind" <*> v .: "toKind" <*> v .: "rel"
      "and"      -> do
        ss <- v .: "specs"
        And <$> mapM parseJSON ss
      "not"      -> Not <$> v .: "spec"
      other      -> fail $ "Unknown spec kind: " ++ other

instance FromJSON SpecDocument where
  parseJSON = withObject "SpecDocument" $ \v -> do
    name  <- v .:? "name"
    raw   <- v .: "specs"
    specs <- mapM parseEntry raw
    pure SpecDocument { docName = name, docSpecs = specs }
    where
      parseEntry o =
        let nm = case o of
                   Object km -> case KM.lookup (Key.fromText "name") km of
                     Just (String t) -> Just t
                     _               -> Nothing
                   _         -> Nothing
        in do
          sp <- parseJSON o
          pure (nm, sp)

-- | Decode an intent-spec document from JSON bytes.
parseSpecDocument :: BL.ByteString -> Either String SpecDocument
parseSpecDocument = eitherDecode
