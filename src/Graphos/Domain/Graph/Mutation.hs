{-# LANGUAGE StrictData #-}
-- | Pure mutation operations over the in-memory property graph, backing
-- the openCypher write clause subset (openspec change
-- opencypher-write-mutations).
--
-- Model reconciliation rules:
--
--   * Node labels: the primary label is 'nodeKind'. Extra labels are
--     stored in @nodeExtra.extra_labels@ (JSON array). Reads (Mapping)
--     consult both, so @SET n:L@ / @REMOVE n:L@ round-trip.
--   * Properties: known model fields (label, source_file, weight,
--     confidence, ...) are written in place; any other property is stored
--     in @nodeExtra@ / @edgeExtra@. 'Mapping' resolves reads from both.
--   * Relationship types are restricted to the closed 'Relation' enum —
--     enforced at parse time (Parser.checkWriteRelTypes).
--   * No parallel edges: creating a relationship for an existing
--     (source, target) pair upserts the existing edge (properties merged,
--     'rels_upserted' instead of 'rels_created').
--
-- Pure — no IO, fully testable.
module Graphos.Domain.Graph.Mutation
  ( -- * Mutated graph + summary
    MutationSummary(..)
  , emptyMutationSummary

    -- * Node operations
  , putNode
  , addNodeLabel
  , removeNodeLabel
  , setNodeProp
  , removeNodeProp
  , nodeExtraLabels
  , withNodeExtraLabels
  , deleteNode

    -- * Edge operations
  , putEdgeUpsert
  , setEdgeProp
  , removeEdgeProp
  , deleteEdge
  , deleteEdgesTouching

    -- * Rebuild
  , rebuildAdjacency
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (ShortText, fromText, toText)

import Graphos.Domain.Types (NodeId, Node(..), Edge(..), EdgeId(..), FileType(..), Confidence(..))
import Graphos.Domain.Graph.Core (Graph(..), computeGraphHash)

-- ───────────────────────────────────────────────
-- Summary
-- ───────────────────────────────────────────────

-- | Counts of applied mutation effects.
data MutationSummary = MutationSummary
  { msNodesCreated      :: !Int
  , msRelsCreated       :: !Int
  , msRelsUpserted      :: !Int
  , msPropertiesSet     :: !Int
  , msPropertiesRemoved :: !Int
  , msNodesDeleted      :: !Int
  , msRelsDeleted       :: !Int
  } deriving (Eq, Show)

emptyMutationSummary :: MutationSummary
emptyMutationSummary = MutationSummary 0 0 0 0 0 0 0

-- ───────────────────────────────────────────────
-- Extra-object helpers
-- ───────────────────────────────────────────────

-- | Read the @extra_labels@ array from a node's extra object.
nodeExtraLabels :: Node -> [Text]
nodeExtraLabels n =
  case nodeExtra n of
    Just (Object km) ->
      case KM.lookup (Key.fromText "extra_labels") km of
        Just (Array xs) -> [ t | String t <- V.toList xs ]
        _               -> []
    _ -> []

-- | Set @extra_labels@ on a node's extra object, merging with any
-- existing extra values.
withNodeExtraLabels :: [Text] -> Node -> Node
withNodeExtraLabels ls n =
  let key = Key.fromText "extra_labels"
      base = case nodeExtra n of
        Just (Object km) -> km
        _                -> KM.empty
  in n { nodeExtra = Just (Object (KM.insert key (Array (V.fromList (map String ls))) base)) }

-- | Add an extra label, deduplicating.
withNodeExtraLabelsUnique :: Text -> Node -> Node
withNodeExtraLabelsUnique lbl n =
  let cur = nodeExtraLabels n
  in if lbl `elem` cur
       then n
       else withNodeExtraLabels (cur ++ [lbl]) n

-- ───────────────────────────────────────────────
-- Node operations
-- ───────────────────────────────────────────────

-- | Insert or replace a node in the graph.
putNode :: Node -> Graph -> Graph
putNode n g = g { gNodes = Map.insert (nodeId n) n (gNodes g) }

-- | Remove a node and every edge touching it (DETACH DELETE).
deleteNode :: NodeId -> Graph -> Graph
deleteNode nid g =
  let (gNoEdges, _) = deleteEdgesTouching nid g { gNodes = Map.delete nid (gNodes g) }
  in gNoEdges

-- | Delete a single edge by its endpoint pair.
deleteEdge :: (NodeId, NodeId) -> Graph -> Graph
deleteEdge pair g = g { gEdges = Map.delete pair (gEdges g) }

-- | Delete every edge incident to a node. Returns the graph and the
-- number of deleted edges.
deleteEdgesTouching :: NodeId -> Graph -> (Graph, Int)
deleteEdgesTouching nid g =
  let (kept, removed) = Map.partitionWithKey (\(s, t) _ -> s /= nid && t /= nid) (gEdges g)
  in (g { gEdges = kept }, Map.size removed)

-- | Add a label: primary @nodeKind@ if unset, else the extra-labels list.
addNodeLabel :: Text -> Node -> Node
addNodeLabel lbl n =
  case nodeKind n of
    Nothing -> n { nodeKind = Just (fromText lbl) }
    Just k
      | toText k == lbl -> n
      | otherwise       -> withNodeExtraLabelsUnique lbl n

-- | Remove a label: an extra label if present there, else the primary
-- kind (cleared, leaving an unlabelled node).
removeNodeLabel :: Text -> Node -> Node
removeNodeLabel lbl n
  | lbl `elem` extras = withNodeExtraLabels (filter (/= lbl) extras) n
  | primaryIs lbl     = n { nodeKind = Nothing }
  | otherwise         = n
  where
    extras = nodeExtraLabels n
    primaryIs l = maybe False (\k -> toText k == l) (nodeKind n)

-- | Write a node property: model fields in place, others into @nodeExtra@.
setNodeProp :: Text -> Value -> Node -> Node
setNodeProp k v n
  | k == "id"           = n
  | k == "label"        = n { nodeLabel = labelOf v }
  | k == "source_file"  = n { nodeSourceFile = labelOf v }
  | k == "file_type"    = n { nodeFileType = fileTypeOf v }
  | k == "line_start"   = n { nodeLineStart = intOf v }
  | k == "line_end"     = n { nodeLineEnd = intOf v }
  | k == "signature"    = n { nodeSignature = labelOf' v }
  | k == "community"    = n { nodeCommunityId = intOf v }
  | k == "community_id" = n { nodeCommunityId = intOf v }
  | k == "degree"       = n { nodeDegree = intOf v }
  | k == "is_bridge"    = n { nodeIsBridge = boolOf v }
  | k == "kind"         = n { nodeKind = labelOf' v }
  | otherwise           = withNodeExtraProp k v n

-- | Remove a node property: model fields reset to their defaults,
-- others deleted from @nodeExtra@.
removeNodeProp :: Text -> Node -> Node
removeNodeProp k n
  | k == "id"           = n
  | k == "label"        = n { nodeLabel = fromText "" }
  | k == "source_file"  = n { nodeSourceFile = fromText "" }
  | k == "line_start"   = n { nodeLineStart = Nothing }
  | k == "line_end"     = n { nodeLineEnd = Nothing }
  | k == "signature"    = n { nodeSignature = Nothing }
  | k == "community"    = n { nodeCommunityId = Nothing }
  | k == "community_id" = n { nodeCommunityId = Nothing }
  | k == "degree"       = n { nodeDegree = Nothing }
  | k == "is_bridge"    = n { nodeIsBridge = Nothing }
  | k == "kind"         = n { nodeKind = Nothing }
  | otherwise           = removeNodeExtraProp k n

-- ───────────────────────────────────────────────
-- Edge operations
-- ───────────────────────────────────────────────

-- | Insert or upsert an edge. If an edge already exists for the
-- (source, target) pair, the new edge replaces it (upsert) and the
-- caller is told via the returned flag (True = created, False =
-- upserted).
putEdgeUpsert :: Edge -> Graph -> (Graph, Bool)
putEdgeUpsert e g
  | Map.member (edgeSource e, edgeTarget e) (gEdges g) =
      (g { gEdges = Map.insert (edgeSource e, edgeTarget e) e (gEdges g) }, False)
  | otherwise =
      (g { gEdges = Map.insert (edgeSource e, edgeTarget e) e (gEdges g) }, True)

-- | Write an edge property: model fields in place, others into @edgeExtra@.
setEdgeProp :: Text -> Value -> Edge -> Edge
setEdgeProp k v e
  | k == "id"         = e { edgeId = idOf v }
  | k == "weight"     = e { edgeWeight = fromMaybe 0.0 (doubleOf v) }
  | k == "confidence" = e { edgeConfidence = confOf v }
  | otherwise         = withEdgeExtraProp k v e

-- | Remove an edge property: model fields reset, others removed from extra.
removeEdgeProp :: Text -> Edge -> Edge
removeEdgeProp k e
  | k == "id"         = e
  | k == "weight"     = e { edgeWeight = 0.0 }
  | k == "confidence" = e
  | otherwise         = removeEdgeExtraProp k e

-- ───────────────────────────────────────────────
-- Adjacency rebuild
-- ───────────────────────────────────────────────

-- | Rebuild adjacency maps and the structural hash from the current
-- nodes/edges. Call after mutating edges.
rebuildAdjacency :: Graph -> Graph
rebuildAdjacency g =
  let edgeList = Map.elems (gEdges g)
      fwd = Map.fromListWith Set.union
        [ (edgeSource e, Set.singleton (edgeTarget e)) | e <- edgeList ]
      bwd = Map.fromListWith Set.union
        [ (edgeTarget e, Set.singleton (edgeSource e)) | e <- edgeList ]
      bwd' = if gDirected g then bwd else Map.unionWith Set.union bwd fwd
  in g
    { gAdjFwd  = fwd
    , gAdjBack = bwd'
    , gHash    = computeGraphHash (gNodes g) (gEdges g)
    }

-- ───────────────────────────────────────────────
-- Internal helpers
-- ───────────────────────────────────────────────

withNodeExtraProp :: Text -> Value -> Node -> Node
withNodeExtraProp k v n =
  let key = Key.fromText k
      base = case nodeExtra n of
        Just (Object km) -> km
        _                -> KM.empty
  in n { nodeExtra = Just (Object (KM.insert key v base)) }

removeNodeExtraProp :: Text -> Node -> Node
removeNodeExtraProp k n =
  let key = Key.fromText k
      base = case nodeExtra n of
        Just (Object km) -> km
        _                -> KM.empty
      km' = KM.delete key base
  in if KM.null km' then n { nodeExtra = Nothing } else n { nodeExtra = Just (Object km') }

withEdgeExtraProp :: Text -> Value -> Edge -> Edge
withEdgeExtraProp k v e =
  let key = Key.fromText k
      base = case edgeExtra e of
        Just (Object km) -> km
        _                -> KM.empty
  in e { edgeExtra = Just (Object (KM.insert key v base)) }

removeEdgeExtraProp :: Text -> Edge -> Edge
removeEdgeExtraProp k e =
  let key = Key.fromText k
      base = case edgeExtra e of
        Just (Object km) -> km
        _                -> KM.empty
      km' = KM.delete key base
  in if KM.null km' then e { edgeExtra = Nothing } else e { edgeExtra = Just (Object km') }

labelOf :: Value -> ShortText
labelOf (String t) = fromText t
labelOf (Number d) = fromText (T.pack (show d))
labelOf (Bool b)   = fromText (T.pack (show b))
labelOf _          = fromText ""

labelOf' :: Value -> Maybe ShortText
labelOf' (String t) = Just (fromText t)
labelOf' _          = Nothing

intOf :: Value -> Maybe Int
intOf (Number d) = Just (round d)
intOf _          = Nothing

doubleOf :: Value -> Maybe Double
doubleOf (Number d) = Just (realToFrac d)
doubleOf _          = Nothing

boolOf :: Value -> Maybe Bool
boolOf (Bool b) = Just b
boolOf _        = Nothing

confOf :: Value -> Confidence
confOf (Number d) = Confidence (realToFrac d)
confOf _          = Confidence 1.0

fileTypeOf :: Value -> FileType
fileTypeOf (String t) = case t of
  "doc"    -> DocFile
  "paper"  -> PaperFile
  "image"  -> ImageFile
  "video"  -> VideoFile
  "audio"  -> AudioFile
  "office" -> OfficeFile
  _        -> CodeFile
fileTypeOf _ = CodeFile

idOf :: Value -> EdgeId
idOf (String t) = EdgeId t
idOf v          = EdgeId (T.pack (show v))