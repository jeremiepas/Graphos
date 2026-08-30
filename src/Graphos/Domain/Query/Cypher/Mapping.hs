-- | Deterministic mapping between Graphos's domain model and the
-- property-graph model exposed by the openCypher/GQL query surface.
--
-- The mapping is fixed and documented (see openspec change
-- opencypher-gql-query, design.md):
--
--   * Cypher node label          <- nodeKind
--   * Cypher relationship type   <- edgeRelation (via relationToText)
--   * node properties            <- the remaining node fields
--   * relationship properties    <- the remaining edge fields
--
-- A property that a node/edge does not declare resolves to null: the
-- accessors below return 'Nothing' for unknown keys, and the evaluator
-- treats that as a null value (the row is excluded from comparisons,
-- no error is raised).
--
-- Pure — no IO, fully testable.
module Graphos.Domain.Query.Cypher.Mapping
  ( -- * Node mapping
    nodeCypherLabel
  , nodeProperties
  , nodeProperty

    -- * Edge mapping
  , edgeCypherType
  , edgeProperties
  , edgeProperty
  ) where

import Data.Aeson (Value, ToJSON(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Text.Short (toText)

import Graphos.Domain.Types.Node (Node(..))
import Graphos.Domain.Types.Edge (Edge(..), relationToText)

-- ───────────────────────────────────────────────
-- Node mapping
-- ───────────────────────────────────────────────

-- | The Cypher label of a node: its 'nodeKind' (e.g. "Function", "Type").
-- 'Nothing' when the node has no kind — such a node matches only
-- unlabelled patterns.
nodeCypherLabel :: Node -> Maybe Text
nodeCypherLabel = fmap toText . nodeKind

-- | All queryable properties of a node.
--
-- Keys are snake_case, matching the JSON export field names where they
-- exist, so a property name used in a query is the same name seen in the
-- exported graph file.
nodeProperties :: Node -> Map Text Value
nodeProperties n =
  Map.fromList
    [ ("id",         toJSON (nodeId n))
    , ("label",      toJSON (nodeLabel n))
    , ("file_type",  toJSON (nodeFileType n))
    , ("source_file", toJSON (nodeSourceFile n))
    , ("line_start", toJSON (nodeLineStart n))
    , ("line_end",   toJSON (nodeLineEnd n))
    , ("signature",  toJSON (nodeSignature n))
    , ("community",  toJSON (nodeCommunityId n))
    , ("degree",     toJSON (nodeDegree n))
    , ("is_bridge",  toJSON (nodeIsBridge n))
    ]
    <> textSnippet
  where
    -- `text` is the snippet property: the entity signature when present.
    textSnippet = case nodeSignature n of
      Just s  -> Map.singleton "text" (toJSON s)
      Nothing -> Map.empty

-- | Look up a single node property. 'Nothing' when the property is not
-- declared (resolves to null in the evaluator).
nodeProperty :: Node -> Text -> Maybe Value
nodeProperty n k = Map.lookup k (nodeProperties n)

-- ───────────────────────────────────────────────
-- Edge mapping
-- ───────────────────────────────────────────────

-- | The Cypher relationship type of an edge: its 'edgeRelation'
-- rendered as text (e.g. "calls", "imports").
edgeCypherType :: Edge -> Text
edgeCypherType = relationToText . edgeRelation

-- | All queryable properties of an edge.
edgeProperties :: Edge -> Map Text Value
edgeProperties e =
  Map.fromList
    [ ("id",         toJSON (edgeId e))
    , ("source",     toJSON (edgeSource e))
    , ("target",     toJSON (edgeTarget e))
    , ("weight",     toJSON (edgeWeight e))
    , ("confidence", toJSON (edgeConfidence e))
    ]

-- | Look up a single edge property. 'Nothing' when the property is not
-- declared (resolves to null in the evaluator).
edgeProperty :: Edge -> Text -> Maybe Value
edgeProperty e k = Map.lookup k (edgeProperties e)
