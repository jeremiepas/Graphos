-- | JSON Graph Format (JGF) envelope model — the canonical on-disk graph
-- document (media type @application\/vnd.jgf+json@, spec at
-- <https://jsongraphformat.info jsongraphformat.info>).
--
-- A graphos graph file is a JGF document:
--
-- > { "graph": { "directed": true
-- >            , "type": "graphos.code-knowledge-graph"
-- >            , "metadata": { "graphos": { "schemaVersion": "1.0", ... } }
-- >            , "nodes": { "<id>": { "id", "label", "metadata" } }
-- >            , "edges": [ { "source", "relation", "target", "directed", "metadata" } ] } }
--
-- Lossless field mapping:
--
--   * Node: @id@ and @label@ stay top-level; @file_type@, @source_file@,
--     @community_id@, @line_start@, @line_end@, @signature@, @kind@, @degree@,
--     @is_bridge@ and @extra@ live under node @metadata@ (absent fields are
--     omitted).
--   * Edge: @source@, @relation@, @target@ (and the @directed@ override) stay
--     top-level; @id@, @weight@, @confidence@ and @extra@ live under edge
--     @metadata@.
--   * Graph-level graphos data (@communities@, @cohesion@, @god_nodes@,
--     @community_labels@, @compositions@, @community_aggregates@,
--     @embeddings_path@, @null_model@, @graph_hash@, @schemaVersion@) lives
--     under @graph.metadata.graphos@ — never as ad-hoc top-level keys.
--
-- The optional JGF @graph.label@ field is unused by graphos (omitted).
-- The envelope is versioned via @schemaVersion@ (@major.minor@): readers
-- reject documents whose major version is unknown.
module Graphos.Domain.Types.JGF
  ( -- * Format constants
    jgfMediaType
  , jgfGraphType
  , jgfSchemaVersion
  , supportedJgfMajorVersions

    -- * Document shape
  , jgfDocument
  , jgfGraphDirectives
  , graphosMetadata

    -- * Node / edge mapping
  , nodeToJGF
  , edgeToJGF
  , jgfNodesObject
  ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import Data.Text.Short (toText)

import Graphos.Domain.Types.Edge (Edge(..), relationToText)
import Graphos.Domain.Types.Node (Node(..))

-- | Media type of the canonical graph document.
jgfMediaType :: Text
jgfMediaType = "application/vnd.jgf+json"

-- | @graph.type@ value identifying a graphos code-knowledge graph.
jgfGraphType :: Text
jgfGraphType = "graphos.code-knowledge-graph"

-- | graphos schema version written under @graph.metadata.graphos.schemaVersion@.
-- Format @major.minor@: minor bumps are additive; major bumps are breaking.
jgfSchemaVersion :: Text
jgfSchemaVersion = "1.0"

-- | Major schema versions the reader fully supports.
supportedJgfMajorVersions :: [Int]
supportedJgfMajorVersions = [1]

-- | Wrap the @graph@ fields in the top-level JGF document envelope:
-- @{ "graph": { ... } }@.
jgfDocument :: [(Text, Value)] -> Value
jgfDocument graphFields =
  object [ "graph" .= object [ Key.fromText k .= v | (k, v) <- graphFields ] ]

-- | The fixed JGF graph directives graphos always emits: @directed: true@ and
-- the graphos @type@ identifier.
jgfGraphDirectives :: [(Text, Value)]
jgfGraphDirectives =
  [ ("directed", Bool True)
  , ("type", String jgfGraphType)
  ]

-- | Map a graphos node to its JGF representation: @id@ and @label@ top-level,
-- every other field under @metadata@ (absent optional fields are omitted).
nodeToJGF :: Node -> Value
nodeToJGF n = object
  [ "id"      .= nodeId n
  , "label"   .= toText (nodeLabel n)
  , "metadata" .= nodeMetadata n
  ]

nodeMetadata :: Node -> Value
nodeMetadata n = object $
  [ "file_type"   .= nodeFileType n
  , "source_file" .= toText (nodeSourceFile n)
  ] ++
  [ "source" .= toText src | Just src <- [nodeSource n] ] ++
  [ "line_start"   .= v | Just v <- [nodeLineStart n] ] ++
  [ "line_end"     .= v | Just v <- [nodeLineEnd n] ] ++
  [ "signature"    .= toText sig | Just sig <- [nodeSignature n] ] ++
  [ "community_id" .= v | Just v <- [nodeCommunityId n] ] ++
  [ "kind"         .= toText knd | Just knd <- [nodeKind n] ] ++
  [ "degree"       .= v | Just v <- [nodeDegree n] ] ++
  [ "is_bridge"    .= v | Just v <- [nodeIsBridge n] ] ++
  [ "extra"        .= v | Just v <- [nodeExtra n] ]

-- | Map a graphos edge to its JGF representation: @source@ / @relation@ /
-- @target@ (plus the @directed@ override) top-level; @id@, @weight@,
-- @confidence@ and @extra@ under @metadata@.
edgeToJGF :: Edge -> Value
edgeToJGF e = object
  [ "source"   .= edgeSource e
  , "target"   .= edgeTarget e
  , "relation" .= relationToText (edgeRelation e)
  , "directed" .= True
  , "metadata" .= object ([ "id"         .= edgeId e
                          , "weight"     .= edgeWeight e
                          , "confidence" .= edgeConfidence e
                          ] ++ [ "extra" .= v | Just v <- [edgeExtra e] ])
  ]

-- | JGF @nodes@ section: an object keyed by node id (spec-canonical).
jgfNodesObject :: [Node] -> Value
jgfNodesObject ns = object [ Key.fromText (nodeId n) .= nodeToJGF n | n <- ns ]

-- | Build the @graph.metadata.graphos@ object. The @schemaVersion@ field is
-- always prepended so every emitted document is versioned.
graphosMetadata :: [(Text, Value)] -> Value
graphosMetadata fields =
  object (("schemaVersion" .= jgfSchemaVersion) : [ Key.fromText k .= v | (k, v) <- fields ])