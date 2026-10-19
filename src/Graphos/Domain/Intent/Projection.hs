{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Intent code-graph projection (intent-graph-verification 3.2).
--
-- The projection is the certified checker's input contract. It carries, for
-- every node, its stable id, its deterministic intent kind, and — for code
-- nodes — the LSP signature and node-level body hash; for every edge, its two
-- declared endpoints. Two invariants make it a sound basis for verification:
--
--   * @'NodeId'@ is preserved verbatim (@\mu@ = identity across before/after),
--     so the before/after correspondence never rewrites an id.
--   * the projection is well-formed: every edge endpoint is a declared node
--     (the hypothesis the Lean @reach_complete@ theorem needs).
--
-- Pure and deterministic: edges are emitted in a canonical order so repeated
-- projections of the same graph are byte-stable.
module Graphos.Domain.Intent.Projection
  ( -- * Projection types
    IntentNode(..)
  , IntentEdge(..)
  , IntentProjection(..)
    -- * Construction
  , projectionFromGraph
  , projectionToGraph
    -- * Well-formedness
  , checkWellFormed
   ) where

import Data.Aeson
import Data.List (nub, intercalate, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Short (fromText, toText)
import GHC.Generics (Generic)

import Graphos.Domain.Types.Node
  ( Node(..), NodeId
  , nodeId, nodeLabel, nodeSourceFile, nodeSignature, nodeBodyHash
  , nodeLineStart, nodeLineEnd, nodeKind
  , bitNodeLineStart, bitNodeLineEnd, bitNodeSignature, bitNodeBodyHash
  , FileType(..) )
import Graphos.Domain.Graph.Core (Graph(..), buildGraph)
import Graphos.Domain.Types.Graph (extractionFromLists)
import Graphos.Domain.Types.Edge
  ( Edge(..), EdgeId(..), Confidence(..)
  , edgeSource, edgeTarget, edgeRelation )
import Graphos.Domain.Intent.Classifier (ClassifierConfig, classifyNode, IntentKind(..))

-- | A projected node: its stable id, intent kind, source span signature and
-- body hash. @intentNodeId@ is the original @'nodeId'@ verbatim.
data IntentNode = IntentNode
  { intentNodeId       :: !NodeId
  , intentLabel        :: !Text
  , intentKind         :: !IntentKind
  , intentSourceFile   :: !Text
  , intentSignature    :: !(Maybe Text)
  , intentBodyHash     :: !(Maybe Text)
  , intentLineStart    :: !(Maybe Int)
  , intentLineEnd      :: !(Maybe Int)
  } deriving (Eq, Show, Generic)

instance ToJSON IntentNode where
  toJSON n = object
    [ "id"             .= intentNodeId n
    , "label"          .= intentLabel n
    , "kind"           .= intentKind n
    , "source_file"    .= intentSourceFile n
    , "signature"      .= intentSignature n
    , "body_hash"      .= intentBodyHash n
    , "line_start"     .= intentLineStart n
    , "line_end"       .= intentLineEnd n
    ]

instance FromJSON IntentNode where
  parseJSON = withObject "IntentNode" $ \v -> IntentNode
    <$> v .:  "id"
    <*> v .:  "label"
    <*> v .:  "kind"
    <*> v .:  "source_file"
    <*> v .:? "signature"
    <*> v .:? "body_hash"
    <*> v .:? "line_start"
    <*> v .:? "line_end"

-- | A projected edge: two declared endpoints and their relation.
data IntentEdge = IntentEdge
  { intentEdgeSource :: !NodeId
  , intentEdgeTarget :: !NodeId
  , intentEdgeRelation :: !Relation
  } deriving (Eq, Show, Generic)

instance ToJSON IntentEdge where
  toJSON e = object
    [ "source"   .= intentEdgeSource e
    , "target"   .= intentEdgeTarget e
    , "relation" .= intentEdgeRelation e
    ]

instance FromJSON IntentEdge where
  parseJSON = withObject "IntentEdge" $ \v -> IntentEdge
    <$> v .:  "source"
    <*> v .:  "target"
    <*> v .:  "relation"

-- | The projected intent graph: a map of declared nodes plus an ordered edge
-- list. Well-formedness requires every edge endpoint to appear in 'intentNodes'.
data IntentProjection = IntentProjection
  { intentNodes     :: !(Map NodeId IntentNode)
  , intentEdges     :: ![IntentEdge]
  } deriving (Eq, Show, Generic)

instance ToJSON IntentProjection where
  toJSON p = object
    [ "nodes" .= Map.elems (intentNodes p)
    , "edges" .= intentEdges p
    ]

instance FromJSON IntentProjection where
  parseJSON = withObject "IntentProjection" $ \v -> IntentProjection
    <$> v .:  "nodes"
    <*> v .:  "edges"

-- | Project a built graph. Node ids pass through verbatim; each node's intent
-- kind comes from the classifier; signature and body hash are carried through.
-- Edges are canonicalised (sorted) for byte-stable output.
projectionFromGraph :: ClassifierConfig -> Graph -> IntentProjection
projectionFromGraph cfg g =
  IntentProjection
    { intentNodes     = Map.fromList [(intentNodeId n, n) | n <- projectedNodes]
    , intentEdges     = sortOn edgeOrder (projectedEdges)
    }
  where
    projectedNodes = [ mkIntentNode n | n <- Map.elems (gNodes g) ]
    projectedEdges = [ mkIntentEdge e | e <- Map.elems (gEdges g) ]
    edgeOrder e = (intentEdgeSource e, intentEdgeTarget e, show (intentEdgeRelation e))

    mkIntentNode n = IntentNode
      { intentNodeId       = nodeId n
      , intentLabel        = toText (nodeLabel n)
      , intentKind         = classifyNode cfg n
      , intentSourceFile   = toText (nodeSourceFile n)
      , intentSignature    = fmap toText (nodeSignature n)
      , intentBodyHash     = fmap toText (nodeBodyHash n)
      , intentLineStart    = nodeLineStart n
      , intentLineEnd      = nodeLineEnd n
      }

    mkIntentEdge e = IntentEdge
      { intentEdgeSource = edgeSource e
      , intentEdgeTarget = edgeTarget e
      , intentEdgeRelation = edgeRelation e
      }

-- | Rebuild a checker-consumable 'Graph' from the projection: every node's
-- @nodeKind@ becomes its intent-kind text, ids / signature / body hash preserved.
-- Dangling edges (if any survived ill-formed input) are dropped by 'buildGraph'.
projectionToGraph :: IntentProjection -> Graph
projectionToGraph p =
  buildGraph False $ extractionFromLists (map mkNode (Map.elems (intentNodes p)))
                              (map mkEdge (intentEdges p))
  where
    mkNode n = Node
      { nodeId           = intentNodeId n
      , nodeLabel        = fromText (intentLabel n)
      , nodeFileType     = CodeFile
      , nodeSourceFile   = fromText (intentSourceFile n)
      , nodeLineStart    = intentLineStart n
      , nodeLineEnd      = intentLineEnd n
      , nodeSignature    = fmap fromText (intentSignature n)
      , nodeCommunityId  = Nothing
      , nodeKind         = Just (toText (intentKind n))
      , nodeDegree       = Nothing
      , nodeIsBridge     = Nothing
      , nodeExtra        = Nothing
      , nodeBodyHash     = intentBodyHash n
      , nodePresentBits  = presentBits n
      }

    mkEdge e = Edge
      (EdgeId (intentEdgeSource e <> "->" <> intentEdgeTarget e))
      (intentEdgeSource e) (intentEdgeTarget e) (intentEdgeRelation e)
      1.0 (Confidence 1.0) Nothing

    presentBits n =
      (maybe 0 (const bitNodeLineStart)     intentLineStart n)
        .|. maybe 0 (const bitNodeLineEnd)       intentLineEnd n
        .|. maybe 0 (const bitNodeSignature)     (intentSignature n)
        .|. maybe 0 (const bitNodeBodyHash)      (intentBodyHash n)

-- | Well-formedness: Right [] when every edge endpoint is a declared node;
-- otherwise Left naming the dangling endpoint(s).
checkWellFormed :: IntentProjection -> Either Text [NodeId]
checkWellFormed p =
  let nodeSet = intentNodes p
      bad = nub $ concat
        [ [ s | e <- intentEdges p, let s = intentEdgeSource e, not (Map.member s nodeSet) ]
        , [ t | e <- intentEdges p, let t = intentEdgeTarget e, not (Map.member t nodeSet) ]
        ]
  in if null bad
       then Right []
       else Left $ "intent projection: edge endpoint(s) not declared: "
                 <> T.pack (intercalate ", " (map T.unpack bad))