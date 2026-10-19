-- | Intent-graph projection orchestration (intent-graph-verification 3.2).
--
-- Pure: projects a built graph into the deterministic, well-formed intent
-- projection consumed by the certified checker.
module Graphos.UseCase.Projection
  ( projectGraph
  , projectionNodeIdents
  , checkProjectionWellFormed
   ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Graphos.Domain.Graph.Core (Graph)
import Graphos.Domain.Intent.Classifier (ClassifierConfig)
import Graphos.Domain.Intent.Projection
  ( IntentProjection, projectionFromGraph, checkWellFormed, intentNodes, intentNodeId )

-- | Project a built graph into its intent-graph projection. Node ids pass
-- through verbatim (@\mu@ = identity), kinds come from the classifier.
projectGraph :: ClassifierConfig -> Graph -> IntentProjection
projectGraph cfg = projectionFromGraph cfg

-- | The stable node ids in a projection, in canonical order — the @\mu@
-- correspondence key shared across before/after projections.
projectionNodeIdents :: IntentProjection -> [Text]
projectionNodeIdents p = [ intentNodeId n | n <- Map.elems (intentNodes p) ]

-- | Well-formedness of a projection: every edge endpoint must be declared.
checkProjectionWellFormed :: IntentProjection -> Either Text [Text]
checkProjectionWellFormed = checkWellFormed
