-- | speccheck orchestration: run the Domain checker, assemble the findings,
-- render the report (spec-graph-checks capability).
--
-- Gate discipline: cycles gate always; unimplemented requirements gate only
-- under strict coverage; contradiction candidates NEVER gate by themselves —
-- they await pairwise adjudication (conflict-adjudication capability, not yet
-- implemented), per the Lean-proved subset chain blocking ⊆ confirmed ⊆
-- candidates.
--
-- Pure — no IO.
module Graphos.UseCase.SpecCheck
  ( SpecReport(..)
  , runSpecCheck
  , reportGates
  , renderSpecReport
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.SpecCheck
import Graphos.Domain.Types

data SpecReport = SpecReport
  { srSpecNodes      :: !Int
  , srSpecEdges      :: !Int
  , srTopoCertificate :: !(Maybe [NodeId]) -- ^ present iff acyclic (re-checked)
  , srCycleWitness   :: !(Maybe [NodeId])  -- ^ present iff cyclic (re-checked)
  , srCandidates     :: ![(NodeId, NodeId)]
  , srStale          :: ![(NodeId, NodeId)]
  , srUnimplemented  :: ![NodeId]
  , srStrictCoverage :: !Bool
  } deriving (Eq, Show)

instance ToJSON SpecReport where
  toJSON r = object
    [ "spec_nodes" .= srSpecNodes r
    , "spec_edges" .= srSpecEdges r
    , "acyclic" .= maybe False (const True) (srTopoCertificate r)
    , "topo_certificate" .= srTopoCertificate r
    , "cycle_witness" .= srCycleWitness r
    , "contradiction_candidates"
        .= map (\(a, b) -> object [ "r1" .= a, "r2" .= b ]) (srCandidates r)
    , "stale_supersessions"
        .= map (\(s, d) -> object [ "source" .= s, "decision" .= d ]) (srStale r)
    , "unimplemented" .= srUnimplemented r
    , "gating" .= reportGates r
    ]

-- | Run the full deterministic catalogue over spec-artifact nodes.
runSpecCheck :: [Node] -> [Edge] -> Bool -> SpecReport
runSpecCheck ns es strictCov =
  let specIds =
        [ nodeId n
        | n <- ns
        , isRequirementNode n || isDecisionNode n
        ]
      certificate = checkAcyclic specIds es specRels
      witness = case certificate of
        Just _  -> Nothing
        Nothing -> findCycleWitness specIds es specRels
  in SpecReport
       { srSpecNodes = length specIds
       , srSpecEdges = length [ e | e <- es, edgeRelation e `elem` specRels ]
       , srTopoCertificate = certificate
       , srCycleWitness = witness
       , srCandidates = candidatePairs ns es
       , srStale = staleSupersessions ns es
       , srUnimplemented = unimplementedRequirements ns es
       , srStrictCoverage = strictCov
       }

-- | Exit gate: cycles always; coverage under strict only; candidates never
-- (they are adjudication input, not verdicts).
reportGates :: SpecReport -> Bool
reportGates r =
  maybe False (const True) (srCycleWitness r)
    || (srStrictCoverage r && not (null (srUnimplemented r)))

-- | Markdown report (stdout default).
renderSpecReport :: SpecReport -> Text
renderSpecReport r = T.unlines $
  [ "# speccheck report"
  , ""
  , "- spec nodes: " <> tshow (srSpecNodes r)
  , "- spec-relation edges: " <> tshow (srSpecEdges r)
  , ""
  ]
  ++ sectionCycles
  ++ sectionCandidates
  ++ sectionStale
  ++ sectionCoverage
  ++ [ "", verdictLine ]
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show

    arrow = T.intercalate " -> "

    sectionCycles = case (srTopoCertificate r, srCycleWitness r) of
      (Just _, _) ->
        [ "## Dependency cycles", "", "None — acyclic (topological certificate in the JSON report)." ]
      (Nothing, Just w) ->
        [ "## Dependency cycles", "", "**CYCLE**: " <> arrow w ]
      (Nothing, Nothing) ->
        [ "## Dependency cycles", ""
        , "**CYCLE detected** (no witness constructed — sort stalled)." ]

    sectionCandidates =
      "" : "## Contradiction candidates (await adjudication — non-gating)" : "" :
      if null (srCandidates r)
        then [ "None." ]
        else map (\(a, b) -> "- " <> a <> " ⇄ " <> b) (srCandidates r)

    sectionStale =
      "" : "## Stale supersessions" : "" :
      if null (srStale r)
        then [ "None." ]
        else map (\(s, d) -> "- " <> s <> " still references superseded " <> d) (srStale r)

    sectionCoverage =
      "" : ("## Unimplemented requirements ("
             <> (if srStrictCoverage r then "gating" else "warning")
             <> ")") : "" :
      if null (srUnimplemented r)
        then [ "None." ]
        else map ("- " <>) (srUnimplemented r)

    verdictLine =
      if reportGates r then "VERDICT: FAIL" else "VERDICT: PASS"
