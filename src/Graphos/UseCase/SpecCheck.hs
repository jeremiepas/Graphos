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
  , runSpecCheck'
  , reportGates
  , renderSpecReport
  , filterReport
  , spofDecisions
  , CheckName(..)
  , parseCheckName
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.SpecCheck
import Graphos.Domain.Types
-- | The check catalogue; @--check NAME@ restricts the run to these.
data CheckName
  = Cycles
  | Coverage
  | Candidates
  | Stale
  | Duplicates
  | Spof
  deriving (Eq, Show)

parseCheckName :: Text -> Maybe CheckName
parseCheckName t = case T.toLower t of
  "cycles"      -> Just Cycles
  "coverage"    -> Just Coverage
  "candidates"  -> Just Candidates
  "stale"       -> Just Stale
  "duplicates"  -> Just Duplicates
  "spof"        -> Just Spof
  _             -> Nothing

data SpecReport = SpecReport
  { srSpecNodes      :: !Int
  , srSpecEdges      :: !Int
  , srTopoCertificate :: !(Maybe [NodeId]) -- ^ present iff acyclic (re-checked)
  , srCycleWitness   :: !(Maybe [NodeId])  -- ^ present iff cyclic (re-checked)
  , srCandidates     :: ![(NodeId, NodeId)]
  , srStale          :: ![(NodeId, NodeId)]
  , srUnimplemented  :: ![NodeId]
  , srStrictCoverage :: !Bool
  , srDuplicates     :: ![(NodeId, NodeId, Double)]
  , srStrictDuplicates :: !Bool
  , srSpof           :: ![NodeId]
  , srAdjudications  :: ![( (NodeId, NodeId), Verdict, Text )]
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
    , "duplication_candidates"
        .= map (\(a, b, s) -> object [ "a" .= a, "b" .= b, "similarity" .= s ])
               (srDuplicates r)
    , "spof_decisions" .= srSpof r
    , "adjudications"
        .= [ object [ "pair" .= object [ "r1" .= fst' p, "r2" .= snd' p ]
                    , "verdict" .= verdictToText v
                    , "rationale" .= rat
                    ]
           | (p, v, rat) <- srAdjudications r
           ]
    , "gating" .= reportGates r
    ]
    where
      fst' (a, _) = a
      snd' (_, b) = b
      verdictToText = \case
        VerdictConflict      -> "conflict" :: Text
        VerdictCompatible    -> "compatible"
        VerdictDuplicate     -> "duplicate"
        VerdictUnadjudicated -> "unadjudicated"

-- | Run the full deterministic catalogue over spec-artifact nodes.
runSpecCheck :: [Node] -> [Edge] -> Bool -> SpecReport
runSpecCheck ns es strictCov = runSpecCheck' ns es strictCov False [] [] Nothing

-- | Full-featured entry: threshold-based duplication candidates and SPOF
-- decisions, plus optional pre-adjudicated verdicts for the gate.
runSpecCheck' :: [Node] -> [Edge] -> Bool -> Bool
              -> [(NodeId, NodeId, Double)] -> [NodeId]
              -> Maybe [((NodeId, NodeId), Verdict, Text)]
              -> SpecReport
runSpecCheck' ns es strictCov strictDup dups spofs madj =
  let specIds =
        [ nodeId n
        | n <- ns
        , isRequirementNode n || isDecisionNode n
        ]
      certificate = checkAcyclic specIds es specRels
      witness = case certificate of
        Just _  -> Nothing
        Nothing -> findCycleWitness specIds es specRels
      verdictOf p = case [ (v, r) | ((a, b), v, r) <- fromMaybe [] madj
                        , a == fst p, b == snd p ] of
        ((v, _) : _) -> v
        []           -> VerdictUnadjudicated
  in SpecReport
       { srSpecNodes = length specIds
       , srSpecEdges = length [ e | e <- es, edgeRelation e `elem` specRels ]
       , srTopoCertificate = certificate
       , srCycleWitness = witness
       , srCandidates = candidatePairs ns es
       , srStale = staleSupersessions ns es
       , srUnimplemented = unimplementedRequirements ns es
       , srStrictCoverage = strictCov
       , srDuplicates = dups
       , srStrictDuplicates = strictDup
       , srSpof = spofs
        , srAdjudications =
            [ (p, verdictOf p, r)
            | p <- candidatePairs ns es
            , Just r' <- [ lookupR p ]
            , let r = r'
            , Just v' <- [ lookupV p ]
            , let _ = v'
            ]
       }
  where
    lookupR p = case [ r | ((a, b), _, r) <- fromMaybe [] madj, a == fst p, b == snd p ] of
      (r : _) -> Just r
      []      -> Nothing
    lookupV p = case [ v | ((a, b), v, _) <- fromMaybe [] madj, a == fst p, b == snd p ] of
      (v : _) -> Just v
      []      -> Nothing

-- | Exit gate: cycles always; coverage under strict only; confirmed conflicts
-- (via adjudication) block; duplicates under strict only; candidates,
-- stale references, SPOF never (they are advisory or adjudication input).
reportGates :: SpecReport -> Bool
reportGates r =
  maybe False (const True) (srCycleWitness r)
    || (srStrictCoverage r && not (null (srUnimplemented r)))
    || not (null confirmed)
    || (srStrictDuplicates r && not (null (srDuplicates r)))
  where
    verdictOfR p =
      case [ v | ((a, b), v, _) <- srAdjudications r, a == fst p, b == snd p ] of
        (v : _) -> v
        []      -> VerdictUnadjudicated
    confirmed = gate verdictOfR (srCandidates r)

-- | Restrict a report to the selected checks. An empty selection runs all
-- (spec: @--check NAME@ is optional and repeatable). Pure.
filterReport :: [CheckName] -> SpecReport -> SpecReport
filterReport checks r
  | null checks = r
  | otherwise = r
      { srCycleWitness    = if Cycles `elem` checks then srCycleWitness r else Nothing
      , srTopoCertificate = if Cycles `elem` checks then srTopoCertificate r else Nothing
      , srCandidates      = if Candidates `elem` checks then srCandidates r else []
      , srStale           = if Stale `elem` checks then srStale r else []
      , srUnimplemented   = if Coverage `elem` checks then srUnimplemented r else []
      , srDuplicates      = if Duplicates `elem` checks then srDuplicates r else []
      , srSpof            = if Spof `elem` checks then srSpof r else []
      , srAdjudications   = if Candidates `elem` checks then srAdjudications r else []
      }

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
  ++ sectionDuplicates
  ++ sectionSpof
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
      (if null (srCandidates r)
         then [ "None." ]
         else map candidateLine (srCandidates r))
      ++ concat
           [ [ "  - verdict: " <> verdictText v <> " — " <> rat
             | ((a', b'), v, rat) <- srAdjudications r
             , a' == a, b' == b ]
           | (a, b) <- srCandidates r
           ]
      where
        candidateLine (a, b) = "- " <> a <> " ⇄ " <> b
        verdictText = \v -> case v of
          VerdictConflict      -> "conflict"
          VerdictCompatible    -> "compatible"
          VerdictDuplicate     -> "duplicate"
          VerdictUnadjudicated -> "unadjudicated"

    sectionStale =
      "" : "## Stale supersessions" : "" :
      if null (srStale r)
        then [ "None." ]
        else map (\(s, d) -> "- " <> s <> " still references superseded " <> d) (srStale r)

    sectionDuplicates =
      "" : ("## Duplication candidates ("
             <> (if srStrictDuplicates r then "gating" else "advisory") <> ")") : "" :
      if null (srDuplicates r)
        then [ "None." ]
        else map (\(a, b, s) -> "- " <> a <> " ≈ " <> b <> " (similarity " <> tshow s <> ")")
                 (srDuplicates r)

    sectionSpof =
      "" : "## Single-point-of-failure decisions (advisory)" : "" :
      if null (srSpof r)
        then [ "None." ]
        else map ("- " <>) (srSpof r)

    sectionCoverage =
      "" : ("## Unimplemented requirements ("
             <> (if srStrictCoverage r then "gating" else "warning")
             <> ")") : "" :
      if null (srUnimplemented r)
        then [ "None." ]
        else map ("- " <>) (srUnimplemented r)

    verdictLine =
      if reportGates r then "VERDICT: FAIL" else "VERDICT: PASS"
