-- | Intent-spec check orchestration: run the Domain checker over a graph,
-- assemble the per-spec verdicts, render the report (spec-graph-checks
-- capability). Pure — no IO.
--
-- Gate discipline: a violated spec is a hard failure (non-zero exit naming the
-- spec and its witnessing nodes); all specs passing is success. Probe graphs
-- add non-triviality warnings only — they never gate (see CLI dispatch).
module Graphos.UseCase.IntentCheck
  ( -- * Report
    VerdictRow(..)
  , IntentReport(..)
  , runIntentCheck
  , reportVerdict
  , violatedSpecs
  , renderIntentReport
    -- * Probes (non-gating)
  , ProbeSummary(..)
  , runProbes
  ) where

import Data.Aeson (Value, ToJSON(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Graph.Core (Graph)
import Graphos.Domain.IntentCheck

-- | One spec verdict row in the report.
data VerdictRow = VerdictRow
  { vrName    :: !(Maybe Text)
  , vrSpec    :: !Spec
  , vrVerdict :: !Verdict
  } deriving (Eq, Show)

instance ToJSON VerdictRow where
  toJSON r = object
    [ "name"      .= vrName r
    , "spec"      .= vrSpec r
    , "verdict"   .= verdictText (vrVerdict r)
    , "witnesses" .= witnesses (vrVerdict r)
    ]

-- | The full intent-check report for one graph.
data IntentReport = IntentReport
  { irGraph :: !Text
  , irRows  :: ![VerdictRow]
  } deriving (Eq, Show)

instance ToJSON IntentReport where
  toJSON r = object
    [ "graph"   .= irGraph r
    , "results" .= map rowToJSON (irRows r)
    ]

-- | The report passes iff every spec holds on the graph.
reportVerdict :: IntentReport -> Bool
reportVerdict r = all holds (map vrVerdict (irRows r))

-- | The violated specs, named for the CLI exit message.
violatedSpecs :: IntentReport -> [(Maybe Text, Verdict)]
violatedSpecs r =
  [ (vrName x, vrVerdict x) | x <- irRows r, not (holds (vrVerdict x)) ]

-- | Run the checker over one graph, recording an optional per-spec name.
runIntentCheck :: Graph -> SpecDocument -> Text -> IntentReport
runIntentCheck g doc name =
  let rows = [ VerdictRow nm s (evaluateSpec g s) | (nm, s) <- docSpecs doc ]
  in IntentReport { irGraph = name, irRows = rows }

-- | A verdict text for the report.
verdictText :: Verdict -> Text
verdictText v = case v of
  Satisfied _ -> "pass"
  Violated _  -> "fail"

-- | One verdict row as a JSON object (shared by report and probe serializers).
rowToJSON :: VerdictRow -> Value
rowToJSON r = object
  [ "name"      .= vrName r
  , "spec"      .= vrSpec r
  , "verdict"   .= verdictText (vrVerdict r)
  , "witnesses" .= witnesses (vrVerdict r)
  ]

-- | Markdown report: one line per spec plus a summary and the violations.
renderIntentReport :: IntentReport -> Text
renderIntentReport r = T.unlines $
  [ "# intentcheck report"
  , ""
  , "- graph: " <> irGraph r
  , "- specs: " <> tshow (length (irRows r))
  , "- verdict: " <> (if reportVerdict r then "PASS" else "FAIL")
  , ""
  ]
  ++ [ "- " <> status x <> " " <> name x <> " " <> specText (vrSpec x)
       | x <- irRows r ]
  ++ [ ""
     , if null (violatedSpecs r)
         then "No violations."
          else T.unlines ("**VIOLATIONS:**" : [ "- " <> violatedLine x | x <- irRows r, not (holds (vrVerdict x)) ])
     ]
  where
    tshow = T.pack . show

    status x = if holds (vrVerdict x) then "pass" else "FAIL"

    name x = maybe "(unnamed)" ("'" <>) (vrName x)

    specText s = T.pack (show s)

    violatedLine x =
      let wit = witnesses (vrVerdict x)
          ws  = if null wit then "(no witness)" else T.intercalate ", " wit
      in "- " <> name x <> ": " <> specText (vrSpec x) <> " — violating nodes: " <> ws

-- | A probe graph verdict, for non-triviality comparison against the main graph.
data ProbeSummary = ProbeSummary
  { prGraph   :: !Text
  , prRows    :: ![VerdictRow]
  , prMatches :: !Bool  -- same pass/fail shape as the main report
  } deriving (Eq, Show)

instance ToJSON ProbeSummary where
  toJSON s = object
    [ "graph"   .= prGraph s
    , "results" .= map rowToJSON (prRows s)
    , "matches" .= prMatches s
    ]

-- | Run the checker over a list of probe graphs. `mainRows` is the main-graph
-- report rows; `matches` flags probes whose pass/fail shape differs (a warning,
-- never a failure).
runProbes :: Graph -> SpecDocument -> Text -> [VerdictRow] -> [Graph] -> [ProbeSummary]
runProbes _main doc p mainRows probes =
  map mkProbe probes
  where
    ok x = (holds (vrVerdict x), T.pack (show (vrSpec x)))

    mkProbe g' =
      let rows = [ VerdictRow nm s (evaluateSpec g' s) | (nm, s) <- docSpecs doc ]
      in ProbeSummary { prGraph = p, prRows = rows, prMatches = map ok rows == map ok mainRows }
