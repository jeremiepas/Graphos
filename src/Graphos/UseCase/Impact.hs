-- | Change-impact obligation orchestration: run the Domain impact analysis,
-- assemble the per-node obligation discharge rows, render the report
-- (change-impact-obligations capability). Pure — no IO.
--
-- Gate discipline: the change is admissible iff every frontier obligation is
-- discharged (by an ack or by a registered rule). The full CI gate also folds in
-- the intent-spec verdicts (see runImpactGate).
module Graphos.UseCase.Impact
  ( Obligation(..)
  , ImpactReport(..)
  , runImpact
  , reportAdmissible
  , renderImpactReport
  , runImpactGate
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T

import Graphos.Domain.Graph.Core (Graph)
import Graphos.Domain.Impact
import Graphos.Domain.IntentCheck (SpecDocument)

-- | One frontier obligation row.
data Obligation = Obligation
  { obNode       :: !Text
  , obDischarged :: !Bool
  , obAcked      :: !Bool
  , obReason     :: !Text  -- "ack" | "stableSignature" | "open"
  } deriving (Eq, Show)

instance ToJSON Obligation where
  toJSON o = object
    [ "node"       .= obNode o
    , "discharged" .= obDischarged o
    , "acked"      .= obAcked o
    , "reason"     .= obReason o
    ]

-- | The change-impact report for one before/after pair.
data ImpactReport = ImpactReport
  { irTouched     :: ![Text]
  , irFrontier    :: ![Text]
  , irObligations :: ![Obligation]
  , irAcks        :: ![Text]
  } deriving (Eq, Show)

instance ToJSON ImpactReport where
  toJSON r = object
    [ "touched"     .= irTouched r
    , "frontier"    .= irFrontier r
    , "obligations" .= map toJSON (irObligations r)
    , "acks"        .= irAcks r
    ]

-- | Build the report for a before/after pair with the given discharges.
runImpact :: Graph -> Graph -> Discharge -> ImpactReport
runImpact b a d =
   let ch  = Change b a
       fr  = frontier b (touched ch)
       rows = [ mkOb v ch d | v <- fr ]
  in ImpactReport
        { irTouched     = touched ch
        , irFrontier    = fr
        , irObligations = rows
        , irAcks        = acks d
        }
  where
    mkOb v c dc =
      let acked   = v `elem` acks dc
          ruleDis = not acked && any (\r -> r c v) (rules dc)
          reason  = if acked then "ack"
                    else if ruleDis then "stableSignature"
                    else "open"
      in Obligation { obNode = v, obDischarged = acked || ruleDis, obAcked = acked, obReason = reason }

-- | The change is admissible iff every frontier obligation is discharged.
reportAdmissible :: ImpactReport -> Bool
reportAdmissible r = all obDischarged (irObligations r)

-- | Markdown report.
renderImpactReport :: ImpactReport -> Text
renderImpactReport r = T.unlines $
  [ "# impact report"
  , ""
  , "- touched: " <> tshow (length (irTouched r))
  , "- frontier: " <> tshow (length (irFrontier r))
  , "- admissible: " <> (if reportAdmissible r then "yes" else "no")
  , ""
  ]
  ++ [ "- " <> status x <> " " <> obNode x
       | x <- irObligations r ]
  ++ [ ""
     , if null (irObligations r)
         then "No open obligations."
          else T.unlines ("**OPEN OBLIGATIONS:**" : [ "- " <> obNode x | x <- irObligations r, not (obDischarged x) ])
     ]
  ++ [ ""
     , "- acks (" <> tshow (length (irAcks r)) <> "): "
        <> (if null (irAcks r) then "(none)" else T.intercalate ", " (irAcks r))
     ]
  where
    tshow = T.pack . show
    status x = if obDischarged x then "ok" else "**OPEN**"

-- | The full gate: admissibility (obligations) ∧ the designated new spec holds
-- on the after projection ∧ no other validated spec regresses. Returns the
-- admissibility verdict and the named failed conjuncts, plus the impact report.
-- With no spec document supplied, the gate checks impact completeness only.
runImpactGate :: Graph -> Graph -> Discharge -> Maybe SpecDocument -> Maybe Text
              -> (Bool, [FailedConjunction], ImpactReport)
runImpactGate b a d mDoc newSpec =
   let ch = Change b a
       rep = runImpact b a d
       failures = case mDoc of
         Just doc -> snd (assess ch d doc newSpec)
         Nothing  -> if reportAdmissible rep then [] else [Uncomplete (openObligations ch d)]
       gateOk = null failures
   in (gateOk, failures, rep)
