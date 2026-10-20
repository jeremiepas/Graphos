-- OpenSpecView.StateDashboard
--
-- Turns a collection of 'ChangeInfo' records into a readable dashboard:
-- aggregate statistics, per-change progress, and risk highlighting. All
-- aggregation logic is pure and operates on '[ChangeInfo]', so it is trivially
-- unit-testable without touching the filesystem.
module OpenSpecView.StateDashboard
  ( ChangeBucket
  , DashboardMetric
  , ChangeProgress(..)
  , DashboardStats(..)
  , computeStats
  , renderDashboard
  , showDashboard
  , isStalled
  , isAtRisk
  ) where

import Data.Char (chr, ord)
import Data.List (sortBy)
import Data.Ord (comparing)

import OpenSpecView.ChangeList
  ( ChangeInfo(..)
  , ChangeName
  , TaskCount
  , TaskStatus
  , readAllChangeInfos
  )

-- | A coarse bucket label used when grouping changes by status. Kept for
-- backwards compatibility with the module's public surface.
type ChangeBucket = String

-- | A named metric value surfaced in the dashboard summary block.
type DashboardMetric = String

-- | Completion threshold (in percent) below which an in-progress change is
-- considered far behind and therefore at risk.
atRiskThreshold :: Double
atRiskThreshold = 10.0

-- | Per-change progress slice, including whether the change is flagged at risk.
data ChangeProgress = ChangeProgress
  { cpName     :: ChangeName
  , cpCompleted :: TaskCount
  , cpTotal     :: TaskCount
  , cpPct      :: Double
  , cpStatus   :: TaskStatus
  , cpAtRisk   :: Bool
  } deriving (Eq, Show)

-- | Aggregate statistics over a collection of changes.
data DashboardStats = DashboardStats
  { dsTotalChanges   :: Int
  , dsNoTasks        :: Int
  , dsInProgress     :: Int
  , dsComplete       :: Int
  , dsCompletedTasks :: TaskCount
  , dsTotalTasks     :: TaskCount
  , dsProgress       :: [ChangeProgress]
  , dsRisks          :: [ChangeProgress]
  } deriving (Eq, Show)

-- | Percentage of a change's tasks completed, in the range [0, 100].
completionPct :: ChangeInfo -> Double
completionPct ci =
  let total = changeTotal ci
      done = changeCompleted ci
  in if total <= 0 then 0.0
     else fromIntegral done / fromIntegral total * 100.0

-- | A stalled change: it is still marked in-progress but has completed no tasks.
isStalled :: ChangeInfo -> Bool
isStalled ci = changeStatus ci == "in-progress" && changeCompleted ci == 0

-- | A change is at risk when it is stalled, or when an in-progress change has
-- completed far below the expected threshold.
isAtRisk :: ChangeInfo -> Bool
isAtRisk ci = isStalled ci
  || (changeStatus ci == "in-progress" && completionPct ci < atRiskThreshold)

toProgress :: ChangeInfo -> ChangeProgress
toProgress ci =
  ChangeProgress
    { cpName = changeName ci
    , cpCompleted = changeCompleted ci
    , cpTotal = changeTotal ci
    , cpPct = completionPct ci
    , cpStatus = changeStatus ci
    , cpAtRisk = isAtRisk ci
    }

-- | Compute aggregate statistics over a collection of changes. Progress rows
-- are sorted by name so the output is stable regardless of input ordering.
computeStats :: [ChangeInfo] -> DashboardStats
computeStats infos =
  let ordered = sortBy (comparing changeName) infos
      byStatus s = length [() | i <- infos, changeStatus i == s]
      progress = map toProgress ordered
      risks = [p | p <- progress, cpAtRisk p]
  in DashboardStats
        { dsTotalChanges = length infos
        , dsNoTasks = byStatus "no-tasks"
        , dsInProgress = byStatus "in-progress"
        , dsComplete = byStatus "complete"
        , dsCompletedTasks = sum [changeCompleted i | i <- infos]
        , dsTotalTasks = sum [changeTotal i | i <- infos]
        , dsProgress = progress
        , dsRisks = risks
        }

-- | One-decimal-place percentage string, e.g. "66.7%".
fmtPct :: Double -> String
fmtPct x =
  let scaled = round (x * 10) :: Integer
      intPart = scaled `div` 10
      fracPart = scaled `mod` 10
  in show intPart ++ "." ++ [chr (ord '0' + fromIntegral fracPart)] ++ "%"

rpad :: Int -> String -> String
rpad n s =
  let l = length s
  in if l >= n then s else replicate (n - l) ' ' ++ s

summaryBlock :: DashboardStats -> [String]
summaryBlock s =
  [ "Summary"
  , "───────"
  , "Total changes: " ++ show (dsTotalChanges s)
  , "Complete: " ++ show (dsComplete s)
  , "In progress: " ++ show (dsInProgress s)
  , "No tasks: " ++ show (dsNoTasks s)
  , ""
  , "Tasks: " ++ show (dsCompletedTasks s)
        ++ " / " ++ show (dsTotalTasks s)
  , ""
  ]

rowLine :: ChangeProgress -> String
rowLine p =
  let marker = if cpAtRisk p then "[!]" else "  "
      name = rpad 12 (cpName p)
      tally = rpad 6 (show (cpCompleted p) ++ "/" ++ show (cpTotal p))
      pct = rpad 7 (fmtPct (cpPct p))
  in marker ++ " " ++ name ++ "  " ++ tally ++ "   "
        ++ pct ++ "  " ++ cpStatus p

riskReason :: ChangeProgress -> String
riskReason p = if cpCompleted p == 0 then "(stalled)" else "(behind)"

perChangeBlock :: DashboardStats -> [String]
perChangeBlock s =
  "Per-change progress"
  : "─────────────────────"
  : ""
  : map rowLine (dsProgress s)
  ++ [""]

riskBlock :: [ChangeProgress] -> [String]
riskBlock risks
  | null risks = [ "Risks", "─────", "", "No risks identified.", "" ]
  | otherwise =
      "Risks"
      : "─────"
      : ""
      : map (\p -> rowLine p ++ "  " ++ riskReason p) risks
      ++ [""]

-- | Render a complete, human-readable dashboard from a collection of changes.
renderDashboard :: [ChangeInfo] -> String
renderDashboard infos =
  let stats = computeStats infos
  in unlines $ concat
        [  [ "OpenSpec State Dashboard"
           , "========================"
           , ""
           ]
        , summaryBlock stats
        , perChangeBlock stats
        , riskBlock (dsRisks stats)
        ]

-- | Read every change under a changes root and render the dashboard.
showDashboard :: FilePath -> IO String
showDashboard root = do
  infos <- readAllChangeInfos root
  pure (renderDashboard infos)
