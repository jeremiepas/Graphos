-- | State-dashboard capability for the @openspec-view@ workspace inspector.
--
-- Aggregates the change list into one view: overall statistics, a row per
-- change (status, task progress, artifact completeness, recency) and a risk
-- section calling out changes that are stuck or stale. The core helpers are
-- pure over 'ChangeRow' / 'AggregateStats' so they stay deterministic and unit
-- tested; 'collectDashboard' is the only filesystem-touching entry point.
module Graphos.OpenSpecView.Dashboard
  ( Dashboard (..)
  , ChangeRow (..)
  , AggregateStats (..)
  , DashboardConfig (..)
  , rowFor
  , aggregateStats
  , detectRisks
  , renderDashboard
  , collectDashboard
  ) where

import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Prelude
import Graphos.OpenSpecView.Types (ChangeStatus (..), ChangeInfo (..))
import Graphos.OpenSpecView.List (parseChanges)
import Graphos.OpenSpecView.ArtifactView (Artifact (..), ArtifactStatus (..), parseArtifacts)

-- | How a single change reads on the dashboard. Pure and value-equalable so it
-- can be asserted directly in tests without touching the filesystem.
data ChangeRow = ChangeRow
  { rowName :: Text
  , rowStatus :: ChangeStatus
  , rowDone :: Int
  , rowTotal :: Int
  , rowSpecs :: Int
  , rowArtifactsDone :: Int
  , rowArtifactsPending :: Int
  , rowHasProposal :: Bool
  , rowAgeDays :: Double
  } deriving (Show, Eq)

-- | Aggregate numbers across every change in the workspace.
data AggregateStats = AggregateStats
  { aTotal :: Int
  , aInProgress :: Int
  , aComplete :: Int
  , aNoTasks :: Int
  , aTotalSpecs :: Int
  , aTotalTasks :: Int
  , aCompletedTasks :: Int
  , aFullyComplete :: Int
  , aArtifactPending :: Int
  } deriving (Show, Eq)

-- | Full dashboard: aggregates, per-change rows and the collected risks.
data Dashboard = Dashboard
  { dStats :: AggregateStats
  , dRows :: [ChangeRow]
  , dRisks :: [Text]
  } deriving (Show, Eq)

-- | Configuration for a dashboard build. Kept as a record rather than bare
-- arguments so the pure core stays readable and the caller controls the clock.
data DashboardConfig = DashboardConfig
  { cfgStaleDays :: Int
  , cfgNow :: UTCTime
  } deriving (Show)

-- | Build one 'ChangeRow' from a parsed change, its spec count and its
-- artifacts. Recency is measured against the config clock so tests are stable.
rowFor :: DashboardConfig -> ChangeInfo -> Int -> [Artifact] -> ChangeRow
rowFor cfg c specCount artifacts =
  ChangeRow
    (changeName c)
    (changeStatus c)
    (completedTasks c)
    (totalTasks c)
    specCount
    (length (filter ((== ArtDone) . artStatus) artifacts))
    (length (filter ((== ArtPending) . artStatus) artifacts))
    (any ((== "proposal") . artName) artifacts)
    (ageDays cfg (lastModified c))

-- | Age of a change in days relative to the config clock.
ageDays :: DashboardConfig -> UTCTime -> Double
ageDays cfg t = realToFrac (diffUTCTime (cfgNow cfg) t) / 86400.0

-- | Collapse every row into the workspace-wide 'AggregateStats'.
aggregateStats :: [ChangeRow] -> AggregateStats
aggregateStats rows =
  AggregateStats
    (length rows)
    (length (filter ((== InProgress) . rowStatus) rows))
    (length (filter ((== Complete) . rowStatus) rows))
    (length (filter ((== NoTasks) . rowStatus) rows))
    (sum (map rowSpecs rows))
    (sum (map rowTotal rows))
     (sum (map rowDone rows))
     (length (filter isFullyComplete rows))
     (length (filter hasPendingArtifacts rows))
   where
     isFullyComplete r = rowHasProposal r && rowArtifactsPending r == 0
     hasPendingArtifacts r = rowArtifactsPending r > 0

-- | Risks for a single change. Order is stable and deterministic: missing
-- required artifacts first, then logical conflicts, then staleness.
detectRisks :: DashboardConfig -> ChangeRow -> [Text]
detectRisks cfg r =
  [ "missing required proposal.md" | not (rowHasProposal r) ]
  ++ [ "artifacts present but no tasks declared"
       | rowArtifactsDone r > 0 || rowSpecs r > 0, rowTotal r == 0 ]
  ++ [ "tasks declared but no artifacts created"
       | rowTotal r > 0 && rowArtifactsDone r == 0 && rowSpecs r == 0 ]
  ++ [ "stale: last modified " <> humanAge (rowAgeDays r) <> " ago" | isStale cfg r ]

isStale :: DashboardConfig -> ChangeRow -> Bool
isStale cfg r = rowAgeDays r >= fromIntegral (cfgStaleDays cfg)

-- | Human-readable age label for the stale risk.
humanAge :: Double -> Text
humanAge d
  | d < 1 = "just now"
  | otherwise = T.pack (show (floor d :: Int)) <> " days"

-- | Render the whole dashboard as text.
renderDashboard :: Dashboard -> Text
renderDashboard d =
  T.concat [ header, summaryBlock d, changesBlock (dRows d), riskBlock (dRisks d) ]

header :: Text
header = "OpenSpec State Dashboard\n========================\n\n"

summaryBlock :: Dashboard -> Text
summaryBlock d =
  T.concat [ "Summary\n"
           , "  Changes:            "
             <> statsWithTotals (dStats d)
           , "\n  Specs:              "
             <> T.pack (show (aTotalSpecs (dStats d)))
           , "\n  Tasks:              "
             <> T.pack (show (aTotalTasks (dStats d)))
             <> " total, "
             <> T.pack (show (aCompletedTasks (dStats d)))
             <> " completed"
    , "\n  Fully complete:     "
      <> T.pack (show (aFullyComplete (dStats d)))
      <> " (artifacts pending in "
      <> T.pack (show (aArtifactPending (dStats d)))
      <> ")"
    , "\n\n" ]
  where
    statsWithTotals s =
      T.pack (show (aTotal s))
        <> " (in-progress: "
        <> T.pack (show (aInProgress s))
        <> ", complete: "
        <> T.pack (show (aComplete s))
        <> ", no-tasks: "
        <> T.pack (show (aNoTasks s))
        <> ")"

changesBlock :: [ChangeRow] -> Text
changesBlock rows
  | null rows = "Changes\n  (no changes)\n\n"
  | otherwise =
      T.concat [ "Changes\n"
               , mconcat [ renderRow r <> "\n" | r <- rows ]
               , "\n" ]

renderRow :: ChangeRow -> Text
renderRow r =
  "  "
  <> rowName r
  <> "  "
  <> statusLabel (rowStatus r)
  <> "  ["
  <> T.pack (show (rowDone r))
  <> "/"
  <> T.pack (show (rowTotal r))
  <> " tasks]  ["
  <> T.pack (show (rowArtifactsDone r))
  <> "/"
  <> T.pack (show (rowArtifactsPending r))
  <> " artifacts]  "
  <> humanAge (rowAgeDays r)
  <> " ago"

riskBlock :: [Text] -> Text
riskBlock risks
  | null risks = "Risks\n  No risks identified\n"
  | otherwise =
      T.concat [ "Risks\n"
               , mconcat [ "  - " <> r <> "\n" | r <- risks ]
               ]

-- | Gather the change list plus per-change spec counts and artifacts, then
-- assemble a 'Dashboard'. This is the sole filesystem entry point.
collectDashboard :: FilePath -> DashboardConfig -> IO Dashboard
collectDashboard root cfg = do
  cis <- parseChanges root
  rows <- mapM (\c -> do
      let dir = root </> T.unpack (changeName c)
      specCount <- specCountFor dir
      artifacts <- parseArtifacts dir
      pure (rowFor cfg c specCount artifacts)
    ) cis
  let stats = aggregateStats rows
      risks = concatMap (detectRisks cfg) rows
  pure (Dashboard stats rows risks)

-- | Count @spec.md@ files under a change's @specs/@ directory. Each capability
-- lives in its own subdirectory (@specs/<capability>/spec.md@), so a spec
-- counts only when that file actually exists on disk.
specCountFor :: FilePath -> IO Int
specCountFor dir = do
  let specsDir = dir </> "specs"
  specs <- listOrNone specsDir
  let specDirs = filter isSpecDir specs
  counts <- mapM (\sd -> do
        hasSpec <- doesFileExist (specsDir </> sd </> "spec.md")
        pure (if hasSpec then 1 else 0)
      ) specDirs
  pure (sum counts)
  where
    isSpecDir s = s /= "." && s /= ".." && not (T.isSuffixOf ".md" (T.pack s))
    listOrNone p = doesDirectoryExist p >>= \b -> if b then listDirectory p else pure []

-- | Status label reused for the dashboard rows.
statusLabel :: ChangeStatus -> Text
statusLabel s = case s of
  InProgress -> "in-progress"
  Complete -> "complete"
  NoTasks -> "no-tasks"
