{-# LANGUAGE OverloadedStrings #-}
-- | Unit tests for 'Graphos.OpenSpecView.Dashboard'.
module Graphos.OpenSpecView.DashboardSpec where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup ((<>))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Prelude
  ( FilePath, IO, Int, Integer, Bool(..), String
  , pure, map, filter, length, sum, not, (++), (==), (>), (<), (.), ($), show, null, read, otherwise )

import Graphos.OpenSpecView.Dashboard
import Graphos.OpenSpecView.Types
import Graphos.OpenSpecView.ArtifactView

mkTime :: Integer -> Int -> Int -> UTCTime
mkTime y m d =
  read (show y <> "-" <> pad2 m <> "-" <> pad2 d <> " 00:00:00") :: UTCTime
  where
    pad2 n | n < 10 = "0" ++ show n
           | otherwise = show n

cfg :: DashboardConfig
cfg = DashboardConfig { cfgStaleDays = 30, cfgNow = mkTime 2024 6 1 }

mk :: Text -> ChangeStatus -> Int -> Int -> UTCTime -> ChangeInfo
mk n st d tot t =
  ChangeInfo { changeName = n, changeStatus = st, completedTasks = d, totalTasks = tot, lastModified = t }

done :: Text -> Artifact
done n = Artifact { artName = n, artStatus = ArtDone }

pend :: Text -> Artifact
pend n = Artifact { artName = n, artStatus = ArtPending }

spec :: Spec
spec = do
  describe "rowFor" $ do
    it "counts tasks, artifacts and the proposal flag from parsed inputs" $
      let c = mk "alpha" InProgress 1 2 (mkTime 2024 5 30)
          r = rowFor cfg c 1 [done "proposal", done "node-schema"]
      in r `shouldBe` ChangeRow "alpha" InProgress 1 2 1 2 0 True 2.0

    it "reports pending spec artifacts separately from done ones" $
      let c = mk "beta" Complete 0 0 (mkTime 2024 6 1)
          r = rowFor cfg c 2 [done "proposal", pend "design"]
      in (rowArtifactsPending r, rowSpecs r) `shouldBe` (1, 2)

  describe "aggregateStats" $ do
    it "totals status counts, specs and task progress" $
      let t = mkTime 2024 6 1
          rows = [ rowFor cfg (mk "a" InProgress 1 2 t) 1 [done "proposal", done "node-schema"]
                 , rowFor cfg (mk "b" Complete 2 2 t) 1 [done "proposal"]
                 , rowFor cfg (mk "c" NoTasks 0 0 t) 0 [] ]
          s = aggregateStats rows
       in s `shouldBe` AggregateStats 3 1 1 1 2 4 3 2 0

    it "counts fully-complete changes only when proposal exists and no spec is pending" $
      let rows = [ ChangeRow "x" Complete 0 0 0 1 0 True 0
                 , ChangeRow "y" InProgress 0 0 1 0 1 True 0 ]
       in aFullyComplete (aggregateStats rows) `shouldBe` 1

    it "counts changes with at least one pending artifact" $
       let rows = [ ChangeRow "x" InProgress 0 0 0 1 1 True 0
                 , ChangeRow "y" Complete 0 0 1 1 0 True 0 ]
       in aArtifactPending (aggregateStats rows) `shouldBe` 1

  describe "detectRisks" $ do
    it "flags a change missing the required proposal" $
      detectRisks cfg (ChangeRow "x" InProgress 0 0 0 0 0 False 0)
        `shouldBe` ["missing required proposal.md"]

    it "flags artifacts present with no declared tasks" $
      detectRisks cfg (ChangeRow "x" NoTasks 0 0 0 1 0 True 0)
        `shouldBe` ["artifacts present but no tasks declared"]

    it "flags declared tasks with no artifacts created" $
      detectRisks cfg (ChangeRow "x" InProgress 0 3 0 0 0 True 0)
        `shouldBe` ["tasks declared but no artifacts created"]

    it "flags changes older than the staleness threshold" $
      detectRisks cfg (ChangeRow "x" Complete 2 2 1 1 0 True 35)
        `shouldBe` ["stale: last modified 35 days ago"]

    it "reports no risks for a healthy change" $
      detectRisks cfg (ChangeRow "x" Complete 2 2 1 1 0 True 3)
        `shouldBe` []

  describe "renderDashboard" $ do
    it "includes the header and summary counts" $
      let out = renderDashboard (Dashboard (aggregateStats []) [] [])
      in ("OpenSpec State Dashboard" `T.isInfixOf` out) `shouldBe` True

    it "falls back to a clean risk message when there are no risks" $
      let out = renderDashboard (Dashboard (aggregateStats []) [] [])
      in ("No risks identified" `T.isInfixOf` out) `shouldBe` True

    it "lists risks with a bullet prefix" $
      let out = renderDashboard (Dashboard (aggregateStats []) [] ["missing required proposal.md"])
      in ("  - missing required proposal.md" `T.isInfixOf` out) `shouldBe` True

  describe "collectDashboard" $ do
    it "builds a dashboard over the real change set" $ do
      now <- getCurrentTime
      d <- collectDashboard "openspec/changes" (DashboardConfig 30 now)
      (aTotal (dStats d) > 0) `shouldBe` True
      (T.isInfixOf "openspec-view" (renderDashboard d)) `shouldBe` True
      not (null (dRisks d)) `shouldBe` True
