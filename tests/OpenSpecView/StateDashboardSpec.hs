module OpenSpecView.StateDashboardSpec (spec) where

import Test.Hspec

import OpenSpecView.ChangeList (ChangeInfo(..))
import OpenSpecView.StateDashboard

mk :: String -> String -> Int -> Int -> ChangeInfo
mk name status done total =
  ChangeInfo { changeName = name
             , changeStatus = status
             , changeCompleted = done
             , changeTotal = total
             , changeLastModified = Nothing }

spec :: Spec
spec = do
  describe "computeStats aggregate counts" $ do
    it "counts total changes and breaks them down by status" $ do
      let stats = computeStats
            [ mk "a" "complete" 2 2
            , mk "b" "in-progress" 1 3
            , mk "c" "no-tasks" 0 0
            ]
      (dsTotalChanges stats) `shouldBe` 3
      (dsComplete stats) `shouldBe` 1
      (dsInProgress stats) `shouldBe` 1
      (dsNoTasks stats) `shouldBe` 1

    it "totals completed vs total tasks across all changes" $ do
      let stats = computeStats
            [ mk "a" "complete" 2 2
            , mk "b" "in-progress" 1 3
            , mk "c" "no-tasks" 0 0
            ]
      (dsCompletedTasks stats) `shouldBe` 3
      (dsTotalTasks stats) `shouldBe` 5

    it "handles empty input without producing spurious entries" $ do
      let stats = computeStats []
      (dsTotalChanges stats) `shouldBe` 0
      (dsCompletedTasks stats) `shouldBe` 0
      (dsTotalTasks stats) `shouldBe` 0
      (dsProgress stats) `shouldBe` []
      (dsRisks stats) `shouldBe` []

  describe "computeStats per-change progress" $ do
    it "computes completion percentage per change, sorted by name" $ do
      let stats = computeStats
            [ mk "zeta" "in-progress" 1 3
            , mk "alpha" "complete" 2 2
            ]
      let names = map cpName (dsProgress stats)
          pct = map cpPct (dsProgress stats)
      names `shouldBe` ["alpha", "zeta"]
      pct `shouldBe` [100.0, 33.33333333333333]

  describe "isStalled" $ do
    it "flags in-progress changes with zero completed tasks" $ do
      isStalled (mk "stalled" "in-progress" 0 4) `shouldBe` True

    it "does not flag a complete or no-tasks change" $ do
      isStalled (mk "done" "complete" 0 0) `shouldBe` False
      isStalled (mk "notasks" "no-tasks" 0 0) `shouldBe` False

  describe "isAtRisk" $ do
    it "flags stalled in-progress changes" $ do
      isAtRisk (mk "stalled" "in-progress" 0 4) `shouldBe` True

    it "flags in-progress changes far below the completion threshold" $ do
      isAtRisk (mk "behind" "in-progress" 1 20) `shouldBe` True

    it "does not flag an on-track in-progress change" $ do
      isAtRisk (mk "ontrack" "in-progress" 5 6) `shouldBe` False

    it "does not flag complete changes" $ do
      isAtRisk (mk "done" "complete" 3 3) `shouldBe` False

  describe "renderDashboard" $ do
    it "emits the structural blocks" $ do
      let out = renderDashboard [ mk "a" "complete" 1 1 ]
      out `shouldContain` "OpenSpec State Dashboard"
      out `shouldContain` "Summary"
      out `shouldContain` "Per-change progress"
      out `shouldContain` "Risks"

    it "reports the aggregate counts in the summary block" $ do
      let out = renderDashboard
            [ mk "a" "complete" 2 2
            , mk "b" "in-progress" 1 3
            , mk "c" "no-tasks" 0 0
            ]
      out `shouldContain` "Total changes: 3"
      out `shouldContain` "Complete: 1"
      out `shouldContain` "In progress: 1"
      out `shouldContain` "No tasks: 1"
      out `shouldContain` "Tasks: 3 / 5"

    it "renders per-change completion percentages" $ do
      let out = renderDashboard
            [ mk "alpha" "complete" 2 2
            , mk "beta" "in-progress" 0 4
            ]
      out `shouldContain` "100.0%"
      out `shouldContain` "0.0%"

    it "marks stalled changes with a risk marker" $ do
      let out = renderDashboard [ mk "stalled" "in-progress" 0 4 ]
      out `shouldContain` "[!]"

    it "labels the risk reason for stalled changes" $ do
      let out = renderDashboard [ mk "stalled" "in-progress" 0 4 ]
      out `shouldContain` "(stalled)"

    it "reports no risks when there are none" $ do
      let out = renderDashboard [ mk "done" "complete" 1 1 ]
      out `shouldContain` "No risks identified."

  describe "renderDashboard empty input" $ do
    it "produces an empty summary and no risks block contents" $ do
      let out = renderDashboard []
      out `shouldContain` "Total changes: 0"
      out `shouldContain` "Tasks: 0 / 0"
      out `shouldContain` "No risks identified."

  describe "showDashboard" $ do
    it "reads the openspec/changes root and renders the dashboard" $ do
      out <- showDashboard "openspec/changes"
      out `shouldContain` "OpenSpec State Dashboard"
      out `shouldContain` "Summary"
