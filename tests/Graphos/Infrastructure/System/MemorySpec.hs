-- | /proc/meminfo reader tests (fix-oom-memory-budget-guard, task 2.1).
module Graphos.Infrastructure.System.MemorySpec where

import Test.Hspec
import Data.Maybe (isJust)
import System.Directory (doesFileExist)

import Graphos.Domain.Config.Memory (MemInfo(..))
import Graphos.Infrastructure.System.Memory (readMemInfo, parseMemInfo)

fixture :: String
fixture = unlines
  [ "MemTotal:       65505708 kB"
  , "MemFree:         4211236 kB"
  , "MemAvailable:   20971520 kB"
  , "Buffers:          572060 kB"
  , "SomeFutureField:      42 kB"
  , "HugePages_Total:       0"
  ]

spec :: Spec
spec = do
  describe "parseMemInfo" $ do
    it "parses MemAvailable from fixture text (kB to bytes)" $
      parseMemInfo fixture `shouldBe` Just (MemInfo (20971520 * 1024))

    it "ignores unknown lines and still finds MemAvailable" $
      parseMemInfo ("Bogus: x\n" ++ fixture) `shouldBe` Just (MemInfo (20971520 * 1024))

    it "returns Nothing when MemAvailable is absent" $
      parseMemInfo "MemTotal: 65505708 kB\nMemFree: 4211236 kB\n" `shouldBe` Nothing

    it "returns Nothing on a malformed MemAvailable value" $
      parseMemInfo "MemAvailable: lots kB\n" `shouldBe` Nothing

    it "returns Nothing on empty input" $
      parseMemInfo "" `shouldBe` Nothing

  describe "readMemInfo" $
    it "returns Just on a machine with /proc/meminfo" $ do
      hasProc <- doesFileExist "/proc/meminfo"
      if hasProc
        then do
          mi <- readMemInfo
          mi `shouldSatisfy` isJust
          case mi of
            Just (MemInfo avail) -> avail `shouldSatisfy` (> 0)
            Nothing -> pure ()
        else pendingWith "/proc/meminfo not available on this platform"
