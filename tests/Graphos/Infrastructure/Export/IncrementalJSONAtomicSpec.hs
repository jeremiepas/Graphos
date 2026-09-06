-- | Tests for the atomic incremental JSON writer used for graph.json.
module Graphos.Infrastructure.Export.IncrementalJSONAtomicSpec (spec) where

import Test.Hspec
import Data.List (isSuffixOf, isInfixOf)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (listDirectory)
import System.FilePath ((</>))

import Graphos.Infrastructure.Export.IncrementalJSON
  (openWriter, closeWriter)

spec :: Spec
spec = do
  describe "IncrementalJSON atomic writer" $ do
    it "places a complete graph.json atomically with no leftover temp files" $
      withSystemTempDirectory "graphos-incr-success-spec" $ \dir -> do
        let target = dir </> "graph.json"
        iw <- openWriter target
        closeWriter iw
        contents <- readFile target
        contents `shouldStartWith` "{"
        contents `shouldSatisfy` \s -> "schema_version" `isInfixOf` s
        leftovers <- listDirectory dir
        leftovers
          `shouldSatisfy` \ls ->
            not (any (\f -> ".tmp" `isSuffixOf` f) ls)

    it "leaves the prior graph.json intact when a run is interrupted before close" $
      withSystemTempDirectory "graphos-incr-interrupt-spec" $ \dir -> do
        let target = dir </> "graph.json"
        writeFile target "{\"schema_version\":\"1.0.0\"}\n"
        _ <- openWriter target
        -- Simulate an interrupted run: open the writer (which streams into a
        -- same-directory temp) but never call closeWriter, so nothing is placed
        -- at the final path and the previously valid graph is untouched.
        original <- readFile target
        original `shouldBe` "{\"schema_version\":\"1.0.0\"}\n"
        original `shouldSatisfy` \s -> "schema_version" `isInfixOf` s
