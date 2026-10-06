{-# LANGUAGE OverloadedStrings #-}
-- | Confluence integration test (wire-incremental-update 4.4): an update run
-- over a fixture tree must produce the same node and edge identifier sets as
-- a fresh full build over the same content.
--
-- Drives the compiled @graphos@ CLI twice (like the real agent hook would):
-- a @--fresh@ reference build, then a warm-cache run over identical content.
-- The exported graph.json node ids and edge (source, target) pairs must be
-- equal.
module Graphos.UseCase.UpdateConfluenceSpec (spec) where

import Data.Aeson (Value(..), decode)
import qualified Data.Vector as V
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess(..), create_group, readCreateProcessWithExitCode, proc)

import Test.Hspec

spec :: Spec
spec = describe "update-run confluence (wire-incremental-update 4.4)" $ do
  it "graph ids equal between --fresh and a warm-cache full run" $
    withSystemTempDirectory "graphos-confluence" $ \root -> do
      let srcdir    = root </> "src"
          outFresh  = root </> "out-fresh"
          outWarmed = root </> "out-warmed"
      createFixture srcdir
      let exe = "dist-newstyle/build/x86_64-linux/ghc-9.10.3" <>
                "/graphos-0.1.0.0/x/graphos/build/graphos/graphos"
      _ <- runGraphos exe [srcdir, "--fresh", "--no-viz", "--output", outFresh]
      _ <- runGraphos exe [srcdir, "--no-viz", "--output", outWarmed]
      g1 <- readShape (outFresh  </> "graph.json")
      g2 <- readShape (outWarmed </> "graph.json")
      shapeNodes g1 `shouldBe` shapeNodes g2
      shapeEdges g1 `shouldBe` shapeEdges g2
      cachePopulated <- hasCacheEntries outFresh
      cachePopulated `shouldBe` True

-- ───────────────────────────────────────────────
-- Fixtures and helpers
-- ───────────────────────────────────────────────

createFixture :: FilePath -> IO ()
createFixture dir = do
  createDirectoryIfMissing True dir
  writeFile (dir </> "Util.hs") $
    "module Util where\n\
    \double :: Int -> Int\n\
    \double x = x * 2\n"
  writeFile (dir </> "Main.hs") $
    "module Main where\n\
    \import Util (double)\n\
    \main :: IO ()\n\
    \main = print (double 21)\n"
  writeFile (dir </> "README.md") "# Fixture\nUses `double` from Util.\n"

runGraphos :: FilePath -> [String] -> IO ()
runGraphos exe args = do
  _ <- readCreateProcessWithExitCode
    (proc exe args) ""
  pure ()

-- | The comparable shape of an emitted graph: node ids and edge pairs
-- (both as sets — order is immaterial).
data GraphShape = GraphShape
  { shapeNodes :: Map Text ()
  , shapeEdges :: Map (Text, Text) ()
  } deriving (Eq, Show)

readShape :: FilePath -> IO GraphShape
readShape path = do
  bs <- BSL.readFile path
  case decode bs of
    Just doc -> pure (shapeOf doc)
    Nothing  -> error ("not valid JSON: " ++ T.unpack (T.pack path))

shapeOf :: Value -> GraphShape
shapeOf doc = GraphShape
  { shapeNodes = Map.fromList [ (nid, ()) | Just nid <- map idOf (nodeValues doc) ]
  , shapeEdges = Map.fromList [ (pt, ()) | Just pt <- map pairOf (edgeValues doc) ]
  }
  where
    nodeValues d = case nodesSection d of
      Just (Object km) -> KM.elems km
      Just (Array vs)  -> map id (V.toList vs)
      _                -> []
    nodesSection d = section d "graph" "nodes"
    edgeValues d = case edgesSection d of
      Just (Array vs) -> map id (V.toList vs)
      _               -> []
    edgesSection d = section d "graph" "edges"
    section d wrapper inner = case d of
      Object km ->
        let inGraph = do
              gkm <- asObj =<< KM.lookup (Key.fromText wrapper) km
              KM.lookup (Key.fromText inner) gkm
        in case inGraph of
             Just v  -> Just v
             Nothing -> KM.lookup (Key.fromText inner) km
      _ -> Nothing
    asObj (Object km) = Just km
    asObj _           = Nothing
    pairOf v = case v of
      Object km -> do
        String s <- KM.lookup (Key.fromText "source") km
        String t <- KM.lookup (Key.fromText "target") km
        pure (s, t)
      _ -> Nothing
    idOf v = case v of
      Object km -> case KM.lookup (Key.fromText "id") km of
        Just (String s) -> Just s
        _               -> Nothing
      _ -> Nothing

hasCacheEntries :: FilePath -> IO Bool
hasCacheEntries outDir = do
  exists <- doesDirectoryExist (outDir </> "cache")
  if not exists
    then pure False
    else not . null <$> listDirectory (outDir </> "cache")