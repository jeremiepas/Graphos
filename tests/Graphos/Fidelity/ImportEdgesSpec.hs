{-# LANGUAGE OverloadedStrings #-}
-- | On-disk ground-truth oracle for @imports@ edge extraction.
--
-- The spec scans a repository directly from disk, resolves every import /
-- re-export specifier to a file, and compares the resulting pair set with the
-- @imports@ edges present in a @graph.json@. It reports counts and the
-- precision/recall gaps as explicit missing/extra pair listings, and fails the
-- spec when either falls below the configured threshold (default 0.99).
module Graphos.Fidelity.ImportEdgesSpec (spec) where

import Control.Monad (forM)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Graphos.Domain.Types (Confidence(..), Edge(..), EdgeId(..), FileType(CodeFile), Node(..), Relation(Imports))
import Graphos.Domain.Types.Graph (LabeledGraph(..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), dropExtension, normalise, takeDirectory, takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | Default fidelity gate: precision and recall must stay above this.
-- Constants live in the module (or are env-overridable) so CI can tune them
-- without touching the oracle logic.
fidelityThreshold :: Double
fidelityThreshold = 0.99

-- | Candidate extensions probed when a specifier has no extension.
sourceExtensions :: [String]
sourceExtensions = [".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs"]

-- | Recursively list the source files under a root directory.
listSourceFiles :: FilePath -> IO [FilePath]
listSourceFiles root = do
  entries <- listDirectory root
  fmap concat $ forM entries $ \e -> do
    let p = root </> e
    isDir <- doesDirectoryExist p
    if isDir
      then listSourceFiles p
      else pure [p | takeExtension p `elem` sourceExtensions]

-- | Extract ground-truth @(sourceFile, targetFile)@ import pairs from a tree.
-- Relative specifiers are resolved against the importing file; bare specifiers
-- (package names) resolve to nothing on disk and are skipped.
scanGroundTruth :: FilePath -> IO (Set.Set (FilePath, FilePath))
scanGroundTruth root = do
  files <- listSourceFiles root
  pairs <- fmap concat (forM files (findImportsIn root))
  pure $ Set.fromList [ (normalise s, normalise t) | (s, t) <- pairs ]

-- | Resolve the import/re-export specifiers of a single file.
findImportsIn :: FilePath -> FilePath -> IO [(FilePath, FilePath)]
findImportsIn _root sourcePath = do
  content <- TIO.readFile sourcePath
  let importLines = filter isImportOrReexport (T.lines content)
  mTargets <- mapM (resolveImport sourcePath) importLines
  pure [ (sourcePath, t) | Just t <- mTargets ]

resolveImport :: FilePath -> Text -> IO (Maybe FilePath)
resolveImport sourcePath line =
  case specifierOf line of
    Nothing -> pure Nothing
    Just sp | T.null sp -> pure Nothing
            | otherwise ->
                let dir = takeDirectory sourcePath
                    base = normalise (dir </> T.unpack sp)
                    noExt = dropExtension base
                in firstExisting noExt

-- | Probe candidate source extensions for a resolved specifier base.
firstExisting :: FilePath -> IO (Maybe FilePath)
firstExisting base = go sourceExtensions
  where
    go [] = pure Nothing
    go (ext : rest) = do
      let cand = base ++ ext
      exists <- doesFileExist cand
      if exists then pure (Just cand) else go rest

-- | True for lines that declare an import or a re-export with a quoted specifier.
isImportOrReexport :: Text -> Bool
isImportOrReexport l =
  (("import " `T.isInfixOf` l) || ("export " `T.isInfixOf` l))
    && (" from " `T.isInfixOf` l)
    && any (\q -> q `T.isInfixOf` l) ["'", "\""]

-- | Extract a quoted specifier from an import/re-export line.
specifierOf :: Text -> Maybe Text
specifierOf line =
  case reverse (T.splitOn " from " line) of
    (lastPart : _) ->
      let targetPart = T.strip (T.takeWhile (/= ';') lastPart)
          quoted = T.dropAround (\c -> c == '\'' || c == '"') targetPart
      in if T.null quoted then Nothing else Just quoted
    [] -> Nothing

-- | Normalised @(sourceFile, targetFile)@ pairs for the @imports@ edges of a graph.
graphImportPairs :: LabeledGraph -> Set.Set (FilePath, FilePath)
graphImportPairs gr = Set.fromList
  [ (normalise (T.unpack (nodeSourceFile (gNodes gr Map.! edgeSource e)))
   , normalise (T.unpack (nodeSourceFile (gNodes gr Map.! edgeTarget e))))
  | e <- Map.elems (gEdges gr)
  , edgeRelation e == Imports
  ]

-- | Machine-readable fidelity report: counts, thresholds and the gap listings.
fidelityReport :: Set.Set (FilePath, FilePath) -> Set.Set (FilePath, FilePath) -> Text
fidelityReport groundTruth graphPairs' =
  let gt = Set.size groundTruth
      ge = Set.size graphPairs'
      tp = Set.size (Set.intersection groundTruth graphPairs')
      precision :: Double
      precision = if ge == 0 then 0.0 else fromIntegral tp / fromIntegral ge
      recall :: Double
      recall = if gt == 0 then 1.0 else fromIntegral tp / fromIntegral gt
      missing   = Set.toList (Set.difference groundTruth graphPairs')
      extra     = Set.toList (Set.difference graphPairs' groundTruth)
  in T.unlines
       [ "imports fidelity"
       , "  ground-truth pairs : " <> T.pack (show gt)
       , "  graph import edges : " <> T.pack (show ge)
       , "  precision          : " <> T.pack (show precision) <> " (threshold " <> T.pack (show fidelityThreshold) <> ")"
       , "  recall             : " <> T.pack (show recall) <> " (threshold " <> T.pack (show fidelityThreshold) <> ")"
       , "  missing pairs      : " <> T.pack (show (length missing))
       , "  extra pairs        : " <> T.pack (show (length extra))
       ]
       <> T.concat [ "  MISSING " <> pairLine p <> "\n" | p <- missing ]
       <> T.concat [ "  EXTRA   " <> pairLine p <> "\n" | p <- extra ]
  where
    pairLine (s, t) = T.pack (s ++ " -> " ++ t)

-- | A minimal graph with an optional @imports@ edge between two file nodes.
simpleGraphWithEdge :: Bool -> FilePath -> FilePath -> LabeledGraph
simpleGraphWithEdge withEdge fileA fileB =
  let nodeA = Node "a" "A" CodeFile (T.pack fileA) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      nodeB = Node "b" "B" CodeFile (T.pack fileB) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      edge  = Edge (EdgeId "e1") "a" "b" Imports 1.0 (Confidence 1.0) Nothing
      (edges, adjFwd, adjBack) = if withEdge
        then ( Map.singleton (EdgeId "e1") edge
             , Map.fromList [("a", Set.singleton "b"), ("b", Set.empty)]
             , Map.fromList [("a", Set.empty), ("b", Set.singleton "a")]
             )
        else ( Map.empty
             , Map.fromList [("a", Set.empty), ("b", Set.empty)]
             , Map.fromList [("a", Set.empty), ("b", Set.empty)]
             )
  in LabeledGraph
       { gNodes   = Map.fromList [("a", nodeA), ("b", nodeB)]
       , gEdges   = edges
       , gAdjFwd  = adjFwd
       , gAdjBack = adjBack
       }

-- | Run the oracle against a fixture dir: a.ts importing b.ts plus a graph.json
-- (with or without the imports edge).
runOracle :: Bool -> IO (Text, Set.Set (FilePath, FilePath), Set.Set (FilePath, FilePath), Double, Bool)
runOracle withEdge = withSystemTempDirectory "graphos-fidelity" $ \tmpDir -> do
  let fileA = tmpDir </> "a.ts"
      fileB = tmpDir </> "b.ts"
  TIO.writeFile fileA "import { x } from './b.js';\n"
  TIO.writeFile fileB "export const x = 1;\n"
  let graph = simpleGraphWithEdge withEdge fileA fileB
  BSL.writeFile (tmpDir </> "graph.json") (encode graph)

  groundTruth <- scanGroundTruth tmpDir
  mGraph <- decode <$> BSL.readFile (tmpDir </> "graph.json")
  case mGraph of
    Nothing -> error "ImportEdgesSpec: failed to decode graph.json"
    Just gr -> do
      let graphPairs = graphImportPairs gr
          gt = Set.size groundTruth
          tp = Set.size (Set.intersection groundTruth graphPairs)
          recall :: Double
          recall = if gt == 0 then 1.0 else fromIntegral tp / fromIntegral gt
          missing = Set.difference groundTruth graphPairs
          extra = Set.difference graphPairs groundTruth
      pure (fidelityReport groundTruth graphPairs, missing, extra, recall, recall >= fidelityThreshold)

spec :: Spec
spec = describe "ImportEdges fidelity" $ do
  it "passes when the graph matches the on-disk imports" $ do
    (report, missing, extra, recall, passed) <- runOracle True
    missing `shouldBe` Set.empty
    extra `shouldBe` Set.empty
    recall `shouldSatisfy` (>= fidelityThreshold)
    passed `shouldBe` True
    report `shouldSatisfy` T.isInfixOf "ground-truth pairs"
    report `shouldSatisfy` T.isInfixOf "precision"

  it "fails when the graph is missing an import edge" $ do
    (report, missing, _, recall, passed) <- runOracle False
    missing `shouldNotBe` Set.empty
    recall `shouldSatisfy` (< fidelityThreshold)
    passed `shouldBe` False
    report `shouldSatisfy` T.isInfixOf "MISSING"

  it "fails with recall 0.0 on a graph with zero imports edges" $ do
    (report, _, _, recall, passed) <- runOracle False
    recall `shouldBe` 0.0
    passed `shouldBe` False
    report `shouldSatisfy` T.isInfixOf "0.0"
