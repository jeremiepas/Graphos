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
import qualified Graphos.Domain.Graph as DG
import Graphos.UseCase.Load (loadGraphFromFile, LoadResult(..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
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

-- | Directories the pipeline ignores (must mirror UseCase/Detect.hs) so the
-- ground-truth scan covers exactly the files the pipeline extracts.
-- Mirrors the production ignore rules in @Detect.hs@: build-output dirs are
-- root-anchored (direct child of the scan root only), while tooling/VCS dirs
-- are ignored at any depth.
rootAnchoredIgnoreDirs :: [String]
rootAnchoredIgnoreDirs =
  [ "build", "out", "target", "dist", "dist-newstyle", "DerivedData", ".build" ]

depthIndependentIgnoreDirs :: [String]
depthIndependentIgnoreDirs =
  [ ".git", ".svn", ".hg"
  , "node_modules", "bower_components", "vendor"
  , "__pypackages__", ".pnpm-store", ".yarn"
  , ".cache", ".sass-cache"
  , "__pycache__", ".pytest_cache", ".mypy_cache", ".tox"
  , ".venv", ".env"
  , ".stack-work", ".gradle"
  , ".next", ".nuxt"
  , ".cargo"
  , ".idea", ".vscode", ".lsp", ".elixir_ls", ".clj-kondo"
  , ".direnv"
  , "graphos-out", ".opencode", ".tmp", ".obsidian"
  , ".github", ".DS_Store", ".pdm-build"
  ]

-- | Recursively list the source files under a root directory, skipping the
-- pipeline-ignored directories (root-anchored at the scan root, depth-
-- independent elsewhere).
listSourceFiles :: FilePath -> IO [FilePath]
listSourceFiles root = go root True
  where
    go dir isRoot = do
      entries <- listDirectory dir
      fmap concat $ forM entries $ \e -> do
        let p = dir </> e
        isDir <- doesDirectoryExist p
        if isDir
          then if (isRoot && e `elem` rootAnchoredIgnoreDirs)
                   || e `elem` depthIndependentIgnoreDirs
               then pure []
               else go p False
          else pure [p | takeExtension p `elem` sourceExtensions]

-- | Extract ground-truth @(sourceFile, targetFile)@ import pairs from a tree.
-- Relative specifiers are resolved against the importing file; bare specifiers
-- (package names) resolve to nothing on disk and are skipped.
-- | Resolve @.@ and @..@ path components lexically (no filesystem access).
-- Mirrors the production @resolveDots@ in @Resolver.hs@ so that ground-truth
-- paths match the graph's resolved paths.
resolveDots :: FilePath -> FilePath
resolveDots p = T.unpack (T.intercalate "/" (go (T.splitOn "/" (T.pack p)) []))
  where
    go :: [Text] -> [Text] -> [Text]
    go [] acc = reverse acc
    go (x : rest) acc
      | x == "."   = go rest acc
      | x == ".."  = case acc of
          [] -> go rest acc
          ("" : _) -> go rest acc
          (_ : _) -> go rest (tail acc)
      | otherwise  = go rest (x : acc)

scanGroundTruth :: FilePath -> IO (Set.Set (FilePath, FilePath))
scanGroundTruth root = do
  files <- listSourceFiles root
  pairs <- fmap concat (forM files (findImportsIn root))
  pure $ Set.fromList [ (resolveDots s, resolveDots t) | (s, t) <- pairs ]

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
-- Matches any line containing a @from '...'@ or @from "..."@ clause, which
-- covers both single-line and multi-line import statements (where the
-- @from@ clause appears on a continuation line after @import { ... }@).
isImportOrReexport :: Text -> Bool
isImportOrReexport l =
  (" from " `T.isInfixOf` l)
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
-- Only file-to-file pairs are included; edges whose target @source_file@ is an
-- @external:@ pseudo-path (node builtins, npm packages) are excluded because the
-- on-disk ground-truth oracle cannot resolve bare specifiers to files.
importPairsFrom :: Map.Map Text Node -> [Edge] -> Set.Set (FilePath, FilePath)
importPairsFrom nodes edges = Set.fromList
  [ (normalise (T.unpack (nodeSourceFile (nodes Map.! edgeSource e)))
   , normalise (T.unpack (nodeSourceFile (nodes Map.! edgeTarget e))))
  | e <- edges
  , edgeRelation e == Imports
  , not (T.isPrefixOf "external:" (nodeSourceFile (nodes Map.! edgeTarget e)))
  ]

graphImportPairs :: LabeledGraph -> Set.Set (FilePath, FilePath)
graphImportPairs gr = importPairsFrom (gNodes gr) (Map.elems (gEdges gr))

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

-- | Summary-only fidelity report (no per-pair listings) for large real corpora.
fidelitySummary :: Set.Set (FilePath, FilePath) -> Set.Set (FilePath, FilePath) -> (Text, Double, Double, Int, Int)
fidelitySummary groundTruth graphPairs' =
  let gt = Set.size groundTruth
      ge = Set.size graphPairs'
      tp = Set.size (Set.intersection groundTruth graphPairs')
      precision :: Double
      precision = if ge == 0 then 0.0 else fromIntegral tp / fromIntegral ge
      recall :: Double
      recall = if gt == 0 then 1.0 else fromIntegral tp / fromIntegral gt
      missing = Set.size (Set.difference groundTruth graphPairs')
      extra   = Set.size (Set.difference graphPairs' groundTruth)
      summary = T.unlines
        [ "imports fidelity (real corpus)"
        , "  ground-truth pairs : " <> T.pack (show gt)
        , "  graph import edges : " <> T.pack (show ge)
        , "  precision          : " <> T.pack (show precision) <> " (threshold " <> T.pack (show fidelityThreshold) <> ")"
        , "  recall             : " <> T.pack (show recall) <> " (threshold " <> T.pack (show fidelityThreshold) <> ")"
        , "  missing pairs      : " <> T.pack (show missing)
        , "  extra pairs        : " <> T.pack (show extra)
        ]
  in (summary, precision, recall, missing, extra)

-- | Run the oracle against a real corpus named by env vars.
-- @GRAPHOS_FIDELITY_ROOT@ is the corpus root; @GRAPHOS_FIDELITY_GRAPH@ is the
-- @graph.json@ produced by the pipeline over that root.
runRealCorpus :: IO (Maybe (Text, Double, Double, Int, Int))
runRealCorpus = do
  mRoot  <- lookupEnv "GRAPHOS_FIDELITY_ROOT"
  mGraph <- lookupEnv "GRAPHOS_FIDELITY_GRAPH"
  case (mRoot, mGraph) of
    (Just root, Just graphPath) -> do
      groundTruth <- scanGroundTruth root
      mLR <- loadGraphFromFile graphPath
      case mLR of
        Left err -> error ("ImportEdgesSpec: failed to load real corpus graph.json: " ++ T.unpack err)
        Right lr -> do
          let g = lrGraph lr
              pairs = importPairsFrom (DG.gNodes g) (Map.elems (DG.gEdges g))
              (summary, precision, recall, missing, extra) = fidelitySummary groundTruth pairs
          TIO.putStr summary
          pure (Just (summary, precision, recall, missing, extra))
    _ -> pure Nothing

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

  it "real corpus fidelity (env GRAPHOS_FIDELITY_ROOT / GRAPHOS_FIDELITY_GRAPH)" $ do
    result <- runRealCorpus
    case result of
      Nothing -> pendingWith "set GRAPHOS_FIDELITY_ROOT and GRAPHOS_FIDELITY_GRAPH to run the real-corpus oracle"
      Just (_, precision, recall, _, _) -> do
        precision `shouldSatisfy` (>= fidelityThreshold)
        recall `shouldSatisfy` (>= fidelityThreshold)
