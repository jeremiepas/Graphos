{-# LANGUAGE OverloadedStrings #-}
-- | Graph coverage accounting.
--
-- Compares the set of source files on disk with the set of files present in a
-- @graph.json@ and reports the difference grouped by the ignore-rule class that
-- most plausibly explains it: root-anchored build output, depth-independent
-- tooling, @.gitignore@, or unexplained. The spec fails when any missing file
-- is unexplained.
module Graphos.Fidelity.GraphCoverageSpec (spec) where

import Control.Monad (forM)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.Text.Short (fromText, toText)
import Graphos.Domain.Types (FileType(CodeFile), Node(..))
import Graphos.Domain.Types.Graph (LabeledGraph(..))
import qualified Graphos.Domain.Graph as DG
import Graphos.UseCase.Load (loadGraphFromFile, LoadResult(..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), makeRelative, normalise, splitDirectories, takeDirectory, takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- | Ignore-rule class that most plausibly explains a missing file.
data MissingClass
  = RootAnchoredBuild
  | DepthIndependentTooling
  | GitIgnored
  | Unexplained
  deriving (Eq, Ord, Show)

-- | Directories treated as depth-independent tooling output.
toolingSegments :: [String]
toolingSegments =
  [ "node_modules", "dist", ".cache", ".venv", "venv", "target"
  , "__pycache__", "coverage", ".pytest_cache", ".tox", ".ruff_cache"
  ]

-- | Classify a missing file given its path relative to the repository root.
classifyMissing :: FilePath -> MissingClass
classifyMissing rel
  | "build" `elem` segments        = RootAnchoredBuild
  | any (`elem` toolingSegments) segments = DepthIndependentTooling
  | any (("." `T.isPrefixOf`) . T.pack) segments = GitIgnored
  | otherwise                      = Unexplained
  where
    segments = filter (not . null) (splitDirectories rel)

-- | Source extensions considered by the coverage scan.
sourceExtensions :: [String]
sourceExtensions = [".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs", ".py"]

-- | Recursively list source files under a root, relative to it.
listSourceFiles :: FilePath -> IO [FilePath]
listSourceFiles root = walk root root
  where
    walk base r = do
      entries <- listDirectory r
      fmap concat $ forM entries $ \e -> do
        let p = r </> e
        isDir <- doesDirectoryExist p
        if isDir
          then walk base p
          else pure [normalise (makeRelative base p) | takeExtension p `elem` sourceExtensions]

-- | Files referenced by the nodes of a graph, relative to the repository root.
graphFilesFrom :: Map.Map Text Node -> Set.Set FilePath
graphFilesFrom nodes = Set.fromList
  [ normalise (T.unpack (toText (nodeSourceFile n)))
  | n <- Map.elems nodes
  ]

graphFiles :: LabeledGraph -> Set.Set FilePath
graphFiles gr = graphFilesFrom (gNodes gr)

-- | Machine-readable coverage report grouped by class.
coverageReport :: Set.Set FilePath -> Map.Map MissingClass [FilePath] -> Text
coverageReport onDisk grouped =
  let total = Set.size onDisk
      accounted = sum (map length (Map.elems grouped))
  in T.unlines
       [ "graph coverage"
       , "  on-disk source files : " <> T.pack (show total)
       , "  missing files        : " <> T.pack (show accounted)
       , "  root-anchored build  : " <> T.pack (show (length (Map.findWithDefault [] RootAnchoredBuild grouped)))
       , "  depth tooling        : " <> T.pack (show (length (Map.findWithDefault [] DepthIndependentTooling grouped)))
       , "  gitignored           : " <> T.pack (show (length (Map.findWithDefault [] GitIgnored grouped)))
       , "  unexplained          : " <> T.pack (show (length (Map.findWithDefault [] Unexplained grouped)))
       ]
       <> T.concat [ "  MISSING (" <> classLabel c <> ") " <> T.pack f <> "\n"
                   | (c, fs) <- Map.toAscList grouped, f <- fs ]
  where
    classLabel RootAnchoredBuild     = "build-output"
    classLabel DepthIndependentTooling = "tooling"
    classLabel GitIgnored            = "gitignore"
    classLabel Unexplained           = "unexplained"

-- | Group the on-disk files absent from the graph by class.
groupMissing :: Set.Set FilePath -> Set.Set FilePath -> Map.Map MissingClass [FilePath]
groupMissing onDisk graphFiles' =
  Map.fromListWith (++) [ (classifyMissing f, [f])
                        | f <- Set.toList (Set.difference onDisk graphFiles') ]

-- | Build a graph containing the given (already normalised, root-relative) files.
graphForFiles :: [FilePath] -> LabeledGraph
graphForFiles files =
  let node i f = Node (T.pack (show i)) (fromText (T.pack f)) CodeFile (fromText (T.pack f))
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0
      nodes = Map.fromList [ (nodeId (node i f), node i f) | (i, f) <- zip [0 :: Int ..] files ]
  in LabeledGraph nodes Map.empty Map.empty Map.empty

-- | Run the coverage oracle: returns (report, grouped missing, unexplained count).
runCoverage :: [FilePath] -> [FilePath] -> IO (Text, Map.Map MissingClass [FilePath], Int)
runCoverage diskFiles graphFiles' = withSystemTempDirectory "graphos-coverage" $ \tmpDir -> do
  _ <- forM diskFiles $ \f -> do
    let full = tmpDir </> f
    createDirectoryIfMissing True (takeDirectory full)
    writeSource full
  BSL.writeFile (tmpDir </> "graph.json") (encode (graphForFiles graphFiles'))
  onDisk <- Set.fromList <$> listSourceFiles tmpDir
  mGraph <- decode <$> BSL.readFile (tmpDir </> "graph.json")
  case mGraph of
    Nothing -> error "GraphCoverageSpec: failed to decode graph.json"
    Just gr -> do
      let grouped = groupMissing onDisk (graphFiles gr)
          unexplained = length (Map.findWithDefault [] Unexplained grouped)
      pure (coverageReport onDisk grouped, grouped, unexplained)
  where
    writeSource f = do
      let body = case takeExtension f of
            ".py"  -> "x = 1\n"
            _      -> "export const x = 1;\n"
      TIO.writeFile f body

-- | Strip a root prefix from an absolute path, yielding a root-relative path.
stripRootPrefix :: FilePath -> FilePath -> FilePath
stripRootPrefix root p
  | root `isPrefixOf` p = dropWhile (== '/') (drop (length root) p)
  | otherwise = p

-- | Directories the pipeline ignores (must mirror UseCase/Detect.hs) so the
-- on-disk scan does not count files the pipeline intentionally skips.
pipelineIgnoredDirs :: [String]
pipelineIgnoredDirs =
  [ "node_modules", "dist", "build", "out", "target", "dist-newstyle"
  , ".git", ".cache", ".venv", "venv", "__pycache__", ".next", ".nuxt"
  , ".stack-work", ".cargo", ".idea", ".vscode", ".direnv", ".tmp"
  ]

-- | True when any path segment of a root-relative path is a pipeline-ignored dir.
underIgnoredDir :: FilePath -> Bool
underIgnoredDir rel = any (`elem` pipelineIgnoredDirs) (splitDirectories rel)

-- | Run the coverage oracle against a real corpus named by env vars
-- (GRAPHOS_FIDELITY_ROOT, GRAPHOS_FIDELITY_GRAPH). Returns the report plus the
-- unexplained-missing count; 'Nothing' when the env vars are unset.
runRealCoverage :: IO (Maybe (Text, Int))
runRealCoverage = do
  mRoot  <- lookupEnv "GRAPHOS_FIDELITY_ROOT"
  mGraph <- lookupEnv "GRAPHOS_FIDELITY_GRAPH"
  case (mRoot, mGraph) of
    (Just root, Just graphPath) -> do
      allOnDisk <- Set.fromList <$> listSourceFiles root
      let onDisk = Set.filter (not . underIgnoredDir) allOnDisk
      mLR <- loadGraphFromFile graphPath
      case mLR of
        Left err -> error ("GraphCoverageSpec: failed to load real corpus graph.json: " ++ T.unpack err)
        Right lr -> do
          let g = lrGraph lr
              normRoot    = normalise root
              graphFilesRel = Set.map (stripRootPrefix normRoot) (graphFilesFrom (DG.gNodes g))
              grouped     = groupMissing onDisk graphFilesRel
              unexplained = length (Map.findWithDefault [] Unexplained grouped)
              report      = coverageReport onDisk grouped
          return (Just (report, unexplained))
    _ -> return Nothing

spec :: Spec
spec = describe "GraphCoverage fidelity" $ do
  it "passes when every source file on disk is present in the graph" $ do
    let disk  = ["src/a.ts", "src/b.ts"]
        graph = ["src/a.ts", "src/b.ts"]
    (report, grouped, unexplained) <- runCoverage disk graph
    Set.fromList (concat (Map.elems grouped)) `shouldBe` Set.empty
    unexplained `shouldBe` 0
    report `shouldSatisfy` T.isInfixOf "missing files"
    report `shouldSatisfy` T.isInfixOf "unexplained"

  it "groups missing files by cause and fails on the unexplained bucket" $ do
    -- build/x.ts -> root-anchored build output; src/templates/y.ts -> unexplained
    let disk  = ["src/a.ts", "build/x.ts", "src/templates/y.ts"]
        graph = ["src/a.ts"]
    (report, grouped, unexplained) <- runCoverage disk graph
    let buildClass  = Map.findWithDefault [] RootAnchoredBuild grouped
        unex        = Map.findWithDefault [] Unexplained grouped
    buildClass `shouldNotBe` []
    buildClass `shouldSatisfy` all (== "build/x.ts")
    unex `shouldNotBe` []
    unexplained `shouldBe` 1
    -- the gate fails: any unexplained file fails the spec
    unexplained `shouldNotBe` 0
    report `shouldSatisfy` T.isInfixOf "MISSING (build-output)"
    report `shouldSatisfy` T.isInfixOf "MISSING (unexplained)"

  it "real corpus coverage (env GRAPHOS_FIDELITY_ROOT / GRAPHOS_FIDELITY_GRAPH)" $ do
    mResult <- runRealCoverage
    case mResult of
      Nothing -> pendingWith "set GRAPHOS_FIDELITY_ROOT and GRAPHOS_FIDELITY_GRAPH to run the real-corpus coverage oracle"
      Just (report, unexplained) -> do
        TIO.putStr report
        unexplained `shouldBe` 0
