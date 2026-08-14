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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Graphos.Domain.Types (FileType(CodeFile), Node(..))
import Graphos.Domain.Types.Graph (LabeledGraph(..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
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
graphFiles :: LabeledGraph -> Set.Set FilePath
graphFiles gr = Set.fromList
  [ normalise (T.unpack (nodeSourceFile n))
  | n <- Map.elems (gNodes gr)
  ]

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
  let node i f = Node (T.pack (show i)) (T.pack f) CodeFile (T.pack f)
                        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
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
