module OpenSpecView.ArtifactView
  ( ArtifactPath
  , ArtifactKind
  , DepthLevel
  , ChangeArtifact(..)
  , ChangeArtifacts(..)
  , readArtifact
  , listSpecs
  , readSpec
  , readChangeArtifacts
  , renderChangeArtifacts
  , displayChangeArtifacts
  ) where

import Control.Monad (forM, filterM)
import Data.List (intercalate, sort)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)

type ArtifactPath = String
type ArtifactKind = String
type DepthLevel = Int

data ChangeArtifact = ChangeArtifact
  { artifactKind    :: ArtifactKind
  , artifactPath    :: ArtifactPath
  , artifactDepth   :: DepthLevel
  , artifactContent :: String
  , artifactMissing :: Bool
  } deriving (Eq, Show)

data ChangeArtifacts = ChangeArtifacts
  {   artifactsName    :: String
  , changeProposal   :: Maybe ChangeArtifact
  , changeDesign     :: Maybe ChangeArtifact
  , changeTasks      :: Maybe ChangeArtifact
  , changeSpecs      :: [ChangeArtifact]
  } deriving (Eq, Show)

combine' :: FilePath -> FilePath -> FilePath
combine' a b
  | not (null a) && last a == '/' = a ++ b
  | null a = b
  | otherwise = a ++ "/" ++ b

takeBaseName :: FilePath -> FilePath
takeBaseName fp = reverse (takeWhile (/= '/') (reverse fp))

readArtifactAt :: DepthLevel -> FilePath -> IO (Maybe ChangeArtifact)
readArtifactAt depth path = do
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else do
      content <- readFile path
      return (Just (ChangeArtifact
        { artifactKind = takeBaseName path
        , artifactPath = path
        , artifactDepth = depth
        , artifactContent = content
        , artifactMissing = False
        }))

readArtifact :: FilePath -> IO (Maybe ChangeArtifact)
readArtifact = readArtifactAt 0

listSpecs :: FilePath -> IO [String]
listSpecs specsDir = do
  exists <- doesDirectoryExist specsDir
  if not exists
    then return []
    else do
      entries <- listDirectory specsDir
      dirs <- filterM (\e -> doesDirectoryExist (combine' specsDir e)) entries
      return (sort dirs)

readSpec :: FilePath -> String -> IO (Maybe ChangeArtifact)
readSpec specsDir specName = readArtifactAt 1 (combine' specsDir (combine' specName "spec.md"))

readChangeArtifacts :: FilePath -> IO ChangeArtifacts
readChangeArtifacts changeDir = do
  mProposal <- readArtifact (combine' changeDir "proposal.md")
  mDesign   <- readArtifact (combine' changeDir "design.md")
  mTasks    <- readArtifact (combine' changeDir "tasks.md")
  let specsDir = combine' changeDir "specs"
  specNames <- listSpecs specsDir
  mSpecs <- forM specNames (\name -> readSpec specsDir name)
  let specs = [ x | Just x <- mSpecs ]
  return (ChangeArtifacts
     { artifactsName = takeBaseName changeDir
    , changeProposal = mProposal
    , changeDesign = mDesign
    , changeTasks = mTasks
    , changeSpecs = specs
    })

renderSection :: String -> Maybe ChangeArtifact -> String
renderSection kind mca =
  case mca of
    Nothing -> "== " ++ kind ++ " ==\n(missing)"
    Just art -> "== " ++ kind ++ " ==\n" ++ artifactContent art

renderSpecsSection :: [ChangeArtifact] -> String
renderSpecsSection arts =
  "== Specs (" ++ show (length arts) ++ ") ==\n" ++
  intercalate "\n" (map renderOneSpec arts)
  where
    renderOneSpec a = "== " ++ artifactKind a ++ " ==\n" ++ artifactContent a

renderChangeArtifacts :: ChangeArtifacts -> String
renderChangeArtifacts ca =
  header ++ "\n\n" ++ intercalate "\n" sections
  where
    header = "OpenSpec Artifact Viewer\nChange: " ++ artifactsName ca
    sections =
          [ renderSection "proposal.md" (changeProposal ca)
          , renderSection "design.md"   (changeDesign ca)
          , renderSection "tasks.md"    (changeTasks ca)
          , renderSpecsSection (changeSpecs ca) ]

displayChangeArtifacts :: FilePath -> IO ()
displayChangeArtifacts changeDir = do
  ca <- readChangeArtifacts changeDir
  putStr (renderChangeArtifacts ca)
