-- | Artifact browsing capability for the @openspec-view@ workspace inspector.
--
-- Reads a change's @proposal.md@, @specs/<capability>/spec.md@ files, and
-- optional @design.md@ / @tasks.md@ artifacts, classifies each as
-- 'ArtDone' / 'ArtPending' / 'ArtNotApplicable', and renders them at
-- 'DepthSummary' / 'DepthFull' / 'DepthSections'.
module Graphos.OpenSpecView.ArtifactView
  ( Artifact(..)
  , ArtifactStatus(..)
  , DepthLevel(..)
  , parseArtifacts
  , renderArtifact
  , renderSectioned
  , splitSections
  ) where

import Data.Char (toLower)
import Data.List (isPrefixOf, sortOn)
import qualified Data.Text as T
import Data.Text (Text)
import Prelude (Bool(True), Eq, FilePath, IO, Maybe(..), Show, String, break, concat, concatMap, lines, map, mapM, not, pure, readFile, span, unlines, (.), (>>=))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeFileName)

-- | Completion status of a single artifact.
data ArtifactStatus
  = ArtDone                 -- ^ artifact exists on disk
  | ArtPending              -- ^ expected by convention but not yet created
  | ArtNotApplicable        -- ^ artifact type does not apply to this change
  deriving (Show, Eq)

-- | Depth of an artifact rendering.
data DepthLevel
  = DepthSummary   -- ^ heading and status only
  | DepthFull      -- ^ complete artifact content
  | DepthSections  -- ^ content grouped by @##@ headings
  deriving (Show, Eq)

-- | A single browsable artifact of a change.
data Artifact = Artifact
  { artName     :: Text        -- ^ logical artifact id (proposal, design, tasks, <capability>)
  , artPath     :: FilePath    -- ^ path on disk
  , artStatus   :: ArtifactStatus
  , artContent  :: Maybe String -- ^ raw markdown when present
  } deriving (Show, Eq)

-- | Parse every artifact under @changeDir@ in a stable, human order:
-- proposal, design, tasks, then capability specs sorted by name.
parseArtifacts :: FilePath -> IO [Artifact]
parseArtifacts changeDir = do
  proposal <- readOptional (changeDir </> "proposal.md") "proposal"
  design <- readOptional (changeDir </> "design.md") "design"
  tasks <- readOptional (changeDir </> "tasks.md") "tasks"
  specs <- listSpecs changeDir
  pure (concat [proposal, design, tasks, specs])
  where
    listSpecs dir = do
      hasSpecs <- doesDirectoryExist (dir </> "specs")
      if not hasSpecs
        then pure []
        else do
          entries <- listDirectory (dir </> "specs")
          dirs <- mapM (\e -> do isDir <- doesDirectoryExist ((dir </> "specs") </> e); pure (isDir, e)) entries
          let sorted = sortOn (map toLower) [e | (True, e) <- dirs]
          artifacts <- mapM (\d -> readSpec ((dir </> "specs") </> d)) sorted
          pure (concat artifacts)
    readOptional path name = do
      exists <- doesFileExist path
      if exists
        then do content <- readFile path
                pure [Artifact (T.pack name) path ArtDone (Just content)]
        else pure []
    readSpec path = do
      exists <- doesFileExist (path </> "spec.md")
      if exists
        then do content <- readFile (path </> "spec.md")
                pure [Artifact (T.pack (takeFileName path)) (path </> "spec.md") ArtDone (Just content)]
        else pure [Artifact (T.pack (takeFileName path)) (path </> "spec.md") ArtPending Nothing]

-- | Render a single artifact at the requested depth.
renderArtifact :: DepthLevel -> Artifact -> Text
renderArtifact depth art = T.concat [name, " ", status, suffix]
  where
    name = artName art
    status = statusText (artStatus art)
    suffix = case depth of
      DepthSummary -> T.empty
      DepthFull -> case artContent art of
        Just c -> T.concat ["\n\n", T.pack c]
        Nothing -> T.empty
      DepthSections -> case artContent art of
        Just c -> T.concat ["\n\n", renderSectioned (T.pack c)]
        Nothing -> T.empty
    statusText s = case s of
      ArtDone -> "done"
      ArtPending -> "pending"
      ArtNotApplicable -> "not-applicable"

-- | Group markdown content by @##@ headings for the depth=sections output.
renderSectioned :: Text -> Text
renderSectioned txt =
  case splitSections (T.unpack txt) of
    [] -> T.empty
    secs -> T.init (T.concat (map renderSection secs))
  where
    renderSection (heading, body) = T.concat [T.pack heading, "\n", T.pack body]

-- | Split markdown into @(heading, body)@ sections keyed by @##@ lines.
-- Leading text before the first @##@ is discarded, and each body stops at the
-- next @##@ heading (no overlap between sections).
splitSections :: String -> [(String, String)]
splitSections = map toSection . groupsByHeading . lines
  where
    toSection (h, body) = (h, unlines body)
    groupsByHeading ls =
      case break isHeading ls of
        (_, []) -> []
        (_, h : t) ->
          let (body, after) = span (not . isHeading) t
          in (h, body) : groupsByHeading after
    isHeading l = "## " `isPrefixOf` l
