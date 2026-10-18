-- OpenSpec-view CLI
--
-- Thin command-line front-end over the OpenSpecView library. Exposes four
-- subcommands that query the OpenSpec workspace on disk:
--
--   changes            list every change with status, task counts, timestamps
--   artifacts          render a change proposal/design/tasks/specs
--   diff               compare a change delta spec against the main spec
--   dashboard          aggregate state dashboard across all changes
--
-- None of this touches Graphos core; it only reads openspec/ and prints.
module Main where

import Control.Monad (unless)
import Data.List (intercalate, isPrefixOf)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (joinPath, splitPath)

import qualified OpenSpecView as V
import qualified OpenSpecView.ArtifactView as AV
import OpenSpecView.ArtifactView (ChangeArtifact(..), ChangeArtifacts(..))
import qualified OpenSpecView.ChangeList as CL
import qualified OpenSpecView.SpecDiff as SD
import qualified OpenSpecView.StateDashboard as D

type Args = [String]
type Opts = [(String, String)]

-- ─────────────────────────────────────────────────────────────────────────────
-- Path helpers (base-only; mirror System.FilePath semantics)
-- ─────────────────────────────────────────────────────────────────────────────

join :: FilePath -> FilePath -> FilePath
join a b
  | null a = b
  | last a == '/' = a ++ b
  | otherwise = a ++ "/" ++ b

parentOf :: FilePath -> FilePath
parentOf p = case reverse (splitPath p) of
  [] -> "."
  (_ : xs) -> joinPath (reverse xs)

-- ─────────────────────────────────────────────────────────────────────────────
-- Locating the workspace
-- ─────────────────────────────────────────────────────────────────────────────

-- | Walk up from the current directory until <dir>/openspec/changes is found.
workspaceChangesRoot :: IO FilePath
workspaceChangesRoot = go "."
  where
    go d = do
      let cand = join (join d "openspec") "changes"
      isDir <- doesDirectoryExist cand
      if isDir
        then pure cand
        else
          let parent = parentOf d
           in if parent == d
                then error "openspec/changes not found from current directory"
                else go parent

-- | The openspec/ directory itself (parent of <...>/openspec/changes).
openspecDir :: IO FilePath
openspecDir = do
  root <- workspaceChangesRoot
  pure (parentOf root)

-- ─────────────────────────────────────────────────────────────────────────────
-- Option extraction: split positional args from `--key value` options
-- ─────────────────────────────────────────────────────────────────────────────

splitOpts :: Args -> (Args, Opts)
splitOpts [] = ([], [])
splitOpts ("--" : rest) = (rest, [])
splitOpts (a : as)
  | "--" `isPrefixOf` a =
      let key = drop 2 a
       in case as of
            (v : rest') ->
              let (ps, o) = splitOpts rest'
               in (ps, (key, v) : o)
            [] -> ([], [(key, "")])
  | otherwise =
      let (ps, o) = splitOpts as
       in (a : ps, o)

optValue :: String -> Opts -> Maybe String
optValue key = lookup key

-- ─────────────────────────────────────────────────────────────────────────────
-- Usage
-- ─────────────────────────────────────────────────────────────────────────────

usage :: String
usage = unlines
  [ "openspec-view - query the OpenSpec workspace state"
  , ""
  , "Usage:"
  , "  openspec-view changes [--status <s>]"
  , "  openspec-view artifacts <name> [--depth <n>]"
  , "  openspec-view diff <change> <spec>"
  , "  openspec-view dashboard"
  , ""
  , "Commands:"
  , "  changes          list all changes with status, task counts, timestamps"
  , "  artifacts        render a change's proposal/design/tasks/specs"
  , "  diff             compare a change delta spec against the main spec"
  , "  dashboard        aggregate state dashboard across all changes"
  ]

-- ─────────────────────────────────────────────────────────────────────────────
-- main
-- ─────────────────────────────────────────────────────────────────────────────

main :: IO ()
main = do
  args <- getArgs
  let (pos, opts) = splitOpts args
  -- Accept an optional leading "view" token so both `openspec-view-cli changes`
  -- and `openspec-view-cli view changes` work.
  let subcmd = case pos of
        ("view" : rest) -> rest
        other -> other
  case subcmd of
    [] -> putStrLn usage >> exitSuccess
    ["--help"] -> putStrLn usage >> exitSuccess
    ["changes"] -> doChanges opts
    ["artifacts", name] -> doArtifacts opts name
    ["diff", change, spec] -> doDiff change spec
    ["dashboard"] -> doDashboard
    _ -> putStrLn usage >> exitFailure

-- ─────────────────────────────────────────────────────────────────────────────
-- Subcommands
-- ─────────────────────────────────────────────────────────────────────────────

doChanges :: Opts -> IO ()
doChanges opts = do
  root <- workspaceChangesRoot
  infos <- CL.readAllChangeInfos root
  let filtered = case optValue "status" opts of
        Just s -> filter ((== s) . V.changeStatus) infos
        Nothing -> infos
  putStrLn $ renderChangeList filtered

renderChangeList :: [V.ChangeInfo] -> String
renderChangeList = intercalate "\n" . map renderOne
  where
    renderOne c =
      unwords
        [ V.changeName c
        , V.changeStatus c
        , "(" ++ show (V.changeCompleted c) ++ "/" ++ show (V.changeTotal c) ++ ")"
        , maybe "<no timestamp>" id (V.changeLastModified c)
        ]

doArtifacts :: Opts -> String -> IO ()
doArtifacts opts name = do
  root <- workspaceChangesRoot
  let changeDir = join root name
  exists <- doesDirectoryExist changeDir
  unless exists $ do
    putStrLn $ "error: no change named '" ++ name ++ "'"
    exitFailure
  let depth = case optValue "depth" opts of
        Just s -> readDepth s
        Nothing -> Nothing
  ca <- AV.readChangeArtifacts changeDir
  putStrLn (renderArtifactsAt depth ca)

-- | Parse a non-negative integer depth; `Nothing` when absent or invalid.
readDepth :: String -> Maybe Int
readDepth s = case reads s of
  [(n, _)] | n >= 0 -> Just n
  _ -> Nothing

-- | A markdown heading is any line whose first non-space character is '#'.
isHeading :: String -> Bool
isHeading s =
  let s' = dropWhile (== ' ') s
   in case s' of
        ('#': _) -> True
        _ -> False

-- | Scope artifact content to a depth level:
--   0 = full, 1 = headings only, >=2 = heading + its section body.
scopeDepth :: Int -> String -> String
scopeDepth d content
  | d <= 0 = content
  | d == 1 = unlines (filter isHeading (lines content))
  | otherwise = unlines (scope (lines content))
  where
    scope [] = []
    scope (l : ls)
      | isHeading l = l : takeWhile (not . isHeading) ls ++ scope ls
      | otherwise = scope ls

-- | Render a change's artifacts, each scoped to the requested depth. Mirrors
-- | the library's own layout but truncates content per artifact by depth.
renderArtifactsAt :: Maybe Int -> ChangeArtifacts -> String
renderArtifactsAt depth ca =
  let d = maybe 0 id depth
      prop = renderSection "proposal.md" (changeProposal ca) d
      des = renderSection "design.md" (changeDesign ca) d
      tas = renderSection "tasks.md" (changeTasks ca) d
      spcs = renderSpecsSection (changeSpecs ca) d
   in "OpenSpec Artifact Viewer\nChange: " ++ artifactsName ca ++ "\n\n"
        ++ intercalate "\n\n" (filter (not . null) [prop, des, tas, spcs])

renderSection :: String -> Maybe ChangeArtifact -> Int -> String
renderSection kind mca d = case mca of
  Nothing -> "== " ++ kind ++ " ==\n(missing)"
  Just art -> "== " ++ kind ++ " ==\n" ++ scopeDepth d (artifactContent art)

renderSpecsSection :: [ChangeArtifact] -> Int -> String
renderSpecsSection arts d =
  "== Specs (" ++ show (length arts) ++ ") ==\n"
    ++ intercalate "\n" (map renderOne arts)
  where
    renderOne a = "== " ++ artifactKind a ++ " ==\n" ++ scopeDepth d (artifactContent a)

doDiff :: String -> String -> IO ()
doDiff change spec = do
  changesRoot <- workspaceChangesRoot
  specRoot <- openspecDir
  let deltaPath = join (join (join changesRoot change) "specs") (join spec "spec.md")
      mainPath = join (join specRoot "specs") (join spec "spec.md")
  result <- SD.diffPaths deltaPath mainPath
  case result of
    Left err -> do
      putStrLn $ "error: " ++ err
      exitFailure
    Right changes -> putStrLn (SD.renderSpecDiff (change ++ "/" ++ spec) changes)

doDashboard :: IO ()
doDashboard = do
  root <- workspaceChangesRoot
  ds <- D.showDashboard root
  putStrLn ds
