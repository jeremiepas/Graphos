-- | openspec-view CLI
--
-- A dependency-free (base / directory / time only) surface over the
-- 'OpenSpecView' library. It exposes the four workspace-metadata capabilities
-- (change list, artifact viewer, spec diff, state dashboard) as subcommands.
-- Kept deliberately simple so it adds no new project dependencies and mirrors
-- the base-only constraint of the library it wraps.
module Main (main) where

import Control.Monad (forM, filterM)
import Data.Char (toLower)
import Data.List (intercalate, isPrefixOf, find)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import OpenSpecView
import OpenSpecView.ArtifactView
import OpenSpecView.ChangeList
import OpenSpecView.SpecDiff

-- Depth levels for the artifact viewer (T8).
data Depth = DSummary | DFull | DSections
  deriving (Eq, Show)

parseDepth :: String -> Either String Depth
parseDepth s = case map toLower s of
  "summary" -> pure DSummary
  "full" -> pure DFull
  "sections" -> pure DSections
  other -> Left ("invalid --depth: " ++ s)

-- Status filter for the change list (T9).
filterByStatus :: Maybe String -> [ChangeInfo] -> [ChangeInfo]
filterByStatus Nothing infos = infos
filterByStatus (Just s) infos = [ i | i <- infos, changeStatus i == s ]

data View = View
  { viewChangesRoot :: FilePath
  , viewSpecsRoot   :: FilePath
  }

defaultView :: View
defaultView = View { viewChangesRoot = "openspec/changes", viewSpecsRoot = "openspec/specs" }

-- Parsed command line.
data Parsed = Parsed
  { pView     :: View
  , pCommand  :: String
  , pChange   :: Maybe String
  , pDepth    :: Maybe String
  , pStatus   :: Maybe String
  }

-- Top-level entry: fold globals, then dispatch on the subcommand.
main :: IO ()
main = do
  raw <- getArgs
  if "--help" `elem` raw || "-h" `elem` raw
    then putStrLn helpText >> exitSuccess
    else case parseArgs raw of
      Left err -> hPutStrLn stderr err >> exitWith (ExitFailure 2)
      Right (Parsed { pView = v, pCommand = cmd, pChange = change,
                      pDepth = depth, pStatus = status }) -> dispatch v cmd change depth status

-- Usage text for `openspec view --help` (T11).
helpText :: String
helpText = unlines
  [ "openspec-view — structured view of the OpenSpec workspace state"
  , ""
  , "USAGE:"
  , "  openspec-view <changes|artifacts|diff|dashboard> [options]"
  , "  openspec-view --help"
  , ""
  , "GLOBAL OPTIONS:"
  , "  --changes-root <dir>   changes directory (default: openspec/changes)"
  , "  --specs-root   <dir>   main specs directory (default: openspec/specs)"
  , "  -h, --help             show this help text"
  , ""
  , "SUBCOMMANDS:"
  , "  changes [--status <state>]"
  , "      List all changes with status, task counts, and last-modified timestamp."
  , "      --status filters by in-progress|complete|archived|no-tasks."
  , "      Example: openspec-view changes --status in-progress"
  , ""
  , "  artifacts <change> --depth SUMMARY|FULL|SECTIONS"
  , "      Browse a change's proposal, specs, design, and tasks."
  , "      Example: openspec-view artifacts openspec-view --depth SUMMARY"
  , ""
  , "  diff <change>"
  , "      Compare the change's delta specs against the main specs under --specs-root."
  , "      Example: openspec-view diff openspec-view"
  , ""
  , "  dashboard [--status <state>]"
  , "      Aggregate per-change progress with risk highlighting."
  , "      Example: openspec-view dashboard"
  , ""
  , "DOCS:"
  , "  Full usage + OpenSpec conventions: openspec/changes/openspec-view/docs/"
  ]

dispatch :: View -> String -> Maybe String -> Maybe String -> Maybe String -> IO ()
dispatch v cmd change depth status = case cmd of
  "changes"   -> runChanges v status
  "dashboard" -> runDashboard v status
  "diff"      -> maybe (usageError "diff requires a CHANGE argument") (runDiff v) change
  "artifacts" -> runArtifactCmd v change depth
  _           -> usageError ("unknown subcommand: " ++ cmd)

runArtifactCmd :: View -> Maybe String -> Maybe String -> IO ()
runArtifactCmd v change depth = case change of
  Just c -> case depth of
    Just d -> case parseDepth d of
      Left err -> usageError err
      Right dd -> runArtifact v c dd
    Nothing -> usageError "artifacts requires --depth SUMMARY|FULL|SECTIONS"
  Nothing -> usageError "artifacts requires a CHANGE argument"

usageError :: String -> IO ()
usageError msg = hPutStrLn stderr (msg ++ "\n\nusage: openspec-view <changes|artifacts|diff|dashboard> [options]")
  >> exitWith (ExitFailure 2)

-- Parse arguments: pull out --changes-root / --specs-root globals, then collect
-- the positional CHANGE and the --depth / --status flag values.
parseArgs :: [String] -> Either String Parsed
parseArgs [] = Left "no subcommand given; use changes|artifacts|diff|dashboard"
parseArgs xs =
  let (view, rest) = foldGlobals defaultView xs
  in case rest of
       [] -> Left "missing subcommand"
       (c:ts) ->
          let (positional, flags) = collect ts
              change = singleChange positional
              status = lookupFlag "status" flags
              depth = lookupFlag "depth" flags
          in case c of
               "changes" -> mk view c change Nothing status
               "dashboard" -> mk view c Nothing Nothing status
               "diff" -> mk view c change Nothing Nothing
               "artifacts" -> mk view c change depth Nothing
               _ -> Left ("unknown subcommand: " ++ c)

mk :: View -> String -> Maybe String -> Maybe String -> Maybe String -> Either String Parsed
mk view cmd c d s = Right (Parsed { pView = view, pCommand = cmd, pChange = c, pDepth = d, pStatus = s })

foldGlobals :: View -> [String] -> (View, [String])
foldGlobals v [] = (v, [])
foldGlobals v ("--":as) = (v, as)
foldGlobals v ("--changes-root":dir:as) = foldGlobals (v { viewChangesRoot = dir }) as
foldGlobals v ("--specs-root":dir:as) = foldGlobals (v { viewSpecsRoot = dir }) as
foldGlobals v (a:as) =
  let (v', rest) = foldGlobals v as
  in (v', a : rest)

collect :: [String] -> ([String], [(String, String)])
collect = go [] []
  where
    go pos acc [] = (reverse pos, reverse acc)
    go pos acc (tok:ts) =
      if "--" `isPrefixOf` tok
        then case break (== '=') tok of
          (flag, rest) ->
            let fname = drop 2 flag in case stripEq rest of
              Just val -> go pos ((fname, val) : acc) ts
              Nothing -> case ts of
                (x:xs) -> go pos ((fname, x) : acc) xs
                [] -> go (fname : pos) acc []
        else go (tok : pos) acc ts
    stripEq ('=':val) = Just val
    stripEq ""        = Nothing
    stripEq _         = Nothing

lookupFlag :: String -> [(String, String)] -> Maybe String
lookupFlag name = fmap snd . find ((== name) . fst)

singleChange :: [String] -> Maybe String
singleChange [p] = Just p
singleChange _   = Nothing

runChanges :: View -> Maybe String -> IO ()
runChanges v status = do
  infos <- readAllChangeInfos (viewChangesRoot v)
  putStr (renderChangeList (filterByStatus status infos))

renderChangeList :: [ChangeInfo] -> String
renderChangeList infos =
  "OpenSpec Change List\n===================="
    ++ "\n"
    ++ intercalate "\n" (map renderRow infos)
    ++ "\nTotal: " ++ show (length infos) ++ " change(s)"
  where
    renderRow i =
      padLeft (changeName i) 38 ++ " "
        ++ padLeft (changeStatus i) 12 ++ " "
        ++ padLeft (show (changeCompleted i)) 4 ++ "/"
        ++ padLeft (show (changeTotal i)) 4 ++ "  "
        ++ maybe "(no mtime)" id (changeLastModified i)

padLeft :: String -> Int -> String
padLeft s n =
  let l = length s
      in if l >= n then s else replicate (n - l) ' ' ++ s

runDashboard :: View -> Maybe String -> IO ()
runDashboard v status = do
  infos <- readAllChangeInfos (viewChangesRoot v)
  putStr (renderDashboard (filterByStatus status infos))

runDiff :: View -> String -> IO ()
runDiff v change = do
  let changeDir = viewChangesRoot v </> change
      deltaDir = changeDir </> "specs"
  exists <- doesDirectoryExist deltaDir
  if not exists
    then hPutStrLn stderr ("no specs/ under " ++ changeDir) >> exitWith (ExitFailure 2)
    else do
      names <- listDirectory deltaDir
      kept <- filterM (\n -> doesFileExist (deltaDir </> n </> "spec.md")) names
      results <- forM (sortByName kept) (\n -> do
        mDiff <- diffPaths (deltaDir </> n </> "spec.md")
                           (viewSpecsRoot v </> n </> "spec.md")
        pure $ case mDiff of
          Left err -> "Spec Diff: " ++ change ++ " (" ++ n ++ ")\n  error: " ++ err
          Right cs -> "Spec Diff: " ++ change ++ " (" ++ n ++ ")\n"
                       ++ renderSpecDiff change cs)
      putStr (intercalate "\n\n" results)

sortByName :: [String] -> [String]
sortByName = go []
  where
    go acc [] = reverse acc
    go acc (x:xs) = go (x : acc) xs

runArtifact :: View -> String -> Depth -> IO ()
runArtifact v change depth = do
  ca <- readChangeArtifacts (viewChangesRoot v </> change)
  putStr (renderChangeArtifactsAt depth ca)

-- Artifact rendering across the three depth levels (T8).
renderChangeArtifactsAt :: Depth -> ChangeArtifacts -> String
renderChangeArtifactsAt DSummary ca =
  "OpenSpec Artifact Viewer\nChange: " ++ artifactsName ca
    ++ "\n"
    ++ intercalate "\n"
         [ statusLine "proposal.md" (changeProposal ca)
         , statusLine "design.md" (changeDesign ca)
         , statusLine "tasks.md" (changeTasks ca)
         , "Specs (" ++ show (length (changeSpecs ca)) ++ ")" ]
  where
    statusLine kind mca =
      case mca of
        Nothing -> "  - " ++ kind ++ " : pending"
        Just _ -> "  - " ++ kind ++ " : done"
renderChangeArtifactsAt DFull ca = renderChangeArtifacts ca
renderChangeArtifactsAt DSections ca =
  "OpenSpec Artifact Viewer\nChange: " ++ artifactsName ca
    ++ "\n\n"
    ++ intercalate "\n\n"
         [ sectionWrap "proposal.md" (changeProposal ca)
         , sectionWrap "design.md" (changeDesign ca)
         , sectionWrap "tasks.md" (changeTasks ca)
         , "Specs (" ++ show (length (changeSpecs ca)) ++ ")" ]

sectionWrap :: String -> Maybe ChangeArtifact -> String
sectionWrap kind mca =
  case mca of
    Nothing -> "== " ++ kind ++ " ==\n(missing)"
    Just a -> "== " ++ kind ++ " ==\n\n" ++ splitSections (artifactContent a)

-- Split markdown into top-level "## section" blocks for the sections depth.
splitSections :: String -> String
splitSections content =
  intercalate "\n\n" (map unlines (reverse (go (lines content) [] [])))
  where
    go [] cur acc = case cur of
      [] -> acc
      _ -> cur : acc
    go (l:ls) cur acc
      | isSectionHeading l =
          if null cur then go ls [l] acc else go ls [l] (cur : acc)
      | otherwise = go ls (cur ++ [l]) acc

isSectionHeading :: String -> Bool
isSectionHeading s = case s of
  ('#':'#':' ':rs) -> not (any (== '#') rs)
  _ -> False
