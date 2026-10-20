module OpenSpecView.SpecDiff
  ( SpecName
  , SpecRequirement(..)
  , SpecDoc(..)
  , SpecChange(..)
  , parseSpecDoc
  , parseSpecFile
  , diffDocs
  , diffPaths
  , specChangeType
  , specChangeReq
  , renderSpecDiff
  ) where

import Data.Char (toLower)
import Data.List (intercalate, find)
import System.Directory (doesFileExist)

startsWith :: String -> String -> Bool
startsWith p s = take (length p) s == p

type SpecName = String

trim :: String -> String
trim = unwords . words

normalize :: String -> String
normalize = unwords . map (map toLower) . words

data SpecRequirement
  = SpecRequirement
      { reqId     :: String
      , reqTitle  :: String
      , reqBody   :: String
      } deriving (Eq, Show)

data SpecDoc
  = SpecDoc
      { docName       :: SpecName
      , docRequirements :: [SpecRequirement]
      } deriving (Eq, Show)

data SpecChange
  = Added   { addReq    :: SpecRequirement }
  | Modified { beforeReq :: SpecRequirement, afterReq :: SpecRequirement }
  | Removed { remReq    :: SpecRequirement }
  deriving (Eq, Show)

firstChar :: String -> Maybe Char
firstChar [] = Nothing
firstChar (c:_) = Just c

isHeading :: String -> Bool
isHeading s = case s of
  ('#':_) -> True
  _ -> False

isLevel1Heading :: String -> Bool
isLevel1Heading s = case s of
  ('#':xs) -> firstChar xs /= Just '#'
  _ -> False

reqHeadingTitle :: String -> Maybe String
reqHeadingTitle l =
  let t = trim l
      noHash = dropWhile (== '#') t
      rest = dropWhile (== ' ') noHash
  in if startsWith "Requirement:" rest
       then Just (trim $ drop 12 (dropWhile (== ' ') rest))
       else Nothing

extractName :: [String] -> String
extractName ls =
  case find isLevel1Heading ls of
    Just l ->
      let t = trim $ drop 1 l
      in if startsWith "Spec:" t
           then trim $ drop 5 t
           else t
    Nothing -> ""

extractRequirements :: [String] -> [SpecRequirement]
extractRequirements ls = numbered (go ls)
  where
    go [] = []
    go (l:rest) =
      case reqHeadingTitle l of
        Just title ->
          let (body, rest') = takeBody rest []
          in SpecRequirement "" title (trim (concat body)) : go rest'
        Nothing -> go rest
    numbered = zipWith (\i r -> r { reqId = "R" ++ show i }) ([1 ..] :: [Int])

takeBody :: [String] -> [String] -> ([String], [String])
takeBody (l:rest) acc
  | isHeading l = (reverse acc, l : rest)
  | otherwise = takeBody rest (l : acc)
takeBody [] acc = (reverse acc, [])

diffDocs :: SpecDoc -> SpecDoc -> [SpecChange]
diffDocs delta main =
  let norm r = normalize (reqTitle r)
      mainNorm = map (\r -> (normalize (reqTitle r), r)) (docRequirements main)
      matchedTitles = map norm (docRequirements delta)
      deltaChanges = concatMap step (docRequirements delta)
      removed = [Removed { remReq = r } | r <- docRequirements main, not (matchTitle r)]
      step d = case lookup (norm d) mainNorm of
        Nothing -> [Added { addReq = d }]
        Just m | normalize (reqBody d) == normalize (reqBody m) -> []
               | otherwise -> [Modified { beforeReq = m, afterReq = d }]
      matchTitle r = any ((==) (norm r)) matchedTitles
  in deltaChanges ++ removed

specChangeType :: SpecChange -> String
specChangeType Added{} = "added"
specChangeType Modified{} = "modified"
specChangeType Removed{} = "removed"

specChangeReq :: SpecChange -> SpecRequirement
specChangeReq c = case c of
  Added {addReq = r} -> r
  Modified {afterReq = r} -> r
  Removed {remReq = r} -> r

parseSpecDoc :: String -> SpecDoc
parseSpecDoc content = SpecDoc name reqs
  where
    ls = lines content
    name = extractName ls
    reqs = extractRequirements ls

parseSpecFile :: FilePath -> IO (Either String SpecDoc)
parseSpecFile path = do
  exists <- doesFileExist path
  if not exists
    then return (Left ("spec file not found: " ++ path))
    else do
      content <- readFile path
      return (Right (parseSpecDoc content))

diffPaths :: FilePath -> FilePath -> IO (Either String [SpecChange])
diffPaths deltaPath mainPath = do
  mDelta <- parseSpecFile deltaPath
  case mDelta of
    Left e -> return (Left e)
    Right docDelta -> do
      mMain <- parseSpecFile mainPath
      case mMain of
        Left e -> return (Left e)
        Right docMain -> pure (Right (diffDocs docDelta docMain))

renderSpecDiff :: SpecName -> [SpecChange] -> String
renderSpecDiff changeName changes =
  header ++ "\n" ++ statusLine ++ "\n" ++ body
  where
    header = "Spec Diff: " ++ changeName
    nAdded = length [() | Added{} <- changes]
    nMod = length [() | Modified{} <- changes]
    nRem = length [() | Removed{} <- changes]
    statusLine = "Summary: " ++ show nAdded ++ " added, " ++ show nMod ++ " modified, " ++ show nRem ++ " removed"
    body = intercalate "\n\n" (map renderOne changes)
    renderOne c = case c of
      Added {addReq = r} -> "++ [added] " ++ reqTitle r ++ "\n    " ++ firstLine (reqBody r)
      Modified {afterReq = a} -> "~ [modified] " ++ reqTitle a ++ "\n    " ++ firstLine (reqBody a)
      Removed {remReq = r} -> "-- [removed] " ++ reqTitle r

firstLine :: String -> String
firstLine s = case break (== '\n') s of
  (l, _) -> l
