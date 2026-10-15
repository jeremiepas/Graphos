-- | Spec-diff capability for the @openspec-view@ workspace inspector.
--
-- Compares a change's delta spec against the corresponding main spec and
-- categorises every requirement as 'Added', 'Modified' or 'Removed', plus a
-- fidelity check that flags redundant no-op entries and deltas that reference a
-- missing main spec.
module Graphos.OpenSpecView.SpecDiff
  ( SpecRequirement(..)
  , SpecChangeType(..)
  , SpecChange(..)
  , parseRequirements
  , diffSpec
  , diffSpecSummary
  , validateFidelity
  ) where

import Data.List (isPrefixOf)
import qualified Data.Text as T
import Data.Text (Text)
import Prelude
  ( Bool(..), Eq, Show, Maybe(..), String
  , break, concatMap, drop, filter, length, lines
  , map, maybe, notElem, id, unlines, seq, (.), (==), (/=), (++), (<>) )

reqPrefix :: String
reqPrefix = "### Requirement: "

-- | A single requirement extracted from a spec file: its name (taken from the
-- @### Requirement:@ heading) and the raw markdown body that follows it.
data SpecRequirement = SpecRequirement
  { reqName :: Text
  , reqBody :: String
  } deriving (Show, Eq)

-- | How a requirement changed between the main spec and the delta spec.
data SpecChangeType = Added | Modified | Removed deriving (Show, Eq)

-- | One requirement-level change with its categorisation.
data SpecChange = SpecChange
  { changeReq :: SpecRequirement
  , changeType :: SpecChangeType
  } deriving (Show, Eq)

-- | Parse every @### Requirement:@ heading in a spec body into a list, keeping
-- the full body (including any @#### Scenario:@ blocks) up to the next heading.
parseRequirements :: String -> [SpecRequirement]
parseRequirements = go . lines
  where
    isReqHeading l = reqPrefix `isPrefixOf` l
    go ls =
      case break isReqHeading ls of
        (_, []) -> []
        (_, h : t) ->
          let (body, rest) = break isReqHeading t
          in SpecRequirement (T.pack (drop (length reqPrefix) h)) (unlines body) : go rest

-- | Diff a main spec (when present) against a delta spec. Results are ordered
-- added, then modified, then removed.
diffSpec :: Maybe [SpecRequirement] -> [SpecRequirement] -> [SpecChange]
diffSpec mMain delta = added ++ modified ++ removed
  where
    mains = maybe [] id mMain
    mainNames = map reqName mains
    deltaNames = map reqName delta
    added = [SpecChange r Added | r <- delta, reqName r `notElem` mainNames]
    modified =
      [SpecChange r Modified
       | r <- delta
       , let m = findReq (reqName r) mains
         , case m of Just mb -> reqBody r /= reqBody mb; Nothing -> False]
    removed = [SpecChange r Removed | r <- mains, reqName r `notElem` deltaNames]

-- | Render the diff grouped by category for display.
diffSpecSummary :: Maybe [SpecRequirement] -> [SpecRequirement] -> Text
diffSpecSummary mMain delta =
  T.intercalate "\n" (renderGroup "Added" added ++ renderGroup "Modified" modified ++ renderGroup "Removed" removed)
  where
    (added, modified, removed) =
      case diffSpec mMain delta of
        cs ->
          ( filter ((== Added) . changeType) cs
          , filter ((== Modified) . changeType) cs
          , filter ((== Removed) . changeType) cs )
    renderGroup label cs =
      (label <> ":") : map (\c -> "  - " <> reqName (changeReq c)) cs

-- | Validate delta fidelity: report redundant no-op entries and deltas that
-- reference a main spec that does not exist.
validateFidelity :: Maybe [SpecRequirement] -> [SpecRequirement] -> [(String, String)]
validateFidelity mMain delta =
  case mMain of
    Nothing -> [("missing-main", "delta references a main spec that does not exist")]
    Just mains -> concatMap check delta
      where
        check r =
          case findReq (reqName r) mains of
            Just mb | reqBody r == reqBody mb ->
              [((reqName r) `seq` "redundant", "present in main spec unchanged (no-op modification)")]
            _ -> []

-- | First requirement in a list matching a name, if any.
findReq :: Text -> [SpecRequirement] -> Maybe SpecRequirement
findReq n rs =
  case filter ((== n) . reqName) rs of
    (r : _) -> Just r
    [] -> Nothing
