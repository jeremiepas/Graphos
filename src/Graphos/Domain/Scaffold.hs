{-# LANGUAGE StrictData #-}
module Graphos.Domain.Scaffold
  ( -- * Target types
    ScaffoldTarget(..)
  , allTargets
  , parseTarget
  , renderTarget

    -- * Scaffold file
  , ScaffoldFile(..)

    -- * Scaffold request
  , ScaffoldRequest(..)

    -- * Placeholder substitution
  , substitutePlaceholders
  , substituteOnce
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as T

data ScaffoldTarget
  = Opencode
  | Claude
  | Generic
  deriving (Eq, Show, Read, Enum, Bounded)

allTargets :: [ScaffoldTarget]
allTargets = [minBound .. maxBound]

renderTarget :: ScaffoldTarget -> Text
renderTarget Opencode = "opencode"
renderTarget Claude   = "claude"
renderTarget Generic  = "generic"

parseTarget :: Text -> Either Text ScaffoldTarget
parseTarget t = case T.toLower (T.strip t) of
  "opencode" -> Right Opencode
  "claude"   -> Right Claude
  "generic"  -> Right Generic
  other      -> Left $ "Unknown agent target: " <> other
                     <> ". Valid targets: opencode, claude, generic"

data ScaffoldFile = ScaffoldFile
  { sfRelativePath :: FilePath
  , sfContent      :: Text
  } deriving (Eq, Show)

data ScaffoldRequest = ScaffoldRequest
  { srTargets    :: NonEmpty ScaffoldTarget
  , srVersion    :: Text
  } deriving (Eq, Show)

substitutePlaceholders :: Text -> [(Text, Text)] -> Text
substitutePlaceholders = foldl substituteOnce

substituteOnce :: Text -> (Text, Text) -> Text
substituteOnce template (placeholder, value) =
  T.replace placeholder value template